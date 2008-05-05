/* This file is part of the nesC compiler.
   Copyright (C) 2002 UC Berkeley

The attached "nesC" software is provided to you under the terms and
conditions of the GNU General Public License Version 2 as published by the
Free Software Foundation.

nesC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with nesC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/*
  Author:   J. Robert von Behren <jrvb@cs.berkeley.edu>
  Created:  7/4/2002
  Modified: 7/8/2002

  This file contains routines for generating documentation from nesC
  source code.  The basic format for documentation is similar to
  Javadoc.  For specifics, look 


  
  Modified: 8/14/2002 by Phil Levis
  - Changed symlink code to unlink symlink before linking (else
    EEXIST occurs when repeating).
  - Removed color map elements -- default graphviz doesn't include
    language cmap.

  Modified 8/15/2002 by Phil Levis
  - Chase down pointer_declarators for functions properly now.

  Modified 8/21/2002 by Rob von Behren
  - add back cmap - requires newer graphviz, but makes for prettier HTML output

  
*/
#include <unistd.h>
#include <limits.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <dirent.h>
#include <errno.h>
#include <string.h>

#include "dhash.h"
#include "parser.h"
#include "c-parse.h"
#include "nesc-doc.h"
#include "nesc-component.h"
#include "nesc-semantics.h"
#include "nesc-c.h"
#include "unparse.h"
#include "AST_utils.h"
#include "edit.h"
#include "semantics.h"
#include "constants.h"
#include "sd_list.h"
#include "nesc-cg.h"
#include "graph.h"
#include "nesc-env.h"
#include "regions.h"
#include "unparse.h"
#include "errors.h"



static region doc_region = NULL;

#ifdef WIN32
#define mkdir(a, b) mkdir((a))
#endif

//////////////////////////////////////////////////////////////////////
//
//  file name munging
//
//////////////////////////////////////////////////////////////////////

// separator character for directory paths
static char dirsep = '/';
static char dirsep_string[2] = "/";

// "package name" to prepend to files in the current directory
static const char *currdir_prefix = NULL;

// full name of original working directory
static char original_wd[1024];

// directory info to strip from the generated docs file names
#define MAX_TOPDIRS 100
static const char *topdir[MAX_TOPDIRS];
static int num_topdirs = 0;

// output directory for generated docs
static const char *docdir = NULL;

// flag, to determine whether or not to use graphviz
static bool use_graphviz = FALSE;

// flag, to determine whether or not to generate the app pages
static bool is_app = FALSE;

/**
 * Initialize the memory region for the doc tools
 **/
static void init_doc_region() 
{
  if(doc_region == NULL) 
    doc_region = newregion();
}


/**
 * Set the graphviz flag
 **/
void doc_use_graphviz(const bool use)
{
  use_graphviz = use;
}


/**
 * Set the app flag
 **/
void doc_is_app(const bool val)
{
  is_app = val;
}


/**
 * Set the doc dir
 **/
void doc_set_outdir(const char *dir)
{
  assert(dir);
  docdir = dir;
}

/**
 * Add a top level source directory
 **/
void doc_add_topdir(const char *dir) 
{
  char realdir[PATH_MAX+1];
  char *temp;

  init_doc_region();

  assert(dir);
  if (num_topdirs >= MAX_TOPDIRS)
    fatal("ERROR: Too many `topdirs' (-fnesc-topdir) directories specified.\n");

  // canonicalize the path
  if(realpath(dir, realdir) == NULL) {
    perror("realpath");
    fatal("ERROR: Bad nesc-topdir option:\n      '%s'\n",dir);
  }
  temp = rstralloc( doc_region, strlen(realdir)+1 );
  assert( temp );
  strcpy(temp, realdir);

  topdir[num_topdirs] = temp;
  num_topdirs++;
}
     

/**
 * Set the directory separator - used for cygwin?
 **/
void doc_set_dirsep(const char c)
{
  dirsep = c;
  dirsep_string[0] = c;
}



/**
 * Initialize directory info.
 **/
static void find_currdir_prefix(const char *ocwd)
{
  int i;
  char *cwd = rstrdup(doc_region, ocwd);

  unixify_path(cwd);

  // filename generation won't work if we don't know the top directories.
  if( num_topdirs <= 0 ) {
    fatal("ERROR: Documentation generation requires specifying\n       -nesc-topdir arguments.\n");
  }

  // look for the topdir that is above the cwd.
  for(i=0; i<num_topdirs; i++) {
    if( !strncmp(topdir[i], cwd, strlen(topdir[i])) ) {
      currdir_prefix = cwd + strlen(topdir[i]);
      while( *currdir_prefix == dirsep )
        currdir_prefix++;
      break;
    }
  }

  if( !currdir_prefix ) {
    error( "ERROR: The current directory is not a subdir of one");
    error( "       of the source topdirs!  Please correct or add");
    error( "       the -nesc-topdir= options");
    error( "");
    error( "CWD: %s",cwd);
    error( "");
    error( "TOPDIRS:");
    for(i=0; i<num_topdirs; i++) {
      error( "     %s", topdir[i]);
    }
    exit(1);
  }
}


/**
 * generate a file name for the doc file, based on the source file
 * name.  This canonicalizes filenames based on the given top of the
 * source tree.  Documentation files are placed in a single directory
 * (no hierarchy), with hierarchies denoted by "." in the
 * filename. (Similar to javadoc)
 *
 * @assume There is no extraneous whitespace around the file name.
 * @assume All file names & path names are relative to the current working dir
 * @assume currdir_prefix and topdir neither begin, nor end with dirsep
 * @assume CWD == docdir
 *
 **/
static char *doc_filename_with_ext(const char *orig_src_filename, const char *ext) 
{
  char buf[PATH_MAX+1];
  char *src_filename;
  char *pos;
  char *ret;
  bool need_prefix = TRUE;
  int i;
  int length;

  // remove symlinks, etc.  We do this from the original working
  // directory, so that relative paths will be resolved correctly.
  assert(chdir(original_wd) == 0);
  if(realpath(orig_src_filename, buf) == NULL) {
    perror("realpath");
    fatal("error expanding path for '%s'\n", orig_src_filename);
  }
  unixify_path(buf);
  src_filename = buf;
  assert(chdir(docdir) == 0);

  // allocate max required space: docdir/currdir_prefix/src_filename.html
  length = 
    strlen(currdir_prefix) + 1 +
    strlen(src_filename) + 
    strlen(ext) +
    1; // for terminator
  ret = rstralloc( doc_region, length );
  assert(ret != NULL);

  memset(ret, 0, length);

  // file begins with the source prefix, so remove it.
  for(i=0; i<num_topdirs; i++) {
    if( !strncmp(topdir[i], src_filename, strlen(topdir[i])) ) {
      src_filename += strlen(topdir[i]);
      while(*src_filename == dirsep) 
        src_filename++;
      need_prefix = FALSE;
      break;
    }
  }

  // file is an absolute path, but not under a top dir
  if( need_prefix && absolute_path(src_filename) ) {
    while(*src_filename == dirsep) 
      src_filename++;
    need_prefix = FALSE;
  }

  // file is in the current directory, so prepend dir info
  if( need_prefix) {
    strcat(ret,currdir_prefix);
    strcat(ret,dirsep_string);
  }

  // add filename, and suffix
  strcat(ret,src_filename);
  strcat(ret,ext);

  // convert dirsep to "."
  pos = ret;
  while( *pos != '\0' ) {
    if( *pos == dirsep )
      *pos = '.';
#ifdef WIN32
    if (*pos == ':')
      *pos = 'X';
#endif
    pos++;
  }
  

  return ret;
}


static char *doc_filename(const char *src_filename) 
{
  return doc_filename_with_ext(src_filename, ".html");
}



struct location doc_empty_location = {"nofile",NULL,-1,0};


static char *interface_docfile_name(const char *interface_name) 
{
  nesc_declaration idecl;
  interface iface;

  idecl = require(l_interface, &doc_empty_location, interface_name);
  iface = CAST(interface, idecl->ast);

  assert(iface->location != dummy_location);

  return doc_filename(iface->location->filename);
}


static char *component_docfile_name(const char *component_name) {
  nesc_declaration cdecl;
  component comp;

  cdecl = require(l_component, NULL /* location */, component_name);
  comp = CAST(component, cdecl->ast);

  assert(comp->location != dummy_location);

  return doc_filename(comp->location->filename);
}


static bool copy_file(const char *srcfile, const char *destfile)
{
  char buf[1024 * 4];
  FILE *in = fopen(srcfile, "r"); 
  FILE *out = fopen(destfile, "w");
  size_t nread;
  size_t nwritten;

  if( !in ) {
    warning("Can't read from source file '%s'", srcfile);
    return FALSE;
  }
  if( !out ) {
    warning("Can't write to file '%s'", destfile);
    return FALSE;
  }

  
  while( !feof(in) ) {
    nread = fread(buf, 1, sizeof(buf), in);
    if( ferror(in) ) {
      warning("error copying '%s' to '%s'.  can't read source", srcfile, destfile);
      fclose(in); fclose(out); return FALSE;
    }
    assert( !ferror(in) );
    if(nread > 0) {
      nwritten = fwrite(buf, 1, nread, out);
      if( ferror(out) || nwritten != nread ) {
        warning("error copying '%s' to '%s'.  can't write dest", srcfile, destfile);
        fclose(in); fclose(out); return FALSE;
      }
    }
  }

  fclose(in);
  fclose(out);
  return TRUE;
}

static void add_source_symlink(const char *orig_src_filename, const char *linkname) 
{
#ifdef WIN32
  char buf[PATH_MAX+1];
  char *srcfile;

  assert(chdir(original_wd) == 0);
  if(realpath(orig_src_filename, buf) == NULL) {
    perror("realpath");
    fatal("error expanding path for '%s'\n", orig_src_filename);
  }
  srcfile = buf;
  assert(chdir(docdir) == 0);

  unlink(linkname);
  if( !copy_file(srcfile, linkname) ) {
    warning("can't copy source file '%s'", srcfile);
  }
#else
  bool cygwin = FALSE;
  char buf[PATH_MAX+1];
  char *srcfile;

  // remove symlinks, etc.  We do this from the original working
  // directory, so that relative paths will be resolved correctly.
  assert(chdir(original_wd) == 0);
  if(realpath(orig_src_filename, buf) == NULL) {
    perror("realpath");
    fatal("error expanding path for '%s'\n", orig_src_filename);
  }
  srcfile = buf;
  assert(chdir(docdir) == 0);

  // determine whether or not we are running under cygwin
  {
    char *ostype = getenv("OSTYPE");
    if(ostype != NULL  &&  !strcmp(ostype,"cygwin")) 
      cygwin = TRUE;
  }

  unlink(linkname);
  if( !cygwin ) {
    if(symlink(srcfile, linkname) != 0) {
      perror("symlink");
      warning("can't create symlink to source file %s",srcfile);
    }
  } else {
    if( !copy_file(srcfile, linkname) ) {
      warning("can't copy source file '%s'", srcfile);
    }
  }
#endif
}


//////////////////////////////////////////////////////////////////////
//
//  output functions
//
//////////////////////////////////////////////////////////////////////
static FILE *current_doc_outfile;

static FILE *open_outfile(const char *outfile) {

  unlink(outfile);
  
  current_doc_outfile = fopen(outfile,"w");
  if( !current_doc_outfile ) 
    fatal("can't write to output file '%s'",outfile);

  // set up unparse routines
  unparse_start(current_doc_outfile, NULL);
  disable_line_directives();
  set_function_separator(".");
  enable_documentation_mode();

  return current_doc_outfile;
} 

static void close_outfile(FILE *doc_outfile) {
  fclose(doc_outfile);
  unparse_end();
}

//////////////////////////////////////////////////////////////////////
//
//  External information cache
//
//////////////////////////////////////////////////////////////////////

typedef struct {
  char **comp;
  int num;
  int slots;
} implementor_list;

typedef struct {
  char *desc;
  bool is_iface;
  implementor_list r_list;
  implementor_list p_list;
} ic_entry;


static env ic_env;
static implementor_list ic_empty_implementor_list;

#if 0
static void ic_print()
{
  env_scanner scanner;
  const char *name;
  ic_entry *entry;
  char *desc, *pos;
  
  // scan throug
  env_scan(ic_env, &scanner);
  while( env_next(&scanner, &name, (void **)&entry) ) {
    assert(entry);
    desc = entry->desc;
    if(desc == NULL) desc = "";

    // kill weird whitespace in the description
    pos = desc;
    while( 1 ) {
      pos += strcspn(pos,"\r\n\t");
      if(*pos == '\0') break;
      *pos = ' ';
    }
    
    printf("%s %d %s\n",name, entry->is_iface, desc);
  }
  printf("\n\n");
}
#endif

static void ilist_add(implementor_list *list, char *name)
{
  int i;
  char *temp;
  
  // skip duplicates
  for(i=0; i<list->num; i++) {
    if(strcmp(name, list->comp[i]) == 0) return;
    if(strcmp(name, list->comp[i]) < 0) break;
  }

  // first allocation
  if(list->comp == NULL) {
    list->comp = rarrayextend(doc_region, NULL, 10, char*);
    list->slots = 10;
    list->num = 0;
  }

  // expand, if necessary
  if(list->num == list->slots) {
    list->comp = rarrayextend(doc_region, list->comp, list->slots+10, char*);
    list->slots += 10;
  }

  // add the item, and shift the rest up
  while( i <= list->num ) {
    temp = list->comp[i];
    list->comp[i] = name;
    name = temp;
    i++;
  }

  list->num++;
}

static ic_entry* ic_get_entry(char *key) 
{
  char *end;
  char save;
  ic_entry *entry;

  // remove extra suffix stuff from the key
  end = strstr(key,".nc");
  if(end == NULL) 
    return NULL;  // should never happen
  end += strlen(".nc");
  save = *end;
  *end = '\0';

  // check the index
  entry = env_lookup(ic_env, key, TRUE);
  *end = save;

  return entry;
}


static void ic_read() 
{
  FILE *f;
  char key[1024];
  int is_iface, nreq, nprov, i, ret;
  char desc[4096];
  ic_entry *entry;
  

  ic_env = new_env(doc_region, NULL);
  ic_empty_implementor_list.num = 0;

  // FIXME: we should really lock the file, so we don't get garbage if
  // there are multiple concurrent compiles

  // open the existing file, if any
  f = fopen("info.idx","r");
  if(f == NULL) return;

  // parse the file
  while( !feof(f) ) {
    key[0] = desc[0] = '\0';
    is_iface = nreq = nprov = 0;
    ret = fscanf(f, "%s%d%d%d%*[ ]%[^\n]\n", key, &is_iface, &nreq, &nprov, desc);
    if(ret == -1 && feof(f)) 
      break;
    if(ret != 4  &&  ret != 5) {
      warning("invalid data in info.idx - ignoring remainder of file\n");
      fclose(f);
      return;
    }

    entry = ic_get_entry(key);
    if(entry == NULL) {
      entry = ralloc(doc_region, ic_entry);
      memset(entry, 0, sizeof(ic_entry));
      env_add(ic_env, rstrdup(doc_region,key), entry);
    }

    if(strlen(desc) > 0)
      entry->desc = rstrdup(doc_region, desc);
    if(is_iface == 1)
      entry->is_iface = TRUE;

    // read interfaces
    for(i=0; i<nreq; i++) {
      desc[0] = '\0';
      ret = fscanf(f,"%s",desc);
      if(ret != 1  ||  desc[0] == '\0') {
        warning("invalid data in info.idx - ignoring remainder of file\n");
        fclose(f);
        return;
      }
      ilist_add(&(entry->r_list), rstrdup(doc_region,desc));
    }
    for(i=0; i<nprov; i++) {
      desc[0] = '\0';
      ret = fscanf(f,"%s",desc);
      if(ret != 1  ||  desc[0] == '\0') {
        warning("invalid data in info.idx - ignoring remainder of file\n");
        fclose(f);
        return;
      }
      ilist_add(&(entry->p_list), rstrdup(doc_region,desc));
    }
  }

  // close the file
  fclose(f);
}


static void ic_write()
{
  FILE *f;
  env_scanner scanner;
  const char *name;
  ic_entry *entry;
  char *desc, *pos;
  int i;

  // open the output file
  f = fopen("info.idx", "w");
  if(f == NULL) {
    warning("can't write to cache file 'info.idx'.\n");
    return;
  }

  // scan throug
  env_scan(ic_env, &scanner);
  while( env_next(&scanner, &name, (void **)&entry) ) {
    assert(entry);
    desc = entry->desc;
    if(desc == NULL) desc = "";

    // kill weird whitespace in the description
    pos = desc;
    while( 1 ) {
      pos += strcspn(pos,"\r\n\t");
      if(*pos == '\0') break;
      *pos = ' ';
    }
    
    // write the file
    fprintf(f,"%s %d %d %d %s\n", name, entry->is_iface?1:0, entry->r_list.num, entry->p_list.num, desc);

    // write interfaces
    for(i=0; i<entry->r_list.num; i++)
      fprintf(f, " %s", entry->r_list.comp[i]);
    for(i=0; i<entry->p_list.num; i++)
      fprintf(f, " %s", entry->p_list.comp[i]);
    if(entry->r_list.num > 0 || entry->p_list.num > 0)
      fprintf(f,"\n");
  }

  // close the file
  fclose(f);

  
  // FIXME: should unlock the file now
}


static char *ic_make_iface_key(const char *interface_name)
{
  nesc_declaration idecl;
  interface iface;

  idecl = require(l_interface, &doc_empty_location, interface_name);
  iface = CAST(interface, idecl->ast);

  return doc_filename_with_ext(iface->location->filename,"");
}

static void ic_scan_rplist(nesc_declaration cdecl, char *name) 
{
  component comp = CAST(component, cdecl->ast);
  declaration dlist, decl;

  interface_ref iref;
  ic_entry *entry;
  implementor_list *list;
  char *ifname;
  
  scan_declaration(dlist, comp->decls) {
    if (is_rp_interface(dlist)) {
      rp_interface rp = CAST(rp_interface, dlist);

      scan_declaration(decl, rp->decls) {
	if( is_interface_ref(decl) ) {
	  iref = CAST(interface_ref,decl);
	  ifname = ic_make_iface_key(iref->word1->cstring.data);
	} else {
	  continue;
	}

	entry = ic_get_entry( ifname );
	if(entry == NULL) {
	  entry = ralloc(doc_region, ic_entry);
	  memset(entry, 0, sizeof(ic_entry));
	  env_add(ic_env, ifname, entry);
	}
        
	// pick the list
	if( rp->required )
	  list = &(entry->r_list);
	else 
	  list = &(entry->p_list);
      
	// add to the list
	ilist_add(list, name);
      }
    }
  }
}




static void ic_add_entry(nesc_declaration cdecl) 
{
  ic_entry *entry;
  char *key;

  // create the local key
  {
    nesc_decl d = CAST(nesc_decl, cdecl->ast);
    key = doc_filename_with_ext(d->location->filename,"");
  }
  
  // get an existing entry
  entry = ic_get_entry(key);
  if(entry == NULL) {
    entry = ralloc(doc_region, ic_entry);
    memset(entry, 0, sizeof(ic_entry));
    env_add(ic_env, key, entry);
  }

  // fill in the entry
  if( cdecl->kind == l_interface )
    entry->is_iface = TRUE;
  else 
    entry->is_iface = FALSE;

  // docstring
  entry->desc = (char *)cdecl->doc.short_s;

  // implementor list
  if( cdecl->kind == l_component ) {
    ic_scan_rplist(cdecl, key);
  }
}

static char *ic_get_desc(char *key) 
{
  ic_entry *entry = ic_get_entry(key);
  if(entry == NULL) return NULL;
  return entry->desc;
}

static bool ic_is_iface(char *key)
{
  ic_entry *entry = ic_get_entry(key);
  if(entry == NULL) return FALSE;
  return entry->is_iface;
}


//////////////////////////////////////////////////////////////////////
//
//  Docstring command parsing
//
//////////////////////////////////////////////////////////////////////

/**
 * Try to figure out if a given '@' sign is actually part of an email
 * address.  If so, beg is assigned the position of the first
 * character of the address, and end is assigned the position of the
 * first whitespace character after the address.
 *
 * The heuristic that is used is as follows:
 *
 * 1.  The string has non-whitespace characters, ended by
 *     ".[a-zA-Z][a-zA-Z][a-zA-Z]? "
 *
 * 2.  The string has no other 
 * 
 **/
static bool check_email_address(const char *docstring, char *pos, char **beg, char **end)
{
  char *b, *e;
  char c;

  // find the beginning of the address
  b = pos-1;
  while( 1 ) {
    // beyond the start of the string
    if(b < docstring) break;

    // not a valid character, so quit
    c = *b;
    if( !((c >= 'a' && c <= 'z')  ||  (c >= 'A' && c <= 'Z')  ||  (c >= '0' && c <= '9')  
          ||  c == '_'  ||  c == '-'  ||  c == '.'))
      break;

    b--;
  }      
  b++;

  // find the end of the address
  e = pos+1;
  while( 1 ) {
    c = *e;
    
    // at the end of the string
    if( c == '\0' ) break;

    // not a valid character
    if( !((c >= 'a' && c <= 'z')  ||  (c >= 'A' && c <= 'Z')  ||  (c >= '0' && c <= '9')  
          ||  c == '_'  ||  c == '-'  ||  c == '.'))
      break;

    e++;
  }
  e--;
  
  if(b < pos  &&  e > pos) {
    *beg = b;
    *end = e;
    return TRUE;
  } else {
    return FALSE;
  }
}


typedef enum {
  in_main,
  in_return,
  in_param,
  in_author,
  in_version,
  in_modified,
  in_signals,
  in_see_also
} docstring_context;

static void output_docstring(char *docstring, location loc)
{
  char *pos = docstring;
  char *at, *space;
  int len;
  docstring_context context = in_main;
  static char *whitespace = " \t\r\n";

  while( 1 ) {
    // find the next @ directive
    at = strchr(pos,'@');
    
    // output the rest, if there are no more @ directives
    if(at == NULL) {
      output_string(pos);
      if(context == in_param) output("</menu></menu>\n");
      if(context != in_main) 
        output("</td></tr></table>\n");
      return;
    }

    // directives must be preceeded by whitespace or an open paren
    space = at-1;
    if(space >= docstring && (*space==' ' || *space=='\t' || *space=='\n' || *space=='\r' || *space=='(') )
    {
      // output up to the @
      *at='\0'; output_string(pos); *at='@';
      pos = at+1;
      
      // do some formatting, based on the command
      len = strcspn(pos, whitespace);
      
      if( len==strlen("return") && !strncasecmp("return",pos,len) ) {
        pos += len;
        if(context == in_param) output("</menu>\n");
        if(context == in_main) output("<table border=\"0\" cellpadding=\"0\">\n");
        else                   output("</td></tr>\n");

        if(context != in_return) {
          output("<tr valign=\"top\"><td><b>Returns:</b></td>\n<td>");
          context = in_return;
        } else {
          output("<tr><td>&nbsp;</td>\n<td>");
        }
      }
      
      else if( len==strlen("param") && !strncasecmp("param",pos,len) ) {
        pos += len;
      
        //if(context == in_param) output("</menu>\n");
        if(context == in_main) output("<table border=\"0\" cellpadding=\"0\">\n");

        if(context != in_param) {
          output("</td></tr>\n");
          output("<tr valign=\"top\"><td><br><b>Parameters:</b></td>\n<td><menu>");
          output("<p STYLE=\"text-indent: -1cm\">");
          context = in_param;
        } else {
          //output("<tr><td>&nbsp;</td>\n<td><menu><menu>");
          //output("<tr><td>&nbsp;</td>\n<td>");
          output("</p><p STYLE=\"text-indent: -1cm\">");
        }

        // print out the parameter name, plus a separator
        pos += strspn(pos, whitespace);
        len = strcspn(pos, whitespace);
        // Null terminate the name
        *(pos + len) = '\0';

        //output("<p STYLE=\"text-indent: -1cm\">");
        output("<b>%*s</b>",len,pos);
        output(" - ");
        // Restore to spaced text.
        *(pos + len) = ' ';
        pos += len;
      }

      else if( len==strlen("author") && !strncasecmp("author",pos,len) ) {
        pos += len;

        if(context == in_param) output("</menu>\n");
        if(context == in_main) output("<table border=\"0\" cellpadding=\"0\">\n");
        else                   output("</td></tr>\n");

        if(context != in_author) {
          output("<tr valign=\"top\"><td><b>Author:</b>\n<td>");
          context = in_author;
        } else {
          output("<tr valign=\"top\"><td>&nbsp;</td>\n<td>");
        }
      }

      else if( len==strlen("version") && !strncasecmp("version",pos,len) ) {
        pos += len;

        if(context == in_param) output("</menu>\n");
        if(context == in_main) output("<table border=\"0\" cellpadding=\"0\">\n");
        else                   output("</td></tr>\n");

        if(context != in_version) {
          output("<tr valign=\"top\"><td><b>Version:</b>\n<td>");
          context = in_version;
        } else {
          output("<tr valign=\"top\"><td>&nbsp;</td>\n<td>");
        }
      }

      else if( len==strlen("modified") && !strncasecmp("modified",pos,len) ) {
        pos += len;

        if(context == in_param) output("</menu>\n");
        if(context == in_main) output("<table border=\"0\" cellpadding=\"0\">\n");
        else                   output("</td></tr>\n");

        if(context != in_modified) {
          output("<tr valign=\"top\"><td><b>Modified:</b>\n<td>");
          context = in_modified;
        } else {
          output("<tr valign=\"top\"><td>&nbsp;</td>\n<td>");
        }
      }

      else if( len==strlen("signals") && !strncasecmp("signals",pos,len) ) {
        pos += len;

        if(context == in_param) output("</menu>\n");
        if(context == in_main) output("<table border=\"0\" cellpadding=\"0\">\n");
        else                   output("</td></tr>\n");

        if(context != in_signals) {
          output("<tr valign=\"top\"><td><b>Events signaled:</b>\n<td>");
          context = in_signals;
        } else {
          output("<tr valign=\"top\"><td>&nbsp;</td>\n<td>");
        }

        // FIXME: should print warnings for things (a) not mentioned
        // or (b) extraneously mentioned

        // FIXME: should add hrefs to the events (?)
      }

      else if( len==strlen("see") && !strncasecmp("see",pos,len) ) {
        pos += len;

        if(context == in_param) output("</menu>\n");
        if(context == in_main) output("<table border=\"0\" cellpadding=\"0\">\n");
        else                   output("</td></tr>\n");

        if(context != in_see_also) {
          output("<tr valign=\"top\"><td><b>See also:</b>\n<td>");
          context = in_see_also;
        } else {
          output("<tr valign=\"top\"><td>&nbsp;</td>\n<td>");
        }

        // FIXME: should add hrefs when possible
      }

      // output a warning, and print the bogus directive as-is
      else {
        warning("%s:%ld:  Unknown documentation directive '@%.*s'\n",
                loc->filename, loc->lineno, len, pos);
        output("@");
      }
    }

    // found an @ that isn't preceded by whitespace or '('.  Don't print a warning.
    else {
      char *begin, *end;
      char save;

      // do a quick check to see if it might be an email address, and format it nicely if so.
      if( check_email_address(docstring, at, &begin, &end) ) {
        // output to the beginning of the address
        save=*begin; *begin='\0'; output_string(pos); *begin=save;

        // print the mailto: URL and the address
        end++;
        save=*end; *end='\0'; 
        output("<a href=\"mailto:%s\">%s</a>",begin,begin);
        *end = save;

        // set the position
        pos=end;
      }

      // not an email address, just output normally
      else {
        *at='\0'; output_string(pos); *at='@';
        pos = at+1;
        output("@");
      }

    }
    

  }

}




//////////////////////////////////////////////////////////////////////
//
// HTML generation functions
//
//////////////////////////////////////////////////////////////////////

typedef enum {
  NAV_APP,
  NAV_COMP,
  NAV_INT,
  NAV_ALL,
  NAV_TREE,
  NAV_OTHER
} navbar_mode;

static void print_navbar(FILE *f, navbar_mode mode, const char *srcfile, const char *srctext) 
{
  char *spaces = "&nbsp;&nbsp;&nbsp;";


  if(srcfile) {
    fprintf(f, "<table BORDER=\"0\" CELLPADDING=\"3\" CELLSPACING=\"0\" width=\"100%%\">\n");
    fprintf(f, "<tr><td>\n");
  }

  fprintf(f,"<font size=\"-1\">\n");

  if(mode == NAV_APP) fprintf(f,"Apps\n");
  else fprintf(f,"<b><font color=\"blue\"><a href=\"apps_p.html\">Apps</a></font></b>\n");
  fprintf(f, "%s\n", spaces);

  if(mode == NAV_COMP) fprintf(f,"Components\n");
  else fprintf(f, "<b><a href=\"components_p.html\">Components</a></b>\n");
  fprintf(f, "%s\n", spaces);

  if(mode == NAV_INT) fprintf(f,"Interfaces\n");
  else fprintf(f, "<b><a href=\"interfaces_p.html\">Interfaces</a></b>\n");
  fprintf(f, "%s\n", spaces);

  if(mode == NAV_ALL) fprintf(f,"All Files\n");
  else fprintf(f, "<b><a href=\"allfiles_p.html\">All Files</a></b>\n");
  fprintf(f, "%s\n", spaces);

  if(mode == NAV_TREE) fprintf(f,"Source Tree\n");
  else fprintf(f, "<b><a href=\"index.html\">Source Tree</a></b>\n");
  fprintf(f, "%s\n", spaces);

  fprintf(f, "</font>\n");

  if(srcfile) {
    fprintf(f,"</td>\n");
    fprintf(f,"<td align=\"right\">\n");
    fprintf(f,"<font size=\"-1\">\n");
    fprintf(f,"source: <b><a href=\"%s\">%s</a></b>\n", srcfile, srctext);
    fprintf(f, "</font>\n");
    fprintf(f,"</td></tr></table>\n");
  }
  
  fprintf(f, "<hr>\n");
}


static void print_short_variable_html(data_decl ddecl, variable_decl vd) {
  output("<li>   ");
  //prt_declarator(d->declarator, d->qualifiers, d->attributes, d->ddecl, psd_skip_container);


  // FIXME: did this fix the variable declaration printing?
  prt_type_elements(ddecl->modifiers, 0); 

  prt_declarator(vd->declarator, NULL, vd->attributes, vd->ddecl, psd_skip_container);

  // NOTE: these lines copied from unparse.c - not sure I want/need them.
  {
    //if (d->asm_stmt)
    //  prt_asm_stmt_plain(d->asm_stmt);

    if (vd->arg1) {
      output(" = ");
      prt_expression(vd->arg1, P_ASSIGN);
    }
  }
  output("\n");

  // show the description, if any 
  if( vd->ddecl->doc.short_s ) { 
    output("        <br><menu>");
    output_docstring((char *)vd->ddecl->doc.short_s, vd->ddecl->doc.loc);
    output("</menu>\n");
  }
  
}


static inline void check_print_func_args(function_decl fd, data_decl dd, variable_decl vd,
                                         function_declarator *fdr, data_declaration *ddecl) {
  if( fd ) {
    assert(dd==NULL);
    assert(vd==NULL);
    if(fdr) *fdr = CAST(function_declarator, fd->fdeclarator);
    if(ddecl) *ddecl = fd->ddecl;
  }
  else {
    assert(fd==NULL);
    assert(dd);    
    assert(vd);
    if (is_function_declarator(vd->declarator)) {
      if(fdr) *fdr = CAST(function_declarator, vd->declarator);
      if(ddecl) *ddecl = vd->ddecl;
    }
    // Chase down pointers if necessary.
    else if (is_pointer_declarator(vd->declarator)) {
      function_declarator fdcl = get_fdeclarator(vd->declarator);
      if(fdr) *fdr = fdcl;
      if(ddecl) *ddecl = vd->ddecl;
    }
    else {
      if(ddecl) *ddecl = vd->ddecl;
    }
  }
}

/**
 * print function return type & modifiers
 **/
static void print_func_return(function_decl fd, data_decl dd, variable_decl vd) 
{
  check_print_func_args(fd, dd, vd, NULL, NULL);
  if(fd) {
    prt_declarator(NULL, fd->modifiers, fd->attributes, fd->ddecl, psd_skip_container);
  } else {
    prt_type_elements(dd->modifiers, psd_skip_command_event); 
  }
}

/**
 * print function name
 **/
static void print_func_name(function_decl fd, data_decl dd, variable_decl vd) 
{
  function_declarator fdr;
  data_declaration ddecl; 

  check_print_func_args(fd, dd, vd, &fdr, &ddecl);
  prt_simple_declarator(fdr->declarator, ddecl, psd_skip_container | psd_need_paren_for_star | psd_need_paren_for_qual);
  
}

/**
 * print function arguments
 **/
// FIXME: this should allow an option to print only arg types, and not
// names - necessary to canonicalize anchors and hrefs.
static void print_func_args(function_decl fd, data_decl dd, variable_decl vd) 
{
  function_declarator fdr;
  data_declaration ddecl; 

  check_print_func_args(fd, dd, vd, &fdr, &ddecl);
  prt_parameters(fdr->gparms ? fdr->gparms :
                 ddecl ? ddecl_get_gparms(ddecl) : NULL,
                 fdr->parms,
                 psd_skip_container);
  
}


/**
 * print the entire header for a function - return name(args)
 **/
static void print_function_header(function_decl fd, data_decl dd, variable_decl vd) 
{
  print_func_return(fd, dd, vd);
  print_func_name(fd, dd, vd);
  print_func_args(fd, dd, vd);
}


/**
 * Check to see if a function has a long description
 **/
static bool has_long_desc(function_decl fd, data_decl dd, variable_decl vd) 
{
  if( fd ) {
    return (fd->ddecl->doc.long_s != NULL);
  } else {
    return (vd->ddecl->doc.long_s != NULL);
  }
}


/**
 * generate the text for a stand-alone function declaration.  This
 * occurs when functions are listed on requires/provides lines in
 * components, or in uses/defines lines in interfaces.
 *
 * FIXME: For "provides", we should really use the actual declaration
 * of the function, since that will have the descriptive text.  For
 * "requires", there probably won't be any(?)
 *
 * NOTE: for interfaces, the descriptive text should be with this
 * variable_decl (there's nowhere else for it to be).
 **/
typedef enum {
  short_desc   = 0x0001,
  long_desc    = 0,

  in_component = 0x0002,
  in_interface = 0,
} function_flag;

static void print_function_html(function_decl fd, data_decl dd, variable_decl vd, function_flag flags) 
{
  char *sdoc = NULL;
  char *ldoc = NULL;
  char *ifile = NULL;
  location loc = NULL;

  // set up the description pointers
  if( fd ) {
    sdoc = (char *)fd->ddecl->doc.short_s;
    ldoc = (char *)fd->ddecl->doc.long_s;
    loc  = fd->ddecl->doc.loc;
  } else {
    sdoc = (char *)vd->ddecl->doc.short_s;
    ldoc = (char *)vd->ddecl->doc.long_s;
    loc  = vd->ddecl->doc.loc;
  }


  // if we're not in the interface, fetch the name of the interface file, for external refs
  if( flags & in_component ) {
    if( fd ) {
      if( fd->ddecl->interface )
        ifile = interface_docfile_name(fd->ddecl->interface->itype->name);
    } else {
      // NOTE: if( !in_interface && !fd ), then this is a function
      // from a requires/provides line in a component.  In this case,
      // there is no interface anyway, so we should leave this blank.
    }
  }

  // short desc: if there is a long description, link to it.  Otherwise, link to the interface
  if( flags & short_desc ) {
    output("<li>\n");


    output("        ");  print_func_return(fd,dd,vd); 

    if(ldoc)  {output("    <a href=\"#"); print_function_header(fd,dd,vd); output("\">\n");}
    else if(ifile) {output("    <a href=\"%s#",ifile); print_function_header(fd,dd,vd); output("\">\n");}
    output("<b>"); print_func_name(fd,dd,vd); output("</b>");
    if(ldoc || ifile)  output("    </a>\n");

    print_func_args(fd,dd,vd); output("\n"); 

    if(sdoc) {
      output("<menu>");
      output_docstring(sdoc,loc);
      output("</menu><p>\n");
    }
    output("\n");
  }

  // long desc: print the anchor.  if we're in the component file, also link to the interface.
  else {
    output("    <a name=\""); print_function_header(fd,dd,vd); output("\"></a>\n");

    output("        <h4>"); print_func_name(fd,dd,vd);  output("</h4>\n");

    output("        "); print_func_return(fd,dd,vd);  
    if(ifile) {output("    <a href=\"%s#",ifile); print_function_header(fd,dd,vd); output("\">");}
    output("<b>"); print_func_name(fd,dd,vd); output("</b>");
    if(ifile) output("    </a>\n");
    print_func_args(fd,dd,vd);

    assert(ldoc);  // should have checked in the caller
    output("<P><menu>");
    output_docstring(ldoc,loc);
    output("</menu>");
    //output("<p><hr>\n");
  }

}



/**
 * Print a nicer looking HTML banner
 **/
static inline void start_html_banner() {
  output("<table BORDER=\"1\" CELLPADDING=\"3\" CELLSPACING=\"0\" WIDTH=\"100%%\">\n");
  output("<tr BGCOLOR=\"#CCCCFF\"><td>\n");
}

static inline void end_html_banner() {
  output("</td></tr></table>\n");
}
static void print_html_banner(const char *text) {
  start_html_banner();
  output("%s",text);
  end_html_banner();
}


//////////////////////////////////////////////////////////////////////
//
//  Connection graph functions
//
//////////////////////////////////////////////////////////////////////



//////////////////////////////////////////////////
// tools for checking uniqueness of edges 
// in the interface connection graph
//////////////////////////////////////////////////

typedef struct iface_graph_entry {
  endp req;
  endp prov;
} *iface_graph_entry;

#define iface_node_name( ep ) \
                    ( ep->component ? \
                      ep->component->ctype->name : \
                      ( ep->function->container ? \
                        ep->function->container->name : \
                        ep->function->ast->location->filename))

static int iface_graph_compare(void *entry1, void *entry2) 
{
  int ret;
  iface_graph_entry e1 = (iface_graph_entry) entry1;
  iface_graph_entry e2 = (iface_graph_entry) entry2;

  ret = strcmp(iface_node_name(e1->req), iface_node_name(e2->req));
  if(ret) return 0;

  ret = strcmp(iface_node_name(e1->prov), iface_node_name(e2->prov));
  if(ret) return 0;
  
  ret = strcmp(e1->req->interface->name, e2->req->interface->name);
  return !ret;
}

static unsigned long iface_graph_hash(void *entry) 
{
  unsigned long rhash, phash, ihash;
  iface_graph_entry e = (iface_graph_entry) entry;

  assert( iface_node_name(e->req) );
  assert( iface_node_name(e->prov) );
  assert( e->req->interface->name );

  rhash = hash_str( iface_node_name(e->req) );
  phash = hash_str( iface_node_name(e->prov) );
  ihash = hash_str( e->req->interface->name );

  return rhash ^ (phash<<1) ^ ihash;
}




static dhash_table new_iface_graph_table() {
  return new_dhash_table(newregion(), 10, iface_graph_compare, iface_graph_hash);
}





//static int fixme_graph_num = 1;

static bool connection_already_printed(dhash_table table, 
                                endp ep1, endp ep2, 
                                endp *req, endp *prov)
{
  // FIXME: kludge, to try to get things working with parameterized interfaces
  if(!ep1->component && !ep1->interface && ep1->function->interface)
    ep1->interface = ep1->function->interface;
  if(!ep2->component && !ep2->interface && ep2->function->interface)
    ep2->interface = ep2->function->interface;


  // sort out which is the "requires" side, and which is the "provides" side
  // For interfaces: ep1 is req, ep2 is prov if connecting a command,
  // 		     the other way round if connecting an event
  // For functions: the direction in the graph is always the one we want
  if (ep1->interface)
    {
      if (ep1->function->ftype == function_command)
	{
	  *req = ep1;
	  *prov = ep2;
	}
      else
	{
	  *req = ep2;
	  *prov = ep1;
	}
    }
  else
    {
      *req = ep1;
      *prov = ep2;
    }

  // special case: if the interface is empty, we always show the connection
  if((*prov)->interface == NULL) {
    assert( (*req)->interface == NULL );
    return FALSE;
  }

  // special case: skip loopback edges, created by parameterized interfaces
  if( !strcmp(iface_node_name(ep1),iface_node_name(ep2)) ) {
    return TRUE;
  }

  // see if this one has already been printed.  This is done by checking a hashtable for the tripple of req,prov,iface
  {
    // create a table entry
    iface_graph_entry e = ralloc(regionof(table), struct iface_graph_entry);
    e->req = *req;
    e->prov = *prov;

    // do the lookup
    if( dhlookup(table,e) )
      return TRUE;

    // add the new item to the table
    dhadd(table, e);
    return FALSE;
  }

}



/**
 * Display the nodes in the graph, along w/ info on edges
 **/
static void print_cg_html(const char *component_name, const char *component_file_name, cgraph cg, bool app_graph) {
  gnode n;
  dhash_table table = NULL;
  char *iface_dot, *func_dot;
  char *iface_gif, *func_gif;
  char *iface_cmap, *func_cmap;
  FILE *iface_file, *func_file;

  char *text_only_name;
  FILE *text_file;

  bool do_func_graph = FALSE;  // FIXME: disable the function graph for now


  // FIXME: increment graph num for debugging
  /*
  fixme_graph_num++;
  fprintf(stderr, "%d.\n", fixme_graph_num);
  */

  // create filenames
  if( app_graph ) {
    iface_dot = doc_filename_with_ext(component_file_name,".app.if.dot");
    iface_gif = doc_filename_with_ext(component_file_name,".app.if.gif");
    iface_cmap = doc_filename_with_ext(component_file_name,".app.if.cmap");
  } else {
    iface_dot = doc_filename_with_ext(component_file_name,".if.dot");
    iface_gif = doc_filename_with_ext(component_file_name,".if.gif");
    iface_cmap = doc_filename_with_ext(component_file_name,".if.cmap");
  }

  if( do_func_graph ) {
    if( app_graph ) {
      func_dot = doc_filename_with_ext(component_file_name,".app.func.dot");
      func_gif = doc_filename_with_ext(component_file_name,".app.func.gif");
      func_cmap = doc_filename_with_ext(component_file_name,".app.func.cmap");
    } else {
      func_dot = doc_filename_with_ext(component_file_name,".func.dot");
      func_gif = doc_filename_with_ext(component_file_name,".func.gif");
      func_cmap = doc_filename_with_ext(component_file_name,".func.cmap");
    }
  }

  text_only_name = doc_filename_with_ext(component_file_name,".text.html");


  // start the text output
  {
    text_file  = fopen(text_only_name, "w");  
    if( !text_file ) fatal("can't write text connection graph file '%s'", text_file);

    // print some additional HTML stuff, if we are linking in to this file externally
    if( use_graphviz ) {
      fprintf(text_file, "<html>\n");
      fprintf(text_file, "<head><title>Text Connection Graph: %s</title></head>\n", component_name);
      fprintf(text_file, "<body>\n");
      fprintf(text_file, "<h1 align=\"center\">Text Connection Graph: %s</h1>\n", component_name);
      fprintf(text_file, "\n");
    }

    // start the output table
    fprintf(text_file, "<center>\n<table border=0 cellpadding=2>\n");
  }

  // start the dot output
  if( use_graphviz ) {
    char *graphviz_opts = "\n\
    rankdir=LR;\n\
    ratio=compress;\n\
    margin=\"0,0\";\n\
    ranksep=0.0005; \n\
    nodesep=0.1; \n\
    node [shape=ellipse style=filled fillcolor=\"#e0e0e0\"];\n\
    node [fontsize=10 height=.1 width=.1];\n\
    edge [fontsize=9 arrowsize=.8];\n\
";

    iface_file = fopen(iface_dot, "w");  
    if( !iface_file ) fatal("can't write to dot file '%s'", iface_dot);
    fprintf(iface_file, "digraph \"%s_if\" {%s", component_name, graphviz_opts);
    fprintf(iface_file, "    node [fontcolor=blue];\n");
    fprintf(iface_file, "    edge [fontcolor=blue];\n");
    fprintf(iface_file, "\n");

    if( do_func_graph ) {
      func_file  = fopen(func_dot,  "w");  assert(func_file);
      if( !func_file ) fatal("can't write to dot file '%s'", func_dot);
      fprintf(func_file, "digraph \"%s_func\" {%s\n\n", component_name, graphviz_opts);
      fprintf(func_file, "    node [fontcolor=blue];\n");
      fprintf(func_file, "    edge [fontcolor=blue];\n");
      fprintf(func_file, "\n");
    }

  }
  

  // examine connections
  graph_scan_nodes(n, cgraph_graph(cg)) 
    {
      gedge e;
      endp ep1, ep2;
      ep1 = NODE_GET(endp, n);

      // out edges
      graph_scan_out(e,n) {
        ep2 = NODE_GET(endp, graph_edge_to(e));
        // assertions already done above

        // connection graph - all functions  (input for /usr/bin/dot)
        if( do_func_graph ) 
        {
          // graphviz stuff
          if( use_graphviz ) 
          {
            // FIXME: print the node info (ie URLs)

            // print the edge info
            fprintf(func_file, "    %s -> %s [label = \"%s.%s\"];\n", 
                    ep1->component ? ep1->component->ctype->name : "C_code", 
                    ep2->component ? ep2->component->ctype->name : "C_code", 
                    ep2->interface ? ep2->interface->itype->name : "C_code", 
                    ep2->function->name);
          }

          // text stuff
          {
            // FIXME: should we bother with text function graph?
          }
        }

        // connection graph - interfaces  (input for /usr/bin/dot)
        {
          endp req, prov;
         
          // allocate a new hashtable for removing duplicates
          if(table == NULL) {
            table = new_iface_graph_table();
          }
 
          if( !connection_already_printed(table, ep1, ep2, &req, &prov) ) 
          {
            // graphviz stuff
            if( use_graphviz )
            {
              // print the node info (ie URLs)
              fprintf(iface_file, "    %s [URL=\"%s\"];\n", 
                      iface_node_name(req),
                      component_docfile_name( iface_node_name(req) ));
              fprintf(iface_file, "    %s [URL=\"%s\"];\n", 
                      iface_node_name(prov),
                      component_docfile_name( iface_node_name(prov) ));
              
              // edge info
              fprintf(iface_file, "    %s -> %s [",
                        iface_node_name( req ),
                        iface_node_name( prov ));
              if(req->interface) {
                if(prov->interface && req->interface->required == prov->interface->required) 
                  fprintf(iface_file, " style=dashed"); 
                fprintf(iface_file, " label=\"%s\" URL=\"%s\"", 
                        prov->interface->itype->name,
                        interface_docfile_name(prov->interface->itype->name));
              } else {
                fprintf(iface_file, " label=\"func:%s\" fontcolor=black", prov->function->name);
              }
              fprintf(iface_file, " ];\n");
            }
            
            // text stuff
            //
            // FIXME: probably better to sort these in some nice way.
            // Perhaps a topological ordering startig from the main
            // component, with a breadth-first display?
            {
              // row start
              fprintf(text_file, "<tr>\n");

              // requires side
              fprintf(text_file, "    <td align=\"right\"><a href=\"%s\">%s</a>.",
                      component_docfile_name( iface_node_name(req) ),
                      iface_node_name(req));
              if(req->interface)
                fprintf(text_file, "<a href=\"%s\">%s</a>",
                        interface_docfile_name(req->interface->itype->name),
                        req->interface->itype->name);
              else 
                fprintf(text_file, "%s",req->function->name);
              fprintf(text_file, "</td>\n");

              // arrow
              if(prov->interface && req->interface->required == prov->interface->required) 
                fprintf(text_file, "    <td align=\"center\">&nbsp;=&nbsp;</td>\n");
              else 
                fprintf(text_file, "    <td align=\"center\">&nbsp;->&nbsp;</td>\n");

              // provides side
              fprintf(text_file, "    <td align=\"left\"><a href=\"%s\">%s</a>.",
                      component_docfile_name( iface_node_name(prov) ),
                      iface_node_name(prov));
              if(req->interface)
                fprintf(text_file, "<a href=\"%s\">%s</a>",
                        interface_docfile_name(prov->interface->itype->name),
                        prov->interface->itype->name);
              else 
                fprintf(text_file, "%s",prov->function->name);
              fprintf(text_file, "</td>\n");

              // row end
              fprintf(text_file, "</tr>\n");
            }

          }
        }


      }
    }

  // clean up the region 
  if(table) 
    deleteregion( regionof(table) );


  // finish up the graphviz output
  if( use_graphviz ) {
    // finish if file
    fprintf(iface_file, "}\n");  
    fclose(iface_file);


    // finish func file
    if( do_func_graph ) {
      fprintf(func_file, "}\n");  
      fclose(func_file);
    }

  }

  // finish up the text output
  {
    fprintf(text_file, "</table>\n</center>\n\n");
    fprintf(text_file, "<hr>Generated by <a href=\"../../../nesc/doc/nesdoc.html\">nesdoc</a><br>\n");
    if( use_graphviz ) {
      fprintf(text_file, "</body>\n");
      fprintf(text_file, "</html>\n");
    }
    fclose(text_file);
  }


  // use dot to generate output
  if( use_graphviz ) 
  {
    char cmd[1024];
    int ret;
    memset(cmd, 0, sizeof(cmd));

    // FIXME: error handling could be better here
    ret = snprintf(cmd,sizeof(cmd)-1,"dot -Tgif -o%s %s", iface_gif, iface_dot); assert(ret > 0);
    ret = system(cmd); 
    if(ret == -1)
      fatal("ERROR: error running graphviz - please check your graphviz and font installations..\n");
    ret = snprintf(cmd,sizeof(cmd)-1,"dot -Tcmap -o%s %s", iface_cmap, iface_dot); assert(ret > 0);
    ret = system(cmd); 
    if(ret == -1) fatal("error running graphviz command:\n   %s\n",cmd);

    if( do_func_graph ) {
      ret = snprintf(cmd,sizeof(cmd)-1,"dot -Tgif -o%s %s", func_gif, func_dot); assert(ret > 0);
      ret = system(cmd); assert(ret != -1);
      if(ret == -1)
        fatal("ERROR: error running graphviz - please check your graphviz and font installations..\n");
      ret = snprintf(cmd,sizeof(cmd)-1,"dot -Tcmap -o%s %s", func_cmap, func_dot); assert(ret > 0);
      ret = system(cmd); 
      if(ret == -1) fatal("error running graphviz command:\n   %s\n",cmd);
    }
  }

  // add the HTML
  if( use_graphviz ) {
    start_html_banner();
    output("<h3>Component Graph &nbsp;&nbsp;");
    output("<font size=-1>(<a href=\"%s\">text version</a>,",text_only_name);
    output("&nbsp; &nbsp;<a href=\"cg_help.html\">help</a>)</font> </h3>\n");
    end_html_banner();

    // FIXME: add a link to the function graph page here.
    output("<br>\n");

    output("<map name=\"comp\">\n");
    copy_file_to_output(iface_cmap);
    output("</map>\n");
    output("<center><image src=\"%s\" usemap=\"#comp\" border=0></center>\n", iface_gif);
  }
  else {
    // just copy in the text output, if we aren't generating graphs
    print_html_banner("<h3>Component Graph</h3>");
    output("<center>\n");
    copy_file_to_output(text_only_name);
    unlink(text_only_name);
    output("</center>\n");
  }

  if( do_func_graph ) {
    // FIXME: this stuff should all go to a seperate function graph HTML page
    print_html_banner("<h3>Component Function Graph</h3>");

    output("<map name=\"func\">\n");
    copy_file_to_output(func_cmap);
    output("</map>\n");
    output("<center><image src=\"%s\" usemap=\"#func\" border=0></center>\n", func_gif);
  }

  // remove temp files
  //unlink(iface_dot);
  unlink(iface_cmap);
  //unlink(func_dot);
  unlink(func_cmap);

}







//////////////////////////////////////////////////////////////////////
//
// Main routine for creating docs for a component
//
//////////////////////////////////////////////////////////////////////

static void generate_component_html(nesc_declaration cdecl) 
{
  FILE *outfile;
  component comp = CAST(component, cdecl->ast);

  // open the appropriate output file
  outfile = open_outfile( component_docfile_name(cdecl->name) );


  // start the HTML
  {
    char *sourcelink = doc_filename_with_ext(cdecl->ast->location->filename, ".source");
    char *sourcetext = doc_filename_with_ext(cdecl->ast->location->filename, "");

    add_source_symlink(cdecl->ast->location->filename, sourcelink);

    output("\n\
<html>\n\
<head>\n\
<title>Component: %s</title>\n\
</head>\n\
<body>\n\
", cdecl->name);

    print_navbar(outfile, NAV_OTHER, sourcelink, sourcetext);

    output("\n\
<h1 align=\"center\">Component: %s</h1>\n\
", cdecl->name);
  }  

  // print the overview documentation
  if( cdecl->doc.short_s ) {
    output("<p>\n");
    if(cdecl->doc.long_s)   output_docstring((char *)cdecl->doc.long_s, cdecl->ast->location);
    else                        output_docstring((char *)cdecl->doc.short_s, cdecl->ast->location);
    output("\n<p>\n\n");
  }


  // module declaration (requires / provides stuff)
  if( comp->decls ) 
  {
    rp_interface rp;
    declaration dlist, decl;
    interface_ref iref;
    bool header_printed = FALSE;


    // requires
    scan_declaration(dlist, comp->decls) {
      if(is_rp_interface(dlist) &&
	 (rp = CAST(rp_interface, dlist))->required ) {

        scan_declaration(decl,rp->decls) {
          // commands / events
          if( is_data_decl(decl) ) {
            data_decl dd = CAST(data_decl, decl);
            variable_decl vd;
            scan_variable_decl(vd, CAST(variable_decl,dd->decls)) {
              if(!header_printed) {print_html_banner("<h3>Require Interfaces</h3>"); output("<ul>\n"); header_printed=TRUE;}
              print_function_html(NULL, dd, vd, short_desc|in_component);
            }
          }

          // interfaces
          else if( is_interface_ref(decl) ) {
            if(!header_printed) {print_html_banner("<h3>Required Interfaces</h3>"); output("<ul>\n"); header_printed=TRUE;}
            iref = CAST(interface_ref,decl);
            output("<li>    <a href=\"%s\">%s</a> %s\n", 
                   interface_docfile_name(iref->word1->cstring.data),
                   iref->word1->cstring.data,
                   iref->word2 ? iref->word2->cstring.data : "");
          }
        
          // everything else is an error
          else {
            AST_print( CAST(node,cdecl->ast) );
            assert(0);
          }
        }
      }
    }    
    if(header_printed) output("</ul>\n\n");

    // provides
    header_printed = FALSE;
    scan_declaration(dlist, comp->decls) {
      if(is_rp_interface(dlist) &&
	 !(rp = CAST(rp_interface, dlist))->required ) {
        scan_declaration(decl,rp->decls) {
          // commands / events
          if( is_data_decl(decl) ) {
            data_decl dd = CAST(data_decl, decl);
            variable_decl vd;
            scan_variable_decl(vd, CAST(variable_decl,dd->decls)) {
              if(!header_printed) {print_html_banner("<h3>Provided Interfaces</h3>"); output("<ul>\n"); header_printed=TRUE;}
              print_function_html(NULL, dd, vd, short_desc|in_component);
            }
          }

          // interfaces
          else if( is_interface_ref(decl) ) {
            if(!header_printed) {print_html_banner("<h3>Provided Interfaces</h3>"); output("<ul>\n"); header_printed=TRUE;}
            iref = CAST(interface_ref,decl);
            output("<li>    <a href=\"%s\">%s</a> %s\n", 
                   interface_docfile_name(iref->word1->cstring.data),
                   iref->word1->cstring.data,
                   iref->word2 ? iref->word2->cstring.data : "");
          }
        
          // everything else is an error
          else {
            AST_print( CAST(node,cdecl->ast) );
            assert(0);
          }
        }
      }
    }    
    if(header_printed) output("</ul>\n\n");

  }
  

  // configuration (aka wiring)
  if( is_configuration(cdecl->impl) )
  {
    print_cg_html(cdecl->name,
                  CAST(component, cdecl->ast)->location->filename, 
                  cdecl->connections,
                  FALSE);
  }

  // otherwise, we have a module (aka function defs) 
  else if (is_module(cdecl->impl))
  {
    module mod = CAST(module, cdecl->impl);
    declaration decl;
    bool printed_heading;
    bool first;

    // print variable descriptions
    printed_heading = FALSE;
    scan_declaration(decl, mod->decls) {
      // deal with single variable decls - I don't know if this ever happens....
      if( is_variable_decl(decl) )
        //print_short_variable_html( CAST(variable_decl,decl) );
        assert(0);

      // deal with lists of variable decls
      else if( is_data_decl(decl) ) {
        data_decl dd = CAST(data_decl,decl);
        declaration d2;
        scan_declaration(d2, dd->decls) 
          if( is_variable_decl(d2) ) {
            if(!printed_heading) {print_html_banner("<h3>Variables</h3>"); output("<ul>\n"); printed_heading=TRUE;}
            print_short_variable_html( dd, CAST(variable_decl,d2) );
          }
      }
    }
    if(printed_heading) output("</ul>\n\n");
    


    // print short function descriptions
    printed_heading = FALSE;
    scan_declaration(decl, mod->decls) {
      if( is_function_decl(decl) ) {
        if(!printed_heading) {print_html_banner("<h3>Function Index</h3>"); output("<ul>\n"); printed_heading=TRUE;}
        print_function_html(CAST(function_decl,decl), NULL, NULL, in_component|short_desc);
      }
    }
    if(printed_heading) output("</ul>\n\n");


    // print long function descriptions
    printed_heading = FALSE;
    first = TRUE;
    scan_declaration(decl, mod->decls) {
      if( is_function_decl(decl) && has_long_desc(CAST(function_decl,decl),NULL,NULL) ) {
        if(!printed_heading) {print_html_banner("<h3>Function Descriptions</h3>\n"); printed_heading=TRUE;}
        if( !first ) output("<hr>\n");
        print_function_html(CAST(function_decl,decl), NULL, NULL, in_component|long_desc);
        first = FALSE;
      }
    }

  }


  close_outfile(outfile);
}


static void generate_intf_function_list(const char *heading,
					nesc_declaration idecl,
					int kind, int flags)
{
  declaration funcs = CAST(interface, idecl->ast)->decls;
  declaration f;
  bool printed_heading = FALSE;

  // commands
  scan_declaration(f,funcs) {
    data_decl dd = CAST(data_decl, f);
    variable_decl vd;

    scan_variable_decl(vd, CAST(variable_decl,dd->decls)) {
      if( vd->ddecl->ftype == kind ) {
        if( flags & short_desc  ||  has_long_desc(NULL,dd,vd) ) {
          if(!printed_heading) {
            print_html_banner(heading); 
            if(flags & short_desc) output("<ul>\n"); 
            printed_heading=TRUE;
          } else {
            if( !(flags & short_desc) ) 
              output("<hr>\n");
          }
          print_function_html(NULL,dd,vd,in_interface|flags);
        }
      }
    }
  }
  if(printed_heading && flags & short_desc) output("</ul>\n");
}

//////////////////////////////////////////////////
// generate HTML for an interface file
//////////////////////////////////////////////////
static void generate_interface_html(nesc_declaration idecl) 
{
  FILE *outfile;

  // open the output file
  outfile = open_outfile( interface_docfile_name(idecl->name) );
                          

  // start the HTML
  {
    char *sourcelink = doc_filename_with_ext(idecl->ast->location->filename, ".source");
    char *sourcetext = doc_filename_with_ext(idecl->ast->location->filename, "");

    add_source_symlink(idecl->ast->location->filename, sourcelink);

    output("\n\
<html>\n\
<head>\n\
<title>Interface: %s</title>\n\
</head>\n\
<body>\n\
", idecl->name);

    print_navbar(outfile, NAV_OTHER, sourcelink, sourcetext);

    output("\n\
<h1 align=\"center\">Interface: %s</h1>\n\
", idecl->name);
  }  
  if( idecl->doc.short_s ) {
    output("<p>\n");
    if(idecl->doc.long_s)   output_docstring((char *)idecl->doc.long_s, idecl->ast->location);
    else                        output_docstring((char *)idecl->doc.short_s, idecl->ast->location);
    output("\n<p>\n\n");
  }

  // print cross refs
  {
    int i;
    char *name, *end;
    ic_entry *entry = ic_get_entry( ic_make_iface_key(idecl->name) );
    assert(entry);
    if(entry->p_list.num > 0) {
      output("<dl>\n<dt>Components providing this interface:\n<dd>\n");
      for(i=0; i<entry->p_list.num; i++) {
        name = entry->p_list.comp[i];
        output("    <a href=\"%s.html\">",name);
        end = strstr(name,".nc"); assert(end);
        *end = '\0';
        output("%s</a><br>\n",name);
        *end = '.';
      }
      output("</dl><p>\n\n");
    }
    if(entry->r_list.num > 0) {
      output("<dl>\n<dt>Components requiring this interface:\n<dd>\n");
      for(i=0; i<entry->r_list.num; i++) {
        name = entry->r_list.comp[i];
        output("    <a href=\"%s.html\">",name);
        end = strstr(name,".nc"); assert(end);
        *end = '\0';
        output("%s</a><br>\n",name);
        *end = '.';
      }
      output("</dl><p>\n\n");
    }
  }
  
  // summary
  generate_intf_function_list("<h3>Commands</h3>",
			      idecl, function_command, short_desc);
  generate_intf_function_list("<h3>Events</h3>",
			      idecl, function_event, short_desc);

  // detailed descriptions
  generate_intf_function_list("<h3>Commands - Details</h3>",
			      idecl, function_command, long_desc);
  generate_intf_function_list("<h3>Events - Details</h3>",
			      idecl, function_event, long_desc);

  close_outfile(outfile);
}


//////////////////////////////////////////////////
// Create index files
//////////////////////////////////////////////////

typedef struct _index_entry {
  char *name;
  char *path;
  char *fname; // for interfaces:  path.name.ti.html
               // for components:  path.name.td.html
               // for apps:        name.app.html
} index_entry;


typedef struct {
  int num;
  index_entry *ent;
} file_index;


/**
 * compare two entries - used by qsort
 **/
static int index_entry_comparator_short(const void *va, const void *vb) {
  int ret;
  index_entry *a = (index_entry*) va;
  index_entry *b = (index_entry*) vb;
  
  ret = strcmp(a->name, b->name);
  if(ret) return ret;

  return strcmp(a->path, b->path);
}
static int index_entry_comparator_full(const void *va, const void *vb) {
  int ret;
  index_entry *a = (index_entry*) va;
  index_entry *b = (index_entry*) vb;
  
  ret = strcmp(a->path, b->path);
  if(ret) return ret;

  return strcmp(a->name, b->name);
}


/**
 * add an entry to the list
 **/
static void insert_entry(file_index *fi, char *docfile, char *suffix) {
  char *p;
  index_entry *e = &( fi->ent[fi->num++] );

  // path
  e->path = rstralloc(doc_region, strlen(docfile)+1);
  strcpy(e->path, docfile);

  // chop off the suffix
  p = e->path + strlen(e->path) - strlen(suffix);
  assert( *p == '.' );
  *p = '\0'; p--;

  // separate out the name.  We special case for common source file extensions.
  while(p > e->path  &&  *p != '.') p--;
  if( !strcmp(p,".nc") || !strcmp(p,".td") || !strcmp(p,".ti") || !strcmp(p,".h") || !strcmp(p,".c") ) {
    p--;
    while(p > e->path  &&  *p != '.') p--;
  }

  if(p == e->path) 
    e->name = e->path;
  else {
    *p = '\0';
    e->name = p+1;
  }
  
  // file name
  e->fname = rstralloc(doc_region, strlen(docfile)+1);
  strcpy(e->fname, docfile);
}


/**
 * generate the index file
 **/
typedef enum {
  SORT_FILE = 1,
  SORT_PATH = 2
} index_sort_type;

static void print_index_file(navbar_mode nmode, index_sort_type sort, file_index *ind)
{
  int i;
  FILE *f;
  char *filename;
  char *title, *indexname, *sort_heading;

  // set up some shorthand, based on mode
  if(nmode == NAV_INT) {
    indexname = "interfaces";
    title = "Interface Index";
    sort_heading = "interface";
  } else if(nmode == NAV_COMP) {
    indexname = "components";
    title = "Component Index";
    sort_heading = "component";
  } else if(nmode == NAV_APP) {
    indexname = "apps";
    title = "Application Index";
    sort_heading = "app";
  } else if(nmode == NAV_ALL) {
    indexname = "allfiles";
    title = "All File Index";
    sort_heading = "file";
  } else {
    assert(0);
  }
  

  // create the file name, & open the file
  filename = rstralloc(doc_region, strlen(indexname) + strlen("_f.html") + 1);
  assert(filename);
  strcpy(filename, indexname);
  if( sort == SORT_FILE ) 
    strcat(filename, "_f.html");
  else
    strcat(filename, "_p.html");
  
  // open the file
  f = fopen(filename, "w"); 
  if( !f ) fatal("can't write to index file '%s'",f);
  
  // start the HTML
  fprintf(f, "<html>\n");
  fprintf(f, "<head><title>%s</title></head>\n", title);
  fprintf(f, "<body>\n");


  // add a navigation banner
  print_navbar(f, nmode, NULL, NULL);


  // title, and table tags
  fprintf(f, "<h1 align=\"center\">%s</h1>\n", title);
  fprintf(f, "<center>\n");
  fprintf(f, "<table border=0 cellpadding=0 width=\"80%%\">\n");


  // add the sorting links
  fprintf(f, "<tr valign=\"top\">\n");
  if(sort == SORT_FILE) {
    fprintf(f, "<td><a href=\"%s_p.html\"><em>path</em></a></td>\n", indexname);
    fprintf(f, "<td>&nbsp;&nbsp;&nbsp;</td>\n");
    fprintf(f, "<td><em>%s</em></td>\n", sort_heading);
    fprintf(f, "<td>&nbsp;&nbsp;&nbsp;</td>\n");
    fprintf(f, "<td><em>description</em></td>\n");
  } else {
    fprintf(f, "<td><em>path</em></td>\n");
    fprintf(f, "<td>&nbsp;&nbsp;&nbsp;</td>\n");
    fprintf(f, "<td><a href=\"%s_f.html\"><em>%s</em></a></td>\n", indexname, sort_heading);
    fprintf(f, "<td>&nbsp;&nbsp;&nbsp;</td>\n");
    fprintf(f, "<td><em>description</em></td>\n");
  }
  fprintf(f, "</tr>\n");


  // blank line
  fprintf(f, "<tr valign=\"top\">\n");
  fprintf(f, "    <td>&nbsp;</td>\n");
  fprintf(f, "    <td>&nbsp;</td>\n");
  fprintf(f, "    <td>&nbsp;</td>\n");
  fprintf(f, "    <td>&nbsp;</td>\n");
  fprintf(f, "    <td>&nbsp;</td>\n");
  fprintf(f, "</tr>\n");

  // index 
  for(i=0; i<ind->num; i++) {
    char *desc = ic_get_desc(ind->ent[i].fname);
    if(desc==NULL) desc = "&nbsp;";
    if(sort == SORT_FILE) {
      fprintf(f, "<tr valign=\"top\">\n");
      fprintf(f, "    <td><a href=\"%s\">%s</a></td>\n", ind->ent[i].fname, ind->ent[i].path);
      fprintf(f, "    <td>&nbsp;</td>\n");
      fprintf(f, "    <td>%s</td>\n", ind->ent[i].name);
      fprintf(f, "    <td>&nbsp;</td>\n");
      fprintf(f, "    <td>%s</td>\n",desc);
      fprintf(f, "</tr>\n");
    } else {
      // new path - print a blank line for all but the 'apps' page
      if(i>0 && strcmp(ind->ent[i].path, ind->ent[i-1].path) && nmode!=NAV_APP) {
        fprintf(f, "<tr>\n");
        fprintf(f, "    <td>&nbsp;</td>\n");
        fprintf(f, "    <td>&nbsp;</td>\n");
        fprintf(f, "    <td>&nbsp;</td>\n");
        fprintf(f, "    <td>&nbsp;</td>\n");
        fprintf(f, "    <td>&nbsp;</td>\n");
        fprintf(f, "</tr>\n");
      }
      fprintf(f, "<tr valign=\"top\">\n");
      fprintf(f, "    <td>%s</td>\n", (i>0 && !strcmp(ind->ent[i].path, ind->ent[i-1].path)) ? "&nbsp;" : ind->ent[i].path);
      fprintf(f, "    <td>&nbsp;</td>\n");
      fprintf(f, "    <td><a href=\"%s\">%s</a></td>\n", ind->ent[i].fname, ind->ent[i].name);
      fprintf(f, "    <td>&nbsp;</td>\n");
      fprintf(f, "    <td>%s</td>\n",desc);
      fprintf(f, "</tr>\n");
    }
  }


  // cleanup
  fprintf(f, "</table>\n");
  fprintf(f, "</center>\n");
  fprintf(f, "<hr>Generated by <a href=\"../../../nesc/doc/nesdoc.html\">nesdoc</a><br>\n");
  fprintf(f, "</body>\n");
  fprintf(f, "</html>\n");
  fclose(f);
}


static void print_hierarchical_index_file(const char *filename, file_index *ind)
{
  FILE *f = fopen(filename,"w");
  char *title = "Source Tree";
  char *prevdir;
  int i,j,level;

  if( !f ) fatal("can't write to hierarchical index file '%s'",filename);

  fprintf(f, "<html>\n");
  fprintf(f, "<head><title>%s</title></head>\n", title);
  fprintf(f, "<body>\n");

  // add a navigation banner
  print_navbar(f, NAV_TREE, NULL, NULL);

  // title, and table tags
  fprintf(f, "<h1 align=\"center\">%s</h1>\n", title);
  fprintf(f, "<center>\n");
  fprintf(f, "<table border=0 cellpadding=0>\n");


  // listing 
  prevdir = "";
  for(i=0; i<ind->num; i++) {
    // print directory info, if necessary
    if( strcmp(prevdir,ind->ent[i].path) ) {
      char *a, *b, *dot;

      // find the common prefix
      level = 0;
      a = prevdir;
      b = ind->ent[i].path;
      while(*a == *b) {
        a++;
        b++;
        if(*b == '.') level++;
      }
      while(b > ind->ent[i].path  &&  *b != '.') b--;
      if(*b == '.') b++;

      // print the dir headers
      do {
        dot = strchr(b,'.');
        if(dot != NULL) *dot = '\0';

        fprintf(f, "<tr><td>\n");
        for(j=0; j<level; j++)   fprintf(f, "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ");
        fprintf(f, "\n    %s/<br>\n", b);
        fprintf(f, "</td></tr>\n");
        
        if(dot != NULL) *dot = '.';
        b = dot+1;
        level++;
      } while(dot != NULL);

      prevdir = ind->ent[i].path;
    }

    // print the link
    fprintf(f, "<tr><td>\n");
    for(j=0; j<level; j++) fprintf(f, "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp; ");
    fprintf(f, "\n    <a href=\"%s\">%s</a>\n", ind->ent[i].fname, ind->ent[i].name);
    fprintf(f, "</td></tr>\n");
  }


  // cleanup
  fprintf(f, "</table>\n");
  fprintf(f, "</center>\n");
  fprintf(f, "<hr>Generated by <a href=\"../../../nesc/doc/nesdoc.html\">nesdoc</a><br>\n");
  fprintf(f, "</body>\n");
  fprintf(f, "</html>\n");
  fclose(f);
}



/**
 * Read through the doc directory, and generate appropriate index
 * files.  The indices are sorted by the base interface or component
 * name.  In cases in which there are multiple versions of a file (ie,
 * for different hardware), seperate links are given for each path.
 **/


static void generate_index_html() {
  file_index comp, iface, app, allfiles;

  // read the directory, and sort the entries
  {
    DIR *dir;

    // open the dir
    dir = opendir(".");  
    if( !dir ) fatal("can't open directory '.'");

    // allocate space
    {
      int nument = 0;
      while( readdir(dir) ) nument++;
      rewinddir(dir);

      iface.ent    = rarrayalloc(doc_region, nument, index_entry);
      comp.ent     = rarrayalloc(doc_region, nument, index_entry);
      app.ent      = rarrayalloc(doc_region, nument, index_entry);
      allfiles.ent = rarrayalloc(doc_region, nument, index_entry);

      iface.num    = 0;
      comp.num     = 0;
      app.num      = 0;
      allfiles.num = 0;
    }

    // scan dir
    {
      struct dirent *dent;

      while( (dent=readdir(dir)) != NULL ) {
        char *p;

        // find the second from the last "."
        p = dent->d_name + strlen(dent->d_name) - 1;
        while(p > dent->d_name  &&  *p != '.') p--;
        p--;
        while(p > dent->d_name  &&  *p != '.') p--;

        // add to the appropriate list
        if( !strcmp(p,".nc.html") ) {
          if( ic_is_iface(dent->d_name) )
            insert_entry(&iface, dent->d_name, ".nc.html");
          else
            insert_entry(&comp, dent->d_name, ".nc.html");
          insert_entry(&allfiles, dent->d_name, ".html");
          continue;
        }

        // scan back one more, for app files
        p--;
        while(p > dent->d_name  &&  *p != '.') p--;
        if( !strcmp(p,".nc.app.html") ) {
          insert_entry(&app, dent->d_name, ".nc.app.html");
        }
      }
    }
  }

  // Generate index files, sorted by short name
  {
    // sort
    qsort(iface.ent,    iface.num,    sizeof(index_entry), index_entry_comparator_short);
    qsort(comp.ent,     comp.num,     sizeof(index_entry), index_entry_comparator_short);
    qsort(app.ent,      app.num,      sizeof(index_entry), index_entry_comparator_short);
    qsort(allfiles.ent, allfiles.num, sizeof(index_entry), index_entry_comparator_short);
    
    // index files    
    print_index_file(NAV_INT,  SORT_FILE, &iface);
    print_index_file(NAV_COMP, SORT_FILE, &comp);
    print_index_file(NAV_APP,  SORT_FILE, &app);
    print_index_file(NAV_ALL,  SORT_FILE, &allfiles);
  }

  // generate index files, sorted by long name
  {
    // sort
    qsort(iface.ent,    iface.num,    sizeof(index_entry), index_entry_comparator_full);
    qsort(comp.ent,     comp.num,     sizeof(index_entry), index_entry_comparator_full);
    qsort(app.ent,      app.num,      sizeof(index_entry), index_entry_comparator_full);
    qsort(allfiles.ent, allfiles.num, sizeof(index_entry), index_entry_comparator_full);
    
    // index files    
    print_index_file(NAV_INT,  SORT_PATH, &iface);
    print_index_file(NAV_COMP, SORT_PATH, &comp);
    print_index_file(NAV_APP,  SORT_PATH, &app);
    print_index_file(NAV_ALL,  SORT_PATH, &allfiles);
  }
  

  // hierarchical, top-level index file
  qsort(allfiles.ent, allfiles.num, sizeof(index_entry), index_entry_comparator_full);
  print_hierarchical_index_file("index.html", &allfiles);
  
}



//////////////////////////////////////////////////
// Create whole-app description page
//////////////////////////////////////////////////
static void generate_app_page(const char *filename, cgraph cg) 
{
  char *appname, *p;
  char *fname;
  FILE *f;

  // return if app file generation wasn't explicity requested
  if( !is_app ) 
    return;

  // return, if the component is NULL
  if( cg == NULL )
    return;

  // figure out the app name from the main file name
  appname = doc_filename_with_ext(filename, "");
  p = appname + strlen(appname) - 1;
  while(p > appname  &&  *p != '.') p--;
  *p = '\0';
  while(p > appname  &&  *p != '.') p--;
  if(*p == '.') p++;
  appname = p;
  

  // generate the file
  fname    = doc_filename_with_ext(filename, ".app.html");
  f = open_outfile(fname); 

  fprintf(f, "<html>\n");
  fprintf(f, "<head><title>App: %s</title></head>\n", appname);
  fprintf(f, "<body>\n");

  print_navbar(f, NAV_OTHER, NULL, NULL);

  fprintf(f, "<h1 align=\"center\">App: %s</h1>\n", appname);




  print_cg_html(appname, filename, cg, TRUE);
  fprintf(f, "<hr>Generated by <a href=\"../../../nesc/doc/nesdoc.html\">nesdoc</a><br>\n");
  fprintf(f, "</body>\n");
  fprintf(f, "</html>\n");

  close_outfile(f);

}


//////////////////////////////////////////////////
// Generate connection graph help page
//////////////////////////////////////////////////
#include "cg_help.c"
static void generate_cg_help_page()
{
  FILE *f;

  // open the gif file
  f = fopen("cg_help.gif", "w");
  if( !f ) {
    warning("can't write to cg_help.gif - no help generated");
    return;
  }
  fwrite(cg_help_gif, 1, sizeof(cg_help_gif), f);
  fclose(f);


  // open the HTML file
  f = fopen("cg_help.html", "w");
  if( !f ) {
    warning("can't write to cg_help.gif - no help generated");
    return;
  }

  //  markmark
  fprintf(f, "<html>\n");
  fprintf(f, "<head><title>Connection Graph Help</title></head>\n");
  fprintf(f, "<body>\n");
  print_navbar(f, NAV_OTHER, NULL, NULL);
  fprintf(f, "<h1 align=\"center\">Connection Graph Help</h1>\n");

  fprintf(f, "\n\
<table border=\"0\" width=\"80%%\" align=\"center\">\n\
<tr>\n\
<td>\n\
<img src=\"cg_help.gif\" border=\"0\" align=\"left\">\n\
</td>\n\
\n\
<td>&nbsp;&nbsp;&nbsp;</td>\n\
<td>\n\
\n\
<ul>\n\
\n\
<li><em>A</em> requires interface <em>I</em>, <em>B</em> provides <em>I</em>, and <em>A</em> and <em>B</em> are wired\n\
together.<p>\n\
\n\
<li><em>C</em> and <em>D</em> both require or both provide <em>J</em>.  The direction of the\n\
arrow indicates that the original wiring is \"<em>C = D</em>\".<p>\n\
\n\
<li><em>E</em> requires function <em>f</em>, and F provides function <em>f</em>.<p>\n\
\n\
</ul>\n\
</td>\n\
</tr>\n\
</table>\n\
<p>\n\
\n\
</body>\n\
</html>\n\
");
  fclose(f);

}


//////////////////////////////////////////////////
// Generate all docs
//////////////////////////////////////////////////

bool docs_requested(void)
{
  return docdir != NULL;
}

void generate_docs(const char *ofilename, cgraph cg)
{
  char *filename = rstrdup(doc_region, ofilename);

  unixify_path(filename);

  // if no docdir is specified, then the user didn't request doc generation
  // Initialization
  {
    // create the region
    init_doc_region();

    // get the current working directory
    assert(getcwd(original_wd, sizeof(original_wd)));

    // set up dir info
    find_currdir_prefix(original_wd);

    // cd to the docdir 
    {
      char *pos=strchr(docdir, dirsep);
      if(pos == docdir) pos=strchr(docdir+1, dirsep); // skip the first '/' of an absolute path
      while(pos != NULL) {
        *pos = '\0';

#ifndef ENOMEDIUM
	/* Ignore ENOMEDIUM on systems that don't have it */
#define ENOMEDIUM EEXIST
#endif

        if(mkdir(docdir, 0755) != 0  &&  errno != EEXIST  &&  errno != ENOMEDIUM) {
          perror("mkdir");
          fatal("error making ancestor directory of docdir '%s'.  errno=%d\n", docdir,errno);
        }
        *pos = dirsep;
        pos = strchr(pos+1, dirsep);
      }
    }
    if(mkdir(docdir, 0755) != 0  &&  errno != EEXIST  &&  errno != ENOMEDIUM) {
      perror("mkdir");
      fatal("error making parent directory of docdir '%s'.  errno=%d\n", docdir,errno);
    }
    if(chdir(docdir) != 0) {
      perror("chdir");
      fatal("error changing directory to docdir '%s'\n", docdir);
    }

    // read the information cache
    ic_read();
  }

  // generate the connection graph help file
  generate_cg_help_page();


  // update information cache with all loaded components
  {
    env_scanner scanner;
    const char *name;
    nesc_declaration decl;

    env_scan(get_nesc_env(), &scanner);
    while( env_next(&scanner, &name, (void **)&decl) )
      ic_add_entry(decl);

    // write the information cache
    ic_write();
  }


  // walk through the list of all components, and generate docs
  {
    env_scanner scanner;
    const char *name;
    nesc_declaration cdecl;

    if (flag_verbose) printf("\n    Generating component docs\n");
    env_scan(get_nesc_env(), &scanner);
    while( env_next(&scanner, &name, (void **)&cdecl) ) 
      if (cdecl->kind == l_component) {
	if (flag_verbose) printf("        %s\n", cdecl->name);
	generate_component_html(cdecl);
      }
  }

  // generate docs for all interfaces 
  {
    env_scanner scanner;
    const char *name;
    nesc_declaration idecl;

    if (flag_verbose) printf("\n    Generating interface docs\n");
    env_scan(get_nesc_env(), &scanner);
    while( env_next(&scanner, &name, (void **)&idecl) )
      if (idecl->kind == l_interface) {
	if (flag_verbose) printf("        %s\n", idecl->name);
	generate_interface_html(idecl);
      }
  }

  // generate whole-app wiring page
  generate_app_page(filename,cg);


  // generate index files
  generate_index_html();
  

  // cleanup
  {
    // kill the region
    // NOTE: using the region this way makes this module non-reentrant
    deleteregion(doc_region);
    doc_region = NULL;

    assert(chdir(original_wd) == 0);
  }
}
