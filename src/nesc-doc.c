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
  

  
  FIXME: need to document the doc system...  ;-)

*/
#include <unistd.h>
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

// directory info to strip from the generated docs file names
#define MAX_TOPDIRS 100
static const char *topdir[MAX_TOPDIRS];
static int num_topdirs = 0;

// output directory for generated docs
static const char *docdir = NULL;

// flag, to determine whether or not to use graphviz
static bool use_graphviz = FALSE;


/**
 * Set the graphviz flag
 **/
void doc_use_graphviz(const bool use)
{
  use_graphviz = use;
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
  assert(dir);
  assert(num_topdirs < MAX_TOPDIRS);

  topdir[num_topdirs] = dir;
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
static void find_currdir_prefix(char *cwd)
{
  int i;

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
static char *doc_filename_with_ext(const char *src_filename, const char *ext) 
{
  char *pos;
  char *ret;
  bool need_prefix = TRUE;
  int i;
  int length;

  // allocate max required space: docdir/currdir_prefix/src_filename.html
  length = 
    strlen(currdir_prefix) + 1 +
    strlen(src_filename) + 
    strlen(ext) +
    1; // for terminator
  ret = rstralloc( doc_region, length );
  assert(ret != NULL);

  bzero(ret,length);

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
    pos++;
  }
  

  return ret;
}


static char *doc_filename(const char *src_filename) 
{
  return doc_filename_with_ext(src_filename, ".html");
}



struct location doc_empty_location = {"nofile",-1,0};


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


//////////////////////////////////////////////////////////////////////
//
//  output functions
//
//////////////////////////////////////////////////////////////////////

static FILE *open_outfile(const char *outfile) {
  FILE *doc_outfile;

  unlink(outfile);
  
  doc_outfile = fopen(outfile,"w");
  assert(doc_outfile);

  // set up unparse routines
  unparse_start(doc_outfile);
  disable_line_directives();
  set_function_separator(".");
  enable_documentation_mode();

  return doc_outfile;
} 

static void close_outfile(FILE *doc_outfile) {
  fclose(doc_outfile);
  unparse_end();
}


//////////////////////////////////////////////////////////////////////
//
//  Docstring command parsing
//
//////////////////////////////////////////////////////////////////////

typedef enum {
  in_main,
  in_return,
  in_param
} docstring_context;

static void output_docstring(char *docstring)
{
  char *pos = docstring;
  char *at;
  int len;
  docstring_context context = in_main;
  static char *whitespace = " \t\r\n";

  while( 1 ) {
    // find the next @ directive
    at = strchr(pos,'@');

    // output the rest, if there are no more @ directives
    if(at == NULL) {
      output(pos);
      if(context != in_main) 
        output("</dl>\n");
      return;
    }

    // output up to the @
    *at='\0'; output(pos); *at='@';
    pos = at+1;

    // do some formatting, based on the command
    len = strcspn(pos, whitespace);
    
    if( len==strlen("return") && !strncasecmp("return",pos,len) ) {
      pos += len;
      if(context == in_main)
        output("<p><dl>\n");
      if(context != in_return)
        output("<dt><b>Returns:</b>\n");
      context = in_return;
      output("<dd>");
    }

    else if( len==strlen("param") && !strncasecmp("param",pos,len) ) {
      pos += len;
      
      if(context == in_main)
        output("<p><dl>\n");
      if(context != in_param)
        output("<dt><b>Parameters:</b>\n");
      context = in_param;

      // print out the parameter name, plus a separator
      output("<dd>");
      pos += strspn(pos, whitespace);
      len = strcspn(pos, whitespace);
      output("%*s",len,pos);
      output(" - ");
      pos += len;
    }

    else {
      // FIXME: print correct file info
      error("DOC WARNING: unknown directive @%*s\n",len,pos);
    }

  }

}




//////////////////////////////////////////////////////////////////////
//
// HTML generation functions
//
//////////////////////////////////////////////////////////////////////


static void print_short_variable_html(data_decl ddecl, variable_decl vd) {
  output("<li>   ");
  //prt_declarator(d->declarator, d->qualifiers, d->attributes, d->ddecl, psd_skip_container);


  // FIXME: did this fix the variable declaration printing?
  prt_type_elements(ddecl->modifiers, FALSE); 

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
  if( vd->ddecl->short_docstring ) { 
    output("        <br><menu>");
    output_docstring(vd->ddecl->short_docstring);
    output("</menu>\n");
  }
  
}


static inline void check_print_func_args(function_decl fd, data_decl dd, variable_decl vd,
                                         function_declarator *fdr, data_declaration *ddecl) {
  if( fd ) {
    assert(dd==NULL);
    assert(vd==NULL);
    if(fdr) *fdr = CAST(function_declarator, fd->declarator);
    if(ddecl) *ddecl = fd->ddecl;

  }
  else {
    assert(fd==NULL);
    assert(dd);    
    assert(vd);
    if(fdr) *fdr = CAST(function_declarator, vd->declarator);
    if(ddecl) *ddecl = vd->ddecl;
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
    prt_type_elements(dd->modifiers, FALSE); 
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
                 psd_skip_container | psd_rename_parameters);
  
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
    return (fd->ddecl->long_docstring != NULL);
  } else {
    return (vd->ddecl->long_docstring != NULL);
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

  // set up the description pointes
  if( fd ) {
    sdoc = fd->ddecl->short_docstring;
    ldoc = fd->ddecl->long_docstring;
  } else {
    sdoc = vd->ddecl->short_docstring;
    ldoc = vd->ddecl->long_docstring;
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
      output_docstring(sdoc);
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
    output_docstring(ldoc);
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
  nesc_declaration req;
  nesc_declaration prov;
  nesc_declaration iface;
} *iface_graph_entry;


static int iface_graph_compare(void *entry1, void *entry2) 
{
  iface_graph_entry c1 = (iface_graph_entry) entry1;
  iface_graph_entry c2 = (iface_graph_entry) entry2;
  return (c1->req == c2->req) && (c1->prov == c2->prov) && (c1->iface == c2->iface);
}

static unsigned long iface_graph_hash(void *entry) 
{
  iface_graph_entry c = (iface_graph_entry) entry;
  return ((long)c->req ^ ((long)c->prov << 16) ^ ((long)c->prov >> 16) ^ (long)c->iface);
}


static dhash_table new_iface_graph_table() {
  return new_dhash_table(newregion(), 10, iface_graph_compare, iface_graph_hash);
}

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
  {
    // use interface->required to determine the direction of
    // the arrow.
    // 
    // FIXME: is this correct?
    //
    // FIXME: we don't currently have any good way to keep information
    // on the _labels_ of multiply used interfaces.  This could be
    // done by generating the interface connection graph ourselves.

    // keep the direction, if there is no interface or if ep1 is the requires side
    if( !ep1->interface || ep1->interface->required ) {
      *req = ep1;
      *prov = ep2;
    }

    // ep1 must have been the provides side, so reverse the direction
    else {
      *req = ep2;
      *prov = ep1;
    }
  }

  // special case: if the interface is empty, we always show the connection
  if((*prov)->interface == NULL) {
    assert( (*req)->interface == NULL );
    return FALSE;
  }

  // special case: connection is a pass-through, via the '=' operator
  if( ep1->interface->required == ep2->interface->required ) {
    endp left, right;

    //fprintf(stderr,"\nhandling interface assignment\n");
    //fprintf(stderr,"defined: %d %d    required: %d %d\n", 
    //        ep1->function->defined,   ep2->function->defined, 
    //        ep1->interface->required, ep2->interface->required);

    // figure out which is on the left, and which is on the righ
    if( ep1->component && ep2->component ) {
      left=ep1; right=ep2;
    } else{
      // the side that isn't in a component (ie, it has no def) is on
      // the left hand side of the equals sign.
      if( ep1->component == NULL ) 
        left=ep1, right=ep2;
      else
        left=ep2, right=ep1;
    }

    // set the arrow direction.  If the right-hand component requires
    // the interface, have the arrow go from right to left.  If it
    // provides it, have the arrow go from left to right.
    if( right->interface->required ) {
      *req = right;
      *prov = left;
    } else {
      *req = left;
      *prov = right;
    }
  }



  // see if this one has already been printed.  This is done by checking a hashtable for the tripple of req,prov,iface
  {
    // create a table entry
    iface_graph_entry e = ralloc(regionof(table), struct iface_graph_entry);
    e->req = (*req)->component ? (*req)->component->ctype : NULL;
    e->prov = (*prov)->component ? (*prov)->component->ctype : NULL;
    e->iface = (*prov)->interface ? (*prov)->interface->itype : NULL;

    // do the lookup
    if( dhlookup(table,e) )
      return TRUE;

    // add the new item to the table
    dhadd(table, e);
    //fprintf(stderr, "\n%s  %s  %s\n",
    //        e->iface ? e->iface->name : "null",
    //        e->req ? e->req->name : "null",
    //        e->prov ? e->prov->name : "null"
    //        );
    return FALSE;
  }

}


#define iface_node_name( ep ) \
                    ( ep->component ? \
                      ep->component->ctype->name : \
                      ( ep->function->container ? \
                        ep->function->container->name : \
                        ep->function->ast->location->filename))





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

  // FIXME: disable the function graph for now
  bool do_func_graph = FALSE;


  // create filenames
  iface_dot = doc_filename_with_ext(component_file_name,".if.dot");
  iface_gif = doc_filename_with_ext(component_file_name,".if.gif");
  iface_cmap = doc_filename_with_ext(component_file_name,".if.cmap");

  if( do_func_graph ) {
    func_dot = doc_filename_with_ext(component_file_name,".func.dot");
    func_gif = doc_filename_with_ext(component_file_name,".func.gif");
    func_cmap = doc_filename_with_ext(component_file_name,".func.cmap");
  }

  text_only_name = doc_filename_with_ext(component_file_name,".text.html");


  // start the text output
  {
    text_file  = fopen(text_only_name, "w");  assert(text_file);

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
    char *graphviz_opts = "
    rankdir=LR;
    ratio=compress;
    margin=\"0,0\";
    ranksep=0.0005; 
    nodesep=0.1; 
    node [shape=ellipse style=filled fillcolor=\"#e0e0e0\"];
    node [fontcolor=blue fontname=Times fontsize=16];
    edge [fontcolor=blue fontname=Times fontsize=14];
";

    iface_file = fopen(iface_dot, "w");  assert(iface_file);
    fprintf(iface_file, "digraph \"%s_if\" {%s\n    %s\n", component_name, graphviz_opts,
            app_graph ? "size=\"8,8\"" : "size=\"4,4\"");

    if( do_func_graph ) {
      func_file  = fopen(func_dot,  "w");  assert(func_file);
      fprintf(iface_file, "digraph \"%s_func\" {%s\n    %s\n", component_name, graphviz_opts,
              app_graph ? "size=\"8,8\"" : "size=\"4,4\"");
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
                if(!req->component || !prov->component) 
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
              if( req->component && prov->component )
                fprintf(text_file, "    <td align=\"center\">&nbsp;->&nbsp;</td>\n");
              else 
                fprintf(text_file, "    <td align=\"center\">&nbsp;=&nbsp;</td>\n");

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
    fprintf(iface_file, "}\n");  fclose(iface_file);
    if( do_func_graph ) {
      fprintf(func_file, "}\n");   fclose(func_file);
    }
  }

  // finish up the text output
  {
    fprintf(text_file, "</table>\n</center>\n\n");
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
    bzero(cmd,sizeof(cmd));

    // FIXME: error handling could be better here
    ret = snprintf(cmd,sizeof(cmd)-1,"dot -Tgif -o%s %s", iface_gif, iface_dot); assert(ret > 0);
    ret = system(cmd); 
    if(ret == -1)
      fatal("ERROR: error running graphviz - please check your graphviz and font installations..\n");
    //ret = snprintf(cmd,sizeof(cmd)-1,"dot -Tcmap -o%s %s", iface_cmap, iface_dot); assert(ret > 0);
    //ret = system(cmd); assert(ret != -1);
    //if(ret == -1)
    // fatal("ERROR: error running graphviz - please check your graphviz and font installations..\n");

    if( do_func_graph ) {
      ret = snprintf(cmd,sizeof(cmd)-1,"dot -Tgif -o%s %s", func_gif, func_dot); assert(ret > 0);
      ret = system(cmd); assert(ret != -1);
      if(ret == -1)
        fatal("ERROR: error running graphviz - please check your graphviz and font installations..\n");
      ret = snprintf(cmd,sizeof(cmd)-1,"dot -Tcmap -o%s %s", func_cmap, func_dot); assert(ret > 0);
      ret = system(cmd); assert(ret != -1);
      if(ret == -1)
        fatal("ERROR: error running graphviz - please check your graphviz and font installations..\n");
    }
  }

  // add the HTML
  if( use_graphviz ) {
    start_html_banner();
    output("<h3>Component Graph &nbsp;&nbsp;<font size=-1>(<a href=\"%s\">text version</a>)</font> </h3>\n", text_only_name);
    end_html_banner();

    // FIXME: add a link to the function graph page here.
    output("<br>\n");

    //output("<map name=\"comp\">\n");
    //copy_file_to_output(iface_cmap);
    //output("</map>\n");
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
    char *sourcefile = doc_filename_with_ext(cdecl->ast->location->filename, "");

    unlink(sourcelink);
    assert(symlink(cdecl->ast->location->filename, sourcelink) == 0);
    output("
<html>
<head>
<title>Component: %s</title>
</head>
<body>
", cdecl->name);

    output("
<font size=\"-1\">
<table BORDER=\"0\" CELLPADDING=\"3\" CELLSPACING=\"0\" width=\"100%%\">
<tr ><td>
<b><a href=\"apps.html\">Apps</a></b>&nbsp;&nbsp;&nbsp;
<b><a href=\"components.html\">Components</a></b>&nbsp;&nbsp;&nbsp;
<b><a href=\"interfaces.html\">Interfaces</a></b>&nbsp;&nbsp;&nbsp;
</td>
<td align=\"right\">
source: <a href=\"%s\">%s</a>
</td>
</tr></table>
</font>
<hr>
", sourcelink, sourcefile);


    output("
<h1 align=\"center\">Component: %s</h1>
", cdecl->name);
  }  



  // print the overview documentation
  if( cdecl->short_docstring ) {
    output("<p>\n%s\n<p>\n\n", cdecl->long_docstring ? cdecl->long_docstring : cdecl->short_docstring);
  }


  // module declaration (requires / provides stuff)
  if( comp->rplist ) 
  {
    rp_interface rp;
    declaration decl;
    interface_ref iref;
    bool header_printed = FALSE;


    // requires
    scan_rp_interface(rp, comp->rplist) {
      if( rp->required ) {

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
    scan_rp_interface(rp, comp->rplist) {
      if( !rp->required ) {
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
  else 
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
          if(!printed_heading) {print_html_banner(heading); output("<ul>\n"); printed_heading=TRUE;}
          print_function_html(NULL,dd,vd,in_interface|flags);
        }
      }
    }
  }
  if(printed_heading) output("</ul>\n");
}

//////////////////////////////////////////////////
// generate HTML for an interface file
//////////////////////////////////////////////////
static void generate_interface_html(nesc_declaration idecl) 
{
  FILE *outfile;

  outfile = open_outfile( interface_docfile_name(idecl->name) );
                          

  // start the HTML
  {
    char *sourcelink = doc_filename_with_ext(idecl->ast->location->filename, ".source");
    char *sourcefile = doc_filename_with_ext(idecl->ast->location->filename, "");

    unlink(sourcelink);
    assert(symlink(idecl->ast->location->filename, sourcelink) == 0);

    output("
<html>
<head>
<title>Interface: %s</title>
</head>
<body>
", idecl->name);

    output("
<font size=\"-1\">
<table BORDER=\"0\" CELLPADDING=\"3\" CELLSPACING=\"0\" width=\"100%%\">
<tr ><td>
<b><a href=\"apps.html\">Apps</a></b>&nbsp;&nbsp;&nbsp;
<b><a href=\"components.html\">Components</a></b>&nbsp;&nbsp;&nbsp;
<b><a href=\"interfaces.html\">Interfaces</a></b>&nbsp;&nbsp;&nbsp;
</td>
<td align=\"right\">
source: <a href=\"%s\">%s</a>
</td>
</tr></table>
</font>
<hr>
", sourcelink, sourcefile);


    output("
<h1 align=\"center\">Interface: %s</h1>
", idecl->name);
  }  
  
  // summary
  generate_intf_function_list("<h3>Defined Functions</h3>",
			      idecl, function_command, short_desc);
  generate_intf_function_list("<h3>Used Functions</h3>",
			      idecl, function_event, short_desc);

  // detailed descriptions
  generate_intf_function_list("<h3>Defined Functions - Details</h3>",
			      idecl, function_command, long_desc);
  generate_intf_function_list("<h3>Used Functions - Details</h3>",
			      idecl, function_event, long_desc);
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
static int index_entry_comparator(const void *va, const void *vb) {
  int ret;
  index_entry *a = (index_entry*) va;
  index_entry *b = (index_entry*) vb;
  
  ret = strcmp(a->name, b->name);
  if(ret) return ret;

  return strcmp(a->path, b->path);
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

  // separate out the name
  while(p > e->path  &&  *p != '.') p--;
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
static void print_index_file(const char *filename, file_index *ind)
{
  int i;
  FILE *f;

  // open the file
  f = fopen(filename, "w"); assert(f);
  
  // start the HTML
  { 
    char *title;
    if( !strcmp(filename, "interfaces.html") )        title = "Interface Index";
    else if( !strcmp(filename, "components.html") )   title = "Component Index";
    else if( !strcmp(filename, "apps.html") )         title = "Application Index";
    else assert(0);

    fprintf(f, "<html>\n");
    fprintf(f, "<head><title>%s</title></head>\n", title);
    fprintf(f, "<body>\n");
    fprintf(f, "<h1 align=\"center\">%s</h1>\n", title);
  }

  // add a navigation banner
  fprintf(f, "<center>\n");
  if( !strcmp(filename,"apps.html") )          fprintf(f, "    Apps\n");
  else                                         fprintf(f, "    <a href=\"apps.html\">Apps</a>\n");
  fprintf(f, "    &nbsp;&nbsp;&nbsp;\n");
  if( !strcmp(filename,"components.html") )    fprintf(f, "    Components\n");
  else                                         fprintf(f, "    <a href=\"components.html\">Components</a>\n");
  fprintf(f, "    &nbsp;&nbsp;&nbsp;\n");
  if( !strcmp(filename,"interfaces.html") )    fprintf(f, "    Interfaces\n");
  else                                         fprintf(f, "    <a href=\"interfaces.html\">Interfaces</a>\n");
  fprintf(f, "</center>\n");

  // index 
  fprintf(f, "<table border=0>\n");
  for(i=0; i<ind->num; i++) {
    fprintf(f, "<tr>\n");
    fprintf(f, "    <td>%s</td>\n",
            (i>0 && !strcmp(ind->ent[i].name, ind->ent[i-1].name)) ? "&nbsp;" : ind->ent[i].name);
    fprintf(f, "    <td><a href=\"%s\">%s.%s</a></td>\n", 
            ind->ent[i].fname, ind->ent[i].path, ind->ent[i].name);
    fprintf(f, "</tr>\n");
  }
  fprintf(f, "</table>\n");

  // cleanup
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
  file_index comp, iface, app;

  // read the directory, and sort the entries
  {
    DIR *dir;

    // open the dir
    dir = opendir(".");  assert(dir);

    // allocate space
    {
      int nument = 0;
      while( readdir(dir) ) nument++;
      rewinddir(dir);

      iface.ent = rarrayalloc(doc_region, nument, index_entry);
      comp.ent  = rarrayalloc(doc_region, nument, index_entry);
      app.ent   = rarrayalloc(doc_region, nument, index_entry);

      iface.num = 0;
      comp.num  = 0;
      app.num   = 0;
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
        if( !strcmp(p,".ti.html") ) {
          insert_entry(&iface, dent->d_name, ".ti.html");
          continue;
        }
        if( !strcmp(p,".td.html") ) {
          insert_entry(&comp, dent->d_name, ".td.html");
          continue;
        }

        // scan back one more, for app files
        p--;
        while(p > dent->d_name  &&  *p != '.') p--;
        if( !strcmp(p,".td.app.html") )
          insert_entry(&app, dent->d_name, ".td.app.html");
      }
    }

    // sort
    qsort(iface.ent, iface.num, sizeof(index_entry), index_entry_comparator);
    qsort(comp.ent,  comp.num,  sizeof(index_entry), index_entry_comparator);
    qsort(app.ent,   app.num,   sizeof(index_entry), index_entry_comparator);
  }


  // Interface index.
  print_index_file("interfaces.html", &iface);
  

  // Componenet index.  Same format as interface index
  print_index_file("components.html", &comp);


  // App index.  Sort by app name, 
  print_index_file("apps.html", &app);


  // top-level index file (?)

}



//////////////////////////////////////////////////
// Create whole-app description page
//////////////////////////////////////////////////
static void generate_app_page(const char *filename, cgraph cg) 
{
  char *appname, *p;
  char *fname, *basename;
  FILE *f;


  // FIXME: how do we determine whether or not the main component is
  // intended to be a complete app?  For now, we only attempt a
  // whole-app file if there were no compilation errors.
  if( errorcount ) 
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
  

  // create names for the output files
  //basename = doc_filename_with_ext(filename, ".app");
  basename = rstralloc(doc_region, strlen(filename) + strlen(".app") + 1);
  strcpy(basename, filename);
  strcat(basename, ".app");

  // generate the file
  fname    = doc_filename_with_ext(filename, ".app.html");
  f = open_outfile(fname); assert(f);

  fprintf(f, "<html>\n");
  fprintf(f, "<head><title>App: %s</title></head>\n", appname);
  fprintf(f, "<body>\n");
  fprintf(f, "<h1 align=\"center\">App: %s</h1>\n", appname);

  print_cg_html(appname, basename, cg, TRUE);

  fprintf(f, "</body>\n");
  fprintf(f, "</html>\n");

  close_outfile(f);

}



//////////////////////////////////////////////////
// Generate all docs
//////////////////////////////////////////////////
void generate_docs(const char *filename, cgraph cg)
{
  char old_wd[1024];

  // if no docdir is specified, then the user didn't request doc generation
  if( !docdir ) 
    return;

  // Initialization
  {
    // create the region
    // NOTE: using the region this way makes this module non-reentrant
    doc_region = newregion();

    // get the current working directory
    assert(getcwd(old_wd, sizeof(old_wd)));

    // set up dir info
    find_currdir_prefix(old_wd);

    // cd to the docdir 
    mkdir(docdir, 0755);
    assert(chdir(docdir) == 0);
  }


  // examine program

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

    assert(chdir(old_wd) == 0);
  }
}

