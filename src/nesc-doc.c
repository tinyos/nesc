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

  FIXME: need to document the doc system...  ;-)

*/
#include <unistd.h>
#include <sys/stat.h>
#include <sys/types.h>

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
    error( "ERROR: The current directory is not a subdir of one\n");
    error( "       of the source topdirs!  Please correct or add\n");
    error( "       the -nesc-topdir= options\n");
    error( "\n");
    error( "CWD: %s\n",cwd);
    error( "\n");
    error( "TOPDIRS:\n");
    for(i=0; i<num_topdirs; i++) {
      error( "     %s\n", topdir[i]);
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
  interface_declaration idecl;
  interface iface;

  idecl = require_interface(&doc_empty_location, interface_name);
  iface = CAST(interface, idecl->ast);

  assert(iface->location != dummy_location);

  return doc_filename(iface->location->filename);
}


static char *component_docfile_name(const char *component_name) {
  component_declaration cdecl;
  component comp;

  cdecl = require_component(NULL /* location */, component_name);
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
  //prt_declarator(d->declarator, d->qualifiers, d->attributes, d->ddecl, psd_print_default|psd_skip_container);


  // FIXME: did this fix the variable declaration printing?
  prt_type_elements(ddecl->modifiers, FALSE); 
  prt_type_elements(CAST(type_element, ddecl->attributes), FALSE);

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




/**
 * print a function header, from either a function_decl, or a
 * data_decl,variable_decl pair.  Only one or the other should be defined.
 **/
static void print_function_header(function_decl fd, data_decl dd, variable_decl vd) 
{
  if( fd ) {
    assert(dd==NULL);
    assert(vd==NULL);
    prt_declarator(fd->declarator, fd->qualifiers, fd->attributes, fd->ddecl, psd_print_default|psd_skip_container);
  }
  else {
    assert(fd==NULL);
    assert(dd);    
    assert(vd);
    prt_type_elements(dd->modifiers, FALSE); 
    prt_type_elements(CAST(type_element, dd->attributes), FALSE);
    prt_declarator(vd->declarator, NULL, vd->attributes, vd->ddecl, psd_skip_container);
  }
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

    if(ldoc)  {output("    <a href=\"#"); print_function_header(fd,dd,vd); output("\">\n");}
    else if(ifile) {output("    <a href=\"%s#",ifile); print_function_header(fd,dd,vd); output("\">\n");}

    output("        ");  print_function_header(fd,dd,vd);  output("\n");

    if(ldoc || ifile)  output("    </a>\n");

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

    if(ifile) {output("    <a href=\"%s#",ifile); print_function_header(fd,dd,vd); output("\">");}
    output("        <h4>"); print_function_header(fd,dd,vd);  output("</h4>\n");
    if(ifile) output("    </a>\n");

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
static void print_html_banner(const char *text) {
  output("<table BORDER=\"1\" CELLPADDING=\"3\" CELLSPACING=\"0\" WIDTH=\"100%%\">\n");
  output("<tr BGCOLOR=\"#CCCCFF\"><td>\n");
  output("%s",text);
  output("</td></tr></table>\n");
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
  component_declaration req;
  component_declaration prov;
  interface_declaration iface;
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
  if((*prov)->interface == NULL) 
    return FALSE;


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
    return FALSE;
  }

}


#define iface_node_name( ep ) \
                    ( ep->component ? \
                      ep->component->name : \
                      ( ep->function->container ? \
                        ep->function->container->name : \
                        ep->function->ast->location->filename))



/**
 * Display the nodes in the graph, along w/ info on edges
 **/
static void print_cg_html(const char *component_name, cgraph cg) {
  gnode n;
  dhash_table table = NULL;
  char *iface_dot, *func_dot;
  char *iface_gif, *func_gif;
  char *iface_cmap, *func_cmap;
  FILE *iface_file, *func_file;

  // FIXME: disable the function graph for now
  bool do_func_graph = FALSE;


  // create filenames
  iface_dot = doc_filename_with_ext(component_name,".if.dot");
  iface_gif = doc_filename_with_ext(component_name,".if.gif");
  iface_cmap = doc_filename_with_ext(component_name,".if.cmap");

  func_dot = doc_filename_with_ext(component_name,".func.dot");
  func_gif = doc_filename_with_ext(component_name,".func.gif");
  func_cmap = doc_filename_with_ext(component_name,".func.cmap");


  // open outfiles
  iface_file = fopen(iface_dot, "w");  assert(iface_file);
  func_file  = fopen(func_dot,  "w");  assert(func_file);


  // start the dot output
  fprintf(iface_file, "digraph \"%s_if\" {\n    fontsize=8;\n", component_name);
  fprintf(func_file, "digraph \"%s_func\" {\n    fontsize=8;\n", component_name);
  

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
        {
          // FIXME: print the node info (ie URLs)

          // print the edge info
          fprintf(func_file, "    %s -> %s [label = \"%s.%s\"];\n", 
                  ep1->component ? ep1->component->name : "C_code", 
                  ep2->component ? ep2->component->name : "C_code", 
                  ep2->interface ? ep2->interface->name : "C_code", 
                  ep2->function->name);
        }

        // connection graph - interfaces  (input for /usr/bin/dot)
        {
          endp req, prov;
         
          // allocate a new hashtable for removing duplicates
          if(table == NULL) {
            table = new_iface_graph_table();
          }
 
          if( !connection_already_printed(table, ep1, ep2, &req, &prov) ) {
            /*
            fprintf(stderr, "\n");
            fprintf(stderr, "     req: %s  %s\n",
                   req->function->ast->location->filename,
                   req->function->definition ? req->function->definition->location->filename : "null");
            fprintf(stderr, "     req: %s  %s  %s\n",
                   req->component ? req->component->ctype->ast->location->filename : "null",
                   req->interface ? req->interface->itype->ast->location->filename : "null",
                   req->function->container ? req->function->container->name : "null");

            fprintf(stderr, "     prov: %s  %s\n",
                   prov->function->ast->location->filename,
                   prov->function->definition ? prov->function->definition->location->filename : "null");
            fprintf(stderr, "     prov: %s  %s  %s\n",
                   prov->component ? prov->component->ctype->ast->location->filename : "null",
                   prov->interface ? prov->interface->itype->ast->location->filename : "null",
                   prov->function->container ? prov->function->container->name : "null");
            */

            // print the node info (ie URLs)
            fprintf(iface_file, "    %s [URL = \"%s\"];\n", 
                    iface_node_name(req),
                    component_docfile_name( iface_node_name(req) ));
            fprintf(iface_file, "    %s [URL = \"%s\"];\n", 
                    iface_node_name(prov),
                    component_docfile_name( iface_node_name(prov) ));

            // print the edge info
            fprintf(iface_file, "    %s -> %s ",
                    iface_node_name( req ),
                    iface_node_name( prov ));

            if(req->interface) {
              fprintf(iface_file, "[label = \"%s\" URL = \"%s\"];\n", 
                      prov->interface->itype->name,
                      interface_docfile_name(prov->interface->itype->name));
            } else {
              //fprintf(_file, "[label = \"%s:%s\"];\n", prov->function->ast->location->filename, prov->function->name);
              fprintf(iface_file, "[label = \"func:%s\"];\n", prov->function->name);
            }
          }
        }
      }
    }

  // clean up the region 
  if(table) 
    deleteregion( regionof(table) );


  // finish up the output files
  fprintf(iface_file, "}\n");  fclose(iface_file);
  fprintf(func_file, "}\n");   fclose(func_file);



  // use dot to generate output
  {
    char cmd[1024];
    int ret;
    bzero(cmd,sizeof(cmd));

    // FIXME: error handling could be better here
    ret = snprintf(cmd,sizeof(cmd)-1,"dot -Tgif -o%s %s", iface_gif, iface_dot); assert(ret > 0);
    ret = system(cmd); assert(ret != -1);
    ret = snprintf(cmd,sizeof(cmd)-1,"dot -Tcmap -o%s %s", iface_cmap, iface_dot); assert(ret > 0);
    ret = system(cmd); assert(ret != -1);

    if( do_func_graph ) {
      ret = snprintf(cmd,sizeof(cmd)-1,"dot -Tgif -o%s %s", func_gif, func_dot); assert(ret > 0);
      ret = system(cmd); assert(ret != -1);
      ret = snprintf(cmd,sizeof(cmd)-1,"dot -Tcmap -o%s %s", func_cmap, func_dot); assert(ret > 0);
      ret = system(cmd); assert(ret != -1);
    }
  }

  // add the HTML
  print_html_banner("<h3>Component Graph</h3>");
  output("<map name=\"comp\">\n");
  copy_file_to_output(iface_cmap);
  output("</map>\n");
  output("<image src=\"%s\" usemap=\"#comp\" border=0>\n", iface_gif);

  if( do_func_graph ) {
    print_html_banner("<h3>Component Function Graph</h3>");
    output("<map name=\"func\">\n");
    copy_file_to_output(func_cmap);
    output("</map>\n");
    output("<image src=\"%s\" usemap=\"#func\" border=0>\n", func_gif);
  }

  // remove temp files
  unlink(iface_dot);
  unlink(iface_cmap);
  unlink(func_dot);
  unlink(func_cmap);

}







//////////////////////////////////////////////////////////////////////
//
// Main routine for creating docs for a component
//
//////////////////////////////////////////////////////////////////////

static void generate_component_html(component_declaration cdecl) 
{
  FILE *outfile;
  component comp = CAST(component, cdecl->ast);


  // open the appropriate output file
  outfile = open_outfile( component_docfile_name(cdecl->name) );


  // start the HTML
  output("
<html>
<head>
<title>Component: %s</title>
</head>

<h1 align=\"center\">Component: %s</h1>

", cdecl->name, cdecl->name);


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
    print_cg_html(CAST(component, cdecl->ast)->location->filename, cdecl->connections);
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


//////////////////////////////////////////////////
// generate HTML for an interface file
//////////////////////////////////////////////////
static void generate_interface_html(interface_declaration idecl) 
{
  FILE *outfile;

  outfile = open_outfile( interface_docfile_name(idecl->name) );
                          

  // start the HTML
  output("
<html>
<head>
<title>Interface: %s</title>
</head>

<h1 align=\"center\">Interface: %s</h1>

", idecl->name, idecl->name);
  
  
  // summary
  {
    interface_functions funcs = CAST(interface, idecl->ast)->interface_functions;
    interface_functions f;
    bool printed_heading = FALSE;

    // defines
    scan_interface_functions(f,funcs) {
      data_decl dd;

      scan_data_decl(dd, CAST(data_decl,f->decls)) {
        variable_decl vd;

        scan_variable_decl(vd, CAST(variable_decl,dd->decls)) {
          if( vd->ddecl->defined ) {
            if(!printed_heading) {print_html_banner("<h3>Defined Functions</h3>"); output("<ul>\n"); printed_heading=TRUE;}
            print_function_html(NULL,dd,vd,in_interface|short_desc);
          }
        }
      }
    }
    if(printed_heading) output("</ul>\n");

    // uses
    printed_heading = FALSE;
    scan_interface_functions(f,funcs) {
      data_decl dd;

      scan_data_decl(dd, CAST(data_decl,f->decls)) {
        variable_decl vd;

        scan_variable_decl(vd, CAST(variable_decl,dd->decls)) {
          if( !vd->ddecl->defined ) {
            if(!printed_heading) {print_html_banner("<h3>Used Functions</h3>"); output("<ul>\n"); printed_heading=TRUE;}
            print_function_html(NULL,dd,vd,in_interface|short_desc);
          }
        }
      }
    }
    if(printed_heading) output("</ul>\n");
  }

  // detailed descriptions
  {
    interface_functions funcs = CAST(interface, idecl->ast)->interface_functions;
    interface_functions f;
    bool printed_heading = FALSE;
    bool first = TRUE;

    // defines
    scan_interface_functions(f,funcs) {
      data_decl dd;

      scan_data_decl(dd, CAST(data_decl,f->decls)) {
        variable_decl vd;

        scan_variable_decl(vd, CAST(variable_decl,dd->decls)) {
          if( vd->ddecl->defined  &&  has_long_desc(NULL,dd,vd)) {
            if(!printed_heading) {print_html_banner("<h3>Defined Functions - Details</h3>"); printed_heading=TRUE;}
            if( !first ) output("<hr>\n");
            print_function_html(NULL,dd,vd,in_interface|long_desc);
            first = FALSE;
          }
        }
      }
    }

    // uses
    printed_heading = FALSE;
    first = TRUE;
    scan_interface_functions(f,funcs) {
      data_decl dd;

      scan_data_decl(dd, CAST(data_decl,f->decls)) {
        variable_decl vd;

        scan_variable_decl(vd, CAST(variable_decl,dd->decls)) {
          if( !vd->ddecl->defined  &&  has_long_desc(NULL,dd,vd)) {
            if(!printed_heading) {print_html_banner("<h3>Used Functions - Details</h3>"); printed_heading=TRUE;}
            if( !first ) output("<hr>\n");
            print_function_html(NULL,dd,vd,in_interface|long_desc);
            first = FALSE;
          }
        }
      }
    }
  }

}





//////////////////////////////////////////////////
// Generate all docs
//////////////////////////////////////////////////
void generate_docs(cgraph cg)
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
    component_declaration cdecl;

    printf("\n    Generating component docs\n");
    env_scan(get_component_env(), &scanner);
    while( env_next(&scanner, &name, (void **)&cdecl) ) {
      // FIXME: remove printf
      printf("        %s\n", cdecl->name);
      generate_component_html(cdecl);
    }
  }

  // generate docs for all interfaces 
  {
    env_scanner scanner;
    const char *name;
    interface_declaration idecl;

    printf("\n    Generating interface docs\n");
    env_scan(get_interface_env(), &scanner);
    while( env_next(&scanner, &name, (void **)&idecl) ) {
      // FIXME: remove printf
      printf("        %s\n", idecl->name);
      generate_interface_html(idecl);
    }
  }

  {
    // generate whole-app wiring page
    FILE *f = open_outfile("apps.Blink.whole_app.html");
    print_cg_html("whole_app", cg);
    close_outfile(f);
  }

  // cleanup
  {
    // kill the region
    // NOTE: using the region this way makes this module non-reentrant
    deleteregion(doc_region);

    assert(chdir(old_wd) == 0);
  }
}

