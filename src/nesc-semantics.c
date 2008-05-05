/* This file is part of the nesC compiler.
   Copyright (C) 2002 Intel Corporation

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

#include "parser.h"
#include "nesc-semantics.h"
#include "semantics.h"
#include "nesc-decls.h"
#include "c-parse.h"
#include "AST_utils.h"
#include "nesc-paths.h"
#include "nesc-cpp.h"
#include "nesc-env.h"
#include "nesc-component.h"
#include "nesc-interface.h"
#include "edit.h"
#include "c-parse.h"
#include "semantics.h"
#include "nesc-attributes.h"
#include "attributes.h"
#include "init.h"
#include "unparse.h"
#include "nesc-deputy.h"

#include <ctype.h>
#include <errno.h>

bool nesc_attributep(gcc_attribute a)
/* Returns: TRUE if a is a nesc-specific attribute
 */
{
  const char *name = a->word1->cstring.data;

  return !strcmp(name, "C") || 
    !strcmp(name, "spontaneous") ||
    !strcmp(name, "combine") ||
    !strcmp(name, "nx_base") ||
    !strcmp(name, "nx_base_le") ||
    !strcmp(name, "nx_base_be") ||
    !strcmp(name, "hwevent") ||
    !strcmp(name, "atomic_hwevent");
}

type get_actual_function_type(type t)
/* Returns: The actual function type for a (possibly generic) type t
     representing the type of a function/command/event
 */
{
  if (type_generic(t))
    return type_function_return_type(t);
  else
    return t;
}

function_declarator ddecl_get_fdeclarator(data_declaration fndecl)
/* Effects: Returns fndecl's function_declarator
   Requires: fndecl represent a function or function pointer
*/
{
  declarator fd;

  if (is_variable_decl(fndecl->ast))
    fd = CAST(variable_decl, fndecl->ast)->declarator;
  else
    fd = CAST(function_decl, fndecl->ast)->declarator;

  return get_fdeclarator(fd);
}

declaration ddecl_get_gparms(data_declaration fndecl)
/* Effects: Returns the declaration list for fndecl's generic parameters 
   Requires: fndecl represent a function or function pointer
*/
{
  /* For functions in interfaces we get them from the interface ref */
  if (fndecl->kind == decl_function && fndecl->interface)
    return CAST(interface_ref, fndecl->interface->ast)->gparms;

  /* Otherwise we get them from the function's declarator */
  return ddecl_get_fdeclarator(fndecl)->gparms;
}

bool ddecl_is_command_or_event(data_declaration decl)
{
  return decl->kind == decl_function &&
    (decl->ftype == function_event || decl->ftype == function_command);
}

bool nesc_filename(const char *name)
{
  char *dot = strrchr(lbasename(name), '.');

  if (dot)
    {
      if (!strcmp(dot, ".nc"))
	return TRUE;
    }
  return FALSE; /* C by default */
}

const char *element_name(region r, const char *path)
/* Returns: Return the "identifier part"
     of path, i.e., remove any directory and extension
     The returned string is allocated in region r.
*/
{
  const char *base, *dot;

  base = lbasename(path);
  dot = strrchr(base, '.');

  if (dot)
    {
      /* Extract id */
      char *copy = rarrayalloc(r, dot - base + 1, char);

      memcpy(copy, base, dot - base);
      copy[dot - base] = '\0';

      return copy;
    }
  else
    return rstrdup(r, base);
}

const char *language_name(source_language l)
{
  switch (l)
    {
    case l_interface: return "interface";
    case l_component: case l_implementation: return "component";
    case l_c: return "C file";
    case l_any: return "";
    default: assert(0); return "BUG";
    }
}

node compile(location loc, nesc_declaration container,
	     const char *name, bool name_is_path)
{
  source_language l = container ? container->kind : l_c;
  const char *path =
    name_is_path ? name : find_nesc_file(parse_region, l, name);
  node parse_tree = NULL;
  struct semantic_state old_semantic_state = current;
  bool ok;

  if (!path)
    error_with_location(loc, "%s %s not found", language_name(l), name);
  else
    {
      if (flag_verbose)
	fprintf(stderr, "preprocessing %s\n", path);

      current.file = container;
      current.fileregion = newregion();
      start_semantics(l_c, NULL, global_env);
      ok = start_lex(l, path);
      save_pp_file_start(path);
      if (!ok)
	error_with_location(loc, "failed to preprocess %s", path);
      else
	parse_tree = parse();
      deleteregion_ptr(&current.fileregion);
      end_lex();
      save_pp_file_end();
    }

  current = old_semantic_state;

  return parse_tree;
}

nesc_decl dummy_nesc_decl(location loc, nesc_declaration d)
{
  word wname = build_word(parse_region, d->name);
  nesc_decl nd;

  switch (d->kind)
    {
    case l_component: {
      environment env = new_environment(parse_region, global_env, TRUE, FALSE);
      implementation impl = CAST(implementation,
	new_module(parse_region, loc, env, NULL));
      nd = CAST(nesc_decl,
		new_component(parse_region, dummy_location, wname, NULL, FALSE, NULL, NULL, impl));
      break;
    }
    case l_interface:
      nd = CAST(nesc_decl,
		new_interface(parse_region, loc, wname, NULL, NULL));
      break;
    default:
      assert(0);
      nd = NULL;
      break;
    }
  d->ast = nd;
  nd->cdecl = d;

  build(nd);

  return nd;
}

void build(nesc_decl ast)
{
  struct semantic_state old_semantic_state = current;
  nesc_declaration decl = ast->cdecl;

  current.container = decl;

  resolve_deputy_scopes(ast);

  switch (decl->kind)
    {
    case l_interface:
      build_interface(parse_region, decl);
      break;
    case l_component:
      build_component(parse_region, decl);
      break;
    default:
      assert(0);
    }

  current = old_semantic_state;
}

nesc_declaration load(source_language sl, location l,
		      const char *name, bool name_is_path)
{
  /* The l_any stuff is a bit of a hack. It's for use from nesc-main.c
     only, to allow loading something whose "kind" is not yet known.
     When using l_any, the global environment will record "name" as being
     a component, but the returned declaration may be a component or 
     interface. */
  const char *element = name_is_path ? element_name(parse_region, name) : name;
  const char *actual_name;
  node ptree;
  nesc_decl ast = NULL;
  nesc_declaration decl =
    new_nesc_declaration(parse_region, sl == l_any ? l_component : sl, element);

  /* We don't get duplicates as we only load on demand */
  nesc_declare(decl);

  ptree = compile(l, decl, name, name_is_path);
  if (ptree)
    {
      bool badkind;

      ast = CAST(nesc_decl, ptree);
      build(ast);

      actual_name = ast->word1->cstring.data;
      badkind = sl != l_any && ast->cdecl->kind != sl;
      if (badkind || strcmp(element, actual_name))
	warning_or_error_with_location(!badkind, ast->location,
	  "expected %s `%s', but got %s '%s'",
	  language_name(sl), element,
	  language_name(ast->cdecl->kind), actual_name);

      /* Force creation of dummy AST if we get wrong kind (this avoids
         a duplicate error message in require) */
      if (badkind)
	ast = NULL;
    }

  if (!ast)
    ast = dummy_nesc_decl(new_location(name, 0), decl);

  return ast->cdecl; /* can be different from decl when sl == l_any */
}

nesc_declaration start_nesc_entity(source_language sl, word name)
{
  nesc_declaration decl;

  /* If the kind of entity in the file matches the expected kind
     (passed to load), reuse the existing declaration. Otherwise
     make a temporary one to use while loading the file
  */
  if (sl == current.file->kind)
    decl = current.file;
  else
    decl = new_nesc_declaration(parse_region, sl, name->cstring.data);

  get_latest_docstring(&decl->doc, permanent, NULL);

  start_semantics(sl, decl, decl->env);

  return decl;
}

bool is_module_local_static(data_declaration ddecl)
{
  return ddecl->kind == decl_variable &&
    (ddecl->vtype == variable_static && ddecl->container_function &&
     ddecl->container_function->container); 
}

bool is_module_variable(data_declaration ddecl)
{
  return ddecl->kind == decl_variable &&
    ddecl->Cname == FALSE &&
    /* top-level module var or local static module var */
    (ddecl->container || is_module_local_static(ddecl));
}

nesc_declaration ddecl_container(data_declaration ddecl)
{
  while (ddecl->container_function)
    ddecl = ddecl->container_function;

  return ddecl->container;
}

nesc_declaration tdecl_container(tag_declaration tdecl)
{
#if 0
  if (tdecl->container_function)
    return ddecl_container(tdecl->container_function);
  else
#endif
    return tdecl->container;
}

const char *make_intf_printname(const char *iname, const char *fname)
/* Returns: string "iname.fname" allocated in current.fileregion
 */
{
  size_t ilen = strlen(iname);
  size_t dlen = strlen(fname);
  char *fullname = rstralloc(current.fileregion, ilen + dlen + 2);

  memcpy(fullname, iname, ilen);
  fullname[ilen] = '.';
  strcpy(fullname + ilen + 1, fname);

  return fullname;
}

const char *decl_printname(data_declaration ddecl)
{
  if (ddecl_is_command_or_event(ddecl) && ddecl->interface)
    return make_intf_printname(ddecl->interface->name, ddecl->name);
  else
    return ddecl->name;
}

data_declaration get_function_ddecl(expression e)
/* Returns: If e denotes a specific function, return its data_declaration
     Otherwise return NULL
*/
{
  if (is_identifier(e))
    {
      identifier id = CAST(identifier, e);

      if (id->ddecl->kind == decl_function)
	return id->ddecl;
    }
  else if (is_interface_deref(e))
    return CAST(interface_deref, e)->ddecl;

  return NULL;
}

data_declaration declare_function(location loc, const char *name, type signature)
/* Effects: If 'name' is already declared, check that it is a function with
     the specified signature.
     If it isn't declared, declare it as a function with the specified
     signature.
   Returns: data_declaration for the function, or NULL if an error was
     reported to the user.
*/
{
  data_declaration fdecl;
  bool ok = FALSE;

  /* If function already declared, declaration should match signature.
     If not, we declare it with given signature */
  fdecl = lookup_id(name, FALSE);
  if (fdecl)
    {
      if (fdecl->kind != decl_function ||
	  !(fdecl->ftype == function_normal || fdecl->ftype == function_static))
	error_with_location(loc, "function `%s' is not a C function",
			    name);
      else if (!type_compatible_unqualified(fdecl->type, signature))
	error_with_location(loc, "function `%s' does not have the right signature",
			    name);
      else
	ok = TRUE;
    }
  else
    {
      struct data_declaration tempdecl;
      declaration dummy = make_error_decl();

      /* Declare function  */
      dummy->location = loc;
      init_data_declaration(&tempdecl, dummy, name, signature);
      tempdecl.kind = decl_function;
      tempdecl.ftype = function_normal;
      tempdecl.isexternalscope = tempdecl.isfilescoperef = TRUE;
      fdecl = declare(current.env, &tempdecl, FALSE);

      ok = TRUE;
    }
  return ok ? fdecl : NULL;
}

void handle_combine_attribute(location loc, const char *combiner, type *t)
{
  data_declaration cdecl = 
    declare_function(loc, combiner,
		     build_function_type(parse_region, *t, *t, *t, NULL));

  if (cdecl)
    *t = make_combiner_type(*t, cdecl);
}

void handle_nxbase_attribute(location loc, bool be, bool allow_bf, const char *basename,
			     data_declaration ddecl)
{
  region r = parse_region;
  char *encoder_name, *decoder_name;
  type t = ddecl->type;

  encoder_name = rstralloc(r, strlen(basename) + 13);
  sprintf(encoder_name, "__nesc_hton_%s", basename);
  decoder_name = rstralloc(r, strlen(basename) + 13);
  sprintf(decoder_name, "__nesc_ntoh_%s", basename);

  ddecl->encoder = /* takes buffer and original value. returns original value */
    declare_function(loc, encoder_name,
		     build_function_type(r, t, ptr_void_type, t, NULL));

  ddecl->decoder = /* takes buffer and returns decoded value */
    declare_function(loc, decoder_name,
		     build_function_type(r, t, const_ptr_void_type, NULL));

  if (allow_bf)
    {
      encoder_name = rstralloc(r, strlen(basename) + 15);
      sprintf(encoder_name, "__nesc_htonbf_%s", basename);
      decoder_name = rstralloc(r, strlen(basename) + 15);
      sprintf(decoder_name, "__nesc_ntohbf_%s", basename);

      /* bitfields take additional offset, length fields */
      ddecl->bf_encoder =
	declare_function(loc, encoder_name,
			 build_function_type(r, t, ptr_void_type, unsigned_int_type, unsigned_char_type, t, NULL));

      ddecl->bf_decoder =
	declare_function(loc, decoder_name,
			 build_function_type(r, t, const_ptr_void_type, unsigned_int_type, unsigned_char_type, NULL));
    }
  ddecl->isbe = be;

  /* We do this even if we got an error, to ensure ddecl gets treated as
     a network type. */
  ddecl->basetype = t;
}

/* Create definition for template parameter 'elements d' with attributes
   attributes.
   Returns the declaration for the parameter.
*/
declaration declare_template_parameter(declarator d, type_element elements,
				       attribute attributes)
{
  /* There must be at least a declarator or some form of type specification */
  location l = d ? d->location : elements->location;
  variable_decl vd =
    new_variable_decl(parse_region, l, d, attributes, NULL, NULL, NULL);
  data_decl dd =
    new_data_decl(parse_region, l, elements, CAST(declaration, vd));
  data_declaration ddecl = NULL, old_decl = NULL;
  struct data_declaration tempdecl;
  dd_list extra_attr;
  int class;
  scflags scf;
  const char *name;
  bool defaulted_int;
  type parm_type;

  parse_declarator(elements, vd->declarator, FALSE, FALSE,
		   &class, &scf, NULL, &name, &parm_type,
		   &defaulted_int, NULL, &extra_attr);
  vd->declared_type = parm_type;

  /* Storage class checks */
  if (class)
    {
      /* Detect "typedef t", to declare a type parameter */
      if (class == RID_TYPEDEF && defaulted_int &&
	  is_identifier_declarator(d))
	return declare_type_parameter(d->location,
				      CAST(identifier_declarator, d)->cstring,
				      attributes, extra_attr);
      else if (class == RID_TYPEDEF && d == NULL) 
	{
	  /* Recognise "typedef TYPENAME", and declare TYPENAME as a 
	     type parameter (i.e., we're shadowing a global typedef) */
	  type_element elem;
	  typename tname;
	  bool ok = TRUE;

	  /* Check there's only a typedef and a typename */
	  scan_type_element (elem, elements)
	    if (is_typename(elem))
	      tname = CAST(typename, elem);
	    else if (!(is_attribute(elem) ||
		       (is_rid(elem) && CAST(rid, elem)->id == RID_TYPEDEF)))
	      ok = FALSE;

	  if (ok && tname)
	    {
	      cstring cname = make_cstring(parse_region, tname->ddecl->name,
					   strlen(tname->ddecl->name));
	      return
		declare_type_parameter(l, cname, attributes, extra_attr);
	    }
	}
    }

  if (!name)
    error("no name specified for parameter");
  else if (class)
    error("storage class specified for parameter `%s'", name);

  check_variable_scflags(scf, vd->location, "parameter", name);

/* Allow real, integral and string types. Not allowing complex for now,
   though it would be a trivial extension */
  if (!(type_real(parm_type) || type_chararray(parm_type, TRUE))) 
    error("only char [] and arithmetic types allowed as component arguments");

  if (type_array(parm_type))
    /* Transfer const-ness of array into that of type pointed to.  */
    parm_type =
      make_pointer_type(qualify_type1(type_array_of(parm_type), parm_type));

  init_data_declaration(&tempdecl, CAST(declaration, vd), name, parm_type);
  tempdecl.kind = decl_constant;
  tempdecl.substitute = TRUE;
  tempdecl.definition = tempdecl.ast;

  old_decl = lookup_id(tempdecl.name, TRUE);
  if (old_decl && duplicate_decls(&tempdecl, old_decl, FALSE, FALSE))
    ddecl = old_decl;
  else
    ddecl = declare(current.env, &tempdecl, FALSE);
  vd->ddecl = ddecl;

  ignored_dd_attributes(extra_attr);
  ignored_attributes(attributes);

  return CAST(declaration, dd);
}

declaration declare_type_parameter(location l, cstring id, attribute attribs,
				   dd_list extra_attr)
{
  type_parm_decl d = new_type_parm_decl(parse_region, l, id, NULL);
  data_declaration ddecl;

  if ((ddecl = lookup_id(id.data, TRUE)))
    error("duplicate parameter name `%s' in parameter list", id.data);
  else
    {
      struct data_declaration tempdecl;

      init_data_declaration(&tempdecl, CAST(declaration, d), id.data,
			    error_type);
      tempdecl.kind = decl_typedef;
      tempdecl.typevar_kind = typevar_normal;
      tempdecl.definition = tempdecl.ast;
      handle_decl_attributes(attribs, &tempdecl);
      handle_decl_dd_attributes(extra_attr, &tempdecl);
      ddecl = declare(current.env, &tempdecl, FALSE);
      ddecl->type = make_variable_type(ddecl);
    }
  d->ddecl = ddecl;

  return CAST(declaration, d);
}

expression make_type_argument(asttype t)
{
  type_argument e = new_type_argument(parse_region, t->location, t);

  e->type = t->type;

  return CAST(expression, e);
}

nesc_declaration original_component(nesc_declaration c)
{
  while (c->original)
    c = c->original;

  return c;
}

static void attr_C_tdecl(nesc_attribute attr, tag_declaration tdecl)
{
  if (tdecl->container_function)
    error_with_location(attr->location, "`@C()' is for symbols with external scope only");
  else if (current.container && current.container->abstract)
    error_with_location(attr->location, "@C() cannot be used inside generic components");
  else
    tdecl->Cname = TRUE;
}

static void attr_C_decl(nesc_attribute attr, data_declaration ddecl)
{
  if (!ddecl->isexternalscope)
    error_with_location(attr->location, "`@C()' is for symbols with external scope only");
  else if (current.container && current.container->abstract)
    error_with_location(attr->location, "@C() cannot be used inside generic components");
  else
    ddecl->Cname = TRUE;
}

static bool require_function(nesc_attribute attr, data_declaration ddecl)
{
  if (ddecl->kind == decl_function && ddecl->ftype == function_normal)
    return TRUE;

  error_with_location(attr->location, "`@%s()' is for external functions only", attr->word1->cstring.data);
  return FALSE;
}

static void attr_hwevent_decl(nesc_attribute attr, data_declaration ddecl)
{
  if (require_function(attr, ddecl))
    {
      ddecl->async = TRUE;
      ddecl->spontaneous = c_call_nonatomic;
    }
}

static void attr_atomic_hwevent_decl(nesc_attribute attr, data_declaration ddecl)
{
  if (require_function(attr, ddecl))
    {
      ddecl->async = TRUE;
      ddecl->spontaneous = c_call_atomic;
    }
}


static void attr_spontaneous_decl(nesc_attribute attr, data_declaration ddecl)
{
  if (require_function(attr, ddecl))
    {
      /* The test avoids overriding the effect of atomic_hwevent */
      if (!ddecl->spontaneous)
	ddecl->spontaneous = c_call_nonatomic;
    }
}

static void attr_combine_decl(nesc_attribute attr, data_declaration ddecl)
{
  ivalue fn_init = lookup_attribute_field(attr, "fn");
  data_declaration fn_name_ddecl;
  char *fn_name;

  if (fn_init && fn_init->kind == iv_base &&
      (fn_name_ddecl = string_ddecl(fn_init->u.base.expr)) &&
      (fn_name = ddecl2str(parse_region, fn_name_ddecl)))
    {
      if (ddecl->kind == decl_typedef)
	handle_combine_attribute(attr->location, fn_name, &ddecl->type);
      else
	error_with_location(attr->location, "@combine(\"function-name\") can only be used with typedef");
    }
  else
    error_with_location(attr->location, "usage is @combine(\"function-name\")");
}

static void attr_macro_tdecl(nesc_attribute attr, tag_declaration tdecl)
{
  ivalue macro_name_init = lookup_attribute_field(attr, "macro_name");
  data_declaration macro_name_ddecl;
  char *macro_name, *m;

  if (tdecl->kind != kind_attribute_ref)
    {
      error_with_location(attr->location, "@macro() can only be applied to attribute declarations");
      return;
    }

  if (!(macro_name_init && macro_name_init->kind == iv_base &&
	(macro_name_ddecl = string_ddecl(macro_name_init->u.base.expr)) &&
	(macro_name = ddecl2str(parse_region, macro_name_ddecl))))
    goto bad;

  /* Check that the symbol name is a valid macro name (C symbol) */
  if (!(isalpha(macro_name[0]) || macro_name[0] == '_'))
    goto bad;

  for (m = macro_name + 1; *m; m++)
    if (!(isalnum(*m) || *m == '_'))
      goto bad;

  tdecl->macro_name = macro_name;
  return;

 bad:
  error_with_location(attr->location, "usage is @macro(\"macro-name\")");
}

void init_internal_nesc_attributes(void)
{
  define_internal_attribute("C", NULL, attr_C_decl, attr_C_tdecl, NULL, NULL,
			    NULL);
  define_internal_attribute("hwevent", NULL, attr_hwevent_decl, NULL, NULL,
			    NULL, NULL);
  define_internal_attribute("atomic_hwevent", NULL, attr_atomic_hwevent_decl,
			    NULL, NULL, NULL, NULL);
  define_internal_attribute("spontaneous", NULL, attr_spontaneous_decl, NULL,
			    NULL, NULL, NULL);
  define_internal_attribute("combine", NULL, attr_combine_decl, NULL, NULL,
			    NULL,
			    "fn", make_pointer_type(char_type), NULL);
  define_internal_attribute("macro", NULL, NULL, attr_macro_tdecl, NULL, NULL,
			    "macro_name", make_pointer_type(char_type), NULL);
}

void check_name(const char *name)
{
  const char *occ, *sep = get_function_separator();
  int lsep = strlen(sep);

  /* Ignore leading instances of the separator */
  while (!strncmp(name, sep, lsep))
    name++;

  occ = strstr(name, sep);
  if (!occ)
    return;

  /* Ignore trailing instances of the separator */
  while (occ[lsep] && !strncmp(occ + 1, sep, lsep))
    occ++;
  if (occ[lsep])
    {
      static int first = 1;

      warning("symbol `%s' contains the separator `%s' used in generated code",
	      name, sep);
      if (first)
	{
	  warning("This can cause bugs or compile errors when the C code");
	  warning("generated by nesC is passed to the underlying C compiler");
	  first = FALSE;
	}
    }
}
