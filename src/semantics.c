/* This file is part of the nesC compiler.

This file is derived from RC and the GNU C Compiler. It is thus
   Copyright (C) 1987, 88, 89, 92-7, 1998 Free Software Foundation, Inc.
   Copyright (C) 2000-2001 The Regents of the University of California.
Changes for nesC are
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
Boston, MA 02111-1307, USA. */

#include "parser.h"
#include "semantics.h"
#include "flags.h"
#include "c-parse.h"
#include "c-lex.h"
#include "env.h"
#include "expr.h"
#include "stmt.h"
#include "AST_utils.h"
#include "constants.h"
#include "nesc-component.h"
#include "nesc-interface.h"
#include "nesc-semantics.h"
#include "nesc-doc.h"
#include "nesc-cpp.h"
#include "machine.h"
#include "attributes.h"
#include "nesc-task.h"

/* Predefined __builtin_va_list type */
type builtin_va_list_type;
data_declaration builtin_va_arg_decl;
data_declaration builtin_constant_p;

/* The current semantic state */
struct semantic_state current;

dd_list spontaneous_calls;

data_declaration bad_decl;

static type implicit_function_type, dummy_function_type;

static function_declarator dummy_function_declarator;

/* The global environment */
environment global_env;

static known_cst onecst, zerocst;
static expression oneexpr;

environment new_environment(region r, environment parent,
			    bool global_level, bool parm_level)
{
  environment env = ralloc(r, struct environment);

#if 0
  env->fdecl = NULL;
#endif
  env->parent = parent;
  env->parm_level = parm_level;
  env->global_level = global_level;
  env->deputy_scope = FALSE;
  if (parent)
    {
      env->fdecl = parent->fdecl;
      env->id_env = new_env(r, parent->id_env);
      /* ANSI C is weird */
      if (parent->parm_level)
	env->tag_env = parent->tag_env;
      else
	env->tag_env = new_env(r, parent->tag_env);
    }
  else
    {
      env->id_env = new_env(r, NULL);
      env->tag_env = new_env(r, NULL);
    }

  return env;
}

void init_data_declaration(data_declaration dd, declaration ast,
			   const char *name, type t)
{
  dd->kind = 0;
  dd->name = name;
  dd->type = t;
  dd->attributes = NULL;
  dd->safe = flag_default_safe;

  dd->shadowed = NULL;
  dd->ast = ast;
  dd->definition = NULL;
  dd->isexternalscope = FALSE;
  dd->isfilescoperef = FALSE;
  dd->needsmemory = FALSE;
  dd->isused = FALSE;
  dd->in_system_header = ast->location->in_system_header;
  dd->ftype = 0;
  dd->isinline = FALSE;
  dd->noinlinep = FALSE;
  dd->isexterninline = FALSE;
  dd->oldstyle_args = NULL;
  dd->vtype = 0;
  dd->islocal = FALSE;
  dd->isparameter = FALSE;
  dd->islimbo = FALSE;
  dd->value = NULL;
  dd->schars.data = NULL;
  dd->schars.length = 0;
  dd->id = 0;
  dd->defined = FALSE;
  dd->suppress_definition = FALSE;
  dd->uncallable = FALSE;
  dd->async = FALSE;
  dd->actual_async = FALSE;
  dd->required = FALSE;
  dd->itype = NULL;
  dd->gparms = NULL;
  dd->ctype = NULL;
  dd->container = NULL;
  dd->interface = NULL;
  dd->Cname = FALSE;
  dd->nuses = NULL;
  dd->fn_uses = NULL;
  dd->connections = NULL;
  dd->spontaneous = 0;
  dd->magic_fold = NULL;
  dd->substitute = FALSE;
  dd->makeinline = FALSE;
  dd->container_function = NULL;
  dd->use_summary = 0;
  dd->async_access = dd->async_write = FALSE;
  dd->norace = FALSE;
  dd->instantiation = NULL;
  dd->instanceof = NULL;
  dd->call_contexts = dd->extra_contexts = 0;
  dd->printed = FALSE;
  dd->dumped = FALSE;
  dd->encoder = dd->decoder = NULL;
  dd->bf_encoder = dd->bf_decoder = NULL;
  dd->basetype = NULL;
  dd->typevar_kind = typevar_none;
  dd->doc.short_s = dd->doc.long_s = NULL;
  dd->doc.loc = NULL;
}

data_declaration lookup_id(const char *s, bool this_level_only)
{
  return env_lookup(current.env->id_env, s, this_level_only);
}

data_declaration lookup_global_id(const char *s)
{
  return env_lookup(global_env->id_env, s, TRUE);
}

data_declaration declare(environment b, data_declaration from,
			 bool ignore_shadow)
{
  data_declaration dd = ralloc(parse_region, struct data_declaration);

  *dd = *from;

  if (dd->name)
    {
      check_name(dd->name);

      /* In PCC-compatibility mode, extern decls of vars with no current decl
	 take effect at top level no matter where they are.  */
      /* We don't do the GCC "type exists at global scope" check because
	 it's mostly meaningless.
	 XXX: review if I start freeing mem earlier */
      if (!b->global_level)
	{
	  /* Here to install a non-global value.  */
	  data_declaration shadowed = lookup_id(dd->name, FALSE);
	  char *warnstring = 0;

	  /* Warn if shadowing an argument at the top level of the body.  */
	  if (shadowed && shadowed->islocal
	      /* This warning doesn't apply to the parms of a nested fcn.  */
	      && !b->parm_level
	      /* Check that this is one level down from the parms.  */
	      && b->parent->parm_level
	      /* Check that the decl exists in the parm level */
	      && env_lookup(b->parent->id_env, dd->name, TRUE))
	    {
	      if (shadowed->isparameter)
		warnstring = "declaration of `%s' shadows a parameter";
	      else
		warnstring = "declaration of `%s' shadows a symbol from the parameter list"; 
	    }
	  /* Maybe warn if shadowing something else.  */
	  else if (warn_shadow && !ignore_shadow)
	    {

	      if (dd->isparameter
		  && b->parent->parm_level)
		/* Don't warn about the parm names in function declarator
		   within a function declarator.
		   It would be nice to avoid warning in any function
		   declarator in a declaration, as opposed to a definition,
		   but there is no way to tell it's not a definition.  */
		;
	      else if (shadowed && shadowed->isparameter)
		warnstring = "declaration of `%s' shadows a parameter";
	      else if (shadowed && shadowed->islocal)
		warnstring = "declaration of `%s' shadows previous local";
	      else if (shadowed)
		warnstring = "declaration of `%s' shadows global declaration";

	    }
	  if (warnstring)
	    {
	      (error_shadow ? error : warning)(warnstring, dd->name);
	      (error_shadow ? error_with_location : warning_with_location)(shadowed->ast->location, "location of shadowed declaration");
	    }
	}

      /* Parameters are declared before the function_decl is created
	 (and may be declared in several places). The id's of parameters
	 are set in start_function */
      if (dd->kind == decl_variable && dd->islocal && !dd->isparameter)
	dd->id = b->fdecl->nlocals++;

   }
  assert(!dd->islimbo);

  if (b->global_level)
    dd->container = current.container;
  if (current.function_decl)
    dd->container_function = current.function_decl->ddecl;

  env_add(b->id_env, dd->name, dd);
  if (!dd->container || (dd->container && !dd->container->abstract))
    {
      /* C names go all the way to the top... */
      if (dd->Cname)
	env_add(global_env->id_env, dd->name, dd);
      if (!dd->container_function && flag_c && !dd->spontaneous)
	dd->spontaneous = c_call_nonatomic;
      if (dd->spontaneous || (getenv("ALLCODE") && dd->kind == decl_function))
	dd_add_last(parse_region, spontaneous_calls, dd);
    }

  return dd;
}

tag_declaration declare_tag_env(environment env, tag_ref t)
{
  tag_declaration tdecl = ralloc(parse_region, struct tag_declaration);
  const char *name = t->word1 ? t->word1->cstring.data : NULL;

  tdecl->kind = t->kind;
  tdecl->name = name;
  tdecl->size = tdecl->alignment = tdecl->user_alignment = cval_top;
#if 0
  /* ralloc guarantees 0 / NULL */
  tdecl->fields = NULL;
  tdecl->fieldlist = NULL;
  tdecl->defined = tdecl->being_defined = FALSE;
  tdecl->fields_const = tdecl->fields_volatile = FALSE;
  tdecl->transparent_union = FALSE;
  tdecl->collapsed = FALSE;
  tdecl->size_cc = FALSE;
  tdecl->container = NULL;
  tdecl->dumped = FALSE;
  tdecl->instanceof = NULL;
  tdecl->Cname = FALSE;
  tdecl->macro_name = NULL;
  tdecl->deputy_scope = FALSE;
#endif

  if (name)
    {
      tdecl->shadowed = env_lookup(env->tag_env, name, FALSE);
      if (tdecl->shadowed && warn_shadow)
	(error_shadow ? error : warning)
	  ("tag %s shadows enclosing struct/union/enum", name);
    }
  else
    tdecl->shadowed = NULL;

  if (env->global_level)
    tdecl->container = current.container;
  if (current.function_decl)
    tdecl->container_function = current.function_decl->ddecl;

  /* We register all tags in the environment, even unnamed ones. */
  env_add(env->tag_env, name, tdecl);

  return tdecl;
}

tag_declaration lookup_tag_env(environment env, tag_ref t, bool this_level_only)
{
  tag_declaration found =
    env_lookup(env->tag_env, t->word1->cstring.data, this_level_only);

  /* Check if wrong kind */
  if (found && found->kind != t->kind)
    current.pending_invalid_xref = t;

  return found;
}

tag_declaration declare_tag(tag_ref t)
{
  return declare_tag_env(current.env, t);
}

tag_declaration lookup_tag(tag_ref t, bool this_level_only)
{
  return lookup_tag_env(current.env, t, this_level_only);
}

tag_declaration declare_global_tag(tag_ref t)
{
  return declare_tag_env(global_env, t);
}

tag_declaration lookup_global_tag(tag_ref t)
{
  return lookup_tag_env(global_env, t, TRUE);
}

/* If elements is 'struct foo' shadow tag foo in the current environment */
void shadow_tag(type_element elements)
{
  shadow_tag_warned(elements, 0);
}

/* Like shadow_tag, but warned is: 1 => we have done a pedwarn;
   2 => we have done a warning, but no pedwarn. */
void shadow_tag_warned(type_element elements, int warned)
{
  type_element elem;
  int found_tag = 0;

  current.pending_invalid_xref = 0;

  scan_type_element (elem, elements)
    {
      if (is_tag_ref(elem))
	{
	  tag_ref tag = CAST(tag_ref, elem);
	  word name = tag->word1;

	  found_tag++;

	  if (name == 0)
	    {
	      if (warned != 1 && !is_enum_ref(elem))
		/* Empty unnamed enum OK */
		{
		  pedwarn ("unnamed struct/union that defines no instances");
		  warned = 1;
		}
	    }
	  else
	    {
	      void *tagdecl = lookup_tag(tag, TRUE);

	      if (tagdecl == 0)
		declare_tag(tag);
	      else
		pending_xref_error();
	    }
	}
      else
	{
	  if (!warned && ! current.lex.input->l.in_system_header)
	    {
	      warning("useless keyword or type name in empty declaration");
	      warned = 2;
	    }
	}
    }

  if (found_tag > 1)
    error("two types specified in one empty declaration");

  if (warned != 1)
    {
      if (found_tag == 0)
	pedwarn("empty declaration");
    }
}

/* Print an error message now
   for a recent invalid struct, union or enum cross reference.
   We don't print them immediately because they are not invalid
   when used in the `struct foo;' construct for shadowing.  */

void pending_xref_error(void)
{
  if (current.pending_invalid_xref != 0)
    {
      error_with_location(current.pending_invalid_xref->location,
			  "`%s' defined as wrong kind of tag",
			  current.pending_invalid_xref->word1->cstring.data);
      current.pending_invalid_xref = 0;
    }
}

declaration make_void_parm(location loc)
{
  region r = parse_region;
  rid rvoid = new_rid(r, loc, RID_VOID);
  variable_decl vdvoid = new_variable_decl(r, loc, NULL, NULL, NULL, NULL, NULL);
  data_decl ddvoid = new_data_decl(r, loc, CAST(type_element, rvoid),
				   CAST(declaration, vdvoid));

  return CAST(declaration, ddvoid);
}

/* At end of parameter list, warn about any struct, union or enum tags
   defined within.  Do so because these types cannot ever become complete.  */
static void parmlist_tags_warning(environment parm_env)
{
  env_scanner scan_tags;
  const char *tagname;
  tag_declaration tagdecl;
  static bool already = FALSE;

  env_scan(parm_env->tag_env, &scan_tags);

  while (env_next(&scan_tags, &tagname, (void **)&tagdecl))
    {
      int kind = tagdecl->kind;
      const char *kindname = tagkind_name(kind);

      /* An anonymous union parm type is meaningful as a GNU extension.
	 So don't warn for that.  */
      if (kind == kind_union_ref && !tagname && !pedantic)
	continue;

      if (tagname)
	warning("`%s %s' declared inside parameter list", kindname,
		tagname);
      else
	warning("anonymous %s declared inside parameter list", kindname);

      if (!already)
	{
	  warning("its scope is only this definition or declaration,");
	  warning("which is probably not what you want.");
	  already = TRUE;
	}
    }
}

typelist make_arg_types(bool definition, declaration parameters, bool *varargs)
{
  declaration parameter;
  typelist arg_types = new_typelist(parse_region);

  *varargs = FALSE;
  if (!is_void_parms(parameters))
    scan_declaration (parameter, parameters)
      if (is_ellipsis_decl(parameter))
	*varargs = TRUE;
      else if (is_error_decl(parameter))
	{
	  /* Make an "accept everything" signature for arg lists with
	     error_decls */
	  *varargs = TRUE;
	  arg_types = new_typelist(parse_region);
	  typelist_append(arg_types, error_type);

	  return arg_types;
	}
      else
	{
	  data_decl dp = CAST(data_decl, parameter);
	  variable_decl vp = CAST(variable_decl, dp->decls);

	  assert(!vp->next);
	  if (!vp->ddecl->name && definition)
	    error_with_decl(dp->decls, "parameter name omitted");
	  typelist_append(arg_types, vp->ddecl->type);
	}

  return arg_types;
}

bool new_style(declaration parameters)
{
  return parameters && !is_oldidentifier_decl(parameters);
}

static dd_list push_attribute(dd_list al, attribute attr)
{
  /* pstate.ds_region would be good, but isn't quite accessible */
  if (!al)
    al = dd_new_list(parse_region);
  dd_add_last(parse_region, al, attr);

  return al;
}

static void check_duplicate_rid(int specbits, rid rspec)
{
  if (specbits & 1 << rspec->id)
    pedwarn_with_location(rspec->location, "duplicate `%s'", rid_name(rspec));
}

static void check_duplicate_qualifiers1(location l, type_quals new1, type_quals old)
{
  if (old & new1)
    pedwarn_with_location(l, "duplicate `%s'", qualifier_name(new1));
}

static void check_duplicate_qualifiers(location l, type_quals new, type_quals old)
{
  type_quals qual;

  for (qual = 1; qual < last_qualifier; qual <<= 1)
    if (new & qual)
      check_duplicate_qualifiers1(l, qual, old);
}

static void check_legal_qualifiers(location l, type_quals quals)
{
  /* Placeholder for checks for any extra qualifiers */
}

static type parse_qualifiers(type t, location l, type_element qlist,
			     dd_list *oattributes)
{
  type_element q;
  type_quals tqs = no_qualifiers;

  scan_type_element (q, qlist)
    if (is_qualifier(q))
      {
	qualifier qq = CAST(qualifier, q);

	check_duplicate_qualifiers1(qq->location, qq->id, tqs);
	tqs |= qq->id;
      }
    else if (is_attribute(q))
      {
	/* Filter out type-only attributes */
	if (!handle_type_attribute(CAST(attribute, q), &t))
	  *oattributes = push_attribute(*oattributes, CAST(attribute, q));
      }
  check_legal_qualifiers(l, tqs);
  return make_qualified_type(t, tqs);
}

static type make_nesc_function_type(int class, type returns, typelist argtypes,
				    bool varargs)
{
  switch (class)
    {
    case RID_TASK: return make_task_type(returns, argtypes, varargs);
    case RID_EVENT: return make_event_type(returns, argtypes, varargs);
    case RID_COMMAND: return make_command_type(returns, argtypes, varargs);
    default: return make_function_type(returns, argtypes, varargs, FALSE);
    }
}

scflags parse_scflags(int specbits)
{
  scflags scf = 0;

  if (specbits & 1 << RID_INLINE)
    scf |= scf_inline;
  if (specbits & 1 << RID_DEFAULT)
    scf |= scf_default;
  if (specbits & 1 << RID_ASYNC)
    scf |= scf_async;
  if (specbits & 1 << RID_NORACE)
    scf |= scf_norace;

  return scf;
}

void check_variable_scflags(scflags scf,
			    location l, const char *kind, const char *name)
{
  const char *badqual = NULL;
  void (*msg)(location l, const char *format, ...) = error_with_location;

  /* default already covered in parse_declarator */
  if (scf & scf_inline)
    {
      badqual = "inline";
      msg = pedwarn_with_location; /* this is what gcc does */
    }
  if (scf & scf_async)
    badqual = "async";

  if (badqual)
    msg(l, "%s `%s' declared `%s'", kind, name, badqual);
}

void check_array_size(expression size, const char *printname)
{
  if (!check_constant_once(size, cst_numerical))
    return;

  if (size->cst && constant_integral(size->cst))
    {
      if (pedantic)
	{
	  constant_overflow_warning(size->cst);
	  if (definite_zero(size))
	    pedwarn_with_location
	      (size->location, "ANSI C forbids zero-size array `%s'", printname);
	}

      if (cval_intcompare(size->cst->cval, cval_zero) < 0)
	error_with_location(size->location,
			    "size of array `%s' is negative", printname);
    }
  else if (!(current.function_decl || current.env->parm_level))
    {
      if (size->cst)
	error_with_location(size->location, "type size can't be explicitly evaluated");
      else
	error_with_location(size->location, "variable-size type declared outside of any function");
    }
  else if (pedantic)
    {
      if (size->cst)
	pedwarn_with_location(size->location, "ANSI C forbids array `%s' whose size can't be evaluated", printname);
      else
	pedwarn_with_location(size->location, "ANSI C forbids variable-size array `%s'", printname);
    }
}

void parse_declarator(type_element modifiers, declarator d, bool bitfield, 
		      bool require_parm_names,
		      int *oclass, scflags *oscf,
		      const char **ointf, const char **oname,
		      type *ot, bool *owarn_defaulted_int,
		      function_declarator *ofunction_declarator,
		      dd_list *oattributes)
{
  location loc = d ? d->location : modifiers->location;
  int specbits = 0, nclasses = 0;
  type_quals specquals = no_qualifiers;
  bool longlong = FALSE;
  const char *printname, *iname;
  type_element spec;
  type t = NULL;
  bool modified;
  dd_list attributes = NULL;

  *owarn_defaulted_int = FALSE;

  /* We get the name now so that we have a name for error messages */
  if (ointf)
    *ointf = NULL;
  declarator_name(d, oname, &iname);
  printname = nice_declarator_name(d);

  *oclass = 0;

  scan_type_element (spec, modifiers)
    {
      type newtype = NULL;

      switch (spec->kind)
	{
	case kind_rid:
	  {
	    rid rspec = CAST(rid, spec);
	    int id = rspec->id;

	    switch (id)
	      {
	      case RID_INT: newtype = int_type; break;
	      case RID_CHAR: newtype = char_type; break;
	      case RID_FLOAT: newtype = float_type; break;
	      case RID_DOUBLE: newtype = double_type; break;
	      case RID_VOID: newtype = void_type; break;
	      case RID_AUTO: case RID_STATIC: case RID_EXTERN:
	      case RID_REGISTER: case RID_TYPEDEF: case RID_COMMAND:
	      case RID_EVENT: case RID_TASK:
		*oclass = id;
		nclasses++;
		break;

	      case RID_LONG: /* long long detection */
		if (specbits & 1 << RID_LONG) 
		  {
		    if (longlong)
		      error_with_location(spec->location,
					  "`long long long' is too long for GCC");
		    else
		      {
			if (pedantic && !current.lex.input->l.in_system_header)
			  pedwarn_with_location(spec->location,
						"ANSI C does not support `long long'");
			longlong = TRUE;
		      }
		    break;
		  }
		/* Fall through */
	      default:
		check_duplicate_rid(specbits, rspec);
		break;
	      }
	    specbits |= 1 << id;
	    break;
	  }
	case kind_qualifier:
	  {
	    qualifier q = CAST(qualifier, spec);
	    int id = q->id;
            
	    check_duplicate_qualifiers1(loc, id, specquals);
	    specquals |= id;
	    break;
	  }
	case kind_typename: case kind_component_typeref:
	  newtype = CAST(typename, spec)->ddecl->type;
	  break;
	case kind_typeof_type:
	  newtype = CAST(typeof_type, spec)->asttype->type;
	  break;
	case kind_typeof_expr:
	  newtype = CAST(typeof_expr, spec)->arg1->type;
	  if (type_generic(newtype) ||
	      (type_functional(newtype) && !type_function(newtype)))
	    {
	      error_with_location(spec->location,
				  "expression does not have a valid type");
	      newtype = error_type;
	    }
	  else if (type_unknown(newtype))
	    {
	      error_with_location(spec->location,
				  "typeof an expression based on an @integer() or @number() type not supported");
	      newtype = error_type;
	    }
	  break;
	case kind_struct_ref: case kind_union_ref: case kind_enum_ref:
	case kind_nx_struct_ref: case kind_nx_union_ref:
	  newtype = make_tagged_type(CAST(tag_ref, spec)->tdecl);
	  break;
	case kind_attribute_ref:
	  error_with_location(spec->location,
			      "attributes cannot be used as types");
	  newtype = error_type;
	  break;
	case kind_gcc_attribute: case kind_target_attribute: case kind_nesc_attribute:
	  attributes = push_attribute(attributes, CAST(attribute, spec));
	  break;
	default: assert(0); break;
	}

      if (newtype)
	{
	  if (t)
	    error_with_location(spec->location,
	       "two or more data types in declaration of `%s'", printname);
	  else
	    t = newtype;
	}
    }

  /* Long double is a special combination.  */
  if ((specbits & 1 << RID_LONG) && !longlong && (specbits & 1 << RID_DOUBLE))
    {
      specbits &= ~(1 << RID_LONG);
      t = long_double_type;
    }

  modified = !!(specbits & (1 << RID_LONG | 1 << RID_SHORT |
			    1 << RID_SIGNED | 1 << RID_UNSIGNED));

  /* No type at all: default to `int', or `double' (if complex specified
     and no long/short/signed/unsigned) */
  if (!t)
    {
      if (specbits & 1 << RID_COMPLEX)
	{
	  if (!modified)
	    {
	      specbits |= 1 << RID_DOUBLE;
	      t = double_type;
	    }
	  else
	    {
	      specbits |= 1 << RID_INT;
	      t = int_type;
	    }
	}
      else
	{
	  /* Defer defaulted int warning to caller (msg depends on context) */
	  if (!modified)
	    *owarn_defaulted_int = TRUE;

	  specbits |= 1 << RID_INT;
	  t = int_type;
	}
    }

  if (nclasses > 1)
    error_with_location(loc, "multiple storage classes in declaration of `%s'", printname);

  /* Now process the modifiers that were specified
     and check for invalid combinations.  */

  /* Check all other uses of type modifiers.  */
  if (modified)
    {
      int ok = 0;

      if ((specbits & 1 << RID_LONG) && (specbits & 1 << RID_SHORT))
	error_with_location(loc, "both long and short specified for `%s'", printname);
      else if ((specbits & 1 << RID_SHORT) && !(specbits & 1 << RID_INT))
	{
	  static int already = 0;

	  error_with_location(loc, "short invalid for `%s'", printname);
	  if (!already && !pedantic)
	    {
	      error_with_location(loc, "short is only valid with int");
	      already = 1;
	    }
	}
      else if ((specbits & 1 << RID_LONG) && !(specbits & 1 << RID_INT))
	{
	  static int already = 0;

	  error_with_location(loc, "long invalid for `%s'", printname);
	  if (!already && !pedantic)
	    {
	      error_with_location(loc, "long is only valid with int or double");
	      already = 1;
	    }
	}
      else if ((specbits & 1 << RID_SIGNED) && (specbits & 1 << RID_UNSIGNED))
	error_with_location(loc, "both signed and unsigned specified for `%s'", printname);
      else if (((specbits & 1 << RID_SIGNED) || (specbits & 1 << RID_UNSIGNED))
	       && !(specbits & (1 << RID_INT | 1 << RID_CHAR)))
	error_with_location(loc, "signed or unsigned invalid for `%s'", printname);
      else
	ok = 1;

      /* Discard the type modifiers if they are invalid.  */
      if (! ok)
	{
	  specbits &= ~(1 << RID_LONG | 1 << RID_SHORT
			| 1 << RID_UNSIGNED | 1 << RID_SIGNED);
	  longlong = 0;
	}
    }

  if ((specbits & 1 << RID_COMPLEX) && !(type_integral(t) || type_floating(t)))
    {
      error_with_location(loc, "complex invalid for `%s'", printname);
      specbits &= ~(1 << RID_COMPLEX);
    }

  /* Decide whether an integer type is signed or not.
     Optionally treat bitfields as signed by default.  */
  if ((specbits & 1 << RID_UNSIGNED)
      /* Traditionally, all bitfields are unsigned.  */
      || (bitfield && flag_traditional
	  && (/*!explicit_flag_signed_bitfields ||*/ !flag_signed_bitfields))
      || (bitfield && !flag_signed_bitfields
	  && ((specbits & 1 << RID_INT) || (specbits & 1 << RID_CHAR))
	  && !(specbits & 1 << RID_SIGNED)))
    {
      if (longlong)
	t = unsigned_long_long_type;
      else if (specbits & 1 << RID_LONG)
	t = unsigned_long_type;
      else if (specbits & 1 << RID_SHORT)
	t = unsigned_short_type;
      else if (t == char_type)
	t = unsigned_char_type;
      else
	t = unsigned_int_type;
    }
  else if ((specbits & 1 << RID_SIGNED) && (specbits & 1 << RID_CHAR))
    t = signed_char_type;
  else if (longlong)
    t = long_long_type;
  else if (specbits & 1 << RID_LONG)
    t = long_type;
  else if (specbits & 1 << RID_SHORT)
    t = short_type;

  if (specbits & 1 << RID_COMPLEX)
    t = make_complex_type(t);

  /* Check for qualifiers redundant with base type */
  check_duplicate_qualifiers(loc, specquals, type_qualifiers(t));

  specquals |= type_qualifiers(t);
  check_legal_qualifiers(loc, specquals);

  t = make_qualified_type(t, specquals);

  *oscf = parse_scflags(specbits);
  if ((*oscf & scf_default) &&
      !(*oclass == RID_EVENT || *oclass == RID_COMMAND))
    {
      *oscf &= ~scf_default;
      error_with_location(loc, "default can only be specified for events or commands");
    }

  if (pedantic && type_function(t) && (type_const(t) || type_volatile(t)) &&
      !current.lex.input->l.in_system_header)
    pedwarn_with_location(loc, "ANSI C forbids const or volatile function types");

  /* Now figure out the structure of the declarator proper.
     Descend through it, creating more complex types, until we reach
     the declared identifier (or NULL, in an abstract declarator).  */

  while (d && d->kind != kind_identifier_declarator)
    {
      switch (d->kind)
	{
	case kind_array_declarator:
	  {
	    array_declarator ad = CAST(array_declarator, d);
	    expression size = ad->arg1;

	    d = ad->declarator;

	    /* Check for some types that there cannot be arrays of.  */
	    if (type_void(t))
	      {
		error_with_location(ad->location,
				    "declaration of `%s' as array of voids", printname);
		t = error_type;
	      }

	    if (type_function(t))
	      {
		error_with_location(ad->location,
				    "declaration of `%s' as array of functions", printname);
		t = error_type;
	      }

	    if (size && is_error_expr(size))
	      t = error_type;

	    if (t == error_type)
	      continue;

	    if (size)
	      {
		if (!type_integer(size->type))
		  {
		    error_with_location(ad->location,
					"size of array `%s' has non-integer type", printname);
		    size = oneexpr;
		  }
		else
		  check_array_size(size, printname);
	      }

	    /* Build the array type itself, then merge any constancy or
	       volatility  */
	    t = make_array_type(t, size);
	    break;
	  }

	case kind_function_declarator:
	  {
	    function_declarator fd = CAST(function_declarator, d);
	    bool newstyle;

	    d = fd->declarator;
	    if (ofunction_declarator)
	      *ofunction_declarator = fd;

	    /* Declaring a function type.
	       Make sure we have a valid type for the function to return.  */
	    if (t == error_type)
	      t = int_type;

	    /* Warn about some types functions can't return.  */
	    if (type_function(t))
	      {
		error_with_location(fd->location,
				    "`%s' declared as function returning a function",
		      printname);
		t = int_type;
	      }
	    if (type_array(t))
	      {
		error_with_location(fd->location,
				    "`%s' declared as function returning an array",
		      printname);
		t = int_type;
	      }

#ifndef TRADITIONAL_RETURN_FLOAT
	    /* Traditionally, declaring return type float means double.  */
	    if (flag_traditional && type_float(t))
	      t = qualify_type1(double_type, t);
#endif /* TRADITIONAL_RETURN_FLOAT */

	    /* Require new-style declarations */
	    if (current.language != l_c)
	      {
		/* Force empty parameter lists to void */
		if (!fd->parms)
		  fd->parms = make_void_parm(fd->location);
		newstyle = !is_oldidentifier_decl(fd->parms);
		if (!newstyle)
		  error("old-style parameter lists not supported");
	      }
	    else
	      newstyle = new_style(fd->parms);

	    if (newstyle)
	      {
		bool definition = require_parm_names && 
		  d && d->kind == kind_identifier_declarator;
		bool varargs;
		typelist argtypes = make_arg_types(definition, fd->parms,
						   &varargs);

		if (*oclass == RID_TASK)
		  {
		    if (!is_void_parms(fd->parms))
		      error_with_location(fd->location,
		        "`%s' declared as a task with parameters", printname);
		    if (!type_void(t))
		      error_with_location(fd->location,
			"task `%s' must return void", printname);
		  }

		t = make_nesc_function_type(*oclass, t, argtypes, varargs);

		if (fd->gparms)
		  {
		    argtypes = make_arg_types(definition, fd->gparms, &varargs);
		    t = make_generic_type(t, argtypes);
		  }
	      }
	    else  /* Old-style function */
	      t = make_function_type(t, NULL, FALSE, TRUE);

	    t = parse_qualifiers(t, fd->location, fd->qualifiers, NULL);
	    break;
	  }

	case kind_pointer_declarator:
	  {
	    pointer_declarator pd = CAST(pointer_declarator, d);

	    d = pd->declarator;
	    t = make_pointer_type(t);
	    break;
	  }

	case kind_qualified_declarator:
	  {
	    qualified_declarator qd = CAST(qualified_declarator, d);

	    d = qd->declarator;
	    t = parse_qualifiers(t, qd->location, qd->modifiers, &attributes);
	    break;
	  }

	case kind_interface_ref_declarator:
	  {
	    interface_ref_declarator id = CAST(interface_ref_declarator, d);

	    d = id->declarator;
	    if (ointf)
	      *ointf = id->word1->cstring.data;
	    else
	      error_with_location(id->location,
		"unexpected interface reference in declaration of `%s'",
				  printname);
	    break;
	  }

	default: assert(0);
	}
    }

  *ot = t;
  *oattributes = attributes;
}

static declarator finish_function_declarator(function_declarator fd)
{
  declaration parm;
  environment penv = poplevel();

  fd->env = penv;

  if (new_style(fd->parms) && !is_void_parms(fd->parms))
    scan_declaration (parm, fd->parms)
      if (!is_ellipsis_decl(parm) && !is_error_decl(parm))
	{
	  variable_decl vp = CAST(variable_decl, CAST(data_decl, parm)->decls);

	  if (!vp->ddecl)
	    {
	      error_with_location(fd->location, "parameter declared void");
	      vp->ddecl = bad_decl;
	    }
	  else if (!vp->ddecl->isused)
	    {
	      /* ok, so it's not really a field */
	      const char *pname = nice_field_name(vp->ddecl->name);

	      error_with_location(fd->location,
		"parameter `%s' has just a forward declaration", pname);
	    }
	}

  parmlist_tags_warning(penv);

  return CAST(declarator, fd);
}

declarator finish_array_or_fn_declarator(declarator nested, nested_declarator d)
{
  d->declarator = nested;

  if (is_function_declarator(d))
    return finish_function_declarator(CAST(function_declarator, d));
  else
    return CAST(declarator, d);
}


/* Return zero if the declaration NEWDECL is valid
   when the declaration OLDDECL (assumed to be for the same name and kind
   of declaration) has already been seen.
   Otherwise return an error message format string with a %s
   where the identifier should go.  */

static char *redeclaration_error_message(data_declaration newdecl,
					 data_declaration olddecl,
					 bool newinitialised)
{
  if (olddecl->islimbo)
    return 0;

  if (newdecl->kind == decl_typedef)
    {
      if (flag_traditional && type_compatible(newdecl->type, olddecl->type))
	return 0;
      /* This gets a warning later */
      if (olddecl->in_system_header || newdecl->in_system_header)
	return 0;
      return "redefinition of `%s'";
    }
  else if (newdecl->kind == decl_function)
    {
      /* Declarations of functions can insist on internal linkage
	 but they can't be inconsistent with internal linkage,
	 so there can be no error on that account.
	 However defining the same name twice is no good.  */
      if (olddecl->definition && newdecl->definition
	  /* However, defining once as extern inline and a second
	     time in another way is ok.  */
	  && !(olddecl->isexterninline && !newdecl->isexterninline))
	return "redefinition of `%s'";
      return 0;
    }
  else if (newdecl->kind == decl_constant)
    return "redefinition of `%s'";
  else if (current.env->global_level)
    {
      /* Objects declared at top level:  */
      /* If at least one is a reference, it's ok.  */
      if (newdecl->isfilescoperef || olddecl->isfilescoperef)
	return 0;
      /* Reject two definitions with initialisation.  */
      if (newinitialised && olddecl->initialiser)
	return "redefinition of `%s'";
      /* Now we have two tentative defs, or one tentative and one real def.  */
      /* Insist that the linkage match.  */
      if (olddecl->isexternalscope != newdecl->isexternalscope)
	return "conflicting declarations of `%s'";
      return 0;
    }
  else
    {
      /* Newdecl has block scope.  If olddecl has block scope also, then
	 reject two definitions, and reject a definition together with an
	 external reference.  Otherwise, it is OK, because newdecl must
	 be an extern reference to olddecl.  */
      if (!(newdecl->isexternalscope && olddecl->isexternalscope))
#if 0
	Why check the context ?
	  && DECL_CONTEXT (newdecl) == DECL_CONTEXT (olddecl)
#endif
	return "redeclaration of `%s'";
      return 0;
    }
}

/* Return TRUE if t1 looks like a modern declaration for malloc (&friends) and
   t2 looks like an oldstyle declaration thereof */
static bool looks_like_malloc_redeclaration(type t1, type t2)
{
  type t1return = type_function_return_type(t1);
  type t2return = type_function_return_type(t2);

  return
    type_function_oldstyle(t1) &&
    type_pointer(t1return) && type_pointer(t2return) &&
    type_void(type_points_to(t2return)) &&
    type_char(type_points_to(t1return)) &&
    self_promoting_args(t2);
}

void show_previous_decl(void (*message)(declaration d, const char *format, ...),
			data_declaration olddecl)
{
  if (olddecl->kind == decl_function && olddecl->ftype == function_implicit)
    message(olddecl->ast, "previous implicit declaration of `%s'", olddecl->name);
  else if (ddecl_is_command_or_event(olddecl) && olddecl->definition)
    message(olddecl->definition, "previous declaration of `%s'",
	    decl_printname(olddecl));
  else
    message(olddecl->ast, "previous declaration of `%s'",
	    decl_printname(olddecl));
}

/* Handle when a new declaration NEWDECL
   has the same name as an old one OLDDECL
   in the same binding contour.
   Prints an error message if appropriate.

   If safely possible, alter OLDDECL to look like NEWDECL, and return 1.
   Otherwise, return 0.

   When DIFFERENT_BINDING_LEVEL is true, NEWDECL is an external declaration,
   and OLDDECL is in an outer binding level and should thus not be changed.  */

int duplicate_decls(data_declaration newdecl, data_declaration olddecl,
		    bool different_binding_level, bool newinitialised)
{
  type oldtype = olddecl->type;
  type newtype = newdecl->type;
  char *errmsg = 0;
  void (*previous_message)(declaration d, const char *format, ...) = NULL;
  bool types_match;

  assert(!(newdecl->kind == decl_function &&
	   newdecl->ftype == function_implicit));

  /* New decl is completely inconsistent with the old one =>
     tell caller to replace the old one. This is an error,
     except if traditional and in different binding levels */
  if (newdecl->kind != olddecl->kind)
    {
      bool iswarning = 
	(flag_traditional && different_binding_level) || olddecl->islimbo;

      warning_or_error(iswarning,
		       "`%s' redeclared as different kind of symbol", decl_printname(olddecl));
      show_previous_decl(iswarning ? warning_with_decl : error_with_decl, olddecl);
      newdecl->shadowed = olddecl;
      return 0;
    }

  if (newtype == error_type || oldtype == error_type)
    types_match = FALSE;
  else
    types_match = type_compatible_unqualified(newtype, oldtype);

  /* For real parm decl following a forward decl, or a declaration of an old
     style parameter (oldtype == void) return 1 so old decl will be reused. */
  if ((oldtype == void_type || types_match) && newdecl->isparameter &&
      !olddecl->isused)
    {
      /* Point to the latest declaration */
      olddecl->ast = newdecl->ast;
      return 1;
    }

  /* The new declaration is the same kind of object as the old one.
     The declarations may partially match.  Print warnings if they don't
     match enough.  Ultimately, copy most of the information from the new
     decl to the old one, and keep using the old one.  */

  if (flag_traditional && olddecl->kind == decl_function
      && olddecl->ftype == function_implicit)
    /* If -traditional, avoid error for redeclaring fcn
       after implicit decl.  */
    ;
  /* Permit char *foo () to match void *foo (...) if not pedantic,
     if one of them came from a system header file.  */
  else if (!types_match && olddecl->kind == decl_function
	   && (olddecl->in_system_header || newdecl->in_system_header)
	   && (looks_like_malloc_redeclaration(oldtype, newtype) ||
	       looks_like_malloc_redeclaration(newtype, oldtype)))
    {
      if (pedantic)
	pedwarn_with_decl(newdecl->ast, "conflicting types for `%s'", decl_printname(olddecl));
      /* Make sure we keep void * as ret type, not char *.  */
      if (type_void(type_points_to(type_function_return_type(oldtype))))
	newdecl->type = newtype = oldtype;

      /* Set IN_SYSTEM_HEADER, so that if we see another declaration
	 we will come back here again.  */
      newdecl->in_system_header = TRUE;
    }
  else if (!types_match
	   /* Permit char *foo (int, ...); followed by char *foo ();
	      if not pedantic.  */
	   && !(olddecl->kind == decl_function && !pedantic
		&& type_function_oldstyle(newtype)
		/* Return types must still match.  */
		&& type_compatible(type_function_return_type(oldtype),
				   type_function_return_type(newtype))))
    {
      void (*message)(const char *format, ...) =
	olddecl->islimbo ? warning : error;

      previous_message =
	olddecl->islimbo ? warning_with_decl : error_with_decl;

      message("conflicting types for `%s'", decl_printname(olddecl));
      /* Check for function type mismatch
	 involving an empty arglist vs a nonempty one.  */
      if (newdecl->kind == decl_function
	  && type_compatible(type_function_return_type(oldtype),
			     type_function_return_type(newtype))
	  && ((type_function_oldstyle(oldtype) && !olddecl->definition)
	      || (type_function_oldstyle(newtype) && !newdecl->definition)))
	{
	  /* Classify the problem further.  */
	  if (type_function_varargs(newtype) || type_function_varargs(oldtype))
	    {
	      message("A parameter list with an ellipsis can't match");
	      message("an empty parameter name list declaration.");
	    }
	  else
	    {
	      typelist_scanner scanargs;
	      type t;
	      typelist args = type_function_arguments(oldtype);

	      if (!args)
		args = type_function_arguments(newtype);

	      typelist_scan(args, &scanargs);
	      while ((t = typelist_next(&scanargs)))
		if (!type_self_promoting(t))
		  {
		    message("An argument type that has a default promotion");
		    message("can't match an empty parameter name list declaration.");
		    break;
		  }
	    }
	}
    }
  else
    {
      errmsg = redeclaration_error_message(newdecl, olddecl, newinitialised);
      if (errmsg)
	{
	  error_with_decl(newdecl->ast, errmsg, decl_printname(olddecl));
	  previous_message = error_with_decl;
	}
      else if (newdecl->kind == decl_typedef &&
	       (olddecl->in_system_header || newdecl->in_system_header))
	{
	  warning_with_decl(newdecl->ast, "redefinition of `%s'", decl_printname(olddecl));
	  previous_message = warning_with_decl;
	}
      else if (olddecl->kind == decl_function
	       && olddecl->oldstyle_args
	       && !type_function_oldstyle(newtype))
	{
	  int nargs;
	  typelist_scanner oldparms, newparms;
	  /* Prototype decl follows defn w/o prototype.  */

	  typelist_scan(olddecl->oldstyle_args, &oldparms);
	  typelist_scan(type_function_arguments(newtype), &newparms);
	  nargs = 1;
	  errmsg = NULL;
	  for (;;)
	    {
	      type oldparm = typelist_next(&oldparms);
	      type newparm = typelist_next(&newparms);

	      if (!oldparm && !newparm)
		break;

	      if (!oldparm || !newparm)
		{
		  errmsg = "prototype for `%s' follows and number of arguments";
		  break;
		}
	      /* Type for passing arg must be consistent
		 with that declared for the arg.  */
	      if (!type_compatible(oldparm, newparm)
		  /* If -traditional, allow `unsigned int' instead of `int'
		     in the prototype.  */
		  && !(flag_traditional
		       && type_equal_unqualified(oldparm, int_type)
		       && type_equal_unqualified(newparm, unsigned_int_type)))
		{
		  errmsg = "prototype for `%s' follows and argument %d";
		  break;
		}
	      nargs++;
	    }
	  if (errmsg)
	    {
	      warning_or_error_with_decl(olddecl->islimbo, newdecl->ast,
					 errmsg, decl_printname(olddecl), nargs);
	      warning_or_error_with_decl(olddecl->islimbo, olddecl->ast,
			      "doesn't match non-prototype definition here");
	    }
	  else
	    {
	      warning_with_decl(newdecl->ast, "prototype for `%s' follows",
				decl_printname(olddecl));
	      warning_with_decl(olddecl->ast, "non-prototype definition here");
	    }
	}
      /* Warn about mismatches in various flags. */
      else if (newdecl->kind == decl_function)
	{
	  /* Warn if function is now inline
	     but was previously declared not inline and has been called. */
	  if (!olddecl->isinline && newdecl->isinline)
	    {
	      if (olddecl->isused)
		{
		  warning("`%s' declared inline after being called", decl_printname(olddecl));
		  previous_message = warning_with_decl;
		}
	      if (olddecl->definition)
		{
		  warning("`%s' declared inline after its definition", decl_printname(olddecl));
		  previous_message = warning_with_decl;
		}
	    }

	  /* Warn for static following external */
	  if (newdecl->ftype == function_static &&
	      olddecl->ftype == function_implicit)
	    {
	      pedwarn("`%s' was declared implicitly `extern' and later `static'", decl_printname(olddecl));
	      previous_message = pedwarn_with_decl;
	    }
	  else if (newdecl->ftype == function_static &&
		   olddecl->ftype == function_normal)
	    {
	      pedwarn("static declaration for `%s' follows non-static", decl_printname(olddecl));
	      previous_message = pedwarn_with_decl;
	    }

	  /* Warn for mismatched async */
	  if (newdecl->async != olddecl->async)
	    {
	      error("`%s': async mismatch with declaration",
		    decl_printname(olddecl));
	      previous_message = error_with_decl;
	    }
	}
      else if (newdecl->kind == decl_variable)
	{
	  /* If pedantic, warn when static declaration follows a non-static
	     declaration. */
	  if (pedantic &&
	      olddecl->isexternalscope && !newdecl->isexternalscope)
	    {
	      pedwarn("static declaration for `%s' follows non-static", decl_printname(olddecl));
	      previous_message = pedwarn_with_decl;
	    }
	  /* Warn when const declaration follows a non-const declaration */
	  if (!type_const(oldtype) && type_const(newtype))
	    warning("const declaration for `%s' follows non-const", decl_printname(olddecl));
	  /* These bits are logically part of the type, for variables. */
	  else if (pedantic && 
		   (type_const(oldtype) != type_const(newtype)
		    || type_volatile(oldtype) != type_volatile(newtype)))
	    {
	      pedwarn("type qualifiers for `%s' conflict with previous decl",
		      decl_printname(olddecl));
	      previous_message = pedwarn_with_decl;
	    }
	}
    }

  /* Optionally warn about more than one declaration for the same name.  */
  /* Let's try a different test than GCC */
  if (errmsg == 0 && warn_redundant_decls && !olddecl->islimbo &&
      !(olddecl->isfilescoperef && !newdecl->isfilescoperef))
    {
      warning_with_decl(newdecl->ast, "redundant redeclaration of `%s' in same scope", decl_printname(olddecl));
      previous_message = warning_with_decl;
    }

  if (previous_message)
    show_previous_decl(previous_message, olddecl);


  /* Copy all the DECL_... slots specified in the new decl
     except for any that we copy here from the old type. */

  /* If either decl says `inline', this fn is inline,
     unless its definition was passed already.  */
  if (newdecl->isinline && !olddecl->definition)
    olddecl->isinline = TRUE;
  newdecl->isinline = olddecl->isinline;

  /* If either of the decls says noinline, make sure that none of the
     declerations are made inline. */
  if (newdecl->noinlinep || olddecl->noinlinep) 
    newdecl->noinlinep = olddecl->noinlinep = TRUE;

  if (different_binding_level)
    {
      /* newdecl must be a reference to something at file scope */
      assert(newdecl->isfilescoperef && !newdecl->needsmemory);
      assert(!(newdecl->kind == decl_variable &&
	       newdecl->vtype == variable_static));
      assert(!(newdecl->kind == decl_function &&
	       (newdecl->ftype == function_implicit ||
		newdecl->ftype == function_nested)));

      /* We copy some info over to the newdecl which will shadow olddecl */
      newdecl->shadowed = olddecl;
      newdecl->definition = olddecl->definition;
      newdecl->isinline = olddecl->isinline;
      newdecl->isexterninline = olddecl->isexterninline;
      newdecl->oldstyle_args = olddecl->oldstyle_args;
      if (olddecl->in_system_header)
	newdecl->in_system_header = TRUE;

      newdecl->isexternalscope = olddecl->isexternalscope;

      /* We don't copy the type */

      return 0;
    }

  /* newdecl should be too new to have any oldstyle args yet */
  assert(!newdecl->oldstyle_args);

  /* Merge the data types specified in the two decls.  */
  if (types_match)
    olddecl->type = common_type(newtype, oldtype);
  else if (newtype != error_type)
    /* GCC keeps the old type. I think it makes more sense to keep the new
       one. And it means I can examine the decl in current.function_decl
       and find the type of the current function, not something random */
    olddecl->type = newtype;
  olddecl->islimbo = FALSE;

  if (newdecl->definition)
    olddecl->definition = newdecl->definition;

  olddecl->isexternalscope &= newdecl->isexternalscope;
  olddecl->isfilescoperef &= newdecl->isfilescoperef;
  olddecl->needsmemory |= newdecl->needsmemory;

  olddecl->Cname |= newdecl->Cname;
  if (newdecl->spontaneous && !olddecl->spontaneous)
    {
      olddecl->spontaneous = newdecl->spontaneous;
      dd_add_last(parse_region, spontaneous_calls, olddecl);
    }
  olddecl->norace |= newdecl->norace;

  /* For functions, static overrides non-static.  */
  if (newdecl->kind == decl_function)
    {
      if (olddecl->ftype != function_static)
	olddecl->ftype = newdecl->ftype;

      /* Also set isexterninline correctly */
      if ((olddecl->definition && !olddecl->isexterninline) ||
	  (newdecl->definition && !newdecl->isexterninline))
	olddecl->isexterninline = FALSE;
      else if (olddecl->isexterninline || newdecl->isexterninline)
	olddecl->isexterninline = TRUE;
      /* the last case is 2 non-inline externs, so isexterninline is correct */
    }
  else if (newdecl->kind == decl_variable)
    {
      /* static overrides extern (the combinations with register
	 are errors anyway) */ 
      if (olddecl->vtype != variable_static)
	olddecl->vtype = variable_static;
    }

  olddecl->in_system_header = newdecl->in_system_header =
    olddecl->in_system_header || newdecl->in_system_header;

  /* Point to the latest declaration (except for commands and events) */
  if (!ddecl_is_command_or_event(olddecl))
    olddecl->ast = newdecl->ast;

  return 1;
}

static void transparent_union_argument(data_declaration ddecl)
{
  ddecl->type = make_qualified_type
    (ddecl->type, type_qualifiers(ddecl->type) | transparent_qualifier);
}

bool is_doublecharstar(type t)
{
  return type_pointer(t) && type_charstar(type_points_to(t));
}

void check_function(data_declaration dd, declaration fd, int class,
		    scflags scf, const char *name, type function_type,
		    bool nested, bool isdeclaration, bool defaulted_int)
{
  type return_type, actual_function_type;

  if (defaulted_int && (warn_implicit_int || warn_return_type))
    warning("return-type defaults to `int'");

  if (scf & scf_norace)
    error("norace is for variables only");

  actual_function_type = type_generic(function_type) ?
    type_function_return_type(function_type) : function_type;
  return_type = type_function_return_type(actual_function_type);

  /* XXX: Does this volatile/const stuff actually work with my imp ? */
  if (pedantic && type_void(return_type) &&
      (type_const(return_type) || type_volatile(return_type)) &&
      !current.lex.input->l.in_system_header)
    pedwarn("ANSI C forbids const or volatile void function return type");

  if (type_volatile(function_type) && !type_void(return_type))
    warning("`noreturn' function returns non-void value");

  /* Record presence of `inline', if it is reasonable.  */
  if (scf & scf_inline && !strcmp(name, "main") && !nested)
    {
      warning("cannot inline function `main'");
      scf &= ~scf_inline;
    }

  if (nested && (class == RID_COMMAND || class == RID_EVENT))
    error("commands and events cannot be declared inside functions");

  /* Warn for unlikely, improbable, or stupid declarations of `main'. */
  if (current.language == l_c && warn_main && !strcmp("main", name) && !nested)
    {
      if (!type_equal_unqualified(return_type, int_type))
	pedwarn("return type of `%s' is not `int'", name);

      /* Just being "bug"-compatible w/ GCC here */
      if (!type_function_oldstyle(function_type))
	{
	  typelist_scanner scanargs;
	  type argtype;
	  int argct = 0;

	  typelist_scan(type_function_arguments(function_type), &scanargs);
	  while ((argtype = typelist_next(&scanargs)))
	    {
	      ++argct;
	      switch (argct)
		{
		case 1:
		  if (!type_equal_unqualified(argtype, int_type))
		    pedwarn("first argument of `%s' should be `int'", name);
		  break;

		case 2:
		  if (!is_doublecharstar(argtype))
		    pedwarn("second argument of `%s' should be `char **'",
			    name);
		  break;

		case 3:
		  if (!is_doublecharstar(argtype))
		    pedwarn("third argument of `%s' should probably be `char **'",
			    name);
		  break;
		}
	    }

	  /* It is intentional that this message does not mention the third
	     argument, which is warned for only pedantically, because it's
	     blessed by mention in an appendix of the standard. */
	  if (argct > 0 && (argct < 2 || argct > 3))
	    pedwarn("`%s' takes only zero or two arguments", name);

	  if (argct == 3 && pedantic)
	    pedwarn("third argument of `%s' is deprecated", name);

	  if (class == RID_STATIC)
	    pedwarn("`%s' is normally a non-static function", name);
	}
    }

  init_data_declaration(dd, fd, name, function_type);
  dd->kind = decl_function;
  dd->isexternalscope = FALSE;
  if (nested)
    dd->ftype = function_nested;
  else if (class == RID_STATIC)
    dd->ftype = function_static;
  else if (class == RID_COMMAND)
    dd->ftype = function_command;
  else if (class == RID_EVENT)
    dd->ftype = function_event;
  else
    {
      dd->ftype = function_normal;
      dd->isexternalscope = TRUE;
    }
  /* XXX: Should probably be FALSE for extern inline */
  dd->needsmemory = !isdeclaration;
  dd->isinline = (scf & scf_inline) != 0;
  dd->isexterninline = dd->isinline && class == RID_EXTERN;
  dd->isfilescoperef = dd->isexterninline || isdeclaration;
  if (scf & scf_async)
    {
      if (dd->ftype == function_command || dd->ftype == function_event)
	dd->async = TRUE;
      else
	error("`async' is for commands and events only");
    }
}

data_declaration declare_string(const char *name, cstring value, bool wide)
{
  struct data_declaration tempdecl;
  expression expr_l = build_uint_constant(parse_region, dummy_location, size_t_type, value.length + 1);
  type value_type = make_array_type(wide ? wchar_type : char_type, expr_l);

  init_data_declaration(&tempdecl, new_error_decl(parse_region, dummy_location),
			name, value_type);
  tempdecl.kind = decl_magic_string;
  tempdecl.needsmemory = TRUE;
  tempdecl.in_system_header = TRUE;
  tempdecl.vtype = variable_static;
  tempdecl.schars = value;

  return declare(current.env, &tempdecl, TRUE);
}

static void declare_magic_string(const char *name, const char *value)
{
  declare_string(name, str2cstring(parse_region, value), FALSE);
}

bool builtin_declaration(data_declaration dd)
/* Returns: TRUE if dd is a declaration for something builtin (i.e.,
     starts with __builtin_
*/
{
  return strncmp(dd->name, "__builtin_", 10) == 0;
}

data_declaration declare_builtin_type(const char *name, type t)
{
  struct data_declaration tempdecl;

  init_data_declaration(&tempdecl, new_error_decl(parse_region, dummy_location),
			name, t);
  tempdecl.kind = decl_typedef;
  tempdecl.in_system_header = TRUE;

  return declare(current.env, &tempdecl, TRUE);
}

static tag_declaration make_anonymous_struct(void)
{
  tag_ref tref = newkind_tag_ref(parse_region, kind_struct_ref, dummy_location,
				 NULL, NULL, NULL, TRUE);

  return declare_global_tag(tref);
}

static void declare_builtin_types(void)
{
  builtin_va_list_type = 
    make_pointer_type(make_tagged_type(make_anonymous_struct()));
  declare_builtin_type("__builtin_va_list", builtin_va_list_type);
}

static data_declaration declare_builtin(const char *name, data_kind kind, type t)
{
  struct data_declaration tempdecl;

  init_data_declaration(&tempdecl, new_error_decl(parse_region, dummy_location),
			name, t);
  tempdecl.kind = kind;
  tempdecl.needsmemory = TRUE;
  tempdecl.in_system_header = TRUE;
  tempdecl.vtype = variable_static;

  return declare(global_env, &tempdecl, TRUE);
}

data_declaration declare_builtin_identifier(const char *name, type t)
{
  return declare_builtin(name, decl_variable, t);
}

data_declaration declare_builtin_function(const char *name, type t)
{
  return declare_builtin(name, decl_function, t);
}

static void declare_builtin_identifiers(void)
{
  typelist emptylist = new_typelist(parse_region);
  type default_function_type = make_function_type(int_type, emptylist, FALSE, TRUE);

  /* Use = as a suffix for this "dummy" identifier (used in function_call
     nodes that represent calls to __builtin_va_arg) */
  builtin_va_arg_decl = declare_builtin_identifier("=va_arg", int_type);

  builtin_constant_p =
    declare_builtin_function("__builtin_constant_p", default_function_type);
}

static void declare_function_name(void)
{
  const char *name, *printable_name;

  if (current.function_decl == NULL)
    {
      name = "";
      printable_name = "top level";
    }
  else
    {
      name = current.function_decl->ddecl->name;
      printable_name = name;
    }

  declare_magic_string("__FUNCTION__", name);
  declare_magic_string("__PRETTY_FUNCTION__", printable_name);
}

static void error_assert(bool ok)
{
  if (!ok)
    {
      error("confused by earlier errors - bailing out");
      exit(FATAL_EXIT_CODE);
    }
}

static void detect_bogus_env(void)
{
  /* We should not come here with the current env as parm level and not
     in a function. If we do, it's because the error recovery productions
     failed to pop some levels. So do it now. */
  while (!current.function_decl && current.env->parm_level)
    {
      /* This should only be possible after a (parse) error. Ensure that
	 we aren't confused... */
      assert(errorcount > 0);
      poplevel();
    }

}

/* Start definition of function 'elements d' with attributes attribs.
   nested is true for nested function definitions.
   Returns false in case of error.
   Sets current.function_decl to the declaration for this function */
bool start_function(type_element elements, declarator d, attribute attribs,
		    bool nested)
{
  int class;
  scflags scf;
  const char *name, *intf;
  type function_type, actual_function_type;
  bool defaulted_int, old_decl_has_prototype, normal_function;
  data_declaration old_decl, ddecl;
  function_decl fdecl;
  function_declarator fdeclarator;
  struct data_declaration tempdecl;
  env_scanner scan;
  const char *id;
  void *idval;
  dd_list doc_tags = NULL;
  dd_list extra_attr;

  detect_bogus_env();

  if (!nested)
    error_assert(current.env->global_level && current.function_decl == NULL);

  parse_declarator(elements, d, FALSE, TRUE, &class, &scf,
		   &intf, &name, &function_type, &defaulted_int, &fdeclarator,
		   &extra_attr);

  actual_function_type = type_generic(function_type) ?
    type_function_return_type(function_type) : function_type;

  if (!type_functional(actual_function_type))
    return FALSE;

  /* We don't set current.function_decl yet so that error messages do not
     say "In function <thisfunctioname>" as we're not "in" the function yet */
  fdecl = new_function_decl(parse_region, d->location, d, elements, attribs,
			    NULL, NULL, current.function_decl, NULL);
  fdecl->declared_type = function_type;
  fdecl->undeclared_variables = new_env(parse_region, NULL);
  fdecl->current_loop = NULL;

  if (class == RID_AUTO)
    {
      if (pedantic || !nested)
	pedwarn("function definition declared `auto'");
      class = 0;
    }
  else if (class == RID_REGISTER)
    {
      error("function definition declared `register'");
      class = 0;
    }
  else if (class == RID_TYPEDEF)
    {
      error("function definition declared `typedef'");
      class = 0;
    }
  else if (class == RID_EXTERN && nested)
    {
      error("nested function `%s' declared `extern'", name);
      class = 0;
    }
  else if ((class == RID_STATIC || scf & scf_inline) && nested)
    {
      if (pedantic)
	pedwarn("invalid storage class for function `%s'", name);
      class = 0;
    }
  
  if (class == RID_COMMAND || class == RID_EVENT || class == RID_TASK)
    {
      if (nested)
	{
	  error("commands, events or tasks cannot be nested");
	  class = 0;
	}
      else if (current.language == l_c)
	{
	  error("commands, events or tasks not allowed in C files");
	  class = 0;
	}
    }

  if (fdeclarator->gparms && !(class == RID_COMMAND || class == RID_EVENT))
    {
      error("generic parameters only allowed on commands and events");
      fdeclarator->gparms = NULL;
    }

  if (!type_void(type_function_return_type(actual_function_type)) &&
      type_incomplete(type_function_return_type(actual_function_type)))
    {
      type t;

      error("return-type is an incomplete type");

      /* Yuck */
      t = make_function_type(void_type,
			     type_function_arguments(actual_function_type),
			     type_function_varargs(actual_function_type),
			     type_function_oldstyle(actual_function_type));
      if (type_generic(function_type))
	t = make_generic_type(t, type_function_arguments(function_type));

      function_type = qualify_type1(t, function_type);
    }

  check_function(&tempdecl, CAST(declaration, fdecl), class, scf,
		 name, function_type, nested, FALSE, defaulted_int);
  tempdecl.definition = tempdecl.ast;
  if (current.container)
    tempdecl.safe = current.container->safe;

  handle_decl_attributes(attribs, &tempdecl);
  handle_decl_dd_attributes(extra_attr, &tempdecl);

  if (intf)
    {
      data_declaration iref = lookup_id(intf, FALSE);

      old_decl = NULL;
      if (!iref || iref->kind != decl_interface_ref)
	error("unknown interface `%s'", intf);
      else
	{
	  old_decl = interface_lookup(iref, name);
	  if (!old_decl)
	    error("`%s' is not in interface `%s'", name, intf);
	}
    }
  else if (class == RID_COMMAND || class == RID_EVENT)
    {
      old_decl = lookup_id(name, FALSE);
      if (!old_decl)
	error("unknown command or event `%s'", name);
    }
  else
    old_decl = lookup_id(name, !tempdecl.Cname);

  if (old_decl)
    {
      if (((class == RID_COMMAND || class == RID_EVENT) && !old_decl->defined)
	  ^ ((scf & scf_default) != 0))
	{
	  if (scf & scf_default)
	    {
	      error("`%s' is defined, not used, in this component", name);
	      error("(default implementations are only for used commands or events)");
	    }
	  else
	    error("`%s' is used, not defined, in this component", name);
	}
    }
  else
    scf &= ~scf_default;

  old_decl_has_prototype = old_decl && old_decl->kind == decl_function &&
    !type_function_oldstyle(old_decl->type);

  normal_function = !nested && class != RID_STATIC;
  /* Optionally warn of old-fashioned def with no previous prototype.  */
  if (warn_strict_prototypes
      && type_function_oldstyle(function_type)
      && !old_decl_has_prototype)
    warning("function declaration isn't a prototype");
  /* Optionally warn of any global def with no previous prototype.  */
  else if (warn_missing_prototypes
	   && normal_function && !old_decl_has_prototype
	   && strcmp("main", name))
    warning("no previous prototype for `%s'", name);
  /* Optionally warn of any def with no previous prototype
     if the function has already been used.  */
  else if (warn_missing_prototypes
	   && old_decl && old_decl->ftype == function_implicit)
    warning("`%s' was used with no prototype before its definition", name);
  /* Optionally warn of any global def with no previous declaration.  */
  else if (warn_missing_declarations
	   && normal_function && !old_decl && strcmp("main", name))
    warning("no previous declaration for `%s'", name);
  /* Optionally warn of any def with no previous declaration
     if the function has already been used.  */
  else if (warn_missing_declarations
	   && old_decl && old_decl->ftype == function_implicit)
    warning("`%s' was used with no declaration before its definition", name);

  /* If return types match and old declaraton has a prototype and new
     declaration hasn't, borrow the prototype. We will check for errors
     in store_parm_decls. */
  if (old_decl_has_prototype && type_function_oldstyle(function_type) &&
      type_compatible(type_function_return_type(old_decl->type),
		      type_function_return_type(function_type)))
    {
      function_type = qualify_type1
	(make_function_type(type_function_return_type(function_type),
			    type_function_arguments(old_decl->type),
			    type_function_varargs(old_decl->type),
			    FALSE), old_decl->type);

      tempdecl.type = function_type;
    }

  if (old_decl && duplicate_decls(&tempdecl, old_decl, FALSE, FALSE))
    {
      ddecl = old_decl;
      /* Safety annotation from implementation is the only one that counts */
      ddecl->safe = tempdecl.safe;
    }
  else
    ddecl = declare(current.env, &tempdecl, FALSE);

  fdecl->base_labels = fdecl->scoped_labels =
    new_env(parse_region,
	    current.function_decl ? current.function_decl->scoped_labels : NULL);
  fdecl->ddecl = ddecl;
  fdecl->fdeclarator = fdeclarator;

  get_latest_docstring(&ddecl->doc, current.fileregion, &doc_tags);
  handle_fdecl_doc_tags(ddecl->doc.loc, ddecl, fdeclarator, doc_tags);

  /* If requested, replace post/task by references to an interface */
  if (type_task(ddecl->type) && flag_use_scheduler)
    handle_task_definition(fdecl);

  /* save environments */
  current.env = fdeclarator->env;
  current.env->fdecl = current.function_decl = fdecl;

  /* Set id of parameters (done here rather than in declare because parameters
     of a given function may be declared several times) */
  env_scan(current.env->id_env, &scan);
  while (env_next(&scan, &id, &idval))
    {
      data_declaration iddecl = idval;

      if (iddecl->kind == decl_variable)
	{
	  assert(iddecl->isparameter);
	  iddecl->id = current.function_decl->nlocals++;
	}
    }

  return TRUE;
}

void implicit_decl_warning(data_declaration ddecl)
{
  if (builtin_declaration(ddecl))
      return;

  if (current.language != l_c || mesg_implicit_function_declaration == 2)
    error("implicit declaration of function `%s'", ddecl->name);
  else if (mesg_implicit_function_declaration == 1)
    warning("implicit declaration of function `%s'", ddecl->name);
}

data_declaration implicitly_declare(identifier fnid)
{
  struct data_declaration tempdecl;
  declaration pseudo_ast =
    CAST(declaration, new_implicit_decl(parse_region, fnid->location, fnid));

  init_data_declaration(&tempdecl, pseudo_ast,
			fnid->cstring.data, implicit_function_type);
  tempdecl.kind = decl_function;
  tempdecl.isexternalscope = TRUE;
  tempdecl.isfilescoperef = TRUE;
  tempdecl.ftype = function_implicit;
  /* Point to the limbo version of any previous implicit declaration */
  tempdecl.shadowed = lookup_global_id(tempdecl.name);

  if (!tempdecl.shadowed) /* warn once only */
    implicit_decl_warning(&tempdecl);

  return declare(current.env, &tempdecl, FALSE);
}

/* Declare parameters, either from new style declarations in the 
   declarator, or from old_parms */
void store_parm_decls(declaration old_parms)
{
  if (!oldstyle_function(current.function_decl))
    {
      /* This case is when the function was defined with an ANSI prototype.
	 The parms already have decls, so we need not do anything here
	 except record them as in effect
	 and complain if any redundant old-style parm decls were written.  */
      if (old_parms)
	error_with_decl(CAST(declaration, current.function_decl),
			"parm types given both in parmlist and separately");
    }
  else
    {
      oldidentifier_decl parm, parms;

      current.function_decl->old_parms = old_parms;
      /* Need to either:
	 - compare arg types to previous prototype
	 - or build pseudo-prototype for this function
      */
      parms = CAST(oldidentifier_decl,
		   current.function_decl->fdeclarator->parms);
      scan_oldidentifier_decl (parm, parms)
	/* If no declaration given, default to int.  */
	if (parm->ddecl->type == void_type)
	  {
	    parm->ddecl->type = int_type;
	    if (extra_warnings)
	      warning_with_decl(CAST(declaration, parm),
				"type of `%s' defaults to `int'",
				parm->cstring.data);
	  }
    }

  /* Declare __FUNCTION__ and __PRETTY_FUNCTION__ for this function.  */
  declare_function_name();
}

/* End definition of current function, furnishing it it's body. */
declaration finish_function(statement body)
{
  declaration fn = CAST(declaration, current.function_decl);

  current.function_decl->stmt = body;
  error_assert(current.env->parm_level);
  poplevel(); /* Pop parameter level */
  check_labels();
  current.function_decl = current.function_decl->parent_function;

  return fn;
}

/* Start a new scope */
void pushlevel(bool parm_level)
{
  current.env = new_environment(parse_region, current.env, FALSE, parm_level);
}

/* Pop back to enclosing scope */
environment poplevel(void)
{
  environment old = current.env;

  current.env = current.env->parent;

  return old;
}

void push_label_level(void)
{
  current.function_decl->scoped_labels =
    new_env(parse_region, current.function_decl->scoped_labels);
}

void pop_label_level(void)
{
  check_labels();
  current.function_decl->scoped_labels =
    env_parent(current.function_decl->scoped_labels);
  assert(current.function_decl->scoped_labels);
}

void declarator_name(declarator d, const char **oname, const char **iname)
{
  *oname = *iname = NULL;
  while (d)
    {
      switch (d->kind)
	{
	case kind_identifier_declarator:
	  *oname = CAST(identifier_declarator, d)->cstring.data;
	  return;
	case kind_interface_ref_declarator:
	  *iname = CAST(interface_ref_declarator, d)->word1->cstring.data;
	  /* fall through */
	default:
	  d = CAST(nested_declarator, d)->declarator;
	  break;
	}
    }
}

const char *nice_declarator_name(declarator d)
/* Returns: a user-friendly name for declarator d, allocated in 
     current.fileregion if necessary
*/
{
  const char *name, *iname;

  declarator_name(d, &name, &iname);
  if (!name)
    return "type name";
  else if (iname)
    return make_intf_printname(iname, name);
  else
    return name;
}

dd_list check_parameter(data_declaration dd,
			type_element elements, variable_decl vd)
/* Returns: Attributes found while parsing the declarator */
{
  int class;
  scflags scf;
  const char *name, *printname;
  bool defaulted_int;
  type parm_type;
  dd_list extra_attr;

  parse_declarator(elements, vd->declarator, FALSE, FALSE,
		   &class, &scf, NULL, &name, &parm_type,
		   &defaulted_int, NULL, &extra_attr);
  vd->declared_type = parm_type;
  printname = name ? name : "type name";

  /* Storage class checks */
  if (class && class != RID_REGISTER)
    {
      error("storage class specified for parameter `%s'", printname);
      class = 0;
    }

  check_variable_scflags(scf, vd->location, "parameter", printname);

  /* A parameter declared as an array of T is really a pointer to T.
     One declared as a function is really a pointer to a function.  */
  if (type_array(parm_type))
    /* Transfer const-ness of array into that of type pointed to.  */
    parm_type =
      make_pointer_type(qualify_type1(type_array_of(parm_type), parm_type));
  else if (type_function(parm_type))
    parm_type = make_pointer_type(parm_type);

  init_data_declaration(dd, CAST(declaration, vd), name, parm_type);
  dd->kind = decl_variable;
  dd->definition = dd->ast;
  dd->isexternalscope = FALSE;
  dd->isfilescoperef = FALSE;
  dd->needsmemory = FALSE;
  dd->isused = TRUE;
  dd->vtype = class == RID_REGISTER ? variable_register : variable_normal;
  dd->islocal = dd->isparameter = TRUE;
  dd->norace = (scf & scf_norace) != 0;

  return extra_attr;
}

static bool error_signature(type fntype)
/* Returns: TRUE if fntype is the "error in function declaration"
     type signature (varargs with one argument of type error_type)
*/
{
  typelist tl;
  typelist_scanner stl;

  if (!type_function_varargs(fntype))
    return FALSE;

  tl = type_function_arguments(fntype);

  if (!tl || empty_typelist(tl))
    return FALSE;

  typelist_scan(tl, &stl);
  return typelist_next(&stl) == error_type && !typelist_next(&stl);
}

/* Start definition of variable 'elements d' with attributes attributes, 
   asm specification astmt.
   If initialised is true, the variable has an initialiser.
   Returns the declaration for the variable.
*/
declaration start_decl(declarator d, asm_stmt astmt, type_element elements,
		       bool initialised, attribute attributes)
{
  variable_decl vd = 
    new_variable_decl(parse_region, d->location, d, attributes, NULL,
		      astmt, NULL);
  dd_list extra_attr;
  struct data_declaration tempdecl;
  data_declaration ddecl = NULL, old_decl;
  dd_list doc_tags = NULL;

  detect_bogus_env();

  if (current.env->parm_level)
    {
      extra_attr = check_parameter(&tempdecl, elements, vd);

      handle_decl_attributes(attributes, &tempdecl);
      handle_decl_dd_attributes(extra_attr, &tempdecl);

      if (type_void(tempdecl.type))
	{
	  error("parameter `%s' declared void", tempdecl.name);
	  tempdecl.type = int_type;
	}

      /* Update environment for old-style declarations only if function
	 doesn't have a prototype. We will report an error message later
	 for arguments specified for functions with prototypes. */
      if (oldstyle_function(current.function_decl))
	{
	  /* Traditionally, a parm declared float is actually a double.  */
	  if (flag_traditional &&
	      type_equal_unqualified(tempdecl.type, float_type))
	    tempdecl.type = qualify_type1(double_type, tempdecl.type);

	  old_decl = lookup_id(tempdecl.name, TRUE);

	  if (old_decl && duplicate_decls(&tempdecl, old_decl, FALSE, FALSE))
	    {
	      /* Don't allow more than one "real" duplicate
		 of a forward parm decl.  */
	      ddecl = old_decl;
	      ddecl->type = tempdecl.type;
	      ddecl->ast = CAST(declaration, vd);
	      ddecl->isused = TRUE;
	    }
	  else
	    {
	      error("declaration for parameter `%s' but no such parameter",
		    tempdecl.name);
	      ddecl = declare(current.env, &tempdecl, FALSE);
	    }
	}
      else
	/* Make a dummy decl to keep everyone happy */
	ddecl = declare(current.env, &tempdecl, FALSE);

      if (initialised)
	error("parameter `%s' is initialized",
	      vd->ddecl ? vd->ddecl->name : "type name");
    }
  else
    {
      int class;
      scflags scf;
      const char *name, *printname;
      bool defaulted_int;
      type var_type;
      bool different_binding_level = FALSE;
      function_declarator fdeclarator = NULL;

      parse_declarator(elements, d, FALSE, FALSE,
		       &class, &scf, NULL, &name, &var_type,
		       &defaulted_int, &fdeclarator, &extra_attr);
      vd->declared_type = var_type;
      printname = name ? name : "type name";

      if (current.language == l_interface)
	{
	  if (!(type_command(var_type) || type_event(var_type)))
	    {
	      error("only commands and events can be defined in interfaces");
	      class = RID_COMMAND;
	      var_type = dummy_function_type;
	      fdeclarator = dummy_function_declarator;
	    }
	}
      else if (current.language == l_component)
	{
	  if (type_command(var_type) || type_event(var_type))
	    {
	      if (current.spec_section == spec_normal)
		error("commands/events must be provided or used");
	    }
	  else if (class == RID_TYPEDEF)
	    {
	      if (current.spec_section != spec_normal)
		error("typedefs cannot be provided or used");
	    }
	  else
	    {
	      error("variables and functions cannot be declared in component specifications");
	      var_type = error_type;
	    }
	}
      else if (current.language == l_implementation &&
	       current.container->configuration)
	{
	  if (class != RID_TYPEDEF)
	    error("only types and constants can be declared in configurations");
	}
      else if (class == RID_COMMAND || class == RID_EVENT)
	{
	  if (current.language == l_implementation)
	    error("commands or events can only be defined, not declared");
	  else
	    error("commands or events not allowed in C files");
	  class = 0;
	}

      if (warn_implicit_int && defaulted_int && !type_function(var_type))
	warning("type defaults to `int' in declaration of `%s'", printname);

      init_data_declaration(&tempdecl, CAST(declaration, vd), name, var_type);

      /* The fun begins */

      /* `extern' with initialization is invalid if not at top level.  */
      if (class == RID_EXTERN && initialised)
	{
	  if (current.env->global_level)
	    warning("`%s' initialized and declared `extern'", printname);
	  else
	    error("`%s' has both `extern' and initializer", printname);
	}

      if (class == RID_AUTO && current.env->global_level)
	{
	  error("top-level declaration of `%s' specifies `auto'", printname);
	  class = 0;
	}

      if (class == RID_TYPEDEF)
	{
	  /* typedef foo = bar  means give foo the same type as bar.
	     We haven't parsed bar yet, so `finish_decl' will fix that up.
	     Any other case of an initialization in a TYPE_DECL is an error. */
	  if (initialised && (pedantic || elements->next))
	    error("typedef `%s' is initialized", printname);

	  tempdecl.kind = decl_typedef;
	  tempdecl.definition = tempdecl.ast;
	  tempdecl.isexternalscope = FALSE;
	  tempdecl.isfilescoperef = FALSE;
	  tempdecl.needsmemory = FALSE;

	  /* XXX: should give errors for silly values of scf
	     (but gcc doesn't even complain about
	     inline typedef int foo;) */
	}
      else if (type_functional(var_type) || type_generic(var_type))
	{
	  /* Note: type_generic here can only be for generic functions
	     (generic interfaces only show up in components), and use
	     declare_interface_ref */
	  bool nested = !current.env->global_level && class == RID_AUTO;

	  if (initialised)
	    error("function `%s' is initialized like a variable",
		  printname);

	  if (class == RID_AUTO && pedantic)
	    pedwarn("invalid storage class for function `%s'", name);
	  if (class == RID_REGISTER)
	    {
	      error("invalid storage class for function `%s'", name);
	      class = 0;
	    }
	  /* Function declaration not at top level.
	     Storage classes other than `extern' are not allowed
	     and `extern' makes no difference.  */
	  if (!current.env->global_level && pedantic
	      && (class == RID_STATIC || class == RID_INLINE))
	    pedwarn("invalid storage class for function `%s'", name);

	  if (fdeclarator && fdeclarator->gparms)
	    {
	      if (current.language == l_interface)
		error("generic parameters not allowed in interfaces");
	      else if (!(class == RID_COMMAND || class == RID_EVENT))
		error("generic parameters not allowed on functions");
	    }

	  if ((type_command(var_type) || type_event(var_type)) &&
	      type_function_varargs(var_type) && !error_signature(var_type))
	    error("varargs commands and events are not supported");

	  check_function(&tempdecl, CAST(declaration, vd), class, scf,
			 name, var_type, nested, TRUE, defaulted_int);
	}
      else
	{
	  int extern_ref = !initialised && class == RID_EXTERN;

	  if (type_void(var_type) &&
	      !(class == RID_EXTERN ||
		(current.env->global_level &&
		 !(class == RID_STATIC || class == RID_REGISTER))))
	    {
	      error("variable `%s' declared void", printname);
	      var_type = int_type;
	    }

	  /* It's a variable.  */
	  check_variable_scflags(scf, d->location, "variable", printname);
#if 0
	  /* Don't allow initializations for incomplete types
	     except for arrays which might be completed by the initialization.  */
	  if (TYPE_SIZE (TREE_TYPE (decl)) != 0)
	    {
	      /* A complete type is ok if size is fixed.  */

	      if (TREE_CODE (TYPE_SIZE (TREE_TYPE (decl))) != INTEGER_CST
		  || C_DECL_VARIABLE_SIZE (decl))
		{
		  error ("variable-sized object may not be initialized");
		  initialised = 0;
		}
	    }
	  else if (TREE_CODE (TREE_TYPE (decl)) != ARRAY_TYPE)
	    {
	      error ("variable `%s' has initializer but incomplete type",
		     IDENTIFIER_POINTER (DECL_NAME (decl)));
	      initialised = 0;
	    }
	  else if (TYPE_SIZE (TREE_TYPE (TREE_TYPE (decl))) == 0)
	    {
	      error ("elements of array `%s' have incomplete type",
		     IDENTIFIER_POINTER (DECL_NAME (decl)));
	      initialised = 0;
	    }
#endif
	  tempdecl.kind = decl_variable;
	  tempdecl.vtype =
	    class == RID_REGISTER ? variable_register :
	    class == RID_STATIC ? variable_static :
	    variable_normal;
	  tempdecl.isfilescoperef = extern_ref;
	  if (!extern_ref)
	    tempdecl.definition = tempdecl.ast;
	  if (current.env->global_level)
	    {
	      tempdecl.isexternalscope =
		class != RID_STATIC && class != RID_REGISTER;
	      tempdecl.needsmemory = !extern_ref;
	      tempdecl.islocal = FALSE;
	    }
	  else
	    {
	      tempdecl.isexternalscope = extern_ref;
	      tempdecl.needsmemory = class == RID_STATIC;
	      tempdecl.islocal = !(extern_ref || class == RID_STATIC);
	    }
	  tempdecl.norace = (scf & scf_norace) != 0;
	}

      if (warn_nested_externs && tempdecl.isfilescoperef &&
	  !current.env->global_level && !tempdecl.in_system_header)
	warning("nested extern declaration of `%s'", printname);

      handle_decl_attributes(attributes, &tempdecl);
      handle_decl_dd_attributes(extra_attr, &tempdecl);

      old_decl = lookup_id(name, !tempdecl.Cname);

      if ((current.language == l_interface || current.language == l_component)
	  && current.env->global_level)
	{
	  if (old_decl)
	    error("redefinition of `%s'", printname);
	  old_decl = NULL;
	}
      else if (!old_decl && tempdecl.isfilescoperef)
	{
	  /* Check the global environment if declaring something with file
	     scope */
	  old_decl = lookup_global_id(name);
	  /* global typedefs don't count */
	  if (old_decl && old_decl->kind == decl_typedef)
	    old_decl = NULL;
	  if (old_decl)
	    different_binding_level = TRUE;
	}

      if (old_decl &&
	  duplicate_decls(&tempdecl, old_decl, different_binding_level, initialised))
	ddecl = old_decl;
      else
	ddecl = declare(current.env, &tempdecl, FALSE);

      ddecl->defined = current.spec_section == spec_provides;
    }
  assert(ddecl);
  vd->ddecl = ddecl;

  /* If requested, replace post/task by references to an interface */
  if (type_task(ddecl->type) && flag_use_scheduler)
    handle_task_declaration(vd);

  if (ddecl->kind == decl_typedef)
    set_typedef_type(ddecl, ddecl->basetype != NULL);

  get_latest_docstring(&ddecl->doc, current.fileregion, &doc_tags);
  handle_ddecl_doc_tags(ddecl->doc.loc, ddecl, doc_tags);

  return CAST(declaration, vd);
}

/* Finish definition of decl, furnishing the optional initialiser init.
   Returns decl */
declaration finish_decl(declaration decl, expression init)
{
  variable_decl vd = CAST(variable_decl, decl);
  data_declaration dd = vd->ddecl;

  vd->arg1 = init;
  dd->initialiser = init;

  if (init)
    {
      if (dd->kind == decl_typedef)
	dd->type = init->type;
      else if (type_array(dd->type))
	{
	  /* Incomplete array types get their size from the initialiser
	     (this is set correctly for both strings and init_lists) */
	  if (!type_array_size(dd->type))
	    dd->type = init->type;
	}
      else if (type_network_base_type(dd->type))
	error_with_decl(decl, "initialisation of network base types not yet supported");
    }
  /* Check for a size */
  if (type_array(dd->type))
    {
      /* Don't you love gcc code? */
      int do_default
	= (dd->needsmemory
	   /* Even if pedantic, an external linkage array
	      may have incomplete type at first.  */
	   ? pedantic && !dd->isexternalscope
	   : !dd->isfilescoperef);

      if (!type_array_size(dd->type))
	{
	  if (do_default)
	    error_with_decl(decl, "array size missing in `%s'",
			    decl_printname(dd));
	  /* This is what gcc has to say about the next line
	     (see comment/question above):
	     If a `static' var's size isn't known,
	     make it extern as well as static, so it does not get
	     allocated.
	     If it is not `static', then do not mark extern;
	     finish_incomplete_decl will give it a default size
	     and it will get allocated.  */
	  else if (!pedantic && dd->needsmemory && !dd->isexternalscope)
	    dd->isfilescoperef = 1;
	}
    }

  if (is_module_local_static(dd) && use_nido)
    dd_add_last(regionof(current.container->local_statics),
		current.container->local_statics, dd);

  return decl;
}

/* Create definition of function parameter 'elements d' with attributes
   attributes.
   Returns the declaration for the parameter.
*/
declaration declare_parameter(declarator d, type_element elements,
			      attribute attributes)
{
  /* There must be at least a declarator or some form of type specification */
  location l =
    d ? d->location : elements->location;
  variable_decl vd =
    new_variable_decl(parse_region, l, d, attributes, NULL, NULL, NULL);
  data_decl dd =
    new_data_decl(parse_region, l, elements, CAST(declaration, vd));
  data_declaration ddecl = NULL, old_decl = NULL;
  struct data_declaration tempdecl;
  dd_list extra_attr;

  extra_attr = check_parameter(&tempdecl, elements, vd);

  if (tempdecl.name)
    old_decl = lookup_id(tempdecl.name, TRUE);

  if (old_decl && duplicate_decls(&tempdecl, old_decl, FALSE, FALSE))
    {
      /* Don't allow more than one "real" duplicate
	 of a forward parm decl.  */
      ddecl = old_decl;
      ddecl->isused = TRUE;
    }
  else if (!type_void(tempdecl.type))
    ddecl = declare(current.env, &tempdecl, FALSE);

  if (ddecl)
    {
      /* Forward transparent union property from union to parameter */
      if (type_union(ddecl->type) && type_tag(ddecl->type)->transparent_union)
	transparent_union_argument(ddecl);

      handle_decl_attributes(attributes, ddecl);
      handle_decl_dd_attributes(extra_attr, ddecl);
    }
  else
    {
      ignored_dd_attributes(extra_attr);
      ignored_attributes(attributes);
    }

  vd->ddecl = ddecl;

  return CAST(declaration, dd);
}

void allow_parameter_redeclaration(declaration parms, bool mark_forward)
{
  declaration parm;

  /* We could walk the parameter env instead, but this will break some code
     I'm currently writing... */

  scan_declaration (parm, parms)
    if (is_data_decl(parm)) /* skip errors */
      {
	data_decl pd = CAST(data_decl, parm);
	variable_decl vd = CAST(variable_decl, pd->decls);
	
	if (mark_forward)
	  vd->forward = TRUE;
	if (vd->ddecl)
	  {
	    vd->ddecl->isused = FALSE;
	    /* This being non-NULL is used to detect redeclarations 
	       in handle_fdecl_doc_tags - it being non-NULL is an indication
	       that we're not working on a "fresh" (just-parsed) AST */
	    assert(vd->ddecl->ast->parent == NULL);
	  }
      }
}

declaration declare_old_parameter(location l, cstring id)
{
  oldidentifier_decl d = new_oldidentifier_decl(parse_region, l, id, NULL);
  data_declaration ddecl;

  if ((ddecl = lookup_id(id.data, TRUE)))
    error("duplicate parameter name `%s' in parameter list", id.data);
  else
    {
      struct data_declaration tempdecl;

      /* The void type indicates that this is an old-style declaration */
      /* Note that isused is left FALSE to allow one declaration */
      init_data_declaration(&tempdecl, CAST(declaration, d), id.data,
			    void_type);
      tempdecl.kind = decl_variable;
      tempdecl.definition = tempdecl.ast;
      tempdecl.isexternalscope = FALSE;
      tempdecl.isfilescoperef = FALSE;
      tempdecl.needsmemory = FALSE;
      tempdecl.vtype = variable_normal;
      tempdecl.islocal = tempdecl.isparameter = TRUE;
      ddecl = declare(current.env, &tempdecl, FALSE);
    }
  d->ddecl = ddecl;

  return CAST(declaration, d);
}

/* Start definition of struct/union (indicated by skind) type tag. */
type_element start_struct(location l, AST_kind skind, word tag)
{
  tag_ref tref = newkind_tag_ref(parse_region, skind, l, tag, NULL, NULL, TRUE);
  tag_declaration tdecl = tag ? lookup_tag(tref, TRUE) : NULL;

  pending_xref_error();

  if (tdecl && tdecl->kind == skind)
    {
      if (tdecl->defined || tdecl->being_defined)
	{
	  error("redefinition of `%s %s'",
		tagkind_name(skind), tag->cstring.data);
	  tdecl = declare_tag(tref);
	}
    }
  else
    tdecl = declare_tag(tref);

  tref->tdecl = tdecl;
  tdecl->definition = tref;
  tdecl->being_defined = TRUE;
  tdecl->packed |= flag_pack_struct;

  return CAST(type_element, tref);
}

static field_declaration *declare_field(tag_declaration tdecl,
					field_declaration fdecl,
					location floc,
					field_declaration *nextfield)
{
  type field_type = fdecl->type;
  const char *name = fdecl->name;

  fdecl->containing_tag = tdecl;

  if (!tdecl->fields_const)
    {
      type base_field_type = type_base(field_type);
      if (type_const(base_field_type) ||
	  ((type_struct(base_field_type) || type_union(base_field_type)) &&
	   type_tag(base_field_type)->fields_const))
	tdecl->fields_const = TRUE;
    }

  /* XXX: Surely we should do the same as for const here ? */
  if (type_volatile(field_type))
    tdecl->fields_volatile = TRUE;

  if (name)
    {
      if (env_lookup(tdecl->fields, name, TRUE))
	error_with_location(floc, "duplicate member `%s'", name);
      env_add(tdecl->fields, name, fdecl);
    }

#if 0
  fdecl->next = NULL;
#endif

  *nextfield = fdecl;
  return &fdecl->next;
}

cval check_bitfield_width(field_declaration fdecl)
{
  expression w = fdecl->ast->arg1;
  known_cst cwidth = w->cst;
  cval bitwidth = cval_top;
  bool printmsg;
  const char *errormsg = NULL;

  printmsg = check_constant_once(w, cst_numerical);

  if (cwidth && constant_unknown(cwidth))
    bitwidth = cval_unknown_number;
  else if (!(cwidth && constant_integral(cwidth)))
    errormsg = "bit-field `%s' width not an integer constant";
  else
    {
      largest_uint width = constant_uint_value(cwidth);

      if (pedantic && printmsg)
	constant_overflow_warning(cwidth);

      /* Detect and ignore out of range field width.  */
      if (!type_unsigned(cwidth->type) && constant_sint_value(cwidth) < 0)
	errormsg = "negative width in bit-field `%s'";
      else if (width > type_size_int(fdecl->type) * BITSPERBYTE)
	errormsg = "width of `%s' exceeds its type";
      else if (width == 0 && fdecl->name)
	errormsg = "zero width for bit-field `%s'";
      else
	bitwidth = cval_cast(cwidth->cval, size_t_type);
    }

  if (printmsg && errormsg)
    error_with_location(w->location, errormsg, nice_field_name(fdecl->name));

  return bitwidth;
}

static bool is_nx_tag(tag_declaration tdecl)
{
  return tdecl->kind == kind_nx_struct_ref || tdecl->kind == kind_nx_union_ref;
}

/* Finish definition of struct/union furnishing the fields and attribs.
   Computes size and alignment of struct/union (see ASSUME: comments).
   Returns t */
void layout_struct(tag_declaration tdecl)
{
  cval offset, alignment, size;
  bool isunion = tdecl->kind == kind_union_ref || tdecl->kind == kind_nx_union_ref;
  field_declaration fdecl;
  declaration dlist;
  field_decl flist;
  bool isnetwork = is_nx_tag(tdecl);
  bool lastbitfield_be = FALSE;

  offset = size = make_type_cval(0);
  alignment = cval_bitsperbyte;
  
  // We scan all the fields of the struct (field), but we also need to scan
  // the declaration of the struct to handle anonymous struct/union
  // boundaries (maybe we should have saved markers?). To do the latter,
  // we use fdecl/flist, where flist is the "current" list of fields
  // (corresponding to, e.g., `int x, y, z;' in a struct) and fdecl is
  // the declaration following the one being used in flist

  /* ASSUME: This code attempts to replicate gcc's struct layout rules for
     the target, based on it's pcc_bitfield_type_matters,
     structure_size_boundary and empty_field_boundary fields. See the gcc
     internal (`info gccint') documentation for the meaning of these
     fields.
  */
  fdecl = tdecl->fieldlist;
  dlist = tdecl->definition->fields;
  flist = NULL;
  for (;;)
    {
      cval fsize, falign;

      // Get the next data_decl in the struct
      if (!flist)
	{
	  data_decl decl;

	  if (!dlist)
	    break;

	  decl = CAST(data_decl, ignore_extensions(dlist));
	  dlist = CAST(declaration, dlist->next);

	  // Is this a struct/union we should merge in?
	  if (decl->decls)
	    flist = CAST(field_decl, decl->decls); // No.
	  else
	    {
	      tag_declaration anon_tdecl = get_unnamed_tag_decl(decl);
	      field_declaration anon_field;

	      // No?
	      if (!anon_tdecl || !anon_tdecl->defined || anon_tdecl->name)
		continue;

	      // Yes. Get size, alignment of struct/union
	      fsize = cval_times(anon_tdecl->size, cval_bitsperbyte);
	      falign = cval_times(anon_tdecl->alignment, cval_bitsperbyte);

	      /* Adjust copied anonymous fields */
	      offset = cval_align_to(offset, falign);
	      for (anon_field = anon_tdecl->fieldlist; anon_field;
		   anon_field = anon_field->next, fdecl = fdecl->next)
		fdecl->offset = cval_add(anon_field->offset, offset);
	    }
	}

      if (flist)
	{
	  /* decode field_decl field */
	  type field_type = fdecl->type;
	  cval bitwidth = cval_top;

	  if (flist->arg1)
	    bitwidth = check_bitfield_width(fdecl);

	  /* Check for network type fields in network structures once
	     the type is known. Avoid duplicate error messages. */
	  if (isnetwork && !flist->type_checked && !type_variable(field_type))
	    {
	      flist->type_checked = TRUE;
	      if (!type_network(field_type))
		error_with_location(flist->location, "field `%s' must be a network type",
				    fdecl->name);
	    }

	  fdecl->bitwidth = bitwidth;

	  if (type_size_cc(field_type))
	    fsize = cval_times(type_size(field_type), cval_bitsperbyte);
	  else
	    fsize = cval_top;

	  /* don't care about alignment if no size (type_incomplete(field_type)
	     is true, so we got an error above */
	  if (fdecl->packed || tdecl->packed)
	    falign = cval_bitsperbyte;
	  else
	    falign = cval_times
	      (type_has_size(field_type) ? type_alignment(field_type) : cval_top,
	       cval_bitsperbyte);

	  if (target->adjust_field_align && !type_realigned(field_type))
	    falign = target->adjust_field_align(fdecl, falign);

	  if (cval_istop(bitwidth)) /* regular field */
	    offset = cval_align_to(offset, falign); 
	  else if (cval_isunknown(bitwidth))
	    {
	      if (!cval_istop(offset))
		offset = cval_unknown_number;
	    }
	  else if (!cval_boolvalue(bitwidth)) /* ie, 0 */
	    {
	      if (target->pcc_bitfield_type_matters || isnetwork)
		{
		  offset = cval_align_to(offset, falign);
		  falign = make_type_cval(1); /* No structure alignment implications */
		}
	      else
		{
		  /* I'm not to blame for gcc's weirdness. */
		  if (!type_realigned(field_type))
		    falign = make_type_cval(1);
		  falign = cval_lcm(falign, make_cval_unsigned(target->empty_field_boundary, size_t_type));
		  offset = cval_align_to(offset, falign);
		}
	      fsize = bitwidth;
	    }
	  else
	    {
	      assert(cval_intcompare(bitwidth, cval_zero) > 0);
	      if (target->pcc_bitfield_type_matters && !isnetwork)
		{
		  /* skip to next unit on crossing falign-sized boundary.
		     align struct to falign (note the inconsistency with
		     the 0-width bitfield). */

		  /* This tests 
		     ((offset + bitwidth + falign - 1) / falign -
		     offset / falign) > fsize / falign
		  */
		  cval val1 = cval_sub(cval_add(cval_add(offset, bitwidth),
						falign),
				       make_type_cval(1));
		  cval val2 = cval_sub(cval_divide(val1, falign),
				       cval_divide(offset, falign));

		  // if falign or offset are top or unknown, this will
		  // contaminate val2 as appropriate
		  if (!cval_knownvalue(val2))
		    offset = val2;
		  else if (cval_intcompare(val2, cval_divide(fsize, falign)) > 0)
		    offset = cval_align_to(offset, falign);
		}
	      else
		{
		  /* More network type bitfield fun: when switching between
		     big and little-endian bitfields, we align to the next
		     byte boundary (otherwise we could start filling bytes
		     from opposing ends, which would be very confusing) */
		  if (isnetwork && type_network_base_type(field_type))
		    {
		      bool isbe = type_networkdef(field_type)->isbe;

		      if (isbe != lastbitfield_be)
			offset = cval_align_to(offset, cval_bitsperbyte);
		      lastbitfield_be = isbe;
		    }

		  // more gcc fun
		  if (type_realigned(field_type)) 
		    offset = cval_align_to(offset, falign);
		  else // don't align, don't affect struct alignment
		    falign = cval_bitsperbyte;
		}

	      fsize = bitwidth;
	    }

	  fdecl->offset = offset;

	  flist = CAST(field_decl, flist->next);
	  fdecl = fdecl->next;
	}

      if (!isunion)
	{
	  offset = cval_add(offset, fsize);
	  size = offset;
	}
      else
	size = cval_max(fsize, size);

      alignment = cval_lcm(alignment, falign);
    }
  if (!isnetwork)
    alignment = cval_lcm(alignment, make_cval_unsigned(target->structure_size_boundary, size_t_type));
  if (!cval_istop(tdecl->user_alignment))
    alignment = cval_lcm(alignment, cval_times(tdecl->user_alignment, cval_bitsperbyte));

  tdecl->size = cval_divide(cval_align_to(size, alignment), cval_bitsperbyte);
  tdecl->alignment = cval_divide(alignment, cval_bitsperbyte);
}

/* Finish definition of struct/union furnishing the fields and attribs.
   Computes size and alignment of struct/union (see ASSUME: comments).
   Returns t */
type_element finish_struct(type_element t, declaration fields,
			   attribute attribs)
{
  tag_ref s = CAST(tag_ref, t);
  tag_declaration tdecl = s->tdecl;
  bool hasmembers = FALSE;
  field_declaration *nextfield = &tdecl->fieldlist;
  declaration fdecl;
  bool isnetwork = is_nx_tag(tdecl);

  s->fields = fields;
  s->attributes = attribs;
  handle_tag_attributes(attribs, tdecl);
  tdecl->fields = new_env(parse_region, NULL);

  scan_declaration (fdecl, fields)
    {
      /* Get real list of fields */
      data_decl flist = CAST(data_decl, ignore_extensions(fdecl));
      field_decl field;

      if (!flist->decls) /* possibly a struct/union we should merge in */
	{
	  tag_declaration anon_tdecl = get_unnamed_tag_decl(flist);
	  field_declaration anon_field;
	  location floc = flist->location;

	  if (!anon_tdecl)
	    error_with_location(floc,
	      "unnamed fields of type other than struct or union are not allowed");
	  else if (!anon_tdecl->defined)
	    error_with_location(floc, "anonymous field has incomplete type");
	  else if (anon_tdecl->name)
	    warning_with_location(floc, "declaration does not declare anything");
	  else if (isnetwork && !is_nx_tag(anon_tdecl))
	    error_with_location(floc, "field `%s' must be a network type",
				nice_field_name(NULL));
	  else
	    {
	      /* Process alignment to this struct/union in "main" loop below */
	      anon_tdecl->collapsed = TRUE;

	      /* Copy fields */
	      for (anon_field = anon_tdecl->fieldlist; anon_field;
		   anon_field = anon_field->next)
		{
		  field_declaration fdecl = ralloc(parse_region, struct field_declaration);

		  *fdecl = *anon_field;
		  fdecl->ast = NULL;
		  nextfield = declare_field(tdecl, fdecl, floc, nextfield);
		  if (fdecl->name)
		    hasmembers = TRUE;
		}
	    }
	}
      else
	scan_field_decl (field, CAST(field_decl, flist->decls))
	  {
	    /* decode field_decl field */
	    field_declaration fdecl;
	    type field_type;
	    const char *name;
	    int class;
	    scflags scf;
	    const char *printname;
	    bool defaulted_int;
	    type tmpft;
	    location floc = field->location;
	    dd_list extra_attr;

	    fdecl = ralloc(parse_region, struct field_declaration);

	    parse_declarator(flist->modifiers, field->declarator,
			     field->arg1 != NULL, FALSE,
			     &class, &scf, NULL, &name, &tmpft,
			     &defaulted_int, NULL, &extra_attr);
	    field_type = tmpft;

	    /* Grammar doesn't allow scspec: */
	    assert(scf == 0 && class == 0);

	    printname = nice_field_name(name);

	    /* Support "flexible arrays" (y[] as field member) --
	       simply make the size 0 which we already handle */
	    if (type_array(field_type) && !type_array_size(field_type))
	      field_type = make_array_type(type_array_of(field_type),
					   build_zero(parse_region, dummy_location));

	    if (type_function(field_type))
	      {
		error_with_location(floc, "field `%s' declared as a function", printname);
		field_type = make_pointer_type(field_type);
	      }
	    else if (type_void(field_type))
	      {
		error_with_location(floc, "field `%s' declared void", printname);
		field_type = error_type;
	      }
	    else if (type_incomplete(field_type)) 
	      {
		error_with_location(floc, "field `%s' has incomplete type", printname);
		field_type = error_type;
	      }

	    fdecl->type = field_type;
	    handle_field_attributes(field->attributes, fdecl);
	    handle_field_dd_attributes(extra_attr, fdecl);
	    field_type = fdecl->type; /* attributes might change type */

	    if (field->arg1)
	      {
		const char *errmsg = NULL;

		if (!type_integer(field_type))
		  errmsg = "bit-field `%s' has invalid type";
		else if (!(type_integer(field->arg1->type)))
		  errmsg = "bit-field `%s' width not an integer constant";
		else if (type_network_base_type(field_type))
		  {
		    if (!type_networkdef(field_type)->bf_encoder)
		      errmsg = "type of `%s' cannot be used as a bit-field";
		    else if (!isnetwork)
		      errmsg = "bit-field `%s' of network type used inside non-network type";
		  }

		if (errmsg)
		  {
		    error_with_location(floc, errmsg, printname);
		    field->arg1 = NULL;
		  }
	      }

	    fdecl->ast = field;
	    field->fdecl = fdecl;
	    fdecl->name = name;
	    nextfield = declare_field(tdecl, fdecl, floc, nextfield);
	    if (name)
	      hasmembers = TRUE;
	  }
    }

  if (pedantic && !is_attribute_ref(s) && !hasmembers)
    pedwarn("%s has no %smembers", tagkind_name(s->kind),
	    (fields ? "named " : ""));

  tdecl->defined = TRUE;
  tdecl->being_defined = FALSE;

  layout_struct(tdecl);

  return t;
}

/* Return a reference to struct/union/enum (indicated by skind) type tag */
type_element xref_tag(location l, AST_kind skind, word tag)
{
  tag_ref tref = newkind_tag_ref(parse_region, skind, l, tag, NULL, NULL, FALSE);
  tag_declaration tdecl = lookup_tag(tref, FALSE);

  if (!tdecl)
    tdecl = declare_tag(tref);

  tref->tdecl = tdecl;

  return CAST(type_element, tref);
}

static known_cst last_enum_value;

void layout_enum_start(tag_declaration tdecl)
{
  last_enum_value = NULL;
}

void layout_enum_end(tag_declaration tdecl)
{
  declaration names = tdecl->definition->fields;
  cval smallest, largest;
  bool enum_isunsigned;
  type type_smallest, type_largest, enum_reptype;
  enumerator v, values = CAST(enumerator, names);

  /* Pick a representation type for this enum, if not already done. */
  if (tdecl->reptype && !type_unknown_int(tdecl->reptype))
    return;

  /* First, find largest and smallest values defined in this enum. */
  if (!names)
    smallest = largest = cval_zero;
  else
    {
      smallest = largest = value_of_enumerator(values);

      if (!cval_isunknown(smallest))
	scan_enumerator (v, CAST(enumerator, values->next))
         {
	   cval vv = value_of_enumerator(v);

	   if (cval_isunknown(vv))
	     {
	       smallest = vv;
	       break;
	     }
	   if (cval_intcompare(vv, largest) > 0)
	     largest = vv;
	   if (cval_intcompare(vv, smallest) < 0)
	     smallest = vv;
	 }
    }

  if (cval_isunknown(smallest))
    enum_reptype = unknown_int_type;
  else
    {
      /* Pick a type that will hold the smallest and largest values. */
      enum_isunsigned = cval_intcompare(smallest, cval_zero) >= 0;
      type_smallest = type_for_cval(smallest, enum_isunsigned);
      type_largest = type_for_cval(largest, enum_isunsigned);
      assert(type_smallest);
      if (!type_largest)
	{
	  assert(!enum_isunsigned);
	  warning("enumeration values exceed range of largest integer");
	  type_largest = long_long_type;
	}
      if (type_size_int(type_smallest) > type_size_int(type_largest))
	enum_reptype = type_smallest;
      else
	enum_reptype = type_largest;

      /* We use int as the enum type if that fits, except if both:
	 - the values fit in a (strictly) smaller type
	 - the packed attribute was specified 
      */
      if (cval_inrange(smallest, int_type) && cval_inrange(largest, int_type) &&
	  !(tdecl->packed && type_size_int(enum_reptype) < type_size_int(int_type)))
	enum_reptype = int_type;
    }

  tdecl->reptype = enum_reptype;
  tdecl->size = type_size(enum_reptype);
  tdecl->alignment = type_alignment(enum_reptype);

  /* Change type of all enum constants to enum_reptype */
  scan_enumerator (v, values)
    v->ddecl->value = cast_constant(v->ddecl->value, enum_reptype);
}

known_cst layout_enum_value(enumerator e)
{
  const char *name = e->cstring.data;
  expression value = e->arg1;
  known_cst cst = NULL;

  // We're already done if we have a non-unknown type for e's value
  if (e->ddecl && !type_unknown_int(e->ddecl->value->type))
    return e->ddecl->value;

  if (value)
    {
      cst = value->cst;
      if (check_constant_once(value, cst_numerical))
	{
	  if (!value->cst || !constant_integral(value->cst))
	    {
	      error("enumerator value for `%s' not integer constant", name);
	      cst = NULL;
	    }
	}
    }

  if (!cst)
    {
      /* Last value + 1 */
      if (last_enum_value)
	{
	  /* No clear logic anywhere to specify which type we should use
	     (ANSI C must specify int, cf warning below) */
	  type addtype = type_unsigned(last_enum_value->type) ?
	    unsigned_long_long_type : long_long_type;

	  cst = fold_add(addtype, last_enum_value, onecst);
	}
      else
	cst = zerocst;
    }

  if (constant_integral(cst))
    {
      if (pedantic && !cval_inrange(cst->cval, int_type))
	{
	  pedwarn("ANSI C restricts enumerator values to range of `int'");
	  cst = zerocst;
	}

      if (type_size_int(cst->type) < type_size_int(int_type))
	cst->type =
	  type_for_size(type_size(int_type),
			flag_traditional && type_unsigned(cst->type));
    }
  last_enum_value = cst;

  return cst;
}

/* Start definition of struct/union (indicated by skind) type tag. */
type_element start_enum(location l, word tag)
{
  enum_ref tref = new_enum_ref(parse_region, l, tag, NULL, NULL, TRUE);
  tag_declaration tdecl = tag ? lookup_tag(tref, TRUE) : NULL;

  pending_xref_error();

  if (tdecl && tdecl->kind == kind_enum_ref)
    {
      if (tdecl->defined)
	error("redefinition of `enum %s'", tag->cstring.data);
    }
  else
    tdecl = declare_tag(tref);

  tref->tdecl = tdecl;
  tdecl->definition = tref;
  tdecl->being_defined = TRUE;
  tdecl->packed = flag_short_enums;
  layout_enum_start(tdecl);

  return CAST(type_element, tref);
}

/* Finish definition of enum furnishing the names and attribs.
   Returns t */
type_element finish_enum(type_element t, declaration names,
			 attribute attribs)
{
  tag_ref s = CAST(tag_ref, t);
  tag_declaration tdecl = s->tdecl;

  s->fields = names;
  s->attributes = attribs;
  handle_tag_attributes(attribs, tdecl);
  tdecl->fields = 0;
  tdecl->defined = TRUE;
  tdecl->being_defined = FALSE;

  layout_enum_end(tdecl);

  return t;
}

declaration make_enumerator(location loc, cstring id, expression value)
{
  declaration ast;
  struct data_declaration tempdecl;
  data_declaration ddecl, old_decl;
  environment env = current.env;

  if (value && !type_integer(value->type))
    {
      error("enumerator value for `%s' not integer constant", id.data);
      value = NULL;
    }
  
  ast = CAST(declaration, new_enumerator(parse_region, loc, id, value, NULL));
  init_data_declaration(&tempdecl, ast, id.data, int_type);
  tempdecl.kind = decl_constant;
  tempdecl.definition = ast;
  tempdecl.value = layout_enum_value(CAST(enumerator, ast));

  if (current.language == l_interface)
    {
      error("only commands and events can be defined in interfaces");
      /* We don't want the symbol in the interface's env, so give
	 it it's own private home! */
      env = new_environment(parse_region, NULL, FALSE, FALSE);
    }

  old_decl = env_lookup(env->id_env, id.data, TRUE);

  if (old_decl && duplicate_decls(&tempdecl, old_decl, FALSE, FALSE))
    ddecl = old_decl;
  else
    ddecl = declare(env, &tempdecl, FALSE);

  CAST(enumerator, ast)->ddecl = ddecl;

  return ast;
}

/* Create declaration of field 'elements d : bitfield' with attributes
   attributes.
   d can be NULL, bitfield can be NULL, but not both at the same time.
   Returns the declaration for the field.
*/
declaration make_field(declarator d, expression bitfield,
		       type_element elements, attribute attributes)
{
  /* We get at least one of a declarator or a bitfield */
  location l = d ? d->location : bitfield->location;

  return
    CAST(declaration,
	 new_field_decl(parse_region, l, d, attributes, bitfield));
}


/* Create and return type 'elements d' where d is an absolute declarator */
asttype make_type(type_element elements, declarator d)
{
  location l = elements ? elements->location : d->location;
  int class;
  scflags scf;
  const char *name;
  bool defaulted_int;
  asttype t = new_asttype(parse_region, l, d, elements);
  dd_list extra_attr;

  parse_declarator(t->qualifiers, t->declarator, FALSE, FALSE,
		   &class, &scf, NULL, &name, 
		   &t->type, &defaulted_int, NULL, &extra_attr);
  assert(t->type && !(class || scf || name));

  return t;
}


/* Returns name of r */
static char *rid_name_int(int id)
{
  switch (id)
    {
    case RID_INT: return "int";
    case RID_CHAR: return "char";
    case RID_FLOAT: return "float";
    case RID_DOUBLE: return "double";
    case RID_VOID: return "void";
    case RID_UNSIGNED: return "unsigned";
    case RID_SHORT: return "short";
    case RID_LONG: return "long";
    case RID_AUTO: return "auto";
    case RID_STATIC: return "static";
    case RID_EXTERN: return "extern";
    case RID_REGISTER: return "register";
    case RID_TYPEDEF: return "typedef";
    case RID_SIGNED: return "signed";
    case RID_INLINE: return "__inline";
    case RID_COMPLEX: return "__complex";
    case RID_COMMAND: return "command";
    case RID_EVENT: return "event";
    case RID_ASYNC: return "async";
    case RID_TASK: return "task";
    case RID_DEFAULT: return "default";
    case RID_NORACE: return "norace";
    default: assert(0); return NULL;
    }
}

/* Returns name of r */
char *rid_name(rid r)
{
  return rid_name_int(r->id);
}

/* If statement list l1 ends with an unfinished label, attach l2 to that
   label. Otherwise attach l2 to the end of l1 */
statement chain_with_labels(statement l1, statement l2)
{
  node last, last_label;

  if (!l1) return l2;
  if (!l2) return l1;

  last_label = last = last_node(CAST(node, l1));
  /* There may be an unfinished sub-label due to 'a: b:' */
  while (last_label->kind == kind_labeled_stmt)
    {
      labeled_stmt ls = CAST(labeled_stmt, last_label);

      if (!ls->stmt) /* An unfinished labeled statement */
	{
	  ls->stmt = l2;
	  return l1;
	}
      last_label = CAST(node, ls->stmt);
    }

  last->next = CAST(node, l2);

  return l1;
}

void init_semantics(void)
{
  current.fileregion = parse_region;

  spontaneous_calls = dd_new_list(parse_region);

  global_env = current.env = new_environment(parse_region, NULL, TRUE, FALSE);

  bad_decl = ralloc(parse_region, struct data_declaration);
  bad_decl->kind = decl_error;
  bad_decl->name = "undeclared";
  bad_decl->type = error_type;
  bad_decl->ast = new_error_decl(parse_region, dummy_location);

  dummy_function_declarator = 
    new_function_declarator(parse_region, dummy_location, NULL, NULL, NULL, NULL,   NULL);

  implicit_function_type = make_function_type(int_type, NULL, FALSE, TRUE);
  dummy_function_type = make_function_type(int_type, new_typelist(parse_region), FALSE, FALSE);

  /* Create the global bindings for __FUNCTION__ and __PRETTY_FUNCTION__.  */
  declare_function_name ();

  /* Declare builtin type __builtin_va_list */
  declare_builtin_types();
  declare_builtin_identifiers();

  onecst = make_signed_cst(1, int_type);
  zerocst = make_signed_cst(0, int_type);
  oneexpr = build_uint_constant(parse_region, dummy_location, size_t_type, 1);
}

void start_semantics(source_language l, nesc_declaration container,
		     environment env)
{
  current.env = env;
  current.language = l;
  current.function_decl = NULL;
  current.pending_invalid_xref = NULL;
  current.container = container;
  current.in_atomic = NULL;
  current.spec_section = spec_normal;
}

static bool samekind(type t1, type t2)
{
  return (type_integer(t1) && type_integer(t2)) ||
    (type_floating(t1) && type_floating(t2));
}

bool handle_mode_attribute(location loc, data_declaration ddecl, const char *mode)
{
  type tm, t = ddecl->type;

  /* Simplified mode attribute support. We're missing:
     - support for multiple pointer sizes (see comment below)
     - the vector modes
  */

  if (!(ddecl->kind == decl_variable || ddecl->kind == decl_typedef))
    return FALSE;

  tm = type_for_mode(mode, type_unsigned(t));

  if (!tm)
    {
      error_with_location(loc, "unknown machine mode `%s'", mode);
      return TRUE;
    }

  if (type_pointer(t))
    {
      /* Simplified pointer handling. We only allow modes that specify
	 the pointer size (in which case we do nothing). In all other
	 cases, we'll report an error. Most targets do not support more
	 than one pointer mode (the exceptions seem to be s390 and mips,
	 that allow both 32 and 64 bit pointers) */
      if (type_size_int(t) != type_size_int(tm))
	error("invalid pointer mode `%s'", mode);
    }
  else if (samekind(t, tm) ||
      (type_complex(t) && type_complex(tm) &&
       samekind(make_base_type(t), make_base_type(tm))))
    /* If this is a valid resizing, set type. We lose enum-ness here
       but that shouldn't matter. (If it does, add changing-size support
       in types.c) */
    ddecl->type = tm;
  else
    error_with_location(loc, "mode `%s' applied to inappropriate type", mode);

  return TRUE;
}

/* Make "word" argument of attributes into an expression */
expression make_attr_args(location loc, cstring id, expression args)
{
  identifier result = new_identifier(parse_region, loc, id, bad_decl);

  result->type = error_type;
  result->next = CAST(node, args);

  return CAST(expression, result);
}
