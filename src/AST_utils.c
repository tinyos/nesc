/* This file is part of the nesC compiler.

This file is derived from the RC Compiler. It is thus
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
#include "AST_utils.h"
#include "constants.h"

bool oldstyle_function(function_decl fn)
{
  return !fn->fdeclarator->parms || is_oldidentifier_decl(fn->fdeclarator->parms);
}

/* True if the parameters is just the parameter list '(void)' */
bool is_void_parms(declaration parms)
{
  data_decl dd;
  variable_decl vd;

  if (!parms || parms->next || !is_data_decl(parms))
    return FALSE;

  dd = CAST(data_decl, parms);
  vd = CAST(variable_decl, dd->decls);
  assert(!vd->next);

  return !vd->declarator && dd->modifiers && !dd->modifiers->next &&
    is_rid(dd->modifiers) && CAST(rid, dd->modifiers)->id == RID_VOID;
}

function_declarator get_fdeclarator(declarator d)
{
  function_declarator fd = NULL;

  while (d)
    switch (d->kind)
      {
      case kind_identifier_declarator:
	return fd;
      case kind_function_declarator:
	fd = CAST(function_declarator, d);
	/* fallthrough */
      default:
	d = CAST(nested_declarator, d)->declarator;
	break;
      }

  return fd;
}

data_declaration get_parameter(declaration d)
{
  switch (d->kind)
    {
    case kind_ellipsis_decl:
      return NULL;
    case kind_oldidentifier_decl:
      return CAST(oldidentifier_decl, d)->ddecl;
    case kind_data_decl:
      return CAST(variable_decl, CAST(data_decl, d)->decls)->ddecl;
    default:
      assert(0); return NULL;
    }
}

data_declaration base_identifier(data_declaration d)
{
  while (d->isfilescoperef && d->shadowed)
    d = d->shadowed;

  return d;
}

bool same_function(data_declaration d1, data_declaration d2)
{
  d1 = base_identifier(d1);
  d2 = base_identifier(d2);
  return d1->kind == decl_function && d1 == d2;
}

bool call_to(data_declaration fnd, function_call fce)
{
  return is_identifier(fce->arg1) &&
    fnd && same_function(CAST(identifier, fce->arg1)->ddecl, fnd);
}

bool is_localvar(expression e)
{
  data_declaration decl;

  if (!is_identifier(e))
    return FALSE;

  decl = CAST(identifier, e)->ddecl;
  return decl->kind == decl_variable && decl->islocal;
}

compound_stmt parent_block(node n)
{
  /* Don't consider statement expressions as parent blocks - the last statement
     is special (value of the block) which confuses code that assumes arbitrary
     statements can be added to the end of a block */
  while (!is_compound_stmt(n) || is_compound_expr(n->parent))
    n = n->parent;

  return CAST(compound_stmt, n);
}

function_decl parent_function(node n)
{
  while (!is_function_decl(n))
    n = n->parent;

  return CAST(function_decl, n);
}

expression ignore_fields(expression e)
{
  while (e->kind == kind_field_ref)
    e = CAST(field_ref, e)->arg1;

  return e;
}

expression expression_of_stmt(compound_expr ce)
{
  compound_stmt blk = CAST(compound_stmt, ce->stmt);
  statement last_stmt = last_statement(blk->stmts);

  if (last_stmt && is_expression_stmt(last_stmt))
    {
      expression_stmt es = CAST(expression_stmt, last_stmt);

      return es->arg1;
    }
  return NULL;
}

bool expression_used(expression e)
{
  node p1 = e->parent, n1 = e->next;

  /* Figure out if result of assignment is used. Logic:
     used(e): case parent(e) of
     expression_stmt: false
     comma: false if e not last, used(parent(e)) otherwise
     everything else: true */
  for (;;)
    {
      if (is_expression_stmt(p1))
	return FALSE;
      if (!is_comma(p1))
	return TRUE;
      if (n1)
	return FALSE; /* Not last in a comma */
      n1 = p1->next;
      p1 = p1->parent;
    }
}

bool zero_expression(expression e)
{
  /* a = 0 is not a constant expression but it's value is known to be 0,
     and other similar cases */
  for (;;)
    {
      if (is_assign(e))
	{
	  e = CAST(assign, e)->arg2;
	  continue;
	}
      if (is_cast(e))
	{
	  e = CAST(cast, e)->arg1;
	  continue;
	}
      if (is_comma(e))
	{
	  e = CAST(expression, last_node(CAST(node, CAST(comma, e)->arg1)));
	  continue;
	}
      break;
    }

  return definite_zero(e);
}

expression build_int_constant(region r, location loc, type t, largest_int c)
{
  char cstbuf[64];
  cstring csts;
  lexical_cst cst;

  snprintf(cstbuf, sizeof cstbuf, "%lld", c);
  csts.data = rstrdup(r, cstbuf);
  csts.length = strlen(cstbuf);
  cst = new_lexical_cst(r, loc, csts);
  cst->type = t;
  cst->cst = make_cst(make_cval_signed(c, t), t);

  return CAST(expression, cst);
}

expression build_uint_constant(region r, location loc, type t, largest_uint c)
{
  char cstbuf[64];
  cstring csts;
  lexical_cst cst;

  snprintf(cstbuf, sizeof cstbuf, "%llu", c);
  csts.data = rstrdup(r, cstbuf);
  csts.length = strlen(cstbuf);
  cst = new_lexical_cst(r, loc, csts);
  cst->type = t;
  cst->cst = make_cst(make_cval_unsigned(c, t), t);

  return CAST(expression, cst);
}

cval value_of_enumerator(enumerator e)
{
  return e->ddecl->value->cval;
}

expression build_identifier(region r, location loc, data_declaration id)
{
  identifier e = new_identifier(r, loc, str2cstring(r, id->name), id);

  assert(id->kind == decl_variable || id->kind == decl_function ||
	 id->kind == decl_constant || id->kind == decl_magic_function);
  e->type = id->type;
  e->cst = fold_identifier(CAST(expression, e), id, 0);

  return CAST(expression, e);
}

int asm_rwmode(string s)
{
  if (s->ddecl->schars.length > 0)
    return s->ddecl->schars.data[0];
  else
    return -1;
}

declaration ignore_extensions(declaration d)
{
  while (is_extension_decl(d))
    d = CAST(extension_decl, d)->decl;

  return d;
}

tag_declaration get_unnamed_tag_decl(data_decl decl)
{
  /* decl has no actual declarations. Check to see if it is a struct or
     union, and if so return that struct or union's declaration */
  type_element elem;

  scan_type_element (elem, decl->modifiers)
    if (is_struct_ref(elem) || is_union_ref(elem))
      return CAST(tag_ref, elem)->tdecl;

  return NULL;
}

const char *nice_field_name(const char *s)
/* Returns: "(anonymous)" if s == NULL, s otherwise
     This helps printing the name of potentially unnamed entities
 */
{
  if (s)
    return s;
  return "(anonymous)";
}

const char *tagkind_name(int tagkind)
{
  switch (tagkind)
    {
    case kind_attribute_ref: return "attribute";
    case kind_struct_ref: return "struct";
    case kind_union_ref: return "union";
    case kind_nx_struct_ref: return "nx_struct";
    case kind_nx_union_ref: return "nx_union";
    case kind_enum_ref: return "enum";
    default: assert(0); return NULL;
    }
}

conditional conditional_lvalue(expression e)
{
  /* a = 0 is not a constant expression but it's value is known to be 0,
     and other similar cases */
  for (;;)
    {
      if (is_cast(e))
	{
	  e = CAST(cast, e)->arg1;
	  continue;
	}
      if (is_comma(e))
	{
	  e = CAST(expression, last_node(CAST(node, CAST(comma, e)->arg1)));
	  continue;
	}
      break;
    }

  if (is_conditional(e))
    return CAST(conditional, e);
  else
    return NULL;
}

data_declaration string_ddecl(expression s)
{
  if (s->cst && constant_address(s->cst))
    {
      data_declaration sdecl = cval_ddecl(s->cst->cval);

      /* Must be an offsetless string */
      if (sdecl && sdecl->kind == decl_magic_string &&
	  cval_knownbool(s->cst->cval))
	return sdecl;
    }
  return NULL;
}

char *ddecl2str(region r, data_declaration ddecl)
/* Returns: a newly allocated string (in region r) for the string
     specified by ddecl, or NULL if str contains wide characters
   Requires: ddecl->kind == decl_magic_string
*/
{
  assert(ddecl->kind == decl_magic_string);

  if (!type_char(type_array_of(ddecl->type)))
    return NULL; /* Wide string */
  else
    return cstring2str(r, ddecl->schars);
}

/* True if arg is name or __name__ */
bool is_attr_name(const char *arg, const char *name)
{
  int l;

  if (!strcmp(arg, name))
    return TRUE;

  if (strncmp(arg, "__", 2))
    return FALSE;

  l = strlen(name);
  return !strncmp(arg + 2, name, l) && !strcmp(arg + 2 + l, "__");
}

type_element interesting_element(type_element elems)
{
  type_element elem;

  scan_type_element (elem, elems)
    if (is_tag_ref(elem))
      return elem;

  return NULL;
}

