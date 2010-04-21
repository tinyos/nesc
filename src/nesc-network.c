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
Boston, MA 02111-1307, USA. */

#include "parser.h"
#include "constants.h"
#include "nesc-network.h"
#include "nesc-semantics.h"
#include "AST_utils.h"
#include "AST_walk.h"
#include "c-parse.h"
#include "edit.h"
#include "unparse.h"
#include "nesc-uses.h"

static type uchar_ptr_type;

static bool network_bitfield(expression e)
{
  if (is_field_ref(e) && type_network_base_type(e->type))
    {
      field_ref fref = CAST(field_ref, e);
      field_declaration fdecl = fref->fdecl;

      return !cval_istop(fdecl->bitwidth);
    }
  return FALSE;
}

static void xtox_used(data_declaration transcoderfn, function_decl fn)
{
  data_declaration fdecl = fn ? fn->ddecl : NULL;

  ddecl_used(transcoderfn, new_use(dummy_location, fdecl, c_executable | c_fncall));
}

static void hton_used(expression e, function_decl fn)
{
  data_declaration ntdef = type_networkdef(e->type);

  if (network_bitfield(e))
    xtox_used(ntdef->bf_encoder, fn);
  else
    xtox_used(ntdef->encoder, fn);
}

static void ntoh_used(expression e, function_decl fn)
{
  data_declaration ntdef = type_networkdef(e->type);

  if (network_bitfield(e))
    xtox_used(ntdef->bf_decoder, fn);
  else
    xtox_used(ntdef->decoder, fn);
}

static void validate_network_lvalue(expression e)
{
  conditional cond;

  if ((cond = conditional_lvalue(e)) &&
      (type_network_base_type(cond->arg1->type) ||
       type_network_base_type(cond->arg2->type)))
    error_with_location(e->location,
			"Conditional assignment not supported for network types");
}

/* Check if expression e is of network base type.  Parameters whose address
   is not taken are actually of the base type (this should be extended to
   all local vars whose address is not taken, but currently isn't) */
static bool really_network_base(expression e)
{
  if (is_identifier(e))
    {
      identifier id = CAST(identifier, e);
      data_declaration ddecl = id->ddecl;

      if (ddecl->kind == decl_variable && ddecl->isparameter &&
	  !(ddecl->use_summary & c_addressed))
	return FALSE;
    }
  return e->type && type_network_base_type(e->type);
}

static data_declaration add_network_temporary(function_decl fn, type t)
{
  /* See Weird hack comment in prt_network_assignment */
  if (fn)
    return add_temporary(parse_region, CAST(compound_stmt, fn->stmt), t);
  else
    return NULL;
}

static AST_walker_result network_expression(AST_walker spec, void *data,
					    expression *n)
{
  expression e = *n;
  function_decl fn = data;

  if (really_network_base(e) &&
      (e->context & c_read) && !(e->context & c_write))
    ntoh_used(e, fn);

  return aw_walk;
}

static AST_walker_result network_assignment(AST_walker spec, void *data,
					    assignment *n)
{
  assignment a = *n;
  function_decl fn = data;

  if (really_network_base(a->arg1))
    {
      if (a->kind != kind_assign) /* op= reads too */
	{
	  ntoh_used(a->arg1, fn);
	  /* See problem/ugly hack comment in network_increment */
	  /* op= needs a temp */
	  if (!a->temp1)
	    a->temp1 = add_network_temporary(fn, uchar_ptr_type);
	}
      hton_used(a->arg1, fn);
    }

  validate_network_lvalue(a->arg1);

  return aw_walk;
}

static AST_walker_result network_increment(AST_walker spec, void *data,
					   increment *n)
{
  increment i = *n;
  function_decl fn = data;

  if (really_network_base(i->arg1))
    {
      ntoh_used(i->arg1, fn);
      hton_used(i->arg1, fn);

      /* Problem: adding the declarations for the temporaries changes the
	 AST as we're walking through it. If we add a temporary while
	 walking through the first declaration, we'll revisit this
	 declaration. Oops.  Ugly hack fix: don't create the temporaries if
	 they've already been created. Note that this could lead to
	 duplicate error messages from validate_network_lvalue, but that's
	 for use of a deprecated gcc feature which is going away soon. */
      if (!i->temp1)
	{
	  /* we use 2 temps */
	  i->temp1 = add_network_temporary(fn, uchar_ptr_type);
	  i->temp2 = add_network_temporary(fn, type_network_platform_type(i->type));
	}
    }

  validate_network_lvalue(i->arg1);

  return aw_walk;
}

static AST_walker_result network_fdecl(AST_walker spec, void *data,
				       function_decl *fd)
{
  AST_walk_children(spec, *fd, CAST(node, *fd));
  return aw_done;
}

/* An AST walker that does network type processing on the AST */
static AST_walker network_walker;

static void init_network_walker(void)
{
  network_walker = new_AST_walker(parse_region);
  AST_walker_handle(network_walker, kind_expression, network_expression);
  AST_walker_handle(network_walker, kind_increment, network_increment);
  AST_walker_handle(network_walker, kind_assignment, network_assignment);
  AST_walker_handle(network_walker, kind_function_decl, network_fdecl);
}

void handle_network_types(declaration decls)
{
  node n = CAST(node, decls);

  AST_walk_list(network_walker, NULL, &n);
}

static void output_hton(type t)
{
  output_string(type_networkdef(t)->encoder->name);
}

static void output_ntoh(type t)
{
  output_string(type_networkdef(t)->decoder->name);
}

static void output_hton_bf(type t)
{
  output_string(type_networkdef(t)->bf_encoder->name);
}

static void output_ntoh_bf(type t)
{
  output_string(type_networkdef(t)->bf_decoder->name);
}

static void output_hton_expr(expression e)
{
  if (network_bitfield(e))
    output_hton_bf(e->type);
  else
    output_hton(e->type);
}

static void output_ntoh_expr(expression e)
{
  if (network_bitfield(e))
    output_ntoh_bf(e->type);
  else
    output_ntoh(e->type);
}

/* Print a network lvalue, return TRUE if it's a bitfield (in which
   case the field name is not printed) */
static bool prt_network_lvalue(expression e)
{
  bool isbf = network_bitfield(e);

  if (isbf)
    {
      /* Network bitfields have no name in the generated code. Just
	 print the structure. We'll add the offset and size later. */
      output("(unsigned char *)&");
      prt_expression(CAST(field_ref, e)->arg1, P_CALL);
    }
  else
    {
      prt_expression_helper(e, P_CALL);
      output(".nxdata");
    }

  return isbf;
}

static void prt_network_bitfield_info(expression e)
{
  field_ref fref = CAST(field_ref, e);
  field_declaration fdecl = fref->fdecl;

  output(", %llu, %llu",
	 cval_uint_value(fdecl->offset), cval_uint_value(fdecl->bitwidth));
}

static void prt_network_full_lvalue(expression e)
{
  if (prt_network_lvalue(e))
    prt_network_bitfield_info(e);
}

static bool prt_network_assignment(expression e)
{
  char *selfassign = NULL;
  assignment a;

  if (!(is_assignment(e) && really_network_base((CAST(assignment, e))->arg1)))
    return FALSE;

  a = CAST(assignment, e);

  switch (e->kind)
    {
    case kind_plus_assign:   selfassign = "+"; break;
    case kind_minus_assign:  selfassign = "-"; break;
    case kind_times_assign:  selfassign = "*"; break;
    case kind_divide_assign: selfassign = "/"; break;
    case kind_lshift_assign: selfassign = "<<"; break;
    case kind_rshift_assign: selfassign = ">>"; break;
    case kind_bitand_assign: selfassign = "&"; break;
    case kind_bitor_assign:  selfassign = "|"; break;
    case kind_bitxor_assign: selfassign = "^"; break;
    default: break;
    }

  /* Weird hack: when temp1 is not set, this op= is outside
     a function, i.e., in something like a sizeof. We can just
     pretend it's a regular assignment. */
  if (selfassign && !a->temp1)
    selfassign = NULL;

  set_location(e->location);
  if (selfassign)
    {
      const char *temp = a->temp1->name;
      bool bitfield;

      output("(%s = ", temp);
      bitfield = prt_network_lvalue(a->arg1);
      output(", ");
      output_hton_expr(a->arg1);
      output("(%s", temp);
      if (bitfield)
	prt_network_bitfield_info(a->arg1);
      output(", ");
      output_ntoh_expr(a->arg1);
      output("(%s", temp);
      if (bitfield)
	prt_network_bitfield_info(a->arg1);
      output(") %s ", selfassign);
      prt_expression(a->arg2, P_TIMES);
      output("))");
    }
  else
    {
      output_hton_expr(a->arg1);
      output("(");
      prt_network_full_lvalue(a->arg1);
      output(", ");
      prt_expression(a->arg2, P_ASSIGN);
      output(")");
    }
  return TRUE;
}

static bool prt_network_increment(expression e)
{
  increment i;
  const char *temp;
  char incop;
  bool bitfield;

  if (!(is_increment(e) && really_network_base((CAST(increment, e))->arg1)))
    return FALSE;

  i = CAST(increment, e);
  temp = i->temp1->name;
  incop = i->kind == kind_preincrement || i->kind == kind_postincrement ? '+' : '-';

  /* pre-op:  (t1 = &e, HTON(t1, (t2 = NTOH(t1) +/- 1)), t2)
     post-op: (t1 = &e, HTON(t1, (t2 = NTOH(t1)) +/- 1), t2) */
  set_location(i->location);
  output("(%s = ", temp);
  bitfield = prt_network_lvalue(i->arg1);
  output(", ");
  output_hton_expr(i->arg1);
  output("(%s", temp);
  if (bitfield)
    prt_network_bitfield_info(i->arg1);
  output(", ");

  output("(%s = ", i->temp2->name);
  output_ntoh_expr(i->arg1);
  output("(%s", temp);
  if (bitfield)
    prt_network_bitfield_info(i->arg1);
  if (i->kind == kind_postincrement || i->kind == kind_postdecrement)
    output(")) %c 1)", incop);
  else
    output(") %c 1))", incop);

  output(", %s)", i->temp2->name);

  return TRUE;
}

static bool prt_network_read(expression e)
{
  if (!(really_network_base(e) &&
	(e->context & c_read) && !(e->context & c_write)))
    return FALSE;

  output_ntoh_expr(e);
  output("(");
  prt_network_full_lvalue(e);
  output(")");

  return TRUE;
}

bool prt_network_expression(expression e)
{
  return 
    prt_network_read(e) ||
    prt_network_assignment(e) ||
    prt_network_increment(e);
}

bool prt_network_typedef(data_decl d, variable_decl vd)
{
  if (vd->ddecl->kind == decl_typedef && vd->ddecl->basetype)
    {
      /* A Network base type typedef */
      type basetype = vd->ddecl->basetype;

      if (!type_size_cc(basetype) && cval_isinteger(type_size(basetype)))
	error_with_location(vd->location, "network base type `%s' is of unknown size", vd->ddecl->name);
      else
	{
	  set_location(vd->location);
	  output("typedef struct { unsigned char nxdata[%d]; } __attribute__((packed)) %s;",
		 (int)type_size_int(basetype), vd->ddecl->name);
	}
      return TRUE;
    }
  return FALSE;
}

static bool prt_network_parameter_copy(declaration parm, bool copies,
				       bool init)
{
  if (is_data_decl(parm))
    {
      data_decl dd = CAST(data_decl, parm);
      variable_decl vd = CAST(variable_decl, dd->decls);
      data_declaration ddecl = vd->ddecl;

      if (ddecl && type_network_base_type(ddecl->type) &&
	  (ddecl->use_summary & c_addressed))
	{
	  /* We need a real network type copy. */
	  if (!init)
	    {
	      if (!copies)
		{
		  outputln("{");
		  indent();
		}
	      prt_data_decl(dd);
	    }
	  else
	    {
	      output_hton(ddecl->type);
	      outputln("(%s.nxdata, %s%s);", ddecl->name, NXBASE_PREFIX, ddecl->name);
	    }

	  return TRUE;
	}
    }
  return copies;
}

/* Network base type parameters are passed as their underlying type. Thus
   we need to copy them to a real network base type variable if their
   address is taken */
bool prt_network_parameter_copies(function_decl fn)
{
  function_declarator fd = get_fdeclarator(fn->declarator);
  declaration d;
  bool copies = FALSE;

  scan_declaration (d, fd->gparms)
    copies = prt_network_parameter_copy(d, copies, FALSE);
  scan_declaration (d, fd->parms)
    copies = prt_network_parameter_copy(d, copies, FALSE);

  scan_declaration (d, fd->gparms)
    prt_network_parameter_copy(d, copies, TRUE);
  scan_declaration (d, fd->parms)
    prt_network_parameter_copy(d, copies, TRUE);

  return copies;
}

static unsigned long filler_count;

struct network_state
{
  size_t offset;
  bool isextension;
};

static void network_align_to(largest_uint offset, struct network_state *ns)
{
  if (ns->offset < offset) /* There's a gap. Fill it. */
    outputln("unsigned char __nesc_filler%lu[%llu];",
	     filler_count++, offset - ns->offset);
}

void prt_network_field_data_decl(data_decl d, struct network_state *ns)
{
  declaration fd;
  psd_options opts = 0;
  type_element interesting;

  scan_declaration (fd, d->decls)
    {
      field_decl fdd = CAST(field_decl, fd);
      field_declaration fdecl = fdd->fdecl;

      /* bitfields just show up as filler */
      if (cval_istop(fdecl->bitwidth))
	{
	  if (!cval_isinteger(fdecl->offset))
	    error_with_location(fdd->location, "unsupported network type");
	  else 
	    {
	      largest_uint offset = cval_uint_value(fdecl->offset) / BITSPERBYTE;

	      network_align_to(offset, ns);
	      if (type_size_cc(fdecl->type))
		ns->offset = offset + type_size_int(fdecl->type);
	    }
	    
	  if (ns->isextension)
	    output("__extension__ ");
	  prt_type_elements(d->modifiers, opts);
	  opts |= psd_duplicate;
	  prt_field_decl(fdd);
	  outputln(";");
	}
    }
  /* If there's an unnamed struct/union field, we need to print it and 
     account for its size in ns */
  if (!(opts & psd_duplicate))
    scan_type_element (interesting, d->modifiers)
      if (is_tag_ref(interesting))
	{
	  tag_ref tr = CAST(tag_ref, interesting);

	  prt_type_element(interesting, opts);
	  outputln(";");
	  if (cval_isinteger(tr->tdecl->size))
	    ns->offset += cval_uint_value(tr->tdecl->size);	  
	}
}

void prt_network_field_declaration(declaration d, struct network_state *ns)
{
  ns->isextension = FALSE;
  while (is_extension_decl(d))
    {
      ns->isextension = TRUE;
      d = CAST(extension_decl, d)->decl;
    }
  prt_network_field_data_decl(CAST(data_decl, d), ns);
}

void prt_network_fields(tag_ref tref)
{
  declaration d;
  struct network_state ns = { 0, FALSE };

  output(" {");
  indent();
  startline();
  scan_declaration (d, tref->fields)
    prt_network_field_declaration(d, &ns);
  if (cval_isinteger(tref->tdecl->size))
    network_align_to(cval_uint_value(tref->tdecl->size), &ns);
  unindent();
  startline();
  output("} __attribute__((packed))");
}

void init_network(void)
{
  init_network_walker();
  uchar_ptr_type = make_pointer_type(unsigned_char_type);
}
