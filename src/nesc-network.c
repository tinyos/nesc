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

static void validate_network_lvalue(expression e)
{
  conditional cond;

  if ((cond = conditional_lvalue(e)) &&
      (type_network_base_type(cond->arg1->type) ||
       type_network_base_type(cond->arg2->type)))
    error_with_location(e->location,
			"Conditional assignment not yet supported for network types");
}

static data_declaration add_network_temporary(function_decl fn, type t)
{
  /* See Weird hack comment in prt_network_assignment */
  if (fn)
    return add_temporary(parse_region, CAST(compound_stmt, fn->stmt), t);
  else
    return NULL;
}

static AST_walker_result network_assignment(AST_walker spec, void *data,
					    assignment a)
{
  function_decl fn = data;

  validate_network_lvalue(a->arg1);
  if (type_network_base_type(a->type))
    {
      /* op= needs a temp */
      if (a->kind != kind_assign)
	a->temp1 = add_network_temporary(fn, make_pointer_type(a->type));
    }

  return aw_walk;
}

static AST_walker_result network_increment(AST_walker spec, void *data,
					   increment i)
{
  function_decl fn = data;

  validate_network_lvalue(i->arg1);

  if (type_network_base_type(i->type))
    {
      /* pre-ops need 1 temp, post-ops need 2 */
      i->temp1 = add_network_temporary(fn, make_pointer_type(i->type));
      if (i->kind == kind_postincrement || i->kind == kind_postdecrement)
	i->temp2 = add_network_temporary(fn, type_network_platform_type(i->type));
    }

  return aw_walk;
}

static AST_walker_result network_fdecl(AST_walker spec, void *data,
						function_decl fd)
{
  AST_walk_children(spec, fd, CAST(node, fd));
  return aw_done;
}

/* An AST walker that does network type processing on the AST */
static AST_walker network_walker;

static void init_network_walker(void)
{
  network_walker = new_AST_walker(parse_region);
  AST_walker_handle(network_walker, kind_increment, network_increment);
  AST_walker_handle(network_walker, kind_assignment, network_assignment);
  AST_walker_handle(network_walker, kind_function_decl, network_fdecl);
}

void handle_network_types(declaration decls)
{
  node n = CAST(node, decls);

  AST_walk_list(network_walker, NULL, &n);
}

static void output_cvt(char *fmt, type t)
{
  output(fmt, type_unsigned(t) ? "U" : "",
	 (int)type_size_int(t) * BITSPERBYTE);
}

void output_hton(type t)
{
  output_cvt("%sHTON%d", t);
}

void output_ntoh(type t)
{
  output_cvt("NTO%sH%d", t);
}

static void prt_platform_type(type t)
{
  type platformt = type_network_platform_type(t);

  output("typedef %s __nesc_nw_%sint%d_t;\n",
	 type_name(unparse_region, platformt),
	 type_unsigned(t) ? "u" : "",
	 (int)type_size_int(t) * BITSPERBYTE);
}

void prt_network_routines(void)
{
  prt_platform_type(nint1_type); prt_platform_type(nuint1_type);
  prt_platform_type(nint2_type); prt_platform_type(nuint2_type);
  prt_platform_type(nint4_type); prt_platform_type(nuint4_type);
  prt_platform_type(nint8_type); prt_platform_type(nuint8_type);

  output(
"/* Start internal network declarations*/\n"
"#define nw_struct struct\n"
"#define nw_union union\n"
"\n"
"/* Base types. All this code assumes char's are 8-bits */\n"
"typedef struct nw_int8_t  { unsigned char data[1]; } __attribute__((packed)) nw_int8_t;\n"
"typedef struct nw_int16_t { unsigned char data[2]; } __attribute__((packed)) nw_int16_t;\n"
"typedef struct nw_int32_t { unsigned char data[4]; } __attribute__((packed)) nw_int32_t;\n"
"typedef struct nw_int64_t { unsigned char data[8]; } __attribute__((packed)) nw_int64_t;\n"
"typedef struct nw_uint8_t  { unsigned char data[1]; } __attribute__((packed)) nw_uint8_t;\n"
"typedef struct nw_uint16_t { unsigned char data[2]; } __attribute__((packed)) nw_uint16_t;\n"
"typedef struct nw_uint32_t { unsigned char data[4]; } __attribute__((packed)) nw_uint32_t;\n"
"typedef struct nw_uint64_t { unsigned char data[8]; } __attribute__((packed)) nw_uint64_t;\n"
"\n"
"/* Network to host and host to network conversions.\n"
"   Network representation is 2's complement little-endian.\n"
"*/\n"
"static inline __nesc_nw_uint8_t NTOUH8(void *source) {\n"
"  unsigned char *base = source;\n"
"  return (unsigned char)base[0];\n"
"}\n"
"static inline __nesc_nw_uint16_t NTOUH16(void *source) {\n"
"  unsigned char *base = source;\n"
"  return base[1] << 8 | base[0];\n"
"}\n"
"static inline __nesc_nw_uint32_t NTOUH32(void *source) {\n"
"  unsigned char *base = source;\n"
"  return (__nesc_nw_uint32_t)base[3] << 24 |\n"
"         (__nesc_nw_uint32_t)base[2] << 16 |\n"
"         base[1] << 8 | base[0];\n"
"}\n"
"static inline __nesc_nw_uint64_t NTOUH64(void *source) {\n"
"  unsigned char *base = source;\n"
"  return (__nesc_nw_uint64_t)base[7] << 56 |\n"
"         (__nesc_nw_uint64_t)base[6] << 48 |\n"
"         (__nesc_nw_uint64_t)base[5] << 40 |\n"
"         (__nesc_nw_uint64_t)base[4] << 32 |\n"
"         (__nesc_nw_uint64_t)base[3] << 24 |\n"
"         (__nesc_nw_uint64_t)base[2] << 16 |\n"
"         base[1] << 8  | base[0];\n"
"}\n"
"static inline __nesc_nw_int8_t NTOH8(void *source) {\n"
"  return NTOUH8(source);\n"
"}\n"
"static inline __nesc_nw_int16_t NTOH16(void *source) {\n"
"  return NTOUH16(source);\n"
"}\n"
"static inline __nesc_nw_int32_t NTOH32(void *source) {\n"
"  return NTOUH32(source);\n"
"}\n"
"static inline __nesc_nw_int64_t NTOH64(void *source) {\n"
"  return NTOUH64(source);\n"
"}\n"
"\n"
"/* Host to network order assignment */\n"
"static inline __nesc_nw_uint8_t UHTON8(void *target, __nesc_nw_uint8_t value) {\n"
"  unsigned char *base = target;\n"
"  base[0] = value;\n"
"  return value;\n"
"}\n"
"static inline __nesc_nw_uint16_t UHTON16(void *target, __nesc_nw_uint16_t value) {\n"
"  unsigned char *base = target;\n"
"  base[0] = value;\n"
"  base[1] = value >> 8;\n"
"  return value;\n"
"}\n"
"static inline __nesc_nw_uint32_t UHTON32(void *target, __nesc_nw_uint32_t value) {\n"
"  unsigned char *base = target;\n"
"  base[0] = value;\n"
"  base[1] = value >> 8;\n"
"  base[2] = value >> 16;\n"
"  base[3] = value >> 24;\n"
"  return value;\n"
"}\n"
"static inline __nesc_nw_uint64_t UHTON64(void *target, __nesc_nw_uint64_t value) {\n"
"  unsigned char *base = target;\n"
"  base[0] = value;\n"
"  base[1] = value >> 8;\n"
"  base[2] = value >> 16;\n"
"  base[3] = value >> 24;\n"
"  base[4] = value >> 32;\n"
"  base[5] = value >> 40;\n"
"  base[6] = value >> 48;\n"
"  base[7] = value >> 56;\n"
"  return value;\n"
"}\n"
"static inline __nesc_nw_int8_t HTON8(void *target, __nesc_nw_int8_t value) {\n"
"  unsigned char *base = target;\n"
"  base[0] = value;\n"
"  return value;\n"
"}\n"
"static inline __nesc_nw_int16_t HTON16(void *target, __nesc_nw_int16_t value) {\n"
"  unsigned char *base = target;\n"
"  base[0] = value;\n"
"  base[1] = value >> 8;\n"
"  return value;\n"
"}\n"
"static inline __nesc_nw_int32_t HTON32(void *target, __nesc_nw_int32_t value) {\n"
"  unsigned char *base = target;\n"
"  base[0] = value;\n"
"  base[1] = value >> 8;\n"
"  base[2] = value >> 16;\n"
"  base[3] = value >> 24;\n"
"  return value;\n"
"}\n"
"static inline __nesc_nw_int64_t HTON64(void *target, __nesc_nw_int64_t value) {\n"
"  unsigned char *base = target;\n"
"  base[0] = value;\n"
"  base[1] = value >> 8;\n"
"  base[2] = value >> 16;\n"
"  base[3] = value >> 24;\n"
"  base[4] = value >> 32;\n"
"  base[5] = value >> 40;\n"
"  base[6] = value >> 48;\n"
"  base[7] = value >> 56;\n"
"  return value;\n"
"}\n"
"\n");
}

static bool prt_network_assignment(expression e)
{
  char *selfassign = NULL;
  assignment a;

  if (!(is_assignment(e) && type_network_base_type(e->type)))
    return FALSE;

  a = CAST(assignment, e);

  switch (e->kind)
    {
    case kind_plus_assign:   selfassign = "+"; break;
    case kind_minus_assign:  selfassign = "+"; break;
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

      output("(%s = &", temp);
      prt_expression(a->arg1, P_CAST);
      output(", ");
      output_hton(a->type);
      output("(%s, ", temp);
      output_ntoh(e->type);
      output("(%s) %s ", temp, selfassign);
      prt_expression(a->arg2, P_TIMES);
      output("))");
    }
  else
    {
      output_hton(e->type);
      output("(&");
      prt_expression(a->arg1, P_CAST);
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

  if (!(is_increment(e) && type_network_base_type(e->type)))
    return FALSE;

  i = CAST(increment, e);
  temp = i->temp1->name;
  incop = i->kind == kind_preincrement || i->kind == kind_postincrement ? '+' : '-';

  /* pre-op: (t1 = &e, HTON(t1, NTOH(t1) +/- 1))
     post-op: (t1 = &e, HTON(t1, (t2 = NTOH(t1)) +/- 1), t2) */
  set_location(i->location);
  output("(%s = &", temp);
  prt_expression(i->arg1, P_CAST);
  output(", ");
  output_hton(i->type);
  output("(%s, ", temp);
  if (i->temp2)
    {
      const char *val = i->temp2->name;

      output("(%s = ", val);
      output_ntoh(i->type);
      output("(%s)) %c 1), %s)", temp, incop, val);
    }
  else
    {
      output_ntoh(i->type);
      output("(%s) %c 1))", temp, incop);
    }
  return TRUE;
}

static bool prt_network_read(expression e)
{
  if (!(e->type && type_network_base_type(e->type) &&
	(e->context & c_read) && !(e->context & c_write)))
    return FALSE;

  output_ntoh(e->type);
  output("(&");
  prt_expression_helper(e, P_ASSIGN);
  output(")");

  return TRUE;
}

bool prt_network_expression(expression e)
{
  return prt_network_read(e) ||
    prt_network_assignment(e) ||
    prt_network_increment(e);
}

void init_network(void)
{
  init_network_walker();
}
