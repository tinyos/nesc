/* Partial model for supporting Keil C (original version based on work 
   by David Patnode (dpatnode@bradley.edu) for this file). 

   Maps the address space keywords into attributes (see keil_token).
   Maps using, interrupt, etc into attributes (see keil_token).

   Defines typedefs for bit. The magic 
     sfr/sfr16/sbit name = address
   Keil extension is rewritten to 
     sfr __attribute((keil_address(address))) name
   using predefined typedefs for sbit, sfr and sfr16. This rewrite depends
   on the special Keil hack in the parser (look for TARGET_DEF in c-parse.y).

   Does not:
   - compute pointer size correctly (assumes they are all 2 bytes);
     could improve by writing attribute handling functions to compute
     the correct pointer size based on the address space attributes...
     (note the default pointer size should be 2 bytes anyway, because that
     size is used to size ptrdiff_t_type and intptr_type, and those will be
     unhappy if there's no integer type of the same size as the pointer size)
   - check any of the semantic restrictions associated with all these extensions

*/

#include "machine/keil-gperf.h"

static data_declaration keil_sbit_ddecl, keil_sfr_ddecl, keil_sfr16_ddecl;

static void keil_init(void)
{
  declare_builtin_type("bit", unsigned_char_type);

  keil_sbit_ddecl = declare_builtin_type("sbit", unsigned_char_type);
  keil_sfr_ddecl = declare_builtin_type("sfr", unsigned_char_type);
  keil_sfr16_ddecl = declare_builtin_type("sfr16", unsigned_short_type);
}

static int keil_token(const char *token, int len, struct yystype *lvalp)
{
  struct keilword *keyword = is_keil_word(token, len);

  if (keyword)
    {
      lvalp->idtoken.location = last_location();
      lvalp->idtoken.id = make_cstring(parse_region, token, len);
      lvalp->idtoken.decl = NULL;
      return keyword->token;
    }

  return IDENTIFIER;
}

declaration keil_special(location loc, cstring keyword, cstring name,
			 expression address)
{
  /* I just love this kind of code. */
  region r = parse_region;
  /* Build __attribute__((keil_address(address))) */
  word aword = new_word(r, loc, str2cstring(r, "keil_address"));
  target_attribute address_attr = new_target_attribute(r, loc, aword, address);

  /* Build a declaration for name */
  declarator d = make_identifier_declarator(loc, name);
  data_declaration type_ddecl;
  type_element elems;
  declaration vd;
  data_decl dd;

  /* Pick appropriate fake typedef for name based on keyword */
  if (!strcmp(keyword.data, "sbit"))
    type_ddecl = keil_sbit_ddecl;
  else if (!strcmp(keyword.data, "sfr"))
    type_ddecl = keil_sfr_ddecl;
  else
    type_ddecl = keil_sfr16_ddecl;

  elems = CAST(type_element, new_typename(r, loc, type_ddecl));
  vd = start_decl(d, NULL, elems, FALSE, CAST(attribute, address_attr));
  finish_decl(vd, NULL);
  dd = new_data_decl(r, loc, elems, vd);

  return CAST(declaration, dd);
}

/* Basic pointer sizes and alignments for the 8051's compiled w/ Keil C51 */
static machine_spec keil_machine = {
  "keil51", NULL,
  TRUE,				/* big_endian */
  FALSE,			/* pcc_bitfield_type_matters */
  8,				/* empty field boundary - in bits */
  8,				/* structure size boundary - in bits */
  1,				/* word size */
  { 2, 1 },			/* pointer type */
  { 4, 1 },			/* float */
  { 4, 1 },			/* double */
  { 4, 1 },			/* long double */
  { 2, 1 },			/* short */
  { 2, 1 },			/* int */
  { 4, 1 },			/* long */
  { 8, 1 },			/* long long (unsupported in avr-gcc) */
  1, 1, 1, 1,			/* int1/2/4/8 align */
  2, 2,				/* wchar_t, size_t size */
  TRUE, TRUE,			/* char, wchar_t signed */
  "reentrant",			/* attribute for async functions */

  NULL,				/* adjust_field_align function */
  NULL, NULL, NULL, NULL, 	/* attribute handling functions */
  NULL, keil_init,
  keil_token,
  keil_special,			/* Keil C special */
  gcc_global_cpp_init,		/* global cpp support: this should be tailored to keil
				   to get correct behaviour */
  NULL				/* per-file cpp support */
};
