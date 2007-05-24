/* Thanks to David Patnode (dpatnode@bradley.edu) for this file. */

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
    return keyword->token;

  return IDENTIFIER;
}

declaration keil_special(location loc, cstring keyword, cstring name,
			 expression address)
{
  /* I just love this kind of code. */
  region r = parse_region;
  /* Build __attribute__((keil_address(address))) */
  word aword = new_word(r, loc, str2cstring(r, "keil_address"));
  gcc_attribute address_attr = new_gcc_attribute(r, loc, aword, address);

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
  "keil", NULL,
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

  NULL,				/* adjust_field_align function */
  NULL, NULL, NULL, NULL, 	/* attribute handling functions */
  keil_init,
  keil_token,
  keil_special			/* Keil C special */
};
