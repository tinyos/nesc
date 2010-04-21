/* Based on keil.c */
/* Basic pointer sizes and alignments for the 8051's compiled w/ SDCC */

/* Partial model for supporting SDCC.

   Maps the address space keywords into attributes (see sdcc_token).
   Maps __at, __using, __interrupt, etc into attributes (see sdcc_token).
   Defines typedefs for bit, sbit, sfr, sfr16 and sfr32.

   Does not:
   - allow you to say (e.g.) "signed bit", or "unsigned bit"; this applies to
     bit, sbit, sfr, sfr16 and sfr32.
   - compute pointer size correctly (assumes they are all 2 bytes);
     could improve by writing attribute handling functions to compute
     the correct pointer size based on the address space attributes...
     (note the default pointer size should be 2 bytes anyway, because that
     size is used to size ptrdiff_t_type and intptr_type, and those will be
     unhappy if there's no integer type of the same size as the pointer size)
   - check any of the semantic restrictions associated with all these extensions

*/


#include "machine/sdcc-gperf.h"

static void sdcc_init(void)
{
  declare_builtin_type("bit", unsigned_char_type);
  declare_builtin_type("sbit", unsigned_char_type);
  declare_builtin_type("sfr", unsigned_char_type);
  declare_builtin_type("sfr16", unsigned_short_type);
  declare_builtin_type("sfr32", unsigned_long_type);
  declare_builtin_type("__bit", unsigned_char_type);
  declare_builtin_type("__sbit", unsigned_char_type);
  declare_builtin_type("__sfr", unsigned_char_type);
  declare_builtin_type("__sfr16", unsigned_short_type);
  declare_builtin_type("__sfr32", unsigned_long_type);
}

static int sdcc_token(const char *token, int len, struct yystype *lvalp)
{
  struct sdccword *keyword = is_sdcc_word(token, len);

  if (keyword)
    {
      lvalp->idtoken.location = last_location();
      lvalp->idtoken.id = make_cstring(parse_region, token, len);
      lvalp->idtoken.decl = NULL;
      return keyword->token;
    }

  return IDENTIFIER;
}

static machine_spec sdcc_machine = {
  "sdcc51", NULL,
  FALSE,			/* big_endian */
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
  { 8, 1 },			/* long long */
  1, 1, 1, 1,			/* int1/2/4/8 align */
  2, 2,				/* wchar_t, size_t size */
  TRUE, TRUE,			/* char, wchar_t signed */
  NULL,				/* no attribute for async functions */

  NULL,				/* adjust_field_align function */
  NULL, NULL, NULL, NULL, 	/* attribute handling functions */
  NULL, sdcc_init,
  sdcc_token,
  NULL,				/* Keil C special */
  gcc_global_cpp_init,		/* global cpp support: this should be tailored to sdcc
				   to get correct behaviour */
  NULL				/* per-file cpp support */
};
