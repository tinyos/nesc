static bool msp430_decl_attribute(gcc_attribute attr, data_declaration ddecl)
{
  const char *name = attr->word1->cstring.data;

  /* Different from the AVR! */
  if (!strcmp(name, "signal"))
    {
      ddecl->spontaneous = c_call_nonatomic;
      return TRUE;
    }
  else if (!strcmp(name, "interrupt"))
    {
      ddecl->async = TRUE;
      /* The signal attribute may have come first */
      if (ddecl->spontaneous != c_call_nonatomic)
	ddecl->spontaneous = c_call_atomic;
      return TRUE;
    }
  return FALSE;
}

/* Basic pointer sizes and alignments for the TI MSP430 */
static machine_spec msp430_machine = {
  "msp430", 
  gcc_save_machine_options,
  FALSE,			/* big_endian */
  FALSE,			/* pcc_bitfield_type_matters */
  16,				/* empty field boundary - in bits */
  8,				/* structure size boundary - in bits */
  2,				/* word size */
  { 2, 2 },			/* pointer type */
  { 4, 2 },			/* float */
  { 4, 2 },			/* double */
  { 4, 2 },			/* long double */
  { 2, 2 },			/* short */
  { 2, 2 },			/* int */
  { 4, 2 },			/* long */
  { 8, 2 },			/* long long */
  1, 2, 2, 2,			/* int1/2/4/8 align */
  2, 2,				/* wchar_t, size_t size */
  TRUE, TRUE,			/* char, wchar_t signed */
  NULL,				/* no attribute for async functions */

  NULL,				/* adjust_field_align */

  msp430_decl_attribute,	/* Attribute handling: declarations */
  NULL, NULL, NULL,		/* Attribute handling: tag, field, type */
  NULL, NULL,			/* preint, init */
  NULL,				/* token */
  NULL,				/* keil special */
  gcc_global_cpp_init,		/* global cpp support */
  NULL				/* per-file cpp support */
};
