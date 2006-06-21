static bool avr_decl_attribute(gcc_attribute attr, data_declaration ddecl)
{
  const char *name = attr->word1->cstring.data;

  if (!strcmp(name, "signal"))
    {
      ddecl->async = TRUE;
      ddecl->spontaneous = c_call_atomic;
      return TRUE;
    }
  else if (!strcmp(name, "interrupt"))
    {
      ddecl->async = TRUE;
      ddecl->spontaneous = c_call_nonatomic;
      return TRUE;
    }
  return FALSE;
}

/* Basic pointer sizes and alignments for the AVR */
static machine_spec avr_machine = {
  "avr", NULL,
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

  NULL,				/* adjust_field_align */

  avr_decl_attribute,		/* Attribute handling: declarations */
  NULL, NULL, NULL		/* Attribute handling: tag, field, type */
};

