/* Thanks to David Patnode (dpatnode@bradley.edu) for this file. */
/* Basic pointer sizes and alignments for the 8051's compiled w/ Keil C51 */
static machine_spec keil_machine = {
  "keil", NULL,
  FALSE,			/* pcc_bitfield_type_matters */
  8,				/* empty field boundary - in bits */
  8,				/* structure size boundary - in bits */
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
  TRUE, TRUE			/* char, wchar_t signed */
};
