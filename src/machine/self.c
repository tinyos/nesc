/* Basic pointer sizes and alignments for this machine */

/* GCC is broken (could be a broken design issue ;-)), so need the
   typedefs (rather than using the types directly in the calls
   to alignof) */
typedef int __attribute__ ((mode(__byte__))) myint1;
typedef int __attribute__ ((mode(__HI__))) myint2;
typedef int __attribute__ ((mode(__SI__))) myint4;
typedef int __attribute__ ((mode(__DI__))) myint8;

static machine_spec self_machine = {
  "pc",
  TRUE				/* PCC_BITFIELD_TYPE_MATTERS */,
				/* (should come from configure) */
  { sizeof(void *),      __alignof__(void *) },	     /* pointer type */
  { sizeof(float),       __alignof__(float) },	     /* float */
  { sizeof(double),      __alignof__(double) },	     /* double */
  { sizeof(long double), __alignof__(long double) }, /* long double */
  { sizeof(short),       __alignof__(short) },	     /* short */
  { sizeof(int),         __alignof__(int) },	     /* int */
  { sizeof(long),        __alignof__(long) },	     /* long */
  { sizeof(long long),   __alignof__(long long) },   /* long long */
  __alignof__(myint1), __alignof__(myint2),
  __alignof__(myint4), __alignof__(myint8),	     /* int1/2/4/8 align */
  sizeof(wchar_t), sizeof(size_t),		     /* wchar_t, size_t size */
  (char)-1 < 0, (wchar_t)-1 < 0,		     /* char, wchar_t signed */

  NULL, NULL, NULL, NULL	/* No special attributes */
};
