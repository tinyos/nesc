#include <inttypes.h>

/* Basic pointer sizes and alignments for this machine */

/* GCC is broken (could be a broken design issue ;-)), so need the
   typedefs (rather than using the types directly in the calls
   to alignof) */
typedef int __attribute__ ((mode(__word__))) myword;
typedef int __attribute__ ((mode(__byte__))) myint1;
typedef int __attribute__ ((mode(__HI__))) myint2;
typedef int __attribute__ ((mode(__SI__))) myint4;
typedef int __attribute__ ((mode(__DI__))) myint8;

/* Find out how big the smallest struct is. Assume this tells us to
   what multiples structs are rounded. */
struct self_smallest {
  char x;
};

/* Find out to what multiple 0-bit bitfields round thingg */
struct self_efb {
  char x : 1;
  char : 0;
};

/* Detect if bitfield type influences the struct */
struct self_pcc1 {
  int x : 1;
};
struct self_pcc2 {
  char x : 1;
};

static void self_preinit(void);

#ifdef __i386__
#define SELF_ADJUST_FIELD_ALIGN self_adjust_field_align
#define SELF_HANDLE_OPTION self_handle_option

static bool align_double;

static cval self_adjust_field_align(field_declaration fdecl, cval alignment)
{
  if (!align_double && type_arithmetic(type_array_of_base(fdecl->type)))
    alignment = cval_min(make_cval_unsigned(32, size_t_type), alignment);
  return alignment;
}

static void self_handle_option(const char *arg)
{
  if (!strcmp(arg, "-malign-double"))
    align_double = TRUE;
  else if (!strcmp(arg, "-mnoalign-double"))
    align_double = FALSE;
  gcc_save_machine_options(arg);
}

#else
#define SELF_ADJUST_FIELD_ALIGN NULL
#define SELF_HANDLE_OPTION gcc_save_machine_options

#endif

static machine_spec self_machine = {
  "pc", SELF_HANDLE_OPTION,

  FALSE,			/* big_endian, set in preinit */

  /* pcc_bitfield_type_matters */
  sizeof(struct self_pcc1) != sizeof(struct self_pcc2),	

  /* empty field boundary */
  sizeof(struct self_efb) * BITSPERBYTE, 

  /* structure size boundary */
  sizeof(struct self_smallest) * BITSPERBYTE, 

  sizeof(myword),				     /* word size */
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
  NULL,				/* no attribute for async functions */

  SELF_ADJUST_FIELD_ALIGN,			     /* adjust_field_align */

  NULL, NULL, NULL, NULL,	/* No special attributes */
  self_preinit, NULL,		/* init */
  NULL,				/* token */
  NULL,				/* keil special */
  gcc_global_cpp_init,		/* global cpp support */
  NULL				/* per-file cpp support */
};

static void self_preinit(void)
{
  union {
    uint8_t a;
    uint16_t b;
  } endian;

  endian.b = 1;
  self_machine.big_endian = endian.a != 1;
}
