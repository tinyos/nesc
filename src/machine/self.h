#ifndef MACHINE_H
#define MACHINE_H

/* The size of pointers */
#define MACHINE_PTR_SIZE sizeof(void *)
#define MACHINE_PTR_ALIGN __alignof__(void *)

#define MACHINE_FLOAT_SIZE sizeof(float)
#define MACHINE_FLOAT_ALIGN __alignof__(float)

#define MACHINE_DOUBLE_SIZE sizeof(double)
#define MACHINE_DOUBLE_ALIGN __alignof__(double)

#define MACHINE_LONG_DOUBLE_SIZE sizeof(long double)
#define MACHINE_LONG_DOUBLE_ALIGN __alignof__(long double)

/* GCC is broken (could be a broken design issue ;-)), so need the
   typedefs (rather than using the types directly in the calls
   to alignof) */
typedef int __attribute__ ((mode(__byte__))) myint1;
typedef int __attribute__ ((mode(__HI__))) myint2;
typedef int __attribute__ ((mode(__SI__))) myint4;
typedef int __attribute__ ((mode(__DI__))) myint8;

#define MACHINE_INT1_ALIGN __alignof__(myint1)
#define MACHINE_INT2_ALIGN __alignof__(myint2)
#define MACHINE_INT4_ALIGN __alignof__(myint4)
#define MACHINE_INT8_ALIGN __alignof__(myint8)

#define MACHINE_SHORT_SIZE sizeof(short)
#define MACHINE_SHORT_ALIGN __alignof__(short)

#define MACHINE_INT_SIZE sizeof(int)
#define MACHINE_INT_ALIGN __alignof__(int)

#define MACHINE_LONG_SIZE sizeof(long)
#define MACHINE_LONG_ALIGN __alignof__(long)

#define MACHINE_LONG_LONG_SIZE sizeof(long long)
#define MACHINE_LONG_LONG_ALIGN __alignof__(long long)

#define MACHINE_CHAR_SIGNED ((char)-1 < 0)

#define MACHINE_WCHAR_T_SIZE sizeof(wchar_t)
#define MACHINE_WCHAR_T_SIGNED ((wchar_t)-1 < 0)

#define MACHINE_SIZE_T_SIZE sizeof(size_t)

#endif
