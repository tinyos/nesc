/* Copyright (c) 2006 Intel Corporation
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached INTEL-LICENSE     
 * file. If you do not find these files, copies can be found by writing to
 * Intel Research Berkeley, 2150 Shattuck Avenue, Suite 1300, Berkeley, CA, 
 * 94704.  Attention:  Intel License Inquiry.
 */
/* Authors:  David Gay  <dgay@intel-research.net>
 *           Intel Research Berkeley Lab
 */

#ifdef __MSP430
#include <sys/inttypes.h>
#else
#include <inttypes.h>
#endif

/* Network to host and host to network conversions.
   Normal network representation is 2's complement big-endian, but there
   are also little-endian types available.
   Bit-fields can only be created for the big-endian network types.
*/

#define __NX_DECLARE_FUNCTIONS(bits) \
 \
inline int ## bits ## _t __nesc_ntoh_int ## bits(const void *source) { \
  return __nesc_ntoh_uint ## bits(source); \
} \
 \
inline int ## bits ## _t __nesc_hton_int ## bits(void *target, int ## bits ## _t value) { \
  __nesc_hton_uint ## bits(target, value); \
  return value; \
} \
 \
inline int ## bits ## _t __nesc_ntoh_leint ## bits(const void *source) { \
  return __nesc_ntoh_leuint ## bits(source); \
} \
 \
inline int ## bits ## _t __nesc_hton_leint ## bits(void *target, int ## bits ## _t value) { \
  __nesc_hton_leuint ## bits(target, value); \
  return value; \
}

#define __NX_DECLARE_BF_FUNCTIONS(bits) \
 \
uint ## bits ## _t decode ## bits(const uint8_t *msg, unsigned offset, uint8_t length) \
{ \
  uint ## bits ## _t x = 0; \
  unsigned byte_offset = offset >> 3; \
  unsigned bit_offset = offset & 7; \
 \
  /* All in one byte case */ \
  if (length + bit_offset <= 8) \
    return (msg[byte_offset] >> (8 - bit_offset - length)) & \
      ((1 << length) - 1); \
 \
  /* get some high order bits */ \
  if (bit_offset > 0) \
    { \
      length -= 8 - bit_offset; \
      x = (uint ## bits ## _t)(msg[byte_offset] & ((1 << (8 - bit_offset)) - 1)) << length; \
      byte_offset++; \
    } \
 \
  while (length >= 8) \
    { \
      length -= 8; \
      x |= (uint ## bits ## _t)msg[byte_offset++] << length; \
    } \
 \
  /* data from last byte */ \
  if (length > 0) \
    x |= msg[byte_offset] >> (8 - length); \
 \
  return x; \
} \
 \
void encode ## bits(uint8_t *msg, unsigned offset, uint8_t length, uint ## bits ## _t x) \
{ \
  unsigned byte_offset = offset >> 3; \
  unsigned bit_offset = offset & 7; \
 \
  /* all in one byte case */ \
  if (length + bit_offset <= 8) { \
    unsigned mask = ((1 << length) - 1) << (8 - bit_offset - length); \
 \
    msg[byte_offset] =  \
      ((msg[byte_offset] & ~mask) | x << (8 - bit_offset - length)); \
    return; \
  } \
 \
  /* set some high order bits */ \
  if (bit_offset > 0) \
    { \
      unsigned mask = (1 << (8 - bit_offset)) - 1; \
 \
      length -= 8 - bit_offset; \
      msg[byte_offset] = ((msg[byte_offset] & ~mask) | x >> length); \
      byte_offset++; \
    } \
 \
  while (length >= 8) \
    { \
      length -= 8; \
      msg[byte_offset++] = x >> length; \
    } \
 \
  /* data for last byte */ \
  if (length > 0) \
    { \
      unsigned mask = (1 << (8 - length)) - 1; \
 \
      msg[byte_offset] = ((msg[byte_offset] & mask) | x << (8 - length)); \
    } \
} \
 \
inline uint ## bits ## _t __nesc_ntohbf_uint ## bits(const void *source, unsigned offset, uint8_t length) { \
  return decode ## bits(source, offset, length); \
} \
 \
inline int ## bits ## _t __nesc_ntohbf_int ## bits(const void *source, unsigned offset, uint8_t length) { \
  return decode ## bits(source, offset, length); \
} \
 \
inline uint ## bits ## _t __nesc_htonbf_uint ## bits(void *target, unsigned offset, uint8_t length, uint ## bits ## _t value) { \
  encode ## bits(target, offset, length, value); \
  return value; \
} \
 \
inline int ## bits ## _t __nesc_htonbf_int ## bits(void *target, unsigned offset, uint8_t length, int ## bits ## _t value) { \
  encode ## bits(target, offset, length, value); \
  return value; \
}


/* 8-bits */
/* ------ */

inline uint8_t __nesc_ntoh_uint8(const void *source) {
  const uint8_t *base = source;
  return base[0];
}

inline uint8_t __nesc_hton_uint8(void *target, uint8_t value) {
  uint8_t *base = target;
  base[0] = value;
  return value;
}

inline uint8_t __nesc_ntoh_leuint8(const void *source) {
  const uint8_t *base = source;
  return base[0];
}

inline uint8_t __nesc_hton_leuint8(void *target, uint8_t value) {
  uint8_t *base = target;
  base[0] = value;
  return value;
}

__NX_DECLARE_FUNCTIONS(8)
__NX_DECLARE_BF_FUNCTIONS(8)


/* 16-bits */
/* ------- */

inline uint16_t __nesc_ntoh_uint16(const void *source) {
  const uint8_t *base = source;
  return (uint16_t)base[0] << 8 | base[1];
}

inline uint16_t __nesc_hton_uint16(void *target, uint16_t value) {
  uint8_t *base = target;
  base[1] = value;
  base[0] = value >> 8;
  return value;
}

inline uint16_t __nesc_ntoh_leuint16(const void *source) {
  const uint8_t *base = source;
  return (uint16_t)base[1] << 8 | base[0];
}

inline uint16_t __nesc_hton_leuint16(void *target, uint16_t value) {
  uint8_t *base = target;
  base[0] = value;
  base[1] = value >> 8;
  return value;
}

__NX_DECLARE_FUNCTIONS(16)
__NX_DECLARE_BF_FUNCTIONS(16)


/* 32-bits */
/* ------- */
inline uint32_t __nesc_ntoh_uint32(const void *source) {
  const uint8_t *base = source;
  return (uint32_t)base[0] << 24 |
         (uint32_t)base[1] << 16 |
         (uint32_t)base[2] << 8 | base[3];
}

inline uint32_t __nesc_hton_uint32(void *target, uint32_t value) {
  uint8_t *base = target;
  base[3] = value;
  base[2] = value >> 8;
  base[1] = value >> 16;
  base[0] = value >> 24;
  return value;
}

inline uint32_t __nesc_ntoh_leuint32(const void *source) {
  const uint8_t *base = source;
  return (uint32_t)base[3] << 24 |
         (uint32_t)base[2] << 16 |
         (uint32_t)base[1] << 8 | base[0];
}

inline uint32_t __nesc_hton_leuint32(void *target, uint32_t value) {
  uint8_t *base = target;
  base[0] = value;
  base[1] = value >> 8;
  base[2] = value >> 16;
  base[3] = value >> 24;
  return value;
}

__NX_DECLARE_FUNCTIONS(32)
__NX_DECLARE_BF_FUNCTIONS(32)


/* 64-bits */
/* ------- */
inline uint64_t __nesc_ntoh_uint64(const void *source) {
  const uint8_t *base = source;
  return (uint64_t)base[0] << 56 |
         (uint64_t)base[1] << 48 |
         (uint64_t)base[2] << 40 |
         (uint64_t)base[3] << 32 |
         (uint64_t)base[4] << 24 |
         (uint64_t)base[5] << 16 |
         (uint64_t)base[6] << 8  | base[7];
}

inline uint64_t __nesc_hton_uint64(void *target, uint64_t value) {
  uint8_t *base = target;
  base[7] = value;
  base[6] = value >> 8;
  base[5] = value >> 16;
  base[4] = value >> 24;
  base[3] = value >> 32;
  base[2] = value >> 40;
  base[1] = value >> 48;
  base[0] = value >> 56;
  return value;
}

inline uint64_t __nesc_ntoh_leuint64(const void *source) {
  const uint8_t *base = source;
  return (uint64_t)base[7] << 56 |
         (uint64_t)base[6] << 48 |
         (uint64_t)base[5] << 40 |
         (uint64_t)base[4] << 32 |
         (uint64_t)base[3] << 24 |
         (uint64_t)base[2] << 16 |
         (uint64_t)base[1] << 8  | base[0];
}

inline uint64_t __nesc_hton_leuint64(void *target, uint64_t value) {
  uint8_t *base = target;
  base[0] = value;
  base[1] = value >> 8;
  base[2] = value >> 16;
  base[3] = value >> 24;
  base[4] = value >> 32;
  base[5] = value >> 40;
  base[6] = value >> 48;
  base[7] = value >> 56;
  return value;
}

__NX_DECLARE_FUNCTIONS(64)
__NX_DECLARE_BF_FUNCTIONS(64)


/* Standard external types: big-endian */
typedef int8_t nx_int8_t __attribute__((nx_base_bf(int8)));
typedef int16_t nx_int16_t __attribute__((nx_base_bf(int16)));
typedef int32_t nx_int32_t __attribute__((nx_base_bf(int32)));
typedef int64_t nx_int64_t __attribute__((nx_base_bf(int64)));
typedef uint8_t nx_uint8_t __attribute__((nx_base_bf(uint8)));
typedef uint16_t nx_uint16_t __attribute__((nx_base_bf(uint16)));
typedef uint32_t nx_uint32_t __attribute__((nx_base_bf(uint32)));
typedef uint64_t nx_uint64_t __attribute__((nx_base_bf(uint64)));

/* Little endian external types, for those apps that need them.
   Note that bitfields declared using these types are still big-endian. */
typedef int8_t nxle_int8_t __attribute__((nx_base(leint8)));
typedef int16_t nxle_int16_t __attribute__((nx_base(leint16)));
typedef int32_t nxle_int32_t __attribute__((nx_base(leint32)));
typedef int64_t nxle_int64_t __attribute__((nx_base(leint64)));
typedef uint8_t nxle_uint8_t __attribute__((nx_base(leuint8)));
typedef uint16_t nxle_uint16_t __attribute__((nx_base(leuint16)));
typedef uint32_t nxle_uint32_t __attribute__((nx_base(leuint32)));
typedef uint64_t nxle_uint64_t __attribute__((nx_base(leuint64)));

#undef __NX_DECLARE_FUNCTIONS
#undef __NX_DECLARE_BF_FUNCTIONS
