/* 
  This file is provided under a dual BSD/GPLv2 license.  When using or 
  redistributing this file, you may do so under either license.

  GPL LICENSE SUMMARY

  Copyright(c) 2006 Intel Corporation. All rights reserved.

  This program is free software; you can redistribute it and/or modify 
  it under the terms of version 2 of the GNU General Public License as
  published by the Free Software Foundation.

  This program is distributed in the hope that it will be useful, but 
  WITHOUT ANY WARRANTY; without even the implied warranty of 
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU 
  General Public License for more details.

  You should have received a copy of the GNU General Public License 
  along with this program; if not, write to the Free Software 
  Foundation, Inc., 51 Franklin St - Fifth Floor, Boston, MA 02110-1301 USA.
  The full GNU General Public License is included in this distribution 
  in the file called LICENSE.GPL.

  Contact Information:
   David Gay, david.e.gay@intel.com
   Intel Labs Berkeley, 2150 Shattuck Avenue, Suite 1300, Berkeley, CA, 94704

  BSD LICENSE 

  Copyright(c) 2006 Intel Corporation. All rights reserved.
  All rights reserved.

  Redistribution and use in source and binary forms, with or without 
  modification, are permitted provided that the following conditions 
  are met:

    * Redistributions of source code must retain the above copyright 
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright 
      notice, this list of conditions and the following disclaimer in 
      the documentation and/or other materials provided with the 
      distribution.
    * Neither the name of Intel Corporation nor the names of its 
      contributors may be used to endorse or promote products derived 
      from this software without specific prior written permission.

  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 
  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT 
  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR 
  A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT 
  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, 
  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT 
  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, 
  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY 
  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT 
  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE 
  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/
#ifdef __MSP430
#include <sys/inttypes.h>
#else
#include <inttypes.h>
#endif

/* Network to host and host to network conversions.
   Normal network representation is 2's complement big-endian, but there
   are also little-endian types available.
*/

#define __NX_DECLARE_FUNCTIONS(bits)					\
  									\
  inline int ## bits ## _t __nesc_ntoh_int ## bits(const void * COUNT(bits/8) source) @safe() { \
    return __nesc_ntoh_uint ## bits(source);				\
  }									\
									\
  inline int ## bits ## _t __nesc_hton_int ## bits(void * COUNT(bits/8) target, int ## bits ## _t value) @safe() { \
    __nesc_hton_uint ## bits(target, value);				\
    return value;							\
  }									\
									\
  inline int ## bits ## _t __nesc_ntoh_leint ## bits(const void * COUNT(bits/8) source) @safe() { \
    return __nesc_ntoh_leuint ## bits(source);				\
  }									\
									\
  inline int ## bits ## _t __nesc_hton_leint ## bits(void * COUNT(bits/8) target, int ## bits ## _t value) @safe() { \
    __nesc_hton_leuint ## bits(target, value);				\
    return value;							\
  }

#define __NX_DECLARE_BF_FUNCTIONS(bits)					\
									\
  uint ## bits ## _t __nesc_bf_decode ## bits(const uint8_t *msg, unsigned offset, uint8_t length) @safe() \
  {									\
    uint ## bits ## _t x = 0;						\
    unsigned byte_offset = offset >> 3;					\
    unsigned bit_offset = offset & 7;					\
									\
    /* All in one byte case */						\
    if (length + bit_offset <= 8)					\
      return (msg[byte_offset] >> (8 - bit_offset - length)) &		\
	((1 << length) - 1);						\
									\
    /* get some high order bits */					\
    if (bit_offset > 0)							\
      {									\
	length -= 8 - bit_offset;					\
	x = (uint ## bits ## _t)(msg[byte_offset] & ((1 << (8 - bit_offset)) - 1)) << length; \
	byte_offset++;							\
      }									\
									\
    while (length >= 8)							\
      {									\
	length -= 8;							\
	x |= (uint ## bits ## _t)msg[byte_offset++] << length;		\
      }									\
									\
    /* data from last byte */						\
    if (length > 0)							\
      x |= msg[byte_offset] >> (8 - length);				\
									\
    return x;								\
  }									\
									\
  void __nesc_bf_encode ## bits(uint8_t *msg, unsigned offset, uint8_t length, uint ## bits ## _t x) @safe() \
  {									\
    unsigned byte_offset = offset >> 3;					\
    unsigned bit_offset = offset & 7;					\
									\
    x = x & (((uint ## bits ## _t)1 << length) - 1);					\
									\
    /* all in one byte case */						\
    if (length + bit_offset <= 8) {					\
      unsigned mask = ((1 << length) - 1) << (8 - bit_offset - length); \
									\
      msg[byte_offset] =						\
	((msg[byte_offset] & ~mask) | x << (8 - bit_offset - length));	\
      return;								\
    }									\
									\
    /* set some high order bits */					\
    if (bit_offset > 0)							\
      {									\
	unsigned mask = (1 << (8 - bit_offset)) - 1;			\
									\
	length -= 8 - bit_offset;					\
	msg[byte_offset] = ((msg[byte_offset] & ~mask) | x >> length);	\
	byte_offset++;							\
      }									\
									\
    while (length >= 8)							\
      {									\
	length -= 8;							\
	msg[byte_offset++] = x >> length;				\
      }									\
									\
    /* data for last byte */						\
    if (length > 0)							\
      {									\
	unsigned mask = (1 << (8 - length)) - 1;			\
									\
	msg[byte_offset] = ((msg[byte_offset] & mask) | x << (8 - length)); \
      }									\
  }									\
									\
  inline uint ## bits ## _t __nesc_ntohbf_uint ## bits(const void *source, unsigned offset, uint8_t length) @safe() { \
    return __nesc_bf_decode ## bits(source, offset, length);		\
  }									\
									\
  inline int ## bits ## _t __nesc_ntohbf_int ## bits(const void *source, unsigned offset, uint8_t length) @safe() { \
    return __nesc_bf_decode ## bits(source, offset, length);		\
  }									\
									\
  inline uint ## bits ## _t __nesc_htonbf_uint ## bits(void *target, unsigned offset, uint8_t length, uint ## bits ## _t value) @safe() { \
    __nesc_bf_encode ## bits(target, offset, length, value);		\
    return value;							\
  }									\
									\
  inline int ## bits ## _t __nesc_htonbf_int ## bits(void *target, unsigned offset, uint8_t length, int ## bits ## _t value) @safe() { \
    __nesc_bf_encode ## bits(target, offset, length, value);		\
    return value;							\
  }									\
									\
  /* Little-endian functions */						\
  uint ## bits ## _t __nesc_bfle_decode ## bits(const uint8_t *msg, unsigned offset, uint8_t length) @safe() \
  {									\
    uint ## bits ## _t x = 0;						\
    unsigned byte_offset = offset >> 3;					\
    unsigned bit_offset = offset & 7;					\
    unsigned count = 0;							\
									\
    /* all in one byte case */						\
    if (length + bit_offset <= 8)					\
      return (msg[byte_offset] >> bit_offset) & ((1 << length) - 1);	\
									\
    /* get some low order bits */					\
    if (bit_offset > 0)							\
      {									\
	x = msg[byte_offset++] >> bit_offset;				\
	count += 8 - bit_offset;                                        \
      }									\
									\
    while (count + 8 <= length)						\
      {									\
	x |= (uint ## bits ## _t)msg[byte_offset++] << count;		\
	count += 8;                                                     \
      }									\
									\
    /* data from last byte */						\
    if (count < length)							\
      x |= (uint ## bits ## _t)(msg[byte_offset] & ((1 << (length - count)) - 1)) << count; \
									\
    return x;								\
  }									\
									\
  void __nesc_bfle_encode ## bits(uint8_t *msg, unsigned offset, uint8_t length, uint ## bits ## _t x) @safe() 					       \
  {									\
    unsigned byte_offset = offset >> 3;					\
    unsigned bit_offset = offset & 7;					\
    unsigned count = 0;							\
									\
    x = x & (((uint ## bits ## _t)1 << length) - 1);					\
									\
    /* all in one byte case */						\
    if (length + bit_offset <= 8) {					\
      unsigned mask = ((1 << length) - 1) << bit_offset;		\
									\
      msg[byte_offset] =						\
	((msg[byte_offset] & ~mask) | x << bit_offset);			\
      return;								\
    }									\
									\
    /* set some low order bits */					\
    if (bit_offset > 0)							\
      {									\
	unsigned mask = (1 << bit_offset) - 1;				\
									\
	msg[byte_offset] = (msg[byte_offset] & mask) |			\
	  (x << bit_offset);						\
	count += 8 - bit_offset;					\
	byte_offset++;							\
      }									\
									\
    while (count + 8 <= length)						\
      {									\
	msg[byte_offset++] = x >> count;                                \
	count += 8;                                                     \
      }									\
									\
    /* data for last byte */						\
    if (count < length)							\
      {									\
	unsigned remaining = length - count;                            \
	unsigned mask = ~((1 << remaining) - 1);			\
									\
	msg[byte_offset] = ((msg[byte_offset] & mask) | x >> count);    \
      }									\
  }									\
									\
  inline uint ## bits ## _t __nesc_ntohbf_leuint ## bits(const void *source, unsigned offset, uint8_t length) @safe() { \
    return __nesc_bfle_decode ## bits(source, offset, length);		\
  }									\
									\
  inline int ## bits ## _t __nesc_ntohbf_leint ## bits(const void *source, unsigned offset, uint8_t length) @safe() { \
    return __nesc_bfle_decode ## bits(source, offset, length);		\
  }									\
									\
  inline uint ## bits ## _t __nesc_htonbf_leuint ## bits(void *target, unsigned offset, uint8_t length, uint ## bits ## _t value) @safe() { \
    __nesc_bfle_encode ## bits(target, offset, length, value);		\
    return value;							\
  }									\
									\
  inline int ## bits ## _t __nesc_htonbf_leint ## bits(void *target, unsigned offset, uint8_t length, int ## bits ## _t value) @safe() { \
    __nesc_bfle_encode ## bits(target, offset, length, value);		\
    return value;							\
  }									\
									\

/* 8-bits */
/* ------ */

inline uint8_t __nesc_ntoh_uint8(const void * ONE source) @safe() {
  const uint8_t *base = source;
  return base[0];
}

inline uint8_t __nesc_hton_uint8(void * ONE target, uint8_t value) @safe() {
  uint8_t *base = target;
  base[0] = value;
  return value;
}

inline uint8_t __nesc_ntoh_leuint8(const void * ONE source) @safe() {
  const uint8_t *base = source;
  return base[0];
}

inline uint8_t __nesc_hton_leuint8(void * ONE target, uint8_t value) @safe() {
  uint8_t *base = target;
  base[0] = value;
  return value;
}

__NX_DECLARE_FUNCTIONS(8)
__NX_DECLARE_BF_FUNCTIONS(8)


/* 16-bits */
/* ------- */

inline uint16_t __nesc_ntoh_uint16(const void * COUNT(2) source) @safe() {
  const uint8_t *base = source;
  return (uint16_t)base[0] << 8 | base[1];
}

inline uint16_t __nesc_hton_uint16(void * COUNT(2) target, uint16_t value) @safe() {
  uint8_t *base = target;
  base[1] = value;
  base[0] = value >> 8;
  return value;
}

inline uint16_t __nesc_ntoh_leuint16(const void * COUNT(2) source) @safe() {
  const uint8_t *base = source;
  return (uint16_t)base[1] << 8 | base[0];
}

inline uint16_t __nesc_hton_leuint16(void * COUNT(2) target, uint16_t value) @safe() {
  uint8_t *base = target;
  base[0] = value;
  base[1] = value >> 8;
  return value;
}

__NX_DECLARE_FUNCTIONS(16)
__NX_DECLARE_BF_FUNCTIONS(16)


/* 32-bits */
/* ------- */
inline uint32_t __nesc_ntoh_uint32(const void * COUNT(4) source) @safe() {
  const uint8_t *base = source;
  return (uint32_t)base[0] << 24 |
         (uint32_t)base[1] << 16 |
         (uint32_t)base[2] << 8 | base[3];
}

inline uint32_t __nesc_hton_uint32(void * COUNT(4) target, uint32_t value) @safe() {
  uint8_t *base = target;
  base[3] = value;
  base[2] = value >> 8;
  base[1] = value >> 16;
  base[0] = value >> 24;
  return value;
}

inline uint32_t __nesc_ntoh_leuint32(const void * COUNT(4) source) @safe() {
  const uint8_t *base = source;
  return (uint32_t)base[3] << 24 |
         (uint32_t)base[2] << 16 |
         (uint32_t)base[1] << 8 | base[0];
}

inline uint32_t __nesc_hton_leuint32(void * COUNT(4) target, uint32_t value) @safe() {
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
inline uint64_t __nesc_ntoh_uint64(const void * COUNT(8) source) @safe() {
  const uint8_t *base = source;
  return (uint64_t)base[0] << 56 |
         (uint64_t)base[1] << 48 |
         (uint64_t)base[2] << 40 |
         (uint64_t)base[3] << 32 |
         (uint64_t)base[4] << 24 |
         (uint64_t)base[5] << 16 |
         (uint64_t)base[6] << 8  | base[7];
}

inline uint64_t __nesc_hton_uint64(void * COUNT(8) target, uint64_t value) @safe() {
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

inline uint64_t __nesc_ntoh_leuint64(const void * COUNT(8) source) @safe() {
  const uint8_t *base = source;
  return (uint64_t)base[7] << 56 |
         (uint64_t)base[6] << 48 |
         (uint64_t)base[5] << 40 |
         (uint64_t)base[4] << 32 |
         (uint64_t)base[3] << 24 |
         (uint64_t)base[2] << 16 |
         (uint64_t)base[1] << 8  | base[0];
}

inline uint64_t __nesc_hton_leuint64(void * COUNT(8) target, uint64_t value) @safe() {
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
typedef int8_t nx_int8_t __attribute__((nx_base_be(int8)));
typedef int16_t nx_int16_t __attribute__((nx_base_be(int16)));
typedef int32_t nx_int32_t __attribute__((nx_base_be(int32)));
typedef int64_t nx_int64_t __attribute__((nx_base_be(int64)));
typedef uint8_t nx_uint8_t __attribute__((nx_base_be(uint8)));
typedef uint16_t nx_uint16_t __attribute__((nx_base_be(uint16)));
typedef uint32_t nx_uint32_t __attribute__((nx_base_be(uint32)));
typedef uint64_t nx_uint64_t __attribute__((nx_base_be(uint64)));

/* Little endian external types, for those apps that need them. */
typedef int8_t nxle_int8_t __attribute__((nx_base_le(leint8)));
typedef int16_t nxle_int16_t __attribute__((nx_base_le(leint16)));
typedef int32_t nxle_int32_t __attribute__((nx_base_le(leint32)));
typedef int64_t nxle_int64_t __attribute__((nx_base_le(leint64)));
typedef uint8_t nxle_uint8_t __attribute__((nx_base_le(leuint8)));
typedef uint16_t nxle_uint16_t __attribute__((nx_base_le(leuint16)));
typedef uint32_t nxle_uint32_t __attribute__((nx_base_le(leuint32)));
typedef uint64_t nxle_uint64_t __attribute__((nx_base_le(leuint64)));

#undef __NX_DECLARE_FUNCTIONS
#undef __NX_DECLARE_BF_FUNCTIONS
