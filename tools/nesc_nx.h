#ifdef __MSP430
#include <sys/inttypes.h>
#else
#include <inttypes.h>
#endif

/* Network to host and host to network conversions.
   Network representation is 2's complement big-endian.
*/
static inline uint8_t __nesc_ntoh_uint8(void *source) {
  uint8_t *base = source;
  return base[0];
}
static inline uint16_t __nesc_ntoh_uint16(void *source) {
  uint8_t *base = source;
  return (uint16_t)base[0] << 8 | base[1];
}
static inline uint32_t __nesc_ntoh_uint32(void *source) {
  uint8_t *base = source;
  return (uint32_t)base[0] << 24 |
         (uint32_t)base[1] << 16 |
         (uint32_t)base[2] << 8 | base[3];
}
static inline uint64_t __nesc_ntoh_uint64(void *source) {
  uint8_t *base = source;
  return (uint64_t)base[0] << 56 |
         (uint64_t)base[1] << 48 |
         (uint64_t)base[2] << 40 |
         (uint64_t)base[3] << 32 |
         (uint64_t)base[4] << 24 |
         (uint64_t)base[5] << 16 |
         (uint64_t)base[6] << 8  | base[7];
}
static inline int8_t __nesc_ntoh_int8(void *source) {
  return __nesc_ntoh_uint8(source);
}
static inline int16_t __nesc_ntoh_int16(void *source) {
  return __nesc_ntoh_uint16(source);
}
static inline int32_t __nesc_ntoh_int32(void *source) {
  return __nesc_ntoh_uint32(source);
}
static inline int64_t __nesc_ntoh_int64(void *source) {
  return __nesc_ntoh_uint64(source);
}

/* Host to network order assignment */
static inline uint8_t __nesc_hton_uint8(void *target, uint8_t value) {
  uint8_t *base = target;
  base[0] = value;
  return value;
}
static inline uint16_t __nesc_hton_uint16(void *target, uint16_t value) {
  uint8_t *base = target;
  base[1] = value;
  base[0] = value >> 8;
  return value;
}
static inline uint32_t __nesc_hton_uint32(void *target, uint32_t value) {
  uint8_t *base = target;
  base[3] = value;
  base[2] = value >> 8;
  base[1] = value >> 16;
  base[0] = value >> 24;
  return value;
}
static inline uint64_t __nesc_hton_uint64(void *target, uint64_t value) {
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

static inline int8_t __nesc_hton_int8(void *target, int8_t value) {
  __nesc_hton_uint8(target, value);
  return value;
}
static inline int16_t __nesc_hton_int16(void *target, int16_t value) {
  __nesc_hton_uint16(target, value);
  return value;
}
static inline int32_t __nesc_hton_int32(void *target, int32_t value) {
  __nesc_hton_uint32(target, value);
  return value;
}
static inline int64_t __nesc_hton_int64(void *target, int64_t value) {
  __nesc_hton_uint64(target, value);
  return value;
}

/* Network to host and host to network conversions for little-endian types.
*/
static inline uint8_t __nesc_ntoh_leuint8(void *source) {
  uint8_t *base = source;
  return base[0];
}
static inline uint16_t __nesc_ntoh_leuint16(void *source) {
  uint8_t *base = source;
  return (uint16_t)base[1] << 8 | base[0];
}
static inline uint32_t __nesc_ntoh_leuint32(void *source) {
  uint8_t *base = source;
  return (uint32_t)base[3] << 24 |
         (uint32_t)base[2] << 16 |
         (uint32_t)base[1] << 8 | base[0];
}
static inline uint64_t __nesc_ntoh_leuint64(void *source) {
  uint8_t *base = source;
  return (uint64_t)base[7] << 56 |
         (uint64_t)base[6] << 48 |
         (uint64_t)base[5] << 40 |
         (uint64_t)base[4] << 32 |
         (uint64_t)base[3] << 24 |
         (uint64_t)base[2] << 16 |
         (uint64_t)base[1] << 8  | base[0];
}
static inline int8_t __nesc_ntoh_leint8(void *source) {
  return __nesc_ntoh_leuint8(source);
}
static inline int16_t __nesc_ntoh_leint16(void *source) {
  return __nesc_ntoh_leuint16(source);
}
static inline int32_t __nesc_ntoh_leint32(void *source) {
  return __nesc_ntoh_leuint32(source);
}
static inline int64_t __nesc_ntoh_leint64(void *source) {
  return __nesc_ntoh_leuint64(source);
}

/* Host to network order assignment */
static inline uint8_t __nesc_hton_leuint8(void *target, uint8_t value) {
  uint8_t *base = target;
  base[0] = value;
  return value;
}
static inline uint16_t __nesc_hton_leuint16(void *target, uint16_t value) {
  uint8_t *base = target;
  base[0] = value;
  base[1] = value >> 8;
  return value;
}
static inline uint32_t __nesc_hton_leuint32(void *target, uint32_t value) {
  uint8_t *base = target;
  base[0] = value;
  base[1] = value >> 8;
  base[2] = value >> 16;
  base[3] = value >> 24;
  return value;
}
static inline uint64_t __nesc_hton_leuint64(void *target, uint64_t value) {
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

static inline int8_t __nesc_hton_leint8(void *target, int8_t value) {
  __nesc_hton_leuint8(target, value);
  return value;
}
static inline int16_t __nesc_hton_leint16(void *target, int16_t value) {
  __nesc_hton_leuint16(target, value);
  return value;
}
static inline int32_t __nesc_hton_leint32(void *target, int32_t value) {
  __nesc_hton_leuint32(target, value);
  return value;
}
static inline int64_t __nesc_hton_leint64(void *target, int64_t value) {
  __nesc_hton_leuint64(target, value);
  return value;
}

/* Standard external types: big-endian */
typedef int8_t nx_int8_t __attribute__((nx_base(int8)));
typedef int16_t nx_int16_t __attribute__((nx_base(int16)));
typedef int32_t nx_int32_t __attribute__((nx_base(int32)));
typedef int64_t nx_int64_t __attribute__((nx_base(int64)));
typedef uint8_t nx_uint8_t __attribute__((nx_base(uint8)));
typedef uint16_t nx_uint16_t __attribute__((nx_base(uint16)));
typedef uint32_t nx_uint32_t __attribute__((nx_base(uint32)));
typedef uint64_t nx_uint64_t __attribute__((nx_base(uint64)));

/* Little endian external types, for those apps that need them */
typedef int8_t nxle_int8_t __attribute__((nx_base(leint8)));
typedef int16_t nxle_int16_t __attribute__((nx_base(leint16)));
typedef int32_t nxle_int32_t __attribute__((nx_base(leint32)));
typedef int64_t nxle_int64_t __attribute__((nx_base(leint64)));
typedef uint8_t nxle_uint8_t __attribute__((nx_base(leuint8)));
typedef uint16_t nxle_uint16_t __attribute__((nx_base(leuint16)));
typedef uint32_t nxle_uint32_t __attribute__((nx_base(leuint32)));
typedef uint64_t nxle_uint64_t __attribute__((nx_base(leuint64)));
