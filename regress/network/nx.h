#include <inttypes.h>

/* Network to host and host to network conversions.
   Network representation is 2's complement little-endian.
*/
static inline uint8_t ntoh_uint8(void *source) {
  unsigned char *base = source;
  return (unsigned char)base[0];
}
static inline uint16_t ntoh_uint16(void *source) {
  unsigned char *base = source;
  return base[1] << 8 | base[0];
}
static inline uint32_t ntoh_uint32(void *source) {
  unsigned char *base = source;
  return (uint32_t)base[3] << 24 |
         (uint32_t)base[2] << 16 |
         base[1] << 8 | base[0];
}
static inline uint64_t ntoh_uint64(void *source) {
  unsigned char *base = source;
  return (uint64_t)base[7] << 56 |
         (uint64_t)base[6] << 48 |
         (uint64_t)base[5] << 40 |
         (uint64_t)base[4] << 32 |
         (uint64_t)base[3] << 24 |
         (uint64_t)base[2] << 16 |
         base[1] << 8  | base[0];
}
static inline int8_t ntoh_int8(void *source) {
  return ntoh_uint8(source);
}
static inline int16_t ntoh_int16(void *source) {
  return ntoh_uint16(source);
}
static inline int32_t ntoh_int32(void *source) {
  return ntoh_uint32(source);
}
static inline int64_t ntoh_int64(void *source) {
  return ntoh_uint64(source);
}

/* Host to network order assignment */
static inline uint8_t hton_uint8(void *target, uint8_t value) {
  unsigned char *base = target;
  base[0] = value;
  return value;
}
static inline uint16_t hton_uint16(void *target, uint16_t value) {
  unsigned char *base = target;
  base[0] = value;
  base[1] = value >> 8;
  return value;
}
static inline uint32_t hton_uint32(void *target, uint32_t value) {
  unsigned char *base = target;
  base[0] = value;
  base[1] = value >> 8;
  base[2] = value >> 16;
  base[3] = value >> 24;
  return value;
}
static inline uint64_t hton_uint64(void *target, uint64_t value) {
  unsigned char *base = target;
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
static inline int8_t hton_int8(void *target, int8_t value) {
  unsigned char *base = target;
  base[0] = value;
  return value;
}
static inline int16_t hton_int16(void *target, int16_t value) {
  unsigned char *base = target;
  base[0] = value;
  base[1] = value >> 8;
  return value;
}
static inline int32_t hton_int32(void *target, int32_t value) {
  unsigned char *base = target;
  base[0] = value;
  base[1] = value >> 8;
  base[2] = value >> 16;
  base[3] = value >> 24;
  return value;
}

static inline int64_t hton_int64(void *target, int64_t value) {
  unsigned char *base = target;
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

typedef int8_t nw_int8_t __attribute__((nx_base(int8)));
typedef int16_t nw_int16_t __attribute__((nx_base(int16)));
typedef int32_t nw_int32_t __attribute__((nx_base(int32)));
typedef int64_t nw_int64_t __attribute__((nx_base(int64)));
typedef uint8_t nw_uint8_t __attribute__((nx_base(uint8)));
typedef uint16_t nw_uint16_t __attribute__((nx_base(uint16)));
typedef uint32_t nw_uint32_t __attribute__((nx_base(uint32)));
typedef uint64_t nw_uint64_t __attribute__((nx_base(uint64)));
