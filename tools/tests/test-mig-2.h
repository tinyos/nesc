typedef unsigned char u8;
typedef signed char i8;
typedef unsigned short u16;
typedef short i16;
typedef unsigned long u32;
typedef long i32;

struct identity {
  u16 mote_id;
  u16 local_id;
  u32 time_info_starts;
};

struct ident_msg {
  u16 seqno;
  u16 broadcast_period;
  struct identity id;
};

