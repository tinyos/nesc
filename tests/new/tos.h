typedef struct TOS_Msg
{
  short addr;
  char type;
  unsigned char group;
  char data[30];
  short crc;
  short strength;
} TOS_Msg;

typedef TOS_Msg *TOS_MsgPtr;

unsigned short TOS_LOCAL_ADDRESS;

enum {
  TOS_BCAST_ADDR = (short) 0xffff
};
