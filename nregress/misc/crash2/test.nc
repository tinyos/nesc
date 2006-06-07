typedef struct TOS_Msg
{
  char data[1];
  unsigned short crc;
  unsigned char ack;
  unsigned short time;
  unsigned char sendSecurityMode;
} TOS_Msg;

TOS_Msg blah=0;
