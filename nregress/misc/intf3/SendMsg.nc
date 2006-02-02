#ifndef FOO
#define FOO
typedef unsigned char result_t;
typedef unsigned char uint8_t;
typedef unsigned int uint16_t;
enum { SUCCESS, FAIL };

typedef void *TOS_MsgPtr;

#endif

interface SendMsg {
  command result_t send(uint16_t address, uint8_t length, TOS_MsgPtr msg);
  event result_t sendDone(TOS_MsgPtr msg, result_t ok);
}
