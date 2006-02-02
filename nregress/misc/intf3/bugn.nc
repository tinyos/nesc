#ifndef FOO
#define FOO
typedef unsigned char result_t;
typedef unsigned char uint8_t;
typedef unsigned int uint16_t;
enum { SUCCESS, FAIL };

typedef void *TOS_MsgPtr;

#endif

module bugn {
  provides interface SendMsg[uint8_t id];
}
implementation 
{
  command result_t SendMsg.send[uint8_t id](uint16_t address, uint8_t length, TOS_MsgPtr msg) {
    return SUCCESS;
  }
}

