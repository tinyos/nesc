#ifndef FOO
#define FOO
typedef unsigned char result_t;
typedef unsigned char uint8_t;
typedef unsigned int uint16_t;
enum { SUCCESS, FAIL };

typedef void *TOS_MsgPtr;

#endif

module bugm {
  uses interface SendMsg[uint8_t id];
}
implementation 
{
  int main() __attribute__((spontaneous)) {
    call SendMsg.send[1](0, 0, 0);
    return 0;
  }

  default command result_t SendMsg.send[uint8_t id](uint16_t address, uint8_t length, TOS_MsgPtr msg) {
    return SUCCESS;
  }

  event result_t SendMsg.sendDone[uint8_t id](TOS_MsgPtr msg, result_t ok) {
    return SUCCESS;
  }
}

