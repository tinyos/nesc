module bugm {
  uses interface SendMsg[uint8_t id];
}
implementation 
{
  int main() __attribute__((spontaneous)) {
    call SendMsg.send[1](0, 0, NULL);
    return 0;
  }

  default command result_t SendMsg.send[uint8_t id](uint16_t address, uint8_t length, TOS_MsgPtr msg) {
    return SUCCESS;
  }

  event result_t SendMsg.sendDone[uint8_t id](TOS_MsgPtr msg, result_t ok) {
    return SUCCESS;
  }
}

