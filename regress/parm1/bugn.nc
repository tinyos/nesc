module bugn {
  provides interface SendMsg[uint8_t id];
}
implementation 
{
  command result_t SendMsg.send[uint8_t id](uint16_t address, uint8_t length, TOS_MsgPtr msg) {
    return SUCCESS;
  }
}

