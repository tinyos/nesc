module check1 {
  uses interface Clock;
  uses interface Clock as Clock2;
  uses interface SendMsg;
  uses interface SendMsg as SendMsg2;
}
implementation {
  event result_t Clock.fire() {
    return 0;
  }

  async event result_t SendMsg.sendDone(TOS_MsgPtr msg, result_t success) {
    return 0;
  }

  async event result_t Clock2.fire() {
    return 0;
  }

  event result_t SendMsg2.sendDone(TOS_MsgPtr msg, result_t success) {
    return 0;
  }
}


