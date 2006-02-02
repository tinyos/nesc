module test {
  provides interface StdControl;
}
implementation {
  command result_t StdControl.init() {
    return SUCCESS;
  }

  command result_t StdControl.start() {
    return SUCCESS;
  }

  command result_t StdControl.stop() {
    return SUCCESS;
  }

  command result_t StdControl.init() {
    return SUCCESS;
  }
}

