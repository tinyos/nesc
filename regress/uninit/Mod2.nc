module Mod2 {
  provides interface StdControl;
}
implementation {
  uint8_t mode[8];

  command result_t StdControl.init() {
    int i;
    for(i=0;i<6;i++) mode[i] = 2;
    return SUCCESS;
  }

  command result_t StdControl.start() { return SUCCESS; }
  command result_t StdControl.stop() { return SUCCESS; }
}
