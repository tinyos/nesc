module Mod1 {
  provides interface StdControl;
}
implementation {
  uint8_t mode[8];

  result_t xx() {
    int i;
    for(i=0;i<6;i++) mode[i] = 2;
    return SUCCESS;
  }

  command result_t StdControl.init() {
    result_t result1;
    result_t result2;

    result1 = 2;
    result2 = 1;
    result1 = rcombine(result1,result2);
    result2 = xx();
    result1 = rcombine(result1,result2);
    return result1;
  }

  command result_t StdControl.start() { return SUCCESS; }
  command result_t StdControl.stop() { return SUCCESS; }
}
