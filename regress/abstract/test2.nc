abstract module test2(int x) {
  provides interface StdControl;
}
implementation {
  int y = 0;

  command result_t StdControl.init() {
    return y;
  }

  command result_t StdControl.start() {
    return x;
  }

  command result_t StdControl.stop() {
    return -x;
  }
}
