abstract module enum1(int x) {
  provides interface StdControl;
}
implementation {
  struct s {
    int a[x];
  };

  enum fun {
    val1 = x * 2 + 3,
    val2
  };

  enum fun2 {
    val3 = sizeof(struct s),
    val4
  };

  command result_t StdControl.init() {
    return val2;
  }

  command result_t StdControl.start() {
    return val4;
  }

  command result_t StdControl.stop() {
    return val1 + val2;
  }
}
