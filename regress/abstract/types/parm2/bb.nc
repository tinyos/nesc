module bb {
  uses interface i<int> as b1[char num];
  uses interface i<int> as b2[char num];
}
implementation {
  void f() __attribute__((spontaneous)) {
    call b1.bind[2](12);
    call b2.bind[3](13);
  }
}

