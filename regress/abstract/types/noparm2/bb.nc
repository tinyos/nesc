module bb {
  uses interface i<int> as b1;
  uses interface i<int> as b2;
}
implementation {
  void f() __attribute__((spontaneous)) {
    call b1.bind(12);
    call b2.bind(13);
  }
}

