module main {
  uses interface sc;
}
implementation {
  void entry() __attribute((spontaneous)) {
    call sc.init();
  }
}
