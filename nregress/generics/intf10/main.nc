module main {
  uses interface sc<int, int> as sc1;
}
implementation {
  void entry() __attribute((spontaneous)) {
    int b = call sc1.init(55, 2);
  }
}
