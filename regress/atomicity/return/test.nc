module test {
}
implementation {
  int f() {
    int x;

    atomic
      return x + 2;
  }

  void entry() __attribute((spontaneous)) {
    f();
    atomic
      return;
  }
}
