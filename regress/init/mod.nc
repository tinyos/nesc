module mod {}
implementation {
  int fun[][2] = { 1, 2, 3, 4, 5};

  int x, *y = &x;

  void h() __attribute__((spontaneous)) {
    fun;
    y;
  }
}
