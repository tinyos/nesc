module mod {} 
implementation {
  int aa;
  void h() __attribute__((spontaneous)) {
    static int x;
    x =aa;
  }
}
