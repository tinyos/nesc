module mod3 {
  provides interface sc;
}
implementation {
  void fun(int x) { }

  const struct { int x, y; } bar = { 2, 3};

  command void sc.init() {
    static const int foo = 11;
    fun(foo + bar.x);
  }
}
