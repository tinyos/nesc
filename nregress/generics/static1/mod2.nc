module mod2 {
  provides interface sc;
}
implementation {
  void fun(int x) { }

  command void sc.init() {
    static int foo;
    fun(foo);
  }
}
