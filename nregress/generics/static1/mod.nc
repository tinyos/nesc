generic module mod() {
  provides interface sc;
}
implementation {
  void fun(int x) { }

  command void sc.init() {
    static int foo;
    fun(foo);
  }
}
