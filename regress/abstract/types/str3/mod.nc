abstract module mod(char s[]) {
  provides interface sc;
}
implementation {
  void fun(char *s) { }

  command void sc.init() {
    fun(s);
  }
}
