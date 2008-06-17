module test {
  provides interface i<int>;
}
implementation
{
  int x;

  /**
   * nesdoc
   */
  async command void i.set(int a) {
    x = a;
  }

  void f() @spontaneous() {
    call i.set(11);
  }
}
