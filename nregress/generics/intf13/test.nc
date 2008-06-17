struct @single @macro("FUN") { };

module test {
  provides interface i<int>;
}
implementation
{
  int x;

  /**
   * @param 'int @single() a' is fun
   */
  async command void i.set(int a) {
    x = a;
  }

  void f() @spontaneous() {
    call i.set(11);
  }
}
