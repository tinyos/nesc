module silly {
  provides interface a;
}
implementation
{
  int x;

  async command void a.c1() {
    atomic x = 1;
  }

  async command void a.c2() {
    atomic x = 2;
  }
}
