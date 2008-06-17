module void1 { }
implementation
{
  void f();

  void f(void)
  {
  }

  void g()
  {
    f();
  }

  task void p() { }

  task void q(void) { }

  void h(void) @spontaneous()
  {
    g();
  }
}
