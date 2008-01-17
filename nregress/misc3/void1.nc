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

  void h(void) @spontaneous()
  {
    g();
  }
}
