void g();

module void2 { }
implementation
{
  void f() @spontaneous()
  {
    g(1);
  }
}
