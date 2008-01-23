struct @fun @macro("AAA") { int a; };

module macro8 { }
implementation
{
  int x @fun(1);

  void f(void) @spontaneous()
  {
    x = 0;
  }
}

