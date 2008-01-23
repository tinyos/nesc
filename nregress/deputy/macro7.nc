struct @fun @macro("AAA") { int a; };

module macro7 { }
implementation
{
  int x @fun(1, 2);

  void f(void) @spontaneous()
  {
    x = 0;
  }
}

