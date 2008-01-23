struct @fun @macro("AAA") { int a; };

module macro6 { }
implementation
{
  int x @fun();

  void f(void) @spontaneous()
  {
    x = 0;
  }
}

