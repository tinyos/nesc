struct @fun @macro("AAA") { };

module macro5 { }
implementation
{
  int x @fun();

  void f(void) @spontaneous()
  {
    x = 0;
  }
}

