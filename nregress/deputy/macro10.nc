struct @fun @macro("AAA") { int a; };

module macro10 { }
implementation
{
  int x @fun(.a = 1);

  void f(void) @spontaneous()
  {
    x = 0;
  }
}

