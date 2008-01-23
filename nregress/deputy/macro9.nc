struct @fun @macro("AAA") { int a[2]; };

module macro9 { }
implementation
{
  int x @fun( { 1, 2} );

  void f(void) @spontaneous()
  {
    x = 0;
  }
}

