module t3 { }
implementation {

  sfr32 __at(11) x;

  int f() @spontaneous()
  {
    return x;
  }
}
