module t1 { }
implementation {

  int x;

  int f() @spontaneous()
  {
    return x;
  }
}

