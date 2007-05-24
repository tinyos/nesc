module t2 { }
implementation {

  int __data x;

  int f() @spontaneous()
  {
    return x;
  }
}

