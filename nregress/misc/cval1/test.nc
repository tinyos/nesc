module test
{
}
implementation {
  struct silly {
    int x;
    int a;
  };

  int b;

  void f() __attribute__((spontaneous)) {
    switch (b)
      case ((short) &((struct silly *)0)->a) == 0: ;
  }
}
