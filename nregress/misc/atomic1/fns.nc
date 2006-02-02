module fns { }
implementation {
  int x;

  void g() {
    x = 2;
  }

  void f() __attribute__((interrupt, spontaneous)) {
    atomic x = 1;
    g();
  }

  
}
