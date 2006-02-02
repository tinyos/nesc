module fns2 { }
implementation {
  int x;

  void g() {
    x = 2;
  }

  void f() __attribute__((interrupt, spontaneous)) {
    x = 1;
    g();
  }

  
}
