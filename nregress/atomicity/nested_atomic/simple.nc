includes ei;
module simple { 
  uses interface a;
}
implementation {
  void f() __attribute__((atomic_hwevent)) {
    call a.c1();
  }

  void g() __attribute__((atomic_hwevent)) {
    __nesc_enable_interrupt();
    call a.c2();
  }

}
