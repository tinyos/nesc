module test {
  provides interface i<t1>;
  uses interface i<t2> as j;
}
implementation {
  command void i.f() {
    call j.f();
  }
}
