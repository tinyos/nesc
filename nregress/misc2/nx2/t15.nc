module t15 {
  uses interface i as j;
}
implementation {

  void f() @spontaneous() {
    nx_uint32_t *a = call j.get();
  }
}
