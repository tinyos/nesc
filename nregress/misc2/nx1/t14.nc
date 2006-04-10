module t14 {
  provides interface i;
}
implementation {

  command nx_uint32_t i.get() {
    return 0;
  }
}
