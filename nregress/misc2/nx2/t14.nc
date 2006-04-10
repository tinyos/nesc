module t14 {
  provides interface i;
}
implementation {

  nx_uint32_t a;

  command nx_uint32_t *i.get() {
    a = a + a + a;
    a += 1;
    a += a;
    a += a + a + a;
    a += a + a * 2;
    return &a;
  }
}
