module t11 { }
implementation {
  struct {
    nw_uint8_t a, b[10];
  } z;

  int main() __attribute__((C, spontaneous)) {

    z.b[z.a] = 3;
    z.a[z.b] = 4;
    *(z.a + z.b) = 5;
    return 0;
  }
}
