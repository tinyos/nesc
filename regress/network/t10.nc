module t10 { }
implementation {
  struct {
    nw_int8_t a, b[10];
  } z;

  int main() __attribute__((C, spontaneous)) {

    z.b[z.a] = 3;
    z.a[z.b] = 4;
    *(z.a + z.b) = 5;
    return 0;
  }
}
