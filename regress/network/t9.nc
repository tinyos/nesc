module t9 { }
implementation {
  nw_struct fun {
    nw_int8_t x;
    nw_int16_t y;
  } a[10];

  int f() { return 2; }

  int main() __attribute__((C, spontaneous)) {
    a[1].x = 2;
    (*(a + 1)).y = (*(a + 0)).y;
    return 0;
  }
}
