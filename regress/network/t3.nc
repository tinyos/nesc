includes nx;
module t3 { }
implementation {
  nw_struct fun {
    nw_int8_t x;
    nw_int16_t y;
  } a[10];

  int f() { return 2; }

  int main() __attribute__((C, spontaneous)) {
    a[f()].x += a[2*f()].y;
    return 0;
  }
}
