module t3a { }
implementation {
  nw_struct fun {
    nw_int8_t x;
    nw_int16_t y;
  } a[10];

  int f() { return 2; }

  int main() __attribute__((C, spontaneous)) {
    int z = a[f()].x++;
    int zz = ++a[f()].y;
    return z + zz;
  }
}
