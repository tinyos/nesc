module t4 { }
implementation {
  nw_struct fun {
    nw_int8_t x;
    nw_int16_t y;
  } a;

  int main() __attribute__((C, spontaneous)) {
    nw_int16_t *z = &a.y;

    *z = 2;
    return 0;
  }
}
