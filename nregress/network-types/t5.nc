module t5 { }
implementation {
  nw_struct fun {
    nw_int8_t x;
    nw_int16_t y;
  } a;

  int main() __attribute__((C, spontaneous)) {
    int b = (nw_int8_t)a.y;
    return b;
  }
}
