module t1a { }
implementation {
  nw_struct fun {
    nw_int8_t x;
    nw_int16_t y;
  } a;

  int z = sizeof (a.x = a.y);

  int main() __attribute__((C, spontaneous)) {
    return z;
  }
}
