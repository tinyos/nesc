module t6 { }
implementation {
  nw_struct fun {
    nw_int8_t x;
    nw_int16_t y;
  } a;

  void f() { }

  int main() __attribute__((C, spontaneous)) {
    int b = (f(), a.y);
    return b;
  }
}
