module t8 { }
implementation {
  int main() __attribute__((C, spontaneous)) {
    nw_int8_t a, b;

    (a, b) = 3;
    (a ? a : b) = 4;

    return 0;
  }
}
