module t8a { }
implementation {
  int main() __attribute__((C, spontaneous)) {
    nw_int32_t a, b;

    (a, b) = 3;
    (a ? a : b) = 4;
    return 0;
  }
}
