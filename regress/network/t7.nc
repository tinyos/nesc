module t7 { }
implementation {
  int main() __attribute__((C, spontaneous)) {
    nw_int8_t a = 1, b[10];

    b[a] = 3;
    a[b] = 4;
    *(a + b) = 5;
    return 0;
  }
}
