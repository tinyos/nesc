module AppP {
  provides interface Myif[uint32_t a, uint16_t b];
}

implementation {

  command uint32_t Myif.foo[uint32_t a, uint16_t b]() {
    return a;
  }

}