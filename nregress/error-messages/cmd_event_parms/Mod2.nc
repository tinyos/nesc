module Mod2 {
  provides interface BitSPI as Fun[uint8_t id];
}
implementation {
  command bool Fun.txBit(bool bit) {
  }
}

