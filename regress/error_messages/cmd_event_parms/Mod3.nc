module Mod3 {
  provides interface BitSPI as Fun[uint8_t id];
}
implementation {
  command bool Fun.txBit[int id](bool bit) {
  }
}

