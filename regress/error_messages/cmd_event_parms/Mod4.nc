module Mod4 {
  provides interface BitSPI as Fun[uint8_t id];
}
implementation {

  void silly(uint8_t id) { }

  command bool Fun.txBit[uint8_t id](bool bit) {
    call Fun.txBit(0);
    call Fun.txBit["aa"](0);
    call Fun.txBit[1.2](0); // this is ok
    call Fun.txBit[1,2](0);

    silly("aa");
    silly(1.2); // this is ok
    silly(1,2);
  }
}

