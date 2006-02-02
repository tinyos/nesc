module Mod1 {
  provides interface BitSPI;
}
implementation {
  struct { int x; } foo;
  command bool BitSPI.txBit(bool bit) {
    call BitSPI.txBit(foo);
    call BitSPI.txBit("aa");
    return FALSE;
  }

  command int double BitSPI.txBit(bool bit) {
  }
}

