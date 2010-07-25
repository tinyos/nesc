generic module LcpAutomatonP (uint16_t Protocol) {
  provides interface MyProtocol;
} implementation {
  command uint16_t MyProtocol.protocol () { return Protocol; }
}
