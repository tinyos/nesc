configuration Protocol2C {
  provides interface MyProtocol;
  enum {
    Protocol = 0xAB02,
  };
} implementation {
  components new LcpAutomatonC(Protocol);
  MyProtocol = LcpAutomatonC;
}
