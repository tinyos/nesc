configuration Protocol1C {
  provides interface MyProtocol;
  enum {
    Protocol = 0xBA01,
  };
} implementation {
  components new LcpAutomatonC(Protocol);
  MyProtocol = LcpAutomatonC;
}
