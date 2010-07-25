generic configuration LcpAutomatonC (uint16_t Protocol_) {
  provides {
    interface MyProtocol;
  }
  enum {
    /** BUG HERE: Both instances of LcpAutomatonP end up substituting
     * the same value for Protocol */
    Protocol = Protocol_,
  };
} implementation {
  components new LcpAutomatonP(Protocol);
  MyProtocol = LcpAutomatonP;
}
