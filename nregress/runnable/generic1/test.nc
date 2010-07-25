configuration test {
} implementation {
  components TestP;

  components Protocol1C as P1C;
  TestP.MyProtocol1 -> P1C;

  components Protocol2C as P2C;
  TestP.MyProtocol2 -> P2C;
}
