abstract configuration conf(int t) {
  provides interface sc<int, char *>;
}
implementation {
  components new mod(t) as themod;

  sc = themod;
}
