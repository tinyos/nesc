abstract configuration conf(char t[]) {
  provides interface sc;
}
implementation {
  components new mod(t) as themod;

  sc = themod;
}
