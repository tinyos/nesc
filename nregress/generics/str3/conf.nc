abstract configuration conf(char t[]) {
  provides interface sc;
}
implementation {
  components new mod(t + 1) as themod;

  sc = themod;
}
