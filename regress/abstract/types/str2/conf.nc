abstract configuration conf(char t[]) {
  provides interface sc;
}
implementation {
  components mod(t) as themod;

  sc = themod;
}
