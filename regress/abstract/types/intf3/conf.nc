abstract configuration conf(int t) {
  provides interface sc(int, char *);
}
implementation {
  components mod(t) as themod;

  sc = themod;
}
