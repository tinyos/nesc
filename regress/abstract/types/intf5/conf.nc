abstract configuration conf(t, int n) {
  provides interface sc(int, t);
}
implementation {
  components mod(t, n) as themod;

  sc = themod;
}
