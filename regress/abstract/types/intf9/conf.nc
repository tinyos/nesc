abstract configuration conf(int n, t) {
  provides interface sc(int, t);
}
implementation {
  components mod(t, n) as themod;

  sc = themod;
}
