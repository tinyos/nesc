abstract configuration conf(typedef t, int n) {
  provides interface sc<int, t>;
}
implementation {
  components new mod(t, n) as themod;

  sc = themod;
}
