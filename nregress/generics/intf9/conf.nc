abstract configuration conf(int n, typedef t) {
  provides interface sc<int, t>;
}
implementation {
  components new mod(t, n) as themod;

  sc = themod;
}
