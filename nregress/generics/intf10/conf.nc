generic configuration conf(typedef t2, typedef t) {
  provides interface sc<t2, t>;
}
implementation {
  components new mod(t2, t) as themod;

  sc = themod.sc;
}
