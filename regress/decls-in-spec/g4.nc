generic configuration g4(typedef t) { }
implementation {
  struct fun {
    t a;
  };
  components new g2(struct fun) as g;
}
