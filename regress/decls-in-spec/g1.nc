generic configuration g1(int x) {
  enum {
    X = x + 2
  };
}
implementation {
  components m1;

  m1.j -> m1.i[X];
  m1.j -> m1.i[X + 1];
}
