configuration c2 {
  enum {
    X = unique("X")
  };
}
implementation {
  components m1;

  m1.j -> m1.i[X];
  m1.j -> m1.i[X + 1];
}
