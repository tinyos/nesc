configuration c1 {
  enum {
    X = 3
  };
}
implementation {
  components m1;

  m1.j -> m1.i[X];
  m1.j -> m1.i[X + 1];
}
