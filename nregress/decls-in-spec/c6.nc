configuration c6 {
}
implementation {
  components m1;

  enum {
    X = unique("X")
  };

  m1.j -> m1.i[X];
  m1.j -> m1.i[X + 1];

  components m1 as mm;

  mm.j -> m1.i[0];
}
