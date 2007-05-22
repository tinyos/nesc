module test { }
implementation {
  int x = uniqueN("xx", 5) + unique("xx");
  enum { z = uniqueN("xx", 2) };

  int y[uniqueCount("xx")  + z];

  int f() @spontaneous() {
    return y[0] + x + z;
  }
}
