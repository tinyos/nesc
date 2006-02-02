module foo {
  provides interface sc;
}
implementation {
  int y;

  int z = uniqueCount("mod");

  command void sc.init() {
    y += unique("mod");
    y -= unique("mod");
    y += z;
  }
}
