abstract module mod() {
  provides interface sc;
}
implementation {
  int x;

  int z = uniqueCount("mod");

  command void sc.init() {
    x += unique("mod");
    x -= unique("mod");
    x += z;
  }

}
