module mod {
  provides interface sc;
}
implementation {
  int n = unique("fun");
  enum { max = uniqueCount("fun") };

  command void sc.init() {
    n += max;
  }
}
