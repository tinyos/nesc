module mode2 { }
implementation {
  struct foo { int x; } t1 __attribute__((mode(byte)));
  typedef int t2 __attribute__((mode(SF)));
  typedef int t3 __attribute__((mode(OI)));
}
