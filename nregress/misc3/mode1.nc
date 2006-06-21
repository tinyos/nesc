module mode1 { }
implementation {
  typedef int t1 __attribute__((mode(SI)));
  typedef unsigned int t2 __attribute__((mode(__SI__)));
  typedef int t3 __attribute__((mode(word)));
  typedef float f1 __attribute__((mode(SF)));

  int f() @spontaneous() {
    int b1 = sizeof(f1);
    int a1 = sizeof(t1), a2 = sizeof(t2), a3 = sizeof(t3);
  }
}
