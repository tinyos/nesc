abstract module mod(int n) {
  provides interface sc;
}
implementation {
  struct hmm {
    char x[n];
    char f;
  } x;

  int a = sizeof x;
  enum { b = (int)&(((struct hmm *)0)->f) };
  int c = 1 / (b == 2 || b == 20 ? 1 : 0);

  command void sc.init() {
    x.x[0] = a + b + c;
  }
}
