abstract module mod(int n) {
  provides interface sc;
}
implementation {
  struct hmm {
    int x : n;
    char f;
  } x;

  int a = sizeof x;
  enum { b = (int)&(((struct hmm *)0)->f) };
  int c = 1 / (b == 1 || b == 3 ? 1 : 0);

  command void sc.init() {
    x.x = a + b + c;
  }
}
