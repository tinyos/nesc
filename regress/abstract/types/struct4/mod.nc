abstract module mod(int n) {
  provides interface sc;
}
implementation {
  struct hmm {
    int x : n;
    int f;
  } x;

  int a = sizeof x;
  int b = (int)&(((struct hmm *)0)->f);

  command void sc.init() {
    x.x = a + b;
  }
}
