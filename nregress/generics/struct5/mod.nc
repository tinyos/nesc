abstract module mod(int n) {
  provides interface sc;
}
implementation {
  struct hmm {
    int k;
    int x : 1/(n-2);
    int f;
  } x;

  int a = sizeof x;
  int b = (int)&(((struct hmm *)0)->f);
  int c = (int)&(((struct hmm *)0)->k);

  command void sc.init() {
    x.x = a + b + c;
  }
}
