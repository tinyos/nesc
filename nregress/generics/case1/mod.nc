abstract module mod(int n) {
  provides interface sc;
}
implementation {
  int x;

  command void sc.init() {
    switch (x)
      {
      case n:
	return;
      }
  }
}
