module FooImplementation {
  provides interface A<int> as X;
  uses interface A<int> as Y;
}
implementation {
  // This does something mysterious to requests...
  int x;

  command int X.request() {
    return call Y.request() + x;
  }

  event void Y.done(int val) {
    x = val;
  }
}
