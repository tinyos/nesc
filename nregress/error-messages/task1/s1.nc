module s1 {
  provides interface T[int id];
}
implementation {

  command void T.dopost[int id]() {
  }

  // This works.
  task void foo() { }
}
