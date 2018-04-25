module BoolTestM {
  provides interface BoolTest;
}
implementation {
  command void BoolTest.test(const char *test_name, const _Bool arg) {
    signal BoolTest.testDone(test_name, !arg, arg);
  }
}
