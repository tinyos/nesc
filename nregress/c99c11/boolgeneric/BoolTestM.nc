generic module BoolTestM(_Bool val, char test_name[]) {
  provides interface BoolTest;
}
implementation {
  command void BoolTest.unary(const _Bool a) {
    const _Bool result = val ? a : !a;
    signal BoolTest.unaryDone(test_name, result, a);
  }

  command void BoolTest.binary(const bool_test_args args) {
    const _Bool xor = args.a ^ args.b;
    const _Bool result = val ? xor : !xor;
    signal BoolTest.binaryDone(test_name, result, args);
  }
}
