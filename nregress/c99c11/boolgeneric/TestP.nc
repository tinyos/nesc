extern int printf(const char *__restrict format, ...);

module TestP {
  uses interface BoolTest;
}
implementation {
  int main() @C() @spontaneous() {
    bool_test_args args;

    call BoolTest.unary(0);
    call BoolTest.unary(1);
    args.a = args.b = 0;
    call BoolTest.binary(args);
    args.b = 1;
    call BoolTest.binary(args);
    args.a = 1; args.b = 0;
    call BoolTest.binary(args);
    args.a = args.b = 1;
    call BoolTest.binary(args);

    return 0;
  }

  event void BoolTest.unaryDone(const char *test_name, _Bool result, _Bool arg) {
    printf("%15s: %d -> %d\n", test_name, arg, result);
  }

  event void BoolTest.binaryDone(const char *test_name, _Bool result, bool_test_args args) {
    printf("%15s: %d, %d -> %d\n", test_name, args.a, args.b, result);
  }
}
