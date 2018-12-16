extern int printf(const char *__restrict __format, ...);

module TestP {
  uses {
    interface BoolInit;
    interface BoolReturn as ZeroInt;
    interface BoolReturn as PositiveInt;
    interface BoolReturn as NegativeInt;
    interface BoolReturn as ZeroFloat;
    interface BoolReturn as NoneZeroFloat;
    interface BoolReturn as String;
    interface BoolReturn as Null;
  }
}
implementation {
  static void testDone(const char *test_name, _Bool value) {
    printf("%15s: %d\n", test_name, value);
  }

  int main() @C() @spontaneous() {
    printf("===== BoolInit =====\n");
    call BoolInit.test();

#define do_test(test_name) \
    testDone(#test_name, call test_name.get())

    printf("===== BoolReturn =====\n");
    do_test(ZeroInt);
    do_test(ZeroFloat);
    do_test(Null);

    do_test(PositiveInt);
    do_test(NegativeInt);
    do_test(NoneZeroFloat);
    do_test(String);

#undef do_test

      return 0;
  }

  event void BoolInit.testDone(const char *test_name, _Bool value) {
    testDone(test_name, value);
  }
}
