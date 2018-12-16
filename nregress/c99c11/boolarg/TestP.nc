extern int printf(const char *__restrict format, ...);

typedef struct {
  _Bool a;
} bool_test_args;

nx_struct nx_header {
  nx_uint32_t src;
  nx_uint32_t dst;
};

module TestP {
  uses interface BoolTest;
}
implementation {
  int main() @C() @spontaneous() {
#define do_test(test_name, bool_expr)  call BoolTest.test(#test_name, bool_expr)

    do_test(TrueConst, 1);
    do_test(TrueExpr, 1 == 1 && 0 < 1);
    do_test(TrueExprSC, 0 == 0 || 0 > 1);
    do_test(IntTrue, 1234);
    do_test(FloatTrue, 1.23);
    do_test(String, "string");

    do_test(FalseConst, 0);
    do_test(FalseExpr, 0 == 1 || 0 > 1);
    do_test(FalseExprSC, 1 == 0 && 0 < 1);
    do_test(IntZero, 0);
    do_test(FloatZero, 0.0);
    do_test(NullVoid, (const void *)0);
    do_test(NullULL, (unsigned long long *)0);
    do_test(NullStruct, (const bool_test_args *const __restrict)0);
    do_test(NullFnP, (int (*)(char[], double))0);
    do_test(Network, (nx_uint32_t *)0);
    do_test(NetworkStruct, (nx_struct nx_header *)0);
#undef do_test

    return 0;
  }

  event void BoolTest.testDone(const char *test_name, _Bool result, _Bool arg) {
    printf("%15s: %d -> %d\n", test_name, arg, result);
  }
}
