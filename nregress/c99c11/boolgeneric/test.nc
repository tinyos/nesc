nx_struct nx_header {
  nx_uint32_t src;
  nx_uint32_t dst;
};

configuration test {
}
implementation {
  components TestP;
#define define_test(test_name, bool_expr)                               \
  components new BoolTestM(bool_expr, #test_name) as Test##test_name;   \
  Test##test_name <- TestP.BoolTest

  define_test(TrueConst, 1);
  define_test(TrueExpr, 1 == 1 && 0 < 1);
  define_test(TrueExprSC, 0 == 0 || 0 > 1);
  define_test(IntTrue, 1234);
  define_test(FloatTrue, 1.23);
  define_test(String, "string");

  define_test(FalseConst, 0);
  define_test(FalseExpr, 0 == 1 || 0 > 1);
  define_test(FalseExprSC, 1 == 0 && 0 < 1);
  define_test(IntZero, 0);
  define_test(FloatZero, 0.0);
  define_test(NullVoid, (const void *)0);
  define_test(NullULL, (unsigned long long *)0);
  define_test(NullStruct, (const bool_test_args *const __restrict)0);
  define_test(NullFnP, (int (*)(char[], double))0);
  define_test(Network, (nx_uint32_t *)0);
  define_test(NetworkStruct, (nx_struct nx_header *)0);

#undef define_test
}
