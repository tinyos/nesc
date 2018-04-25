configuration test {
}
implementation {
  components TestP;

#define define_test(test_name)                  \
  components test_name##M;                      \
  test_name##M <- TestP.BoolInit;               \
  test_name##M <- TestP.test_name

  define_test(ZeroInt);
  define_test(ZeroFloat);
  define_test(Null);

  define_test(PositiveInt);
  define_test(NegativeInt);
  define_test(NoneZeroFloat);
  define_test(String);

#undef define_test
}
