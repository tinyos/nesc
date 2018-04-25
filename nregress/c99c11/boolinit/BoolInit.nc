interface BoolInit {
  command void test();
  event void testDone(const char *test_name, _Bool value);
}
