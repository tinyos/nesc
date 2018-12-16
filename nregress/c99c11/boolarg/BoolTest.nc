interface BoolTest {
  command void test(const char *test_name, const _Bool arg);
  event void testDone(const char *test_name, _Bool result, _Bool arg);
}
