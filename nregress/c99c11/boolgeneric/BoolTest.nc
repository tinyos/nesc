typedef struct {
  _Bool a:1;
  _Bool b;
} bool_test_args;

interface BoolTest {
  command void unary(const _Bool a);
  command void binary(const bool_test_args args);
  event void unaryDone(const char *test_name, _Bool result, _Bool arg);
  event void binaryDone(const char *test_name, _Bool result, bool_test_args args);
}
