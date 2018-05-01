#include <stdio.h>

module TestP {
  uses interface Stop;
} implementation {
  _Noreturn int warning_if_variable_has_noreturn;

  int main() @C() @spontaneous() {
    puts("Preparing to stop...");
    warning_if_variable_has_noreturn = 0;
    call Stop.stop_now(2);
    // The following code will not been compiled and executed
    // since not_stop_now() is declared as noreturn.
    puts("This code is never executed.");
    return 3;
  }
}
