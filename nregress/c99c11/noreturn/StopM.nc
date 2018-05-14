#include <stdlib.h>
#include <stdnoreturn.h>

module StopM {
  provides interface Stop;
} implementation {
  command noreturn void Stop.stop_now(int i) {
    if (i > 0) exit(i);
    // This causes "`noreturn' function doesn return'" compiler warning.
  }
}
