#include <stdio.h>

module TestP {
  uses {
    interface Check as And00;
    interface Check as And01;
    interface Check as And10;
    interface Check as And11;
    interface Check as Or00;
    interface Check as Or01;
    interface Check as Or10;
    interface Check as Or11;
  }
} implementation {
  int main() @C() @spontaneous() {
    call And00.check();
    call And01.check();
    call And10.check();
    call And11.check();
    call Or00.check();
    call Or01.check();
    call Or10.check();
    call Or11.check();
  }

  event void And00.checkDone(int result) {
    printf("And00.checkDone -> %d\n", result);
  }

  event void And01.checkDone(int result) {
    printf("And01.checkDone -> %d\n", result);
  }

  event void And10.checkDone(int result) {
    printf("And10.checkDone -> %d\n", result);
  }

  event void And11.checkDone(int result) {
    printf("And11.checkDone -> %d\n", result);
  }

  event void Or00.checkDone(int result) {
    printf("Or00.checkDone -> %d\n", result);
  }

  event void Or01.checkDone(int result) {
    printf("Or01.checkDone -> %d\n", result);
  }

  event void Or10.checkDone(int result) {
    printf("Or10.checkDone -> %d\n", result);
  }

  event void Or11.checkDone(int result) {
    printf("Or11.checkDone -> %d\n", result);
  }
}
