#include <stdio.h>

module TestP {
  uses {
    interface MyProtocol as MyProtocol1;
    interface MyProtocol as MyProtocol2;
  }
} implementation {
  int main() @C() @spontaneous() {
    printf("Here I am: P1=%x P2=%x.\n", call MyProtocol1.protocol(), call MyProtocol2.protocol());
  }
}
