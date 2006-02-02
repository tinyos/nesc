module Main {
  uses interface StdControl;
}
implementation {
  int main() @C() @spontaneous() {
    call StdControl.init();
    call StdControl.start();
    call StdControl.stop();
  }
}

