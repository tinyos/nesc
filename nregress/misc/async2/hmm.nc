module hmm {
  uses interface Leds;
}
implementation {
  task void fun() {
    call Leds.redOn();
  }

  void fun2() {
    call Leds.redOn();
  }

  void silly() __attribute__((interrupt, spontaneous)) {
    call Leds.redOn();
    fun2();
    post fun();
  }
}
