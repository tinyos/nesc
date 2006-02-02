abstract configuration test5j(int base) { 
  uses interface Leds;
}
implementation {
  components new test5(base + 119) as aa, new test2(base + 22) as bb, Main;

  Main.StdControl -> bb;
  bb.Leds = Leds;
}
