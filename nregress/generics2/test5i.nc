configuration test5i { }
implementation {
  components Main, new test5(11) as aa, new test2(22) as bb, LedsC as NoLeds;

  Main.StdControl -> bb;
  bb.Leds -> NoLeds;
}
