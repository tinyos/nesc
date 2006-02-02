configuration test5n { }
implementation {
  components new test5l(1000) as bb, new test5l(0) as bb2, new test2(44) as dd, Main, LedsC, NoLeds;

  Main.StdControl -> dd;
  bb.Leds -> LedsC;
  bb2.Leds -> NoLeds;
  dd.Leds -> NoLeds;
}
