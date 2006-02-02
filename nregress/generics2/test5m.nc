configuration test5m { }
implementation {
  components new test5l(0) as bb, new test2(44) as dd, Main, NoLeds;

  Main.StdControl -> dd;
  bb.Leds -> NoLeds;
}
