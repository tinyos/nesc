configuration test5m { }
implementation {
  components test5l(0) as bb, test2(44) as dd, Main, NoLeds;

  Main.StdControl -> dd;
  bb.Leds -> NoLeds;
}
