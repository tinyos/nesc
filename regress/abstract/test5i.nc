configuration test5i { }
implementation {
  components Main, test5(11) as aa, test2(22) as bb, NoLeds;

  Main.StdControl -> bb;
  bb.Leds -> NoLeds;
}
