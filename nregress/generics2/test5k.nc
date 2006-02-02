configuration test5k { }
implementation {
  components new test5j(0) as bb, new test2(33) as cc, Main, NoLeds, LedsC;

  Main.StdControl -> cc;
  bb.Leds -> NoLeds;
  cc.Leds -> LedsC;
}
