abstract configuration test5l(int base) { 
  uses interface Leds;
}
implementation {
  components test5j(base) as bb, test2(base + 33) as cc, Main, LedsC;

  Main.StdControl -> cc;
  cc.Leds -> LedsC;
  bb.Leds = Leds;
}
