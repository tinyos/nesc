configuration test3 { }
implementation {
  components Main, test2(100) as fun, LedsC;

  Main.StdControl -> fun;
  fun.Leds -> LedsC;
}
