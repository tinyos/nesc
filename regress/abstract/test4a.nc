configuration test4a { }
implementation {
  components Main, test2(1) as tt, LedsC;

  Main.StdControl -> tt;
  tt.Leds -> LedsC;
}
