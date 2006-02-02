abstract configuration test5(int xx) { }
implementation {
  components Main, new test2(xx) as tt, LedsC;

  Main.StdControl -> tt;
  tt.Leds -> LedsC;
}
