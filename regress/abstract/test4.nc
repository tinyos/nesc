configuration test4 { }
implementation {
  components Main, test2() as tt;

  Main.StdControl -> tt;
}
