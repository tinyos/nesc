configuration test4 { }
implementation {
  components Main, new test2() as tt;

  Main.StdControl -> tt;
}
