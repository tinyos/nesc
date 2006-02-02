configuration test4b { }
implementation {
  components Main, new test2("aa") as tt;

  Main.StdControl -> tt;
}
