configuration test4b { }
implementation {
  components Main, test2("aa") as tt;

  Main.StdControl -> tt;
}
