configuration test4a { }
implementation {
  components Main, test2(1) as tt;

  Main.StdControl -> tt;
}
