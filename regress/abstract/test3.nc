configuration test3 { }
implementation {
  components Main, test2;

  Main.StdControl -> test2;
}
