configuration enum2 { }
implementation {
  components Main, new enum1(7) as tt;

  Main.StdControl -> tt;
}
