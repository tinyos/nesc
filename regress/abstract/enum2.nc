configuration enum2 { }
implementation {
  components Main, enum1(7) as tt;

  Main.StdControl -> tt;
}
