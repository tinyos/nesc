configuration Conf { }
implementation {
  components Main, Mod1;

  Main.StdControl -> Mod1;
}
