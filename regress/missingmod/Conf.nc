configuration Conf { }
implementation {
  components Main, Mod1, Mod2, LedsC;

  Main.StdControl -> Mod1;
}
