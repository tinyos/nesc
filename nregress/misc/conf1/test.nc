configuration test { }
implementation {
  components Main, Mod1, Mod2;

  Main.StdControl -> Mod1;
}
