configuration test { }
implementation {
  components main, mod(int, 2) as mod1, mod() as mod2;

  main.sc -> mod1;
  main.sc -> mod2;
}
