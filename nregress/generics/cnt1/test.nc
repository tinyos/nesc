configuration test { }
implementation {
  components main, new mod(int, 2) as mod1, new mod() as mod2;

  main.sc -> mod1;
  main.sc -> mod2;
}
