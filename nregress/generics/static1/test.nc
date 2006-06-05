configuration test { }
implementation {
  components main, new mod() as mod1, mod2, mod3;

  main.sc -> mod1;
  main.sc -> mod2;
  main.sc -> mod3;
}
