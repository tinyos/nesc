configuration test { }
implementation {
  components main, mod(2) as mod1, mod(int) as mod2;

  main.sc -> mod1;
  main.sc -> mod2;
}
