configuration test { }
implementation {
  components main, mod(2) as mod1, mod(20) as mod2;

  main.sc -> mod1;
  main.sc -> mod2;
}
