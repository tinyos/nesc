configuration test { }
implementation {
  components main, mod(0) as mod1, mod(1) as mod2;

  main.sc -> mod1;
  main.sc -> mod2;
}
