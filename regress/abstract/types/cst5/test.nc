configuration test { }
implementation {
  components main, new mod(0) as mod1, new mod(1) as mod2;

  main.sc -> mod1;
  main.sc -> mod2;
}
