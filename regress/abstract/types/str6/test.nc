configuration test { }
implementation {
  components main, mod("a") as mod1, mod("oops") as mod2;

  main.sc -> mod1;
  main.sc -> mod2;
}
