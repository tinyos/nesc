configuration test { }
implementation {
  components main, new mod("a") as mod1, new mod("oops") as mod2;

  main.sc -> mod1;
  main.sc -> mod2;
}
