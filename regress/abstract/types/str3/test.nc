configuration test { }
implementation {
  components main, conf("a") as mod1, conf("oops") as mod2;

  main.sc -> mod1;
  main.sc -> mod2;
}
