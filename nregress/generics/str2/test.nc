configuration test { }
implementation {
  components main, new conf("a") as mod1, new conf("oops") as mod2;

  main.sc -> mod1;
  main.sc -> mod2;
}
