configuration test { }
implementation {
  components main, new mod("a") as mod1, new mod("a") as mod1b, new mod("oops") as mod2;

  main.sc -> mod1;
  main.sc -> mod1b;
  main.sc -> mod2;
}
