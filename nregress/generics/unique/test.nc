configuration test { }
implementation {
  components main, new mod() as mod1, new mod() as mod2, foo;

  main.sc -> mod1;
  main.sc -> mod2;
  main.sc -> foo;
}
