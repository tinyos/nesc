configuration test { }
implementation {
  components main, mod() as mod1, mod() as mod2, foo;

  main.sc -> mod1;
  main.sc -> mod2;
  main.sc -> foo;
}
