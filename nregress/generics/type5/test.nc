configuration test { }
implementation {
  components main, new mod(int) as mod1, new mod(float *) as mod2;

  main.sc -> mod1;
  main.sc -> mod2;
}
