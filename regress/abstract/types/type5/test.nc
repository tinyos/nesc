configuration test { }
implementation {
  components main, mod(int) as mod1, mod(float *) as mod2;

  main.sc -> mod1;
  main.sc -> mod2;
}
