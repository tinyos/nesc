configuration test { }
implementation {
  components main, conf(int) as conf1, conf(float *) as conf2;

  main.sc -> conf1;
  main.sc -> conf2;
}
