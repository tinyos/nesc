configuration test { }
implementation {
  components main, new conf(int) as conf1, new conf(float *) as conf2;

  main.sc -> conf1;
  main.sc -> conf2;
}
