configuration test { }
implementation {
  components main, conf(2, int) as conf1, conf(20, char *) as conf2;

  main.sc1 -> conf1;
  main.sc2 -> conf2;
}
