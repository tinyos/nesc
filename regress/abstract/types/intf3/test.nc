configuration test { }
implementation {
  components main, conf(2) as conf1, conf(20) as conf2;

  main.sc -> conf1;
  main.sc -> conf2;
}
