configuration test { }
implementation {
  components main, new conf(2) as conf1, new conf(20) as conf2;

  main.sc -> conf1;
  main.sc -> conf2;
}
