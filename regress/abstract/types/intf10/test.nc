configuration test { }
implementation {
  components main, new conf(int, int) as conf1;

  main.sc1 -> conf1;
}
