configuration test { }
implementation {
  components main, mod;

  main.sc -> mod;
}
