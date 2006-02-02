configuration test { }
implementation {
  components simple, silly;

  simple.a -> silly;
}
