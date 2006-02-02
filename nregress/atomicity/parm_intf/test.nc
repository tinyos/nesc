configuration test { }
implementation {
  components intr, handler;

  intr.i[0] -> handler;
}
