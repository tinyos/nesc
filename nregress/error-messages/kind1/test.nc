configuration test { provides interface Timer; uses interface Timer as T; }
implementation {
  components StdControl;

  Timer = T;
}
