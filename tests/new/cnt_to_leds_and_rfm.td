configuration cnt_to_leds_and_rfm {
}
implementation {
  uses main, counter, int_to_leds, int_to_rfm, clock;

  main -> counter.init;
  counter.clock -> clock;
  int_to_leds -> counter.output;
  counter.output <- int_to_rfm;
}
