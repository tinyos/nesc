configuration test { }
implementation {
  components hmm, LedsC;

  hmm.Leds -> LedsC;
}
