configuration chmm { }
implementation {
  components hmm, LedsC;

  hmm.Leds -> LedsC;
}
