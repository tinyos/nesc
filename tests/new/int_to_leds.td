configuration int_to_leds
{
  requires interface counter_output input;
}
implementation
{
  uses int_to_leds_m, leds;

  input = int_to_leds_m.input;
  int_to_leds_m.leds -> leds;
}
