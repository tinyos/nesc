module leds {
  provides interface leds;
}
implementation
{
  command char leds.init() { }
  command char leds.red_on() { }
  command char leds.red_off() { }
  command char leds.red_toggle() { }
  command char leds.green_on() { }
  command char leds.green_off() { }
  command char leds.green_toggle() { }
  command char leds.yellow_on() { }
  command char leds.yellow_off() { }
  command char leds.yellow_toggle() { }
}
