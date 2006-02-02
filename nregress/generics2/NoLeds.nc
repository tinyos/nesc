module NoLeds {
  provides interface Leds;
}
implementation
{

  async command result_t Leds.init() {
    return SUCCESS;
  }

  async command result_t Leds.redOn() {
    return SUCCESS;
  }

  async command result_t Leds.redOff() {
    return SUCCESS;
  }

  async command result_t Leds.redToggle() {
    return SUCCESS;
  }

  async command result_t Leds.greenOn() {
    return SUCCESS;
  }

  async command result_t Leds.greenOff() {
    return SUCCESS;
  }

  async command result_t Leds.greenToggle() {
    return SUCCESS;
  }

  async command result_t Leds.yellowOn() {
    return SUCCESS;
  }

  async command result_t Leds.yellowOff() {
    return SUCCESS;
  }

  async command result_t Leds.yellowToggle() {
    return SUCCESS;
  }

  async command unsigned char Leds.get() {
    return 0;
  }

  async command result_t Leds.set(unsigned char value) {
    return SUCCESS;
  }
}
