module ActiveMessageAddressC  {
  provides async command int amAddress();
  provides async command void setAmAddress(int a);
}
implementation {
  async command int amAddress() {
    return 0;
  }

  async command void setAmAddress(int a) {
  }
}
