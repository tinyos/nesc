generic module AmAddressAdapterP() {
  provides interface Attribute<int> as Address;
  uses {
    async command int amAddress();
    async command void setAmAddress(int a);
  }
}
implementation {
  
  command int Address.get() { return call amAddress(); }

  command int Address.set(int val) { 
    call setAmAddress(val);
    return 0; 
  }
}
