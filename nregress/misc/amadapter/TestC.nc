includes AM;

configuration TestC {
  provides interface Attribute<int> as Address;
}
implementation {
  components ActiveMessageAddressC;
  components new AmAddressAdapterP();
  Address = AmAddressAdapterP;
  AmAddressAdapterP.amAddress -> ActiveMessageAddressC;
  AmAddressAdapterP.setAmAddress -> ActiveMessageAddressC;
}
