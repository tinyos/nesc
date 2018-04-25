module ZeroFloatM {
  provides {
    interface BoolInit;
    interface BoolReturn;
  }
}
implementation {
  _Bool value = 0.00;

  command void BoolInit.test() {
    signal BoolInit.testDone("ZeroFloat", value);
  }

  command _Bool BoolReturn.get() {
    return 0.00;
  }
}
