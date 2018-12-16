module NegativeIntM {
  provides {
    interface BoolInit;
    interface BoolReturn;
  }
}
implementation {
  _Bool value = -9876;

  command void BoolInit.test() {
    signal BoolInit.testDone("NegativeInt", value);
  }

  command _Bool BoolReturn.get() {
    return -9876;
  }
}
