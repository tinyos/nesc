module ZeroIntM {
  provides {
    interface BoolInit;
    interface BoolReturn;
  }
}
implementation {
  _Bool value = 0;

  command void BoolInit.test() {
    signal BoolInit.testDone("ZeroInt", value);
  }

  command _Bool BoolReturn.get() {
    return 0;
  }
}
