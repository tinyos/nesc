module NoneZeroFloatM {
  provides {
    interface BoolInit;
    interface BoolReturn;
  }
}
implementation {
  _Bool value = 3.456;

  command void BoolInit.test() {
    signal BoolInit.testDone("NonZeroFloat", value);
  }

  command _Bool BoolReturn.get() {
    return 3.456;
  }
}
