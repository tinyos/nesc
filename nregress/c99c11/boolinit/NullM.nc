module NullM {
  provides {
    interface BoolInit;
    interface BoolReturn;
  }
}
implementation {
  _Bool value = (void *)0;

  command void BoolInit.test() {
    signal BoolInit.testDone("Null", value);
  }

  command _Bool BoolReturn.get() {
    return (void *)0;
  }
}
