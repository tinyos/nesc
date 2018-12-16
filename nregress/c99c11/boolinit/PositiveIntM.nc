module PositiveIntM {
  provides {
    interface BoolInit;
    interface BoolReturn;
  }
}
implementation {
  _Bool value = (unsigned int)1234;

  command void BoolInit.test() {
    signal BoolInit.testDone("PositiveInt", value);
  }

  command _Bool BoolReturn.get() {
    return (unsigned int)1234;
  }
}
