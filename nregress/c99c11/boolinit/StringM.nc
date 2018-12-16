module StringM {
  provides {
    interface BoolInit;
    interface BoolReturn;
  }
}
implementation {
  static const char string[] = "string";
  _Bool value = &string;

  command void BoolInit.test() {
    signal BoolInit.testDone("String", value);
  }

  command _Bool BoolReturn.get() {
    return "string";
  }
}
