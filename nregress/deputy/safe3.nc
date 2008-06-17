module safe3 @safe() { }
implementation {
  void f() @spontaneous() @unsafe() {
  }
}
