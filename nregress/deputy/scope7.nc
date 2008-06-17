struct @count @deputy_scope() @macro("DEPUTY_COUNT") { int n; };

module scope7 { }
implementation {
  void *@count(n) f(int *a, int n) @spontaneous() {
    return 0;
  }
}
