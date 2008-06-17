struct @count @deputy_scope() @macro("DEPUTY_COUNT") { int n; };

module scope6 { }
implementation {
  void *@count(k) f(int *a, int n) @spontaneous() {
    return 0;
  }
}
