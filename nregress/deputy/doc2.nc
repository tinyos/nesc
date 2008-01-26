struct @count @deputy_scope() @macro("DEPUTY_COUNT") { int n; };

module doc2 { }
implementation {
  /** 
   * @param a is something
   * @param n is a length
   */
  void f(int *@count(n) a, int n) @spontaneous() {
  }
}
