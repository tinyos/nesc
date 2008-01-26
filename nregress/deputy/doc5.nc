struct @count @deputy_scope() @macro("DEPUTY_COUNT") { int n; };

module doc5 { }
implementation {
  /** 
   * @param 'unsigned int *@count(n) a' is something
   * @param n is a length
   */
  void f(int *a, int n) @spontaneous() {
  }
}
