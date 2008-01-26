struct @count @deputy_scope() @macro("DEPUTY_COUNT") { int n; };

module doc8 { }
implementation {
  /** 
   * @param 'int *@count(n) b' is something
   * @param n is a length
   */
  void f(int *a, int n) @spontaneous() { bad;
  }
}
