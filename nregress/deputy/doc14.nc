struct @count @deputy_scope() @macro("DEPUTY_COUNT") { int n; };

module doc14 { }
implementation {
  /** 
   * @para 'int *@count(n) b' is something
   * @param n is a length
   */
  void f(int *a, int n) @spontaneous() {
  }

  /**
   * @param 'int' depf
   */
  int x;
}
