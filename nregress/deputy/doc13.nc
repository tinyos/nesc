struct @count @deputy_scope() @macro("DEPUTY_COUNT") { int n; };

module doc13 { }
implementation {
  /** 
   * @param 'int *@count(n) a' haha
   */
  void (*f(int *a, int n))(int) @spontaneous() {
    return 0;
  }
}
