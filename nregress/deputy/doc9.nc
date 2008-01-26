struct @count @deputy_scope() @macro("DEPUTY_COUNT") { int n; };

module doc9 { }
implementation {
  /** 
   * @return 'void *@count(n)'
   */
  void *f(int *a, int n) @spontaneous() {
    return 0;
  }
}
