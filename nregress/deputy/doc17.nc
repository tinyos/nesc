struct @count @deputy_scope() @macro("DEPUTY_COUNT") { int n; };

module doc17 { }
implementation {
  /** 
   * @return 'void *@count(k)'
   */
  void *f(int *a, int n) @spontaneous() {
    return 0;
  }
}
