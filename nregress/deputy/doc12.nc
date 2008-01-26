struct @count @deputy_scope() @macro("DEPUTY_COUNT") { int n; };

module doc12 { }
implementation {
  /** 
   * @return 'void (*@count(n))(int)'
   */
  void (*f(int *a, int n))(int) @spontaneous() {
    return 0;
  }
}
