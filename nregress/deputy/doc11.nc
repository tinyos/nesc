struct @count @deputy_scope() @macro("DEPUTY_COUNT") { int n; };

/** 
 * @return 'void *@count(n)'
 */
void *g(int n);

void *g(int n) {
  return 0;
}

module doc11 { }
implementation {
  /** 
   * @return 'void *@count(n)'
   */
  void *f(int *a, int n) @spontaneous() {
    return g(n);
  }
}
