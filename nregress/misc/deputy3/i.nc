struct @count @deputy_scope() @macro("DEPUTY_COUNT") { int n; };

interface i
{
  /** 
   * @param 'int *@count(n) a' is something
   * @param n is a length
   */
  command void f(int *a, int n);
}

  