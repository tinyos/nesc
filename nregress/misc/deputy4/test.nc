struct @count @deputy_scope() @macro("DEPUTY_COUNT") { int n; };

/** 
 * @param 'int *@count(n) a' is something
 * @param n is a length
 * @return 'int *@count(n)' is off
 */
int *g(int *a, int n);

module test @safe() { provides interface i; }
implementation
{
  void f() @spontaneous() {
    call i.f(0, 0);
    g(0, 0);
  }

  command int *i.f(int *a, int n) {
    return a;
  }
}

  