struct @count @deputy_scope() @macro("DEPUTY_COUNT") { int n; };
struct @badcount @macro("DEPUTY_COUNT") { int n; };

module scope8 { }
implementation {
  int n;

  struct fun {
    int *@count(n) a;
    int *@badcount(sizeof(n)) b;
    int n;
  };

  void f() @spontaneous() {
    struct fun *x = 0;
  }
}
