module test { provides interface i; }
implementation
{
  void f() @spontaneous() {
    call i.f(0, 0);
  }

  command void i.f(int *a, int n) {
  }
}

  