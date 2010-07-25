module test { }
implementation {
  int x, y, z;

  int main() @C() @spontaneous() {
    return (x > y) == z;
  }
}
