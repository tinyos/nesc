module main {
  uses interface sc(int, int) as sc1;
  uses interface sc(int, char *) as sc2;
}
implementation {
  void entry() __attribute((spontaneous)) {
    int b = call sc1.init(55, 2);
    int b = call sc2.init("aa", 2);
  }
}
