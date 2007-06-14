module badtype { }
implementation {
  void f() {
    int x = *(volatile unsighned char *)23;
  }
}
