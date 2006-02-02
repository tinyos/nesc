includes bb;
module test { } implementation { 
  void g() __attribute__((spontaneous)) { f(); }
}
