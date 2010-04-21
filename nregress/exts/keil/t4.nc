int h(void) { return 2; }

module t4 { 
  provides async command int f(); 
}
implementation {

  int g(void) { return 1; }

  async command int f()
  {
    return g() + h();
  }
  
  void hmm() @spontaneous() 
  {
    call f();
  }
}
