generic module aa() {
  provides interface i<int> as Basic;
}
implementation {
  command int Basic.bind(int settings){return 0;}
  // etc.
}

