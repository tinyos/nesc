module aa {
  provides interface i<int> as Basic[char num];
}
implementation {
  command int Basic.bind[char num](int settings){return 0;}
  // etc.
}

