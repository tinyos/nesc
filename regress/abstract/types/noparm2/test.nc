configuration test { 
}
implementation {
  components new aa() as b1, new aa() as b2, bb;

  bb.b1 -> b1;
  bb.b2 -> b2;
}
