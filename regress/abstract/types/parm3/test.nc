configuration test { 
}
implementation {
  components aa, bb;

  bb.b1 -> aa;
  bb.b2 -> aa;
}
