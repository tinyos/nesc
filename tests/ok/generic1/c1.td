configuration c1 { }
implementation
{
  uses m1, m2;

  m2.i1[2] -> m1.i1[10];
}
