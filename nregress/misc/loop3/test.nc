configuration test {
  provides interface t2;
}
implementation
{
  components t2;

  t2 = t2.t2;
}
