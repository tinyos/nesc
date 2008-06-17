module test
{
  provides interface i[int a];
}
implementation
{
  command void i.f[int a](int j) { }

  void g() @spontaneous() { call i.f[0](0); }
}
