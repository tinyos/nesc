module m1 {
  provides interface i1[int x];
} 
implementation
{
  command void i1.f[int x]() { }
}