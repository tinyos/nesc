module m2 {
  requires interface i1[int x];
} 
implementation
{
  void silly() 
    {
      call i1.f[5]();
    }
}
