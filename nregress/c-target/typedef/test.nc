configuration test
{
}
implementation
{
  components UserP, ProviderP;

  UserP.Get -> ProviderP;
}
