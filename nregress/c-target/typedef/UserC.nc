
configuration UserC
{
}

implementation
{
	components MainC, UserP, ProviderP;

	UserP.Boot -> MainC;
	UserP.Get -> ProviderP;
}
