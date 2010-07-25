module ProviderP
{
  provides interface Get;
}
implementation
{
  uint16_t data;

  command uint16t* Get.get()
  {
    return &data;
  }
}
