module UserP
{
  uses interface Get;
}
implementation
{
  int main() @C() @spontaneous() {
    uint16t *data;

    data = call Get.get();
		
    *data = 1;

    return 0;
  }
}
