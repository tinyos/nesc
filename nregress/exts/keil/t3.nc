sfr x = 0x11;
sbit y = x ^ 2;

module t3 { }
implementation {

  int f() @spontaneous() interrupt 3 using 2
  {
    return x + y;
  }
}
