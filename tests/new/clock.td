module clock {
  provides interface clock;
}
implementation 
{
  command void clock.set_rate(char interval, char scale) { }
}
