// $Id$

includes Timer;

configuration DebugC 
{
}
implementation
{
  components new DebugM(uint32_t,uint16_t,uint16_t,T32khz);
}

