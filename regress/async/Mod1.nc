module Mod1 
{
  provides {
    async command void cmd1();
    async command void cmd2();
    event void evt1();
  }
}
implementation
{
  async command void cmd1() {
    call cmd2();
  }

  async command void cmd2() {
    signal evt1();
  }

  event void evt1() {
  }
}
