module generic_comm
{
  provides {
    interface send_msg[unsigned char id];
    interface receive_msg[unsigned char id];
  }
}
implementation
{
  default event char send_msg.send_done[unsigned char id](struct TOS_Msg *msg)
    {
      return 1;
    }

  command char send_msg.send[unsigned char id](short address, struct TOS_Msg *msg) 
    { 
      signal send_msg.send_done[id](msg);
      return 0;
    }
}
