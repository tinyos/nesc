component generic_comm
{
  provides {
    interface send_msg[unsigned char id];
    interface receive_msg[unsigned char id];
  }
}
