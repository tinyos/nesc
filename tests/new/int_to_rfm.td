configuration int_to_rfm
{
  requires interface counter_output input;
}
implementation
{
  uses int_to_rfm_m, generic_comm;

  input = int_to_rfm_m.input;
  int_to_rfm_m.send -> generic_comm.send_msg[4];
}

