module int_to_rfm_m 
{
  requires {
    interface counter_output input;
    interface send_msg send;
  }
}
implementation
{
  typedef struct mymsg {
    char val;
    int src;
  } int_to_led_msg;

  char pending;
  struct TOS_Msg data;

  command void input.init()
  {
  }

  command void input.output(int value)
  {
    int_to_led_msg* message = (int_to_led_msg*)data.data;

    if (!pending) 
      {
	pending = 1;
	message->val = value;
	message->src = TOS_LOCAL_ADDRESS;
	if (!call send.send(TOS_BCAST_ADDR, &data))
	  pending = 0;
      }
  }

  event char send.send_done(struct TOS_Msg *msg)
  {
    if (pending && msg == &data)
      {
	pending = 0;
	signal input.output_complete(1);
      }
  }
}
