module counter {
  provides {
    interface simple_init init;
    interface counter_output output;
  }
  requires {
    interface clock;
  }
}
implementation {
  int state;

  command char init.init()
  {
    state = 0;
    call output.init();
    return 1;
  }

  command char init.start()
  {
    call clock.set_rate(1,2);
    return 1;
  }

  event void clock.fire()
  {
    state++;
    call output.output(state);
  }

  event char output.output_complete(char success) 
  {
    return 1;
  }
}

