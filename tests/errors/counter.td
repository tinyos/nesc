component counter {
  provides {
    interface simple_init init;
    interface counter_output output;
  }
  requires {
    interface clock;
  }
}
