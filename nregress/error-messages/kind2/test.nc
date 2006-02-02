configuration test { provides interface Timer; uses interface Timer as T; 
		   uses interface TimerC;
}
implementation {
  Timer = T;
}
