generic module mod(typedef t @integer()) {
}
implementation {
 void barf()
 {
   t a = ((t)1) << 1;
 }
}
