generic module Module(int result) {
  provides interface Check;
} implementation {
  command void Check.check() {
    signal Check.checkDone(result);
  }

  default event void Check.checkDone(int result) {
  }
}
