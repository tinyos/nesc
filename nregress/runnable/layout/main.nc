module main {
  uses interface i as run;
}
implementation {
  int main(int argc, char **argv) @C() @spontaneous() {
    call run.check();
    return 0;
  }
}
