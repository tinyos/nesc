int oops(void) __asm ("""_main") __attribute((__nothrow__));

module test {}
implementation {
  int main(int argc, char **argv) @spontaneous() @C() {
    if (argc < 0)
      oops();
    return 0;
  }
}

