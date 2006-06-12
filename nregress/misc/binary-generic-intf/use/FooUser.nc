module FooUser {
  uses interface A<int> as Xuse;
  provides interface A<int> as Yuse;
}
implementation {

  int main() __attribute__((C, spontaneous)) {
    call Xuse.request();
    signal Yuse.done(5);
  }

  command int Yuse.request() {
    return 2;
  }

  event void Xuse.done(int val) {
  }
}
