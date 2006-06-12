configuration UseBinaryFoo { }
implementation {
  components Foo, FooUser;

  Foo.X <- FooUser.Xuse;
  Foo.Y -> FooUser.Yuse;
}
