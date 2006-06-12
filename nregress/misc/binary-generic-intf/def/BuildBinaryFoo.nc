configuration BuildBinaryFoo { }
implementation {
  components Foo, FooImplementation;

  Foo.X -> FooImplementation.X;
  Foo.Y <- FooImplementation.Y;
}
