component Foo {
  // Note that the uses, provides are reversed from the definition
  // of Foo (and FooImplementation) above!
  uses interface A as X;
  provides interface A as Y;
}

