module Agood {
  requires interface X;
}
implementation {
  command X.bar() {
  }

  event X.barDone() {
  }
}
