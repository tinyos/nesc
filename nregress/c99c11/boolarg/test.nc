configuration test {
}
implementation {
  components TestP, BoolTestM;

  TestP.BoolTest -> BoolTestM;
}
