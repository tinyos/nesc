generic configuration TemInst() {
  provides interface Tem<int> as XX;
}
implementation {
  components TemM;
  XX = TemM.YY[0];
}

