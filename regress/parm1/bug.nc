configuration bug {
}
implementation {
  components bugm, bugn;

  bugm.SendMsg[0] -> bugn.SendMsg[0];
  bugm.SendMsg[1] -> bugn.SendMsg[1];
  bugm.SendMsg[2] -> bugn.SendMsg[2];
}
