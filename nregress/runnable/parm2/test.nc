configuration MainC {

}

implementation {
  components MainP;
  components AppP;

  MainP.Myif -> AppP.Myif[1,2];

}