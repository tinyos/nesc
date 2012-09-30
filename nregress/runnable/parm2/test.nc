configuration test { }
implementation {
  components MainP;
  components AppP;

  MainP.Myif -> AppP.Myif[1,2];
  MainP.Pif -> AppP.Myif;
}