configuration test {
} implementation {
  components TestP;

  components new Module(0 == 1 && 0 > 1) as And00;
  components new Module(0 == 1 && 0 < 1) as And01;
  components new Module(0 != 1 && 0 > 1) as And10;
  components new Module(0 != 1 && 0 < 1) as And11;
  components new Module(0 == 1 || 0 > 1) as Or00;
  components new Module(0 == 1 || 0 < 1) as Or01;
  components new Module(0 != 1 || 0 > 1) as Or10;
  components new Module(0 != 1 || 0 < 1) as Or11;

  TestP.And00 -> And00;
  TestP.And01 -> And01;
  TestP.And10 -> And10;
  TestP.And11 -> And11;
  TestP.Or00 -> Or00;
  TestP.Or01 -> Or01;
  TestP.Or10 -> Or10;
  TestP.Or11 -> Or11;
}
