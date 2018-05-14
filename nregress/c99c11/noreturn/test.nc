configuration test {
} implementation {
  components TestP, StopM;
  TestP.Stop -> StopM;
}
