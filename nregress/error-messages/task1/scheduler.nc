configuration scheduler {
  provides interface T[int id];
}
implementation {
  components s1, s2, new s3() as t1, new s3() as t2;

  T = s1;
}
