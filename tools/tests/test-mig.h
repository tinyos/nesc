typedef struct IntMsg {
  char val;
  int src;
  int x1[5];
  int x2[2][4];
  struct {
    int a : 2;
    int : 0;
    float z;
  } aa;
  int x3;
  struct {
    int m;
    int x4[20];
    int n;
    struct {
      int zz;
    } cc[3][5];
    int nn;
  } bb[7];
  float done;
} IntMsg;

enum {
  AM_INTMSG = 4
};
