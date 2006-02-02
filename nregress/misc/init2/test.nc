module test {}
implementation {
int (*f)[];
 int aa[] = { 1, 2 , 545 }; 

extern int (*(__attribute__((xx)) f2)[])[];

int g[5];

int (*(__attribute__((xx)) f2)[])[] = { &g };

 char z[] = "aa33"; 
 char z2[] = {"aa33"}; 

void h() __attribute__((spontaneous)) {
  f = &g;
  aa;
  f2;
  z;z2;
}
}
