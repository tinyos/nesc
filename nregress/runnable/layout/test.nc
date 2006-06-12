configuration test { }
implementation {
#define T(n, t) components new docheck(n, sizeof(t), __alignof__(t), t) as check ## n

  T(1, int);
  T(2, char);
  T(3, short);
  T(4, long);
  T(5, long long);
  T(6, size_t);
  T(7, double);

  T(11, struct { int a; });
  T(12, struct { char a; });
  T(13, struct { short a; });
  T(14, struct { long a; });
  T(15, struct { long long a; });
  T(16, struct { size_t a; });
  T(17, struct { double a; });

  T(100, struct { char a; });
  T(101, struct { char a : 1; int b : 12; });
  T(102, struct { char a : 2; int : 0; char x : 3; });
  T(103, struct { char a : 3; char : 0; char z : 5; });
  T(104, struct { union { int x; double y; }; char a; });
  T(105, struct { union { char x; short y; }; char a; });
  T(106, struct { union { char x; long long y; }; char a; });
  T(107, union { char x; double y; });
  T(108, struct { char __attribute__((aligned(16))) a; });
  T(109, struct { char a __attribute__((aligned(16))); });
}
