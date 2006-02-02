typedef int tt __attribute__((combine (tc)));

tt tc(tt x1, tt x2) { return 0; }

typedef int tt2 __attribute__((combine (tc2)));

tt2 tc2(tt2 x1, tt2 x2) { return 0; }
