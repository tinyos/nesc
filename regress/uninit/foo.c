char a;

static inline char rcombine(char r1, char r2)
{
  return r1 ? 0 : r2;
}

static inline char Mod1_xx(void)
{
  int i;

  for (i = 0; i < 6; i++) a += i;

  return 1;
}

static inline char Mod1_StdControl_init(void)
{
  char result1;
  char result2;

  result1 = rcombine(2, 1);
  result2 = Mod1_xx();
  result1 = rcombine(result1, result2);
  return result1;
}

int   main(void)
{
  Mod1_StdControl_init();
  return 0;
}

