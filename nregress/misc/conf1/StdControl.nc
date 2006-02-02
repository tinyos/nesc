#ifndef FOO
#define FOO
typedef unsigned char result_t;
typedef unsigned char uint8_t;
enum { SUCCESS, FAIL };

result_t rcombine(result_t r1, result_t r2)
/* Returns: FAIL if r1 or r2 == FAIL , r2 otherwise. This is the standard
     combining rule for results
*/
{
  return r1 == FAIL ? FAIL : r2;
}

#endif

interface StdControl
{
  command result_t init();
  command result_t start();
  command result_t stop();
}
