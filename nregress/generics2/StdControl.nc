#ifndef FOO
#define FOO
typedef unsigned char result_t;
enum { SUCCESS, FAIL };
#endif

interface StdControl
{
  command result_t init();
  command result_t start();
  command result_t stop();
}
