typedef unsigned char result_t;
typedef int bool;
enum { FAIL, SUCCESS };
bool TOS_post(void (*tp) ());
typedef uint8_t __nesc_atomic_t;
__nesc_atomic_t __nesc_atomic_start(void) __attribute__((spontaneous))
{
  return 0;
}

void __nesc_atomic_end(__nesc_atomic_t oldSreg) __attribute__((spontaneous))
{
}

struct @empty { };
struct @useful { int x; char *y; };

struct @atmostonce { };
struct @atleastonce { };
struct @exactlyonce { };
