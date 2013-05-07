#include <stdio.h>
#include <sys/times.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>

#if 0
#ifdef sparc
#include <sys/processor.h>
#include <sys/procset.h>

/* Actually this can break if we're unlucky (context switch between
   rd tick and srlx */
#ifdef __GNUC__
register unsigned long long tscr asm("%o0");
#define tsc() ({ \
    __asm__ __volatile__("rd %%tick,%%o1; srlx %%o1,32,%%o0" \
                          : : : "%o0", "%o1"); \
    tscr; })
#else
#define tsc() 0
#endif

static double proc_frequency(void)
{
  processor_info_t pinfo;

  processor_info(0, &pinfo);
  return pinfo.pi_clock * 1e6;
}
#endif

#ifdef i386
#include <asm/msr.h>

#define tsc() ({unsigned long long x; rdtscll(x); x; })
#define rdpmcll(counter,x) \
     __asm__ __volatile__("rdpmc" \
                          : "=A" (x) \
                          : "c" (counter))
#define pmc0() ({unsigned long long x; rdpmcll(0, x); x; })
#define pmc1() ({unsigned long long x; rdpmcll(1, x); x; })

double proc_frequency(void) { return 550e6; }
#endif


static struct tms s, e;
unsigned long long start_ticks, end_ticks;

static double ll_to_double(unsigned long long x)
{
  return 4294967296.0 * (unsigned long)(x >> 32) + (unsigned long)x;
}

static void print_memory_usage(void)
{
#if 0
  fprintf(stderr, "blocks alloced: %lu, %.1f%% 8K\n",
          (unsigned long)(total_8kblocks + total_otherblocks),
          (100.0 * total_8kblocks) / (total_8kblocks + total_otherblocks));

  fprintf(stderr, "system bytes(kB): %lu\n",
          ((unsigned long)total_system_bytes + 512) / 1024);
  fprintf(stderr, "overhead: %.1f%%\n",
          total_system_bytes * 100.0 / bytes.max - 100);

#endif
  { extern void malloc_stats(void); malloc_stats(); }
  fflush(stderr);
}

static void print_benchmark(void)
{
  double tt, pfreq;

  end_ticks = tsc();
  times(&e);
  pfreq = proc_frequency();

  tt = ll_to_double(end_ticks - start_ticks) / pfreq;

  fprintf(stderr, "runtime: %.3f\n", tt);
  fprintf(stderr, "cputime: %.2f\n", (e.tms_utime - s.tms_utime) / 100.0);
  print_memory_usage();
}

static void benchmark_init(void)
{
  fputs("heap: compat regions\n", stderr);
  times(&s);
  start_ticks = tsc();
  atexit(print_benchmark);
}
#else
static void benchmark_init(void) {}

#endif
