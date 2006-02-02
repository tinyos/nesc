typedef int __nesc_atomic_t;
inline void __nesc_enable_interrupt() 
{ 
}
inline __nesc_atomic_t __nesc_atomic_start(void) __attribute__((spontaneous))
{
  return 0;
}

inline void __nesc_atomic_end(__nesc_atomic_t oldSreg) __attribute__((spontaneous))
{
}
