
generic module DebugM(
  typedef to_size_type,
  typedef from_size_type,
  typedef upper_count_type,
  typedef frequency_tag )
{
  provides interface CounterBase<to_size_type,frequency_tag> as Counter;
  uses interface CounterBase<from_size_type,frequency_tag> as CounterFrom;
  uses interface MathOps<to_size_type> as MathTo;
  uses interface MathOps<upper_count_type> as MathUpper;
}
implementation
{
  upper_count_type m_upper;

  async command to_size_type Counter.get()
  {
#if 0
    to_size_type x;
    atomic
    {
      x = call MathTo.cast_to( call MathUpper.cast_from( m_upper ) );
    }
    return x;
#else
    atomic
    {
      return call MathTo.cast_to( call MathUpper.cast_from( m_upper ) );
    }
#endif
  }

  async command bool Counter.isOverflowPending()
  {
    // (( ALL CODE REMOVED ))
    return FALSE;
  }

  async command void Counter.clearOverflow()
  {
    // (( ALL CODE REMOVED ))
  }

  async event void CounterFrom.overflow()
  {
    // (( ALL CODE REMOVED ))
  }
}

