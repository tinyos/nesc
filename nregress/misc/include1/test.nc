#include "x.h"

configuration test { }
// This #define should not be saved (if it is, bb.nc will get a syntax error)
#define a enum 
implementation {
  components bb;

}
