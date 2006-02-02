//$Id$

//@author Cory Sharp <cssharp@eecs.berkeley.edu>

// The TinyOS Timer structures are discussed in TEP 102.

typedef struct { } TMilli;
typedef struct { } T32khz;
typedef struct { } TMicro;
typedef struct { } TNano;

typedef struct
{
  uint32_t value;
  bool overflow;
} stopwatch_t;

