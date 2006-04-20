#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#ifndef MACOS
#include <malloc.h>
#endif

#ifdef NOUNDERSCORE
void da_memory(
#else
void da_memory_(
#endif

  memory_used)

  long *memory_used;
{
#ifndef MACOS
struct mallinfo result;

result=mallinfo();

*memory_used=result.uordblks;
#else
  *memory_used=0;
#endif
}
