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

  int *memory_used;
{

#if !(defined(vpp) || defined(vpp2) || defined(SUN) || defined(crayx1) || defined(MACOS))
struct mallinfo result;

result=mallinfo();

*memory_used=result.uordblks;
#else
  *memory_used=0;
#endif
}
