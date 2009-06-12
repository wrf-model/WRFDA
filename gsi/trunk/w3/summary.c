/***************************************************************

This code will make a system call to return various
useful parameters.  When subroutine summary is called, a list
of system resource statistics is printed to stdout.

Users need to place a call to start() at the beginning of the
section of code to be "measured" and a call to summary() at the end.

Use as follows:

call start()
   do stuff
call summary()

Jim Tuccillo August 1999

***************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/utsname.h>

void summary_( returnVal  )
int * returnVal;
{

  return;
}

void start_()
{

  return;
}

