/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*  $Id: atomic_fai.c,v 1.1 2003/04/30 19:09:25 toonen Exp $
 *
 *  (C) 2002 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */
/* 
 * Test performance of atomic access operations.
 * This is a *very* simple test.
 */

#include "stdio.h"
#include "unistd.h"

#define MPID_Atomic_fetch_and_inc(count_ptr_, count_old_)						\
    __asm__ __volatile__ ("0: movl %0, %%eax;"								\
			  "movl %%eax, %%ebx;"								\
			  "incl %%ebx;"									\
			  "lock; cmpxchg %%ebx, %0;"							\
			  "jnz 0b;"									\
			  "movl %%eax, %1"								\
			  : "+m" (*count_ptr_), "=q" (count_old_) :: "memory", "cc", "eax", "ebx")

int main( int argc, char **argv )
{
    volatile int count = 0;
    int count_old;
    int failures = 0;
    int i;
    
    for (i = 0; i < 1000; i++)
    {
	MPID_Atomic_fetch_and_inc(&count, count_old);
	if (count_old != i )
	{
	    fprintf(stderr, "count_old=%d, should be %d\n", count_old, i);
	    failures++;
	}
	if (count != i + 1 )
	{
	    fprintf(stderr, "count=%d, should be %d\n", count, i + 1);
	    failures++;
	}
    }
    
    MPID_Atomic_fetch_and_inc(&count, count_old);

    if (failures == 0)
    {
	printf("No Errors\n");
    }
    else
    {
	printf("%d errors encountered\n", failures);
    }
    
    exit(0);
}
