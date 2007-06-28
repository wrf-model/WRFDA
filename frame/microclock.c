#include <sys/time.h>

/* TBH:  shamelessly borrowed from RSL and RSL_LITE */

/*
   Note that when using these macros, you *HAVE* to leave a space
   between the routine name and its argument list.  Example:

         RSL_C_COMPUTE_PROC (P)

   This applies both to declaration and use from C.
*/

/* Only include this definition if RSL or RSL_LITE is not used */
/* TODO:  At present DM_PARALLEL implies RSL or RSL_LITE.  This may */
/* TODO:  not always be true. */
#ifndef DM_PARALLEL

#ifndef CRAY
# ifdef NOUNDERSCORE
#     define RSL_INTERNAL_MICROCLOCK rsl_internal_microclock
# else
#   ifdef F2CSTYLE
#     define RSL_INTERNAL_MICROCLOCK rsl_internal_microclock__
#   else
#     define RSL_INTERNAL_MICROCLOCK rsl_internal_microclock_
#   endif
# endif
#endif

RSL_INTERNAL_MICROCLOCK ()
{
    struct timeval tb ;
    struct timezone tzp ;
    int isec ;  /* seconds */
    int usec ;  /* microseconds */
    int msecs ;
    gettimeofday( &tb, &tzp ) ;
    isec = tb.tv_sec ;
    usec = tb.tv_usec ;
    msecs = 1000000 * isec + usec ;
    return(msecs) ;
}

#endif

