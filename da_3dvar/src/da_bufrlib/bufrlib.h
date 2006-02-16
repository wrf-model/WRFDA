#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
** On certain operating systems, the FORTRAN compiler appends an underscore
** to subprogram names in its object namespace.  Therefore, on such systems,
** a matching underscore must be appended to any C language references to the
** same subprogram names so that the linker can correctly resolve such
** references across the C <-> FORTRAN interface at link time.
*/
#ifdef UNDERSCORE
#define bort_exit  bort_exit_
#define bort	   bort_
#define restd	   restd_
#define numtbd	   numtbd_
#define nemtbb	   nemtbb_
#define wrdesc	   wrdesc_
#define istdesc	   istdesc_
#define uptdd	   uptdd_
#define cadn30	   cadn30_
#define ifxy	   ifxy_
#endif

/*
** In order to ensure that the C <-> FORTRAN interface works properly (and
** portably!), the default size of an "INTEGER" declared in FORTRAN must be 
** identical to that of an "int" declared in C.  If this is not the case (e.g.
** some FORTRAN compilers, most notably AIX via the -qintsize= option, allow the
** sizes of INTEGERs to be definitively prescribed outside of the source code
** itself!), then the following conditional directive (or a variant of it) can
** be used to ensure that the size of an "int" in C remains identical to that
** of an "INTEGER" in FORTRAN.
*/ 
#ifdef F77_INTSIZE_8
    typedef long f77int;
#else
    typedef int f77int;
#endif

/*
** Declare prototypes for ANSI C compatibility.
*/
void bort_exit( void );
void bort( char *, f77int );
void restd( f77int *, f77int *, f77int *, f77int * );
void numtbd( f77int *, f77int *, char *, char *, f77int *, f77int, f77int );
void nemtbb( f77int *, f77int *, char *, f77int *, f77int *, f77int *, f77int );
void wrdesc( f77int, f77int *, f77int * );
f77int istdesc( f77int * );
void uptdd( f77int *, f77int *, f77int *, f77int * );
void cadn30( f77int *, char *, f77int ); 
f77int ifxy( char *, f77int );

/*
** Define the maximum number of FXY descriptors in a fully-expanded Section 3.
*/
#define MAXNC	300
