/************************************************************************
C* BORT_EXIT                                                            *
C*                                                                      *
C* This c subroutine will terminate the application program and return  *
C* an implementation-defined non-zero status code to the executing      *
C* shell script.                                                        *
C*                                                                      *
C* BORT_EXIT                                                            *
C*                                                                      *
C**                                                                     *
C* Log:                                                                 *
C* J. Ator/NCEP         04/03   Original author                         *
C*                                                                      *
C* Remarks:                                                             *
C*    This routine calls: None                                          *
C*    This routine is called by: BORT, BORT2 - normally not called by   *
C*                               any application programs but it        *
C*                               could be                               *
C***********************************************************************/

#include <stdlib.h>

/* depending on the operating system, may need to append an underscore */
#ifdef UNDERSCORE
#define bort_exit bort_exit_
#endif

/* declare prototype for ANSI C compatibility */
void bort_exit( void );

void bort_exit( )

{
    exit( EXIT_FAILURE );
}

