#ifndef CRAY
# ifdef NOUNDERSCORE
#      define GET_TERRAIN get_terrain
# else
#   ifdef F2CSTYLE
#      define GET_TERRAIN get_terrain__
#   else
#      define GET_TERRAIN get_terrain_
#   endif
# endif
#endif
#include <stdio.h>

int GET_TERRAIN (        float *adx,
                         float *xlat,
                         float *xlon,
                         float       *terrain,
                         int   *mix,
                         int   *mjx,
                         int   *iyyn,
                         int   *jxxn,
                         int   *ipath , int * ipathlen)  /* integer coded ASCII string from Funtran and len */

{
 fprintf(stderr, "Stub to get wrfvar to compile on Cray. Needs real code to be   \n" ) ;
 fprintf(stderr, "ported                                                          \n" ) ;
 fprintf(stderr, "***************************************************************\n" ) ;
 return(0) ;
}
