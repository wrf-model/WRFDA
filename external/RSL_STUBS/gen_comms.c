#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "protos.h"
#include "registry.h"
#include "data.h"

/* For detecting variables that are members of a derived type */
#define NULLCHARPTR   (char *) 0
static int parent_type;

int
gen_comm_descrips ( char * dirname )
{
  node_t * p ;
  char * fn = "dm_comm_cpp_flags" ;
  char commname[NAMELEN] ;
  char fname[NAMELEN] ;
  FILE * fp ;
  int ncomm ;

  if ( dirname == NULL ) return(1) ;

  if ( strlen(dirname) > 0 ) { sprintf(fname,"%s/%s",dirname,fn) ; }
  else                       { sprintf(fname,"%s",fn) ; }

  if ((fp = fopen( fname , "w" )) == NULL )
  {
    fprintf(stderr,"WARNING: gen_comm_descrips in registry cannot open %s for writing\n",fname ) ;
  }

  ncomm = 1 ;
  for ( p = Halos ; p != NULL ; p = p->next )
  {
    strcpy( commname, p->name ) ;
    make_upper_case(commname) ;
    fprintf(fp,"-D%s=%d\n",commname,ncomm++) ;
  }
  for ( p = Periods ; p != NULL ; p = p->next )
  {
    strcpy( commname, p->name ) ;
    make_upper_case(commname) ;
    fprintf(fp,"-D%s=%d\n",commname,ncomm++) ;
  }
  for ( p = Xposes ; p != NULL ; p = p->next )
  {
    strcpy( commname, p->name ) ;
    make_upper_case(commname) ;
    fprintf(fp,"-D%s=%d\n",commname,ncomm++) ;
  }
  fprintf(fp,"-DWRF_RSL_NCOMMS=%d\n",ncomm-1 ) ;
  return(0) ;
}

int
gen_comms ( char * dirname )
{
  if ( sw_dm_parallel )
    fprintf(stderr,"ADVISORY: RSL version of gen_comms is linked in with registry program.\n") ;

  gen_comm_descrips( "inc" ) ;

  return(0) ;
}

