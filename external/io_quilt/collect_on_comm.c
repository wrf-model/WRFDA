#include <stdio.h>
#include <stdlib.h>
#include <mpi.h>

#ifndef CRAY
# ifdef NOUNDERSCORE
#      define COLLECT_ON_COMM  collect_on_comm
#      define INT_PACK_DATA  int_pack_data
#      define INT_GET_TI_HEADER_C  int_get_ti_header_c
#      define INT_GEN_TI_HEADER_C  int_gen_ti_header_c
# else
#   ifdef F2CSTYLE
#      define COLLECT_ON_COMM  collect_on_comm__
#      define INT_PACK_DATA  int_pack_data__
#      define INT_GET_TI_HEADER_C  int_get_ti_header_c__
#      define INT_GEN_TI_HEADER_C  int_gen_ti_header_c__
#   else
#      define COLLECT_ON_COMM  collect_on_comm_
#      define INT_PACK_DATA  int_pack_data_
#      define INT_GET_TI_HEADER_C  int_get_ti_header_c_
#      define INT_GEN_TI_HEADER_C  int_gen_ti_header_c_
#   endif
# endif
#endif

COLLECT_ON_COMM ( int * comm, int * typesize ,
                 void * inbuf, int *ninbuf , void * outbuf, int * noutbuf )
{
  int mytask, ntasks, p ;
  int *recvcounts ;
  int *displace ;
  int noutbuf_loc ;

  MPI_Comm_size ( *comm, &ntasks ) ;
  MPI_Comm_rank ( *comm, &mytask ) ;
  recvcounts = (int *) malloc( ntasks * sizeof(int)) ;
  displace   = (int *) malloc( ntasks * sizeof(int)) ;

  /* collect up recvcounts */
  MPI_Gather( ninbuf , 1 , MPI_INT , recvcounts , 1 , MPI_INT , ntasks-1 , *comm ) ;

  if ( mytask == ntasks-1 ) {

    /* figure out displacements */
    for ( p = 1 , displace[0] = 0 , noutbuf_loc = recvcounts[0] ; p < ntasks ; p++ ) {
      displace[p] = displace[p-1]+recvcounts[p-1] ;
      noutbuf_loc = noutbuf_loc + recvcounts[p] ;
    }

    if ( noutbuf_loc > * noutbuf )
    {
      fprintf(stderr,"FATAL ERROR: collect_on_comm: noutbuf_loc (%d) > noutbuf (%d)\n",
		      noutbuf_loc , * noutbuf ) ; 
      fprintf(stderr,"WILL NOT perform the collection operation\n") ;
      MPI_Abort(MPI_COMM_WORLD,1) ;
    }

    /* multiply everything by the size of the type */
    for ( p = 0 ; p < ntasks ; p++ ) {
      displace[p] *= *typesize ;
      recvcounts[p] *= *typesize ;
    }
  }

  MPI_Gatherv( inbuf  , *ninbuf * *typesize  , MPI_CHAR ,
               outbuf , recvcounts , displace, MPI_CHAR ,
               ntasks-1 , *comm ) ;

  free(recvcounts) ;
  free(displace) ;
  return(0) ;
}

