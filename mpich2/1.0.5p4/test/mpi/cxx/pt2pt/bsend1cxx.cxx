/* -*- Mode: C++; c-basic-offset:4 ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */
#include <iostream.h>
#include "mpi.h"
#include "../../../src/binding/cxx/mpicxx.h"
#include <stdio.h>
#include <stdlib.h>

extern "C" void strncpy( char *, const char *, int );
/* 
 * This is a simple program that tests bsend.  It may be run as a single
 * process to simplify debugging; in addition, bsend allows send-to-self
 * programs.
 */
int main( int argc, char *argv[] )
{
  //    MPI::Comm comm = MPI::COMM_WORLD;
/*
    int dest = 0, src = 0, tag = 1;
    char *buf, *bbuf;
    char msg1[7], msg3[17];
    double msg2[2];
    char rmsg1[64], rmsg3[64];
    double rmsg2[64];
    int errs = 0, rank;
    int bufsize, bsize;
    MPI::Status status;
*/
    int rank;

    MPI::Init( );
    rank = MPI::COMM_WORLD.Get_rank();

    printf( "rank is %d\n", rank );
#if 0
    bufsize = 3 * MPI_BSEND_OVERHEAD + 7 + 17 + 2*sizeof(double);
    buf = (char *)malloc( bufsize );
    MPI_Buffer_attach( buf, bufsize );

    strncpy( msg1, "012345", 7 );
    strncpy( msg3, "0123401234012341", 17 );
    msg2[0] = 1.23; msg2[1] = 3.21;

    if (rank == src) {
	/* These message sizes are chosen to expose any alignment problems */
	comm.Bsend( msg1, 7, MPI::CHAR, dest, tag );
	comm.Bsend( msg2, 2, MPI::DOUBLE, dest, tag );
	comm.Bsend( msg3, 17, MPI::CHAR, dest, tag );
    }

    if (rank == dest) {
	comm.Recv( rmsg1, 7, MPI::CHAR, src, tag, status );
	comm.Recv( rmsg2, 10, MPI::DOUBLE, src, tag, status );
	comm.Recv( rmsg3, 17, MPI::CHAR, src, tag, status  );

	if (strcmp( rmsg1, msg1 ) != 0) {
	    errs++;
	    fprintf( stderr, "message 1 (%s) should be %s\n", rmsg1, msg1 );
	}
	if (rmsg2[0] != msg2[0] || rmsg2[1] != msg2[1]) {
	    errs++;
	    fprintf( stderr, 
	  "message 2 incorrect, values are (%f,%f) but should be (%f,%f)\n",
		     rmsg2[0], rmsg2[1], msg2[0], msg2[1] );
	}
	if (strcmp( rmsg3, msg3 ) != 0) {
	    errs++;
	    fprintf( stderr, "message 3 (%s) should be %s\n", rmsg3, msg3 );
	}
    }

    /* We can't guarantee that messages arrive until the detach */
    MPI_Buffer_detach( &bbuf, &bsize );

    
    MPI::Finalize();
#endif
    return 0;
}
