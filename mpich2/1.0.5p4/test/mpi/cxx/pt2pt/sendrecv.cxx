/* -*- Mode: C++; c-basic-offset:4 ; -*- */
/*  
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 *
 */
/* style: c++ header */

/*
 * Simple test program for C++ binding
 */

#include <stdio.h>
#include "mpi.h"

int main( int argc, char *argv[] )
{
    int rank, size;

    MPI::Init();

    rank = MPI::COMM_WORLD.Get_rank();
    size = MPI::COMM_WORLD.Get_size();

    if (size < 2) {
	fprintf( stderr, "Size of comm_world must be at least 2\n" );
	MPI::COMM_WORLD.Abort(1);
    }
    if (rank == 0) {
	int *buf = new int[100];
	int i;
	for (i=0; i<100; i++) buf[i] = i;
	MPI::COMM_WORLD.Send( buf, 100, MPI::INT, size-1, 0 );
    }
    else if (rank == size - 1) {
	int *buf = new int[100];
	int i;
	MPI::COMM_WORLD.Recv( buf, 100, MPI::INT, 0, 0 );
	for (i=0; i<100; i++) {
	    if (buf[i] != i) {
		fprintf( stderr, "Error: buf[%d] = %d\n", i, buf[i] );
	    }
	}
    }
    if (rank == 0) {
	printf( " No Errors\n" );
    }

    MPI::Finalize();
    return 0;
}
