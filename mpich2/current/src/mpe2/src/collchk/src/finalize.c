/*
   (C) 2004 by Argonne National Laboratory.
       See COPYRIGHT in top-level directory.
*/
#include <stdio.h>
#include "mpi.h"

int MPI_Finalize( void )
{
    fprintf( stdout, "Done with MPI Collective and Datatype Checking!\n" );
    return PMPI_Finalize();
}
