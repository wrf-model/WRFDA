/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include <mpiimpl.h>
#include <mpid_dataloop.h>
#include <stdlib.h>

/*@
  MPID_Type_commit
 
  Input Parameters:
. datatype_p - pointer to MPI datatype

  Output Parameters:

  Return Value:
  0 on success, -1 on failure.
@*/
int MPID_Type_commit(MPI_Datatype *datatype_p)
{
    int           mpi_errno=MPI_SUCCESS;
    MPI_Aint      first, last;
    MPID_Datatype *datatype_ptr;
    MPID_Segment  *segp;

    MPIU_Assert(HANDLE_GET_KIND(*datatype_p) != HANDLE_KIND_BUILTIN);

    MPID_Datatype_get_ptr(*datatype_p, datatype_ptr);

    if (datatype_ptr->is_committed == 0) {
	datatype_ptr->is_committed = 1;

	/* determine number of contiguous blocks in the type */
	segp = MPID_Segment_alloc();
        /* --BEGIN ERROR HANDLING-- */
        if (!segp)
        {
            mpi_errno = MPIR_Err_create_code(MPI_SUCCESS,
                                             MPIR_ERR_RECOVERABLE,
                                             "MPID_Type_commit",
                                             __LINE__,
                                             MPI_ERR_OTHER,
                                             "**nomem",
                                             0);
            return mpi_errno;
        }
        /* --END ERROR HANDLING-- */
	MPID_Segment_init(0, 1, *datatype_p, segp, 0); /* first 0 is bufptr,
							* 1 is count
							* last 0 is homogeneous
							*/

	first = 0;
	last  = SEGMENT_IGNORE_LAST;

	MPID_Segment_free(segp);

	MPIU_DBG_PRINTF(("# contig blocks = %d\n",
			 (int) datatype_ptr->n_contig_blocks));

#ifdef USE_MEMORY_TRACING
	if ( ((unsigned int)datatype_ptr->hetero_dloop & 0xFFFFFFFF ) == 0xefefefef) {
	    mpi_errno = MPIR_Err_create_code( MPI_SUCCESS, MPIR_ERR_FATAL,
					      __FILE__, __LINE__, 
					      MPI_ERR_OTHER, "**fail",
					      "**fail %s", "hetero_dloop not set" );
	}
#endif
#if 0
	MPIDI_Dataloop_dot_printf(datatype_ptr->dataloop, 0, 1);
#endif
    }

    return mpi_errno;
}

