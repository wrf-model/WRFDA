/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpidi_ch3_impl.h"

/* FIXME: Who uses this routine?  Should it be a no-op (no routine even)? */
/* FIXME: Should this use the common func enter/exit macros? */
#undef FUNCNAME
#define FUNCNAME MPIDI_CH3_Finalize
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3_Finalize( void )
{
    int mpi_errno = MPI_SUCCESS;

    MPIU_DBG_MSG(CH3_CHANNEL,VERBOSE,"Entering/exiting MPIDI_CH3_Finalize");
    return mpi_errno;
}
