/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpidimpl.h"

/* FIXME: This routine needs to be factored into finalize actions per module,
   In addition, we should consider registering callbacks for those actions
   rather than direct routine calls.
 */

#include "pmi.h"

#undef FUNCNAME
#define FUNCNAME MPID_Finalize
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPID_Finalize(void)
{
    int mpi_errno = MPI_SUCCESS;
    MPIU_THREADPRIV_DECL;                        /* Required for Nest_incr */
    MPIDI_STATE_DECL(MPID_STATE_MPID_FINALIZE);

    MPIDI_FUNC_ENTER(MPID_STATE_MPID_FINALIZE);

    /*
     * Wait for all posted receives to complete.  For now we are not doing 
     * this since it will cause invalid programs to hang.
     * The side effect of not waiting is that posted any source receives 
     * may erroneous blow up.
     *
     * For now, we are placing a warning at the end of MPID_Finalize() to 
     * inform the user if any outstanding posted receives exist.
     */
     /* FIXME: The correct action here is to begin a shutdown protocol
      * that lets other processes know that this process is now
      * in finalize.  
      *
      * Note that only requests that have been freed with MPI_Request_free
      * are valid at this point; other pending receives can be ignored 
      * since a valid program should wait or test for them before entering
      * finalize.  
      * 
      * The easist fix is to allow an MPI_Barrier over comm_world (and 
      * any connected processes in the MPI-2 case).  Once the barrier
      * completes, all processes are in finalize and any remaining 
      * unmatched receives will never be matched (by a correct program; 
      * a program with a send in a separate thread that continues after
      * some thread calls MPI_Finalize is erroneous).
      * 
      * Avoiding the barrier is hard.  Consider this sequence of steps:
      * Send in-finalize message to all connected processes.  Include
      * information on whether there are pending receives.
      *   (Note that a posted receive with any source is a problem)
      *   (If there are many connections, then this may take longer than
      *   the barrier)
      * Allow connection requests from anyone who has not previously
      * connected only if there is an possible outstanding receive; 
      * reject others with a failure (causing the source process to 
      * fail).
      * Respond to an in-finalize message with the number of posted receives
      * remaining.  If both processes have no remaining receives, they 
      * can both close the connection.
      * 
      * Processes with no pending receives and no connections can exit, 
      * calling PMI_Finalize to let the process manager know that they
      * are in a controlled exit.  
      *
      * Processes that still have open connections must then try to contact
      * the remaining processes.
      * 
      */
    
/* commenting out the close protocol and simply using MPI_Barrier until 
   MPI_Comm_disconnect correctly disconnects all VCs */

    MPIU_THREADPRIV_GET;
    MPIR_Nest_incr();
    mpi_errno = NMPI_Barrier(MPI_COMM_WORLD);
    MPIR_Nest_decr();
    if (mpi_errno) { MPIU_ERR_POP(mpi_errno); }

    mpi_errno = MPID_VCRT_Release(MPIR_Process.comm_self->vcrt,0);
    if (mpi_errno != MPI_SUCCESS) {
	MPIU_ERR_POP(mpi_errno);
    }

    MPID_Dev_comm_destroy_hook(MPIR_Process.comm_world);

    mpi_errno = MPID_VCRT_Release(MPIR_Process.comm_world->vcrt,0);
    if (mpi_errno != MPI_SUCCESS) {
	MPIU_ERR_POP(mpi_errno);
    }

    /* Re-enabling the close step because many tests are failing
     * without it, particularly under gforker */
#if 1
    /* FIXME: The close actions should use the same code as the other
       connection close code */
    MPIDI_PG_Close_VCs();
    /*
     * Wait for all VCs to finish the close protocol
     */
    mpi_errno = MPIDI_CH3U_VC_WaitForClose();
    if (mpi_errno) { MPIU_ERR_POP(mpi_errno); }
#endif

    /* FIXME: Progress finalize should be in CH3_Finalize */
    mpi_errno = MPIDI_CH3I_Progress_finalize();
    if (mpi_errno) { MPIU_ERR_POP(mpi_errno); }

    mpi_errno = MPIDI_CH3_Finalize();
    if (mpi_errno) { MPIU_ERR_POP(mpi_errno); }

    /* Tell the process group code that we're done with the process groups.
       This will notify PMI (with PMI_Finalize) if necessary.  It
       also frees al PG structures, including the PG for COMM_WORLD, whose 
       pointer is also saved in MPIDI_Process.my_pg */
    mpi_errno = MPIDI_PG_Finalize();
    if (mpi_errno) { MPIU_ERR_POP(mpi_errno); }

 fn_exit:
    MPIDI_FUNC_EXIT(MPID_STATE_MPID_FINALIZE);
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}
