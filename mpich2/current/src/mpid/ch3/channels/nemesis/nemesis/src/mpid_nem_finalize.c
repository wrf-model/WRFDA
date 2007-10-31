/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2006 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpid_nem_impl.h"
#include "mpid_nem_nets.h"

#undef FUNCNAME
#define FUNCNAME MPID_nem_finalize
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPID_nem_finalize()
{
    int mpi_errno = MPI_SUCCESS;
    int pmi_errno;

    /* this test is not the right one */
/*     MPIU_Assert(MPID_nem_queue_empty( MPID_nem_mem_region.RecvQ[MPID_nem_mem_region.rank])); */
    
    mpi_errno = MPID_nem_net_module_finalize();
    if (mpi_errno) MPIU_ERR_POP (mpi_errno);
    mpi_errno = MPID_nem_detach_shared_memory (MPID_nem_mem_region.memory.base_addr, MPID_nem_mem_region.memory.max_size);    
    if (mpi_errno) MPIU_ERR_POP (mpi_errno);

#ifdef PAPI_MONITOR
    my_papi_close();
#endif /*PAPI_MONITOR */

    pmi_errno = PMI_Barrier();
    MPIU_ERR_CHKANDJUMP1 (pmi_errno != PMI_SUCCESS, mpi_errno, MPI_ERR_OTHER, "**pmi_barrier", "**pmi_barrier %d", pmi_errno);

 fn_exit:
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}

int
MPID_nem_ckpt_shutdown()
{
  MPID_nem_net_module_ckpt_shutdown();
  munmap (MPID_nem_mem_region.memory.base_addr, MPID_nem_mem_region.memory.max_size);    
  return 0;
}
