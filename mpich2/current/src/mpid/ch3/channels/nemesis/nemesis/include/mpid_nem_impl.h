/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2006 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#ifndef MPID_NEM_IMPL_H
#define MPID_NEM_IMPL_H

#include "my_papi_defs.h"
#include "mpidi_ch3_impl.h"
#include "mpid_nem_net_module_defs.h"
#include "mpid_nem_atomics.h"
#include "mpid_nem_defs.h"
#include "mpid_nem_memdefs.h"
#include "mpid_nem_fbox.h"
#include "mpid_nem_nets.h"
#include "mpid_nem_queue.h"

#define MPID_NEM__BYPASS_Q_MAX_VAL  ((MPID_NEM_MPICH2_DATA_LEN) - (sizeof(MPIDI_CH3_Pkt_t)))

int MPID_nem_seg_create(MPID_nem_seg_ptr_t, int, int num_local, int local_rank, MPIDI_PG_t *pg_p);
int MPID_nem_seg_alloc( MPID_nem_seg_ptr_t, MPID_nem_seg_info_ptr_t, int);
int MPID_nem_check_alloc(int);
int MPID_nem_mpich2_init (int ckpt_restart);
int MPID_nem_mpich2_send_ckpt_marker (unsigned short wave, MPIDI_VC_t *vc, int *try_again);
int MPID_nem_coll_barrier_init (void);

#define MPID_nem_mpich2_release_fbox(cell) (MPID_nem_mem_region.mailboxes.in[(cell)->pkt.mpich2.source]->mpich2.flag.value = 0, \
					    MPI_SUCCESS)

/* Shared memory allocation utility functions */
/* MPID_nem_allocate_shared_memory allocates a shared mem region of size "length" and attaches to it.  "handle" points to a string
   descriptor for the region to be passed in to MPID_nem_attach_shared_memory.  "handle" is dynamically allocated and should be
   freed by the caller.*/
int MPID_nem_allocate_shared_memory (char **buf_p, const int length, char *handle[]);
/* MPID_nem_attach_shared_memory attaches to shared memory previously allocated by MPID_nem_allocate_shared_memory */
/*int MPID_nem_attach_shared_memory (char **buf_p, const int length, const char const handle[]);*/
int MPID_nem_attach_shared_memory (char **buf_p, const int length, const char handle[]);
/* MPID_nem_remove_shared_memory removes the OS descriptor associated with the handle.  Once all processes detatch from the region
   the OS resource will be destroyed. */
/*int MPID_nem_remove_shared_memory (const char const handle[]);*/
int MPID_nem_remove_shared_memory (const char handle[]);
/* MPID_nem_detach_shared_memory detaches the shared memory region from this process */
int MPID_nem_detach_shared_memory (const char *buf_p, const int length);

/* initialize shared-memory MPI_Barrier variables */
int MPID_nem_barrier_vars_init (MPID_nem_barrier_vars_t *barrier_region);

static inline void
MPID_nem_waitforlock (MPID_nem_fbox_common_ptr_t pbox, int value, int count)
{
    DO_PAPI2 (PAPI_reset (PAPI_EventSet));
    while (pbox->flag.value != value)
    {
	if(--count == 0)
	{
	    sched_yield();
	}
	DO_PAPI2 (PAPI_reset (PAPI_EventSet));
    }  
    DO_PAPI2 (PAPI_accum_var (PAPI_EventSet, PAPI_vvalues8));
}

static inline int
MPID_nem_islocked (MPID_nem_fbox_common_ptr_t pbox, int value, int count)
{
    while (pbox->flag.value != value && --count == 0)
    {
    }
    return (pbox->flag.value != value);
}

#endif /* MPID_NEM_IMPL_H */
