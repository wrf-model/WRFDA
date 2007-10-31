/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2006 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpid_nem_impl.h"
#include "mpid_nem_fbox.h"
#include "mpid_nem_nets.h"

/* #define BLOCKING_FBOX */

#define DO_PAPI(x)  /*x */
#define DO_PAPI2(x) /*x */
#define DO_PAPI3(x) /*x */

MPID_nem_fboxq_elem_t *MPID_nem_fboxq_head = 0;
MPID_nem_fboxq_elem_t *MPID_nem_fboxq_tail = 0;
MPID_nem_fboxq_elem_t *MPID_nem_fboxq_elem_list = 0;
MPID_nem_fboxq_elem_t *MPID_nem_fboxq_elem_list_last = 0;
MPID_nem_fboxq_elem_t *MPID_nem_curr_fboxq_elem = 0;
MPID_nem_fboxq_elem_t *MPID_nem_curr_fbox_all_poll = 0;

extern int MPID_nem_ckpt_logging_messages; /* are we in logging-message-mode? */
extern int MPID_nem_ckpt_sending_markers; /* are we in the process of sending markers? */
extern struct cli_message_log_total *MPID_nem_ckpt_message_log; /* are we replaying messages? */

MPID_nem_cell_ptr_t MPID_nem_prefetched_cell = 0;

unsigned short *MPID_nem_recv_seqno = 0;

#ifndef ENABLE_NO_SCHED_YIELD
#define POLLS_BEFORE_YIELD 1000
#endif

/* here we include the non-inlined versions of the files in mpid_nem_inline.h */
#define MPID_NEM_DONT_INLINE_FUNCTIONS 1
#include <mpid_nem_inline.h>

#undef FUNCNAME
#define FUNCNAME MPID_nem_mpich2_init
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int
MPID_nem_mpich2_init (int ckpt_restart)
{
    int mpi_errno = MPI_SUCCESS;
    int i;
    MPIU_CHKPMEM_DECL (2);

    /*     printf ("sizeof (MPID_nem_cell_t) == %u\n", sizeof (MPID_nem_cell_t)); */
    /*     printf ("&MPID_nem_mem_region.mailboxes.in[0]->mpich2 = %p\n", &MPID_nem_mem_region.mailboxes.in[0]->mpich2); */
    /*     printf ("&MPID_nem_mem_region.mailboxes.in[1]->mpich2 = %p\n", &MPID_nem_mem_region.mailboxes.in[1]->mpich2); */
    /*     printf ("sizeof (MPID_nem_fastbox_t) = %u\n", sizeof (MPID_nem_fastbox_t)); */
    /*     printf ("sizeof (MPID_nem_mem_region.mailboxes.in[1]->mpich2) = %u\n", sizeof (MPID_nem_mem_region.mailboxes.in[1]->mpich2)); */
    /*     printf ("OFFSETPF (MPID_nem_fbox_mpich2_t, cell) = %u\n", MPID_NEM_OFFSETOF(MPID_nem_fbox_mpich2_t, cell)); */

    MPID_nem_prefetched_cell = NULL;
    
    if (!ckpt_restart)
    {
        MPIU_CHKPMEM_MALLOC (MPID_nem_recv_seqno, unsigned short *, sizeof(*MPID_nem_recv_seqno) * MPID_nem_mem_region.num_procs, mpi_errno, "recv seqno");

	for (i = 0; i < MPID_nem_mem_region.num_procs; ++i)
	{
	    MPID_nem_recv_seqno[i] = 0;
	}
    
	/* set up fbox queue */
        MPIU_CHKPMEM_MALLOC (MPID_nem_fboxq_elem_list, MPID_nem_fboxq_elem_t *, MPID_nem_mem_region.num_local * sizeof(MPID_nem_fboxq_elem_t), mpi_errno, "fastbox element list");
    
	for (i = 0; i < MPID_nem_mem_region.num_local; ++i)
	{
	    MPID_nem_fboxq_elem_list[i].usage = 0;
	    MPID_nem_fboxq_elem_list[i].prev = NULL;
	    MPID_nem_fboxq_elem_list[i].next = NULL;
	    MPID_nem_fboxq_elem_list[i].grank = MPID_nem_mem_region.local_procs[i];
	    MPID_nem_fboxq_elem_list[i].fbox = &MPID_nem_mem_region.mailboxes.in[i]->mpich2;
	}
	
	MPID_nem_fboxq_head = NULL;
	MPID_nem_fboxq_tail = NULL;
	MPID_nem_curr_fboxq_elem = NULL;
	MPID_nem_curr_fbox_all_poll = &MPID_nem_fboxq_elem_list[0];
	MPID_nem_fboxq_elem_list_last = &MPID_nem_fboxq_elem_list[MPID_nem_mem_region.num_local - 1];
    }
    else
    {
	for (i = 0; i < MPID_nem_mem_region.num_local; ++i)
	{
	    MPIU_Assert (MPID_nem_fboxq_elem_list[i].grank == MPID_nem_mem_region.local_procs[i]);
	    MPID_nem_fboxq_elem_list[i].fbox = &MPID_nem_mem_region.mailboxes.in[i]->mpich2;
	}

	MPID_nem_fboxq_head = NULL;
	MPID_nem_fboxq_tail = NULL;
	MPID_nem_curr_fboxq_elem = NULL;
	MPID_nem_curr_fbox_all_poll = &MPID_nem_fboxq_elem_list[0];
	MPID_nem_fboxq_elem_list_last = &MPID_nem_fboxq_elem_list[MPID_nem_mem_region.num_local - 1];
    }
    
    MPIU_CHKPMEM_COMMIT();
 fn_exit:
    return mpi_errno;
 fn_fail:
    /* --BEGIN ERROR HANDLING-- */
    MPIU_CHKPMEM_REAP();
    goto fn_exit;
    /* --END ERROR HANDLING-- */
}

/*
  int MPID_nem_mpich2_send_ckpt_marker (unsigned short wave, MPIDI_VC_t *vc);

  sends checkpoint marker with wave number wave to vc
  Non-blocking
  sets *try_again to 1 if it can't get a free cell
*/
int
MPID_nem_mpich2_send_ckpt_marker (unsigned short wave, MPIDI_VC_t *vc, int *try_again)
{
#ifdef ENABLED_CHECKPOINTING
    MPID_nem_cell_ptr_t el;
    int my_rank;

    my_rank = MPID_nem_mem_region.rank;
    
#ifdef PREFETCH_CELL
    el = MPID_nem_prefetched_cell;
    
    if (!el)
    {
	if (MPID_nem_queue_empty (MPID_nem_mem_region.my_freeQ))
            goto return_again;
	MPID_nem_queue_dequeue (MPID_nem_mem_region.my_freeQ, &el);
    }
#else /*PREFETCH_CELL    */
    if (MPID_nem_queue_empty (MPID_nem_mem_region.my_freeQ)) 
        goto return_again;

    MPID_nem_queue_dequeue (MPID_nem_mem_region.my_freeQ, &el);
#endif  /*PREFETCH_CELL      */

    el->pkt.ckpt.source  = my_rank;
    el->pkt.ckpt.dest    = vc->lpid;
    el->pkt.ckpt.datalen = sizeof(el->pkt.ckpt.wave); /* FIXME: we need a way to handle packet types w/ different sizes */
    el->pkt.ckpt.seqno   = vc->ch.send_seqno++;
    el->pkt.ckpt.type    = MPID_NEM_PKT_CKPT;
    el->pkt.ckpt.wave    = wave;

    if(MPID_NEM_IS_LOCAL (vc->lpid))
    {
	MPID_nem_queue_enqueue( MPID_nem_mem_region.RecvQ[vc->lpid], el);
	/*MPID_nem_rel_dump_queue( MPID_nem_mem_region.RecvQ[vc->lpid] ); */
    }
    else
    {
        MPID_nem_net_module_send (vc, el, el->pkt.ckpt.datalen);
    }


#ifdef PREFETCH_CELL
    if (!MPID_nem_queue_empty (MPID_nem_mem_region.my_freeQ))
	MPID_nem_queue_dequeue (MPID_nem_mem_region.my_freeQ, &MPID_nem_prefetched_cell);
    else
	MPID_nem_prefetched_cell = 0;
#endif /*PREFETCH_CELL */

    *try_again = 0;
    return MPI_SUCCESS;
 return_again:
    *try_again = 1;
    return MPI_SUCCESS;
#endif /*ENABLED_CHECKPOINTING */
    return MPI_SUCCESS;
}

/* Any reference to gm should be taken out from this file */
/* or generic routines should be implemented              */

#if 0
int
MPID_nem_mpich2_lmt_send_pre (struct iovec *iov, int n_iov, int dest, struct iovec *cookie)
{
    int ret = MPID_NEM_MPICH2_SUCCESS;
    
    if (!MPID_NEM_IS_LOCAL (dest))
    {
	if (MPID_nem_gm_module_lmt_send_pre (iov, n_iov, dest, cookie) != 0)
	{
	    ret = MPID_NEM_MPICH2_FAILURE;
	}
    }	    
    return ret;
}

int
MPID_nem_mpich2_lmt_recv_pre (struct iovec *iov, int n_iov, int src, struct iovec *cookie)
{
    int ret = MPID_NEM_MPICH2_SUCCESS;
    
    if (!MPID_NEM_IS_LOCAL (src))
    {
	if (MPID_nem_gm_module_lmt_recv_pre (iov, n_iov, src, cookie) != 0)
	{
	    ret = MPID_NEM_MPICH2_FAILURE;
	}
    }
	    
    return ret;
}

int
MPID_nem_mpich2_lmt_start_send (int dest, struct iovec s_cookie, struct iovec r_cookie, int *completion_ctr)
{
    int ret = MPID_NEM_MPICH2_SUCCESS;
    
    if (!MPID_NEM_IS_LOCAL (dest))
    {
	MPID_nem_gm_module_lmt_start_send (dest, s_cookie, r_cookie, completion_ctr);
    }
    
    return ret;
}

int
MPID_nem_mpich2_lmt_start_recv (int src, struct iovec s_cookie, struct iovec r_cookie, int *completion_ctr)
{
    int ret = MPID_NEM_MPICH2_SUCCESS;
    
    if (!MPID_NEM_IS_LOCAL (src))
    {
	MPID_nem_gm_module_lmt_start_recv (src, s_cookie, r_cookie, completion_ctr);
    }
    
    return ret;
}

int
MPID_nem_mpich2_lmt_send_post (int dest, struct iovec cookie)
{
    int ret = MPID_NEM_MPICH2_SUCCESS;
    
    if (!MPID_NEM_IS_LOCAL (dest))
    {
	if (MPID_nem_gm_module_lmt_send_post (cookie) != 0)
	{
	    ret = MPID_NEM_MPICH2_FAILURE;
	}
    }
    
    return ret;
}

int
MPID_nem_mpich2_lmt_recv_post (int src, struct iovec cookie)
{
    int ret = MPID_NEM_MPICH2_SUCCESS;

    if (!MPID_NEM_IS_LOCAL (src))
    {
	if (MPID_nem_gm_module_lmt_send_post (cookie) != 0)
	{
	    ret = MPID_NEM_MPICH2_FAILURE;
	}
    }
    
    return ret;
}
#endif

