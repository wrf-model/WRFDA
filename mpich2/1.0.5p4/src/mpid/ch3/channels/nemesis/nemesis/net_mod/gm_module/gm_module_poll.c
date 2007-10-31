/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2006 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "gm_module_impl.h"


inline int
MPID_nem_gm_module_recv()
{
    gm_recv_event_t *e;
    MPID_nem_cell_ptr_t c;

    /*    printf_d ("MPID_nem_gm_module_recv()\n"); */
    
    DO_PAPI (PAPI_reset (PAPI_EventSet));
    while (MPID_nem_module_gm_num_recv_tokens && !MPID_nem_queue_empty (MPID_nem_module_gm_free_queue))
    {
	MPID_nem_queue_dequeue (MPID_nem_module_gm_free_queue, &c);
	gm_provide_receive_buffer_with_tag (MPID_nem_module_gm_port, (void *)MPID_NEM_CELL_TO_PACKET (c), PACKET_SIZE, GM_LOW_PRIORITY, 0);
	--MPID_nem_module_gm_num_recv_tokens;
	DO_PAPI (if (!(MPID_nem_module_gm_num_recv_tokens && !MPID_nem_queue_empty (MPID_nem_module_gm_free_queue)))
		 PAPI_accum_var (PAPI_EventSet, PAPI_vvalues10));
   }
    
    DO_PAPI (PAPI_reset (PAPI_EventSet));
    e = gm_receive (MPID_nem_module_gm_port);
    while (gm_ntoh_u8 (e->recv.type) != GM_NO_RECV_EVENT)
    {
	MPID_nem_pkt_header_t *header;
	switch (gm_ntoh_u8 (e->recv.type))
	{
	case GM_FAST_HIGH_PEER_RECV_EVENT:
	case GM_FAST_HIGH_RECV_EVENT:
	case GM_HIGH_PEER_RECV_EVENT:
	case GM_HIGH_RECV_EVENT:
	    printf ("Received unexpected high priority message\n");
	    gm_provide_receive_buffer_with_tag (MPID_nem_module_gm_port, gm_ntohp (e->recv.buffer), gm_ntoh_u8 (e->recv.size), GM_HIGH_PRIORITY,
						gm_ntoh_u8 (e->recv.tag));
	    break;
	case GM_FAST_PEER_RECV_EVENT:
	case GM_FAST_RECV_EVENT:
	    DO_PAPI (PAPI_accum_var (PAPI_EventSet, PAPI_vvalues5));
	    DO_PAPI (PAPI_reset (PAPI_EventSet));
	    /*gm_memorize_message (gm_ntohp (e->recv.message), gm_ntohp (e->recv.buffer), gm_ntoh_u32 (e->recv.length)); */
	    /*my_memcpy (gm_ntohp (e->recv.buffer), gm_ntohp (e->recv.message), gm_ntoh_u32 (e->recv.length)); */
	    MPID_NEM_MEMCPY (gm_ntohp (e->recv.buffer), gm_ntohp (e->recv.message), gm_ntoh_u32 (e->recv.length));
	    DO_PAPI (PAPI_accum_var (PAPI_EventSet, PAPI_vvalues7));
	case GM_PEER_RECV_EVENT:
	case GM_RECV_EVENT:
	    DO_PAPI (if (gm_ntoh_u8 (e->recv.type) == GM_RECV_EVENT || gm_ntoh_u8 (e->recv.type) == GM_PEER_RECV_EVENT)
		     PAPI_accum_var (PAPI_EventSet, PAPI_vvalues5));
	    header = (MPID_nem_pkt_header_t *)gm_ntohp (e->recv.buffer);
	    c = MPID_NEM_PACKET_TO_CELL (header);	    

	    printf_d ("  Received packet\n");
	    printf_d ("    dest %d\n", header->dest);
	    printf_d ("    datalen %d\n", header->datalen);
	    printf_d ("    seqno %d\n", header->seqno);

	    DO_PAPI (PAPI_reset (PAPI_EventSet));	    
	    MPID_nem_queue_enqueue (MPID_nem_process_recv_queue, c);
	    DO_PAPI (PAPI_accum_var (PAPI_EventSet, PAPI_vvalues9));

/* 	    if (!MPID_nem_queue_empty (MPID_nem_module_gm_free_queue)) */
/* 	    { */
/* 		MPID_nem_queue_dequeue (MPID_nem_module_gm_free_queue, &c); */
/* 		gm_provide_receive_buffer_with_tag (MPID_nem_module_gm_port, MPID_NEM_CELL_TO_PACKET (c), PACKET_SIZE, GM_LOW_PRIORITY, 0); */
/* 	    } */
/* 	    else */
/* 	    { */
		++MPID_nem_module_gm_num_recv_tokens; 
/* 	    } */
	    break;
	default:
	    gm_unknown (MPID_nem_module_gm_port, e);
	    DO_PAPI (PAPI_accum_var (PAPI_EventSet, PAPI_vvalues6));
	}
	
	DO_PAPI (PAPI_reset (PAPI_EventSet));
	e = gm_receive (MPID_nem_module_gm_port);
    }
    return MPI_SUCCESS;
}

static inline int
lmt_poll()
{
    int ret;
    MPID_nem_gm_module_lmt_queue_t *e;
    
    MPID_nem_gm_module_queue_dequeue (lmt, &e);
    
    while (e && MPID_nem_module_gm_num_send_tokens)
    {
	ret = MPID_nem_gm_module_lmt_do_get (e->node_id, e->port_id, &e->r_iov, &e->r_n_iov, &e->r_offset, &e->s_iov, &e->s_n_iov, &e->s_offset,
                                             e->compl_ctr);
	if (ret == LMT_AGAIN)
	{
	    MPID_nem_gm_module_queue_free (lmt, e);
	    MPID_nem_gm_module_queue_dequeue (lmt, &e);
	}
	else if (ret == LMT_FAILURE)
	{
	    printf ("error: MPID_nem_gm_module_lmt_do_get failed.  Dequeuing.\n");
	    MPID_nem_gm_module_queue_free (lmt, e);
	    MPID_nem_gm_module_queue_dequeue (lmt, &e);
	}
    }
    return MPI_SUCCESS;
}


#undef FUNCNAME
#define FUNCNAME MPID_nem_gm_module_send_poll
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
inline int
MPID_nem_gm_module_send_poll( void )
{
    int mpi_errno = MPI_SUCCESS;
    
    mpi_errno = MPID_nem_send_from_queue();
    if (mpi_errno) MPIU_ERR_POP (mpi_errno);
    /*lmt_poll(); */
    mpi_errno = MPID_nem_gm_module_recv();
    if (mpi_errno) MPIU_ERR_POP (mpi_errno);

 fn_exit:
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}

#undef FUNCNAME
#define FUNCNAME MPID_nem_gm_module_recv_poll
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
inline int
MPID_nem_gm_module_recv_poll( void )
{
    int mpi_errno = MPI_SUCCESS;
    
    mpi_errno = MPID_nem_gm_module_recv();
    if (mpi_errno) MPIU_ERR_POP (mpi_errno);
    mpi_errno = MPID_nem_send_from_queue();
    if (mpi_errno) MPIU_ERR_POP (mpi_errno);
    /*lmt_poll(); */
 fn_exit:
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}

#undef FUNCNAME
#define FUNCNAME MPID_nem_gm_module_poll
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int
MPID_nem_gm_module_poll(MPID_nem_poll_dir_t in_or_out)
{
    if (in_or_out == MPID_NEM_POLL_OUT)
    {
	return MPID_nem_gm_module_send_poll();
    }
    else
    {
	return MPID_nem_gm_module_recv_poll();
    }
}
