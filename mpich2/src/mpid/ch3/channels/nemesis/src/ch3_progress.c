/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpidi_ch3_impl.h"

/*#include "mpidpre.h"*/
#include "mpid_nem_impl.h"
#if defined (MPID_NEM_INLINE) && MPID_NEM_INLINE
#include "mpid_nem_inline.h"
#endif
#include "pmi.h"

static MPIDI_CH3_PktHandler_Fcn *pktArray[MPIDI_CH3_PKT_END_CH3+1];

#ifndef MPIDI_POSTED_RECV_ENQUEUE_HOOK
#define MPIDI_POSTED_RECV_ENQUEUE_HOOK(x) do {} while (0)
#endif
#ifndef MPIDI_POSTED_RECV_DEQUEUE_HOOK
#define MPIDI_POSTED_RECV_DEQUEUE_HOOK(x) do {} while (0)
#endif

#ifdef BY_PASS_PROGRESS
extern MPID_Request ** const MPID_Recvq_posted_head_ptr;
extern MPID_Request ** const MPID_Recvq_unexpected_head_ptr; 
extern MPID_Request ** const MPID_Recvq_posted_tail_ptr;
extern MPID_Request ** const MPID_Recvq_unexpected_tail_ptr;
#endif

volatile unsigned int MPIDI_CH3I_progress_completion_count = 0;

struct MPID_Request *MPIDI_CH3I_sendq_head[CH3_NUM_QUEUES] = {0};
struct MPID_Request *MPIDI_CH3I_sendq_tail[CH3_NUM_QUEUES] = {0};
struct MPID_Request *MPIDI_CH3I_active_send[CH3_NUM_QUEUES] = {0};

#if !defined(MPIDI_CH3_Progress_start)
#undef FUNCNAME
#define FUNCNAME MPIDI_CH3_Progress_start
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
void MPIDI_CH3_Progress_start (MPID_Progress_state * state)
{
    /* MT - This function is empty for the single-threaded implementation */
    MPIDI_STATE_DECL(MPID_STATE_MPIDI_CH3_PROGRESS_START);

    MPIDI_FUNC_ENTER(MPID_STATE_MPIDI_CH3_PROGRESS_START);
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDI_CH3_PROGRESS_START);
}
#endif

#undef FUNCNAME
#define FUNCNAME MPIDI_CH3I_Progress
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3I_Progress (int is_blocking)
{
    unsigned completions = MPIDI_CH3I_progress_completion_count;
    int mpi_errno = MPI_SUCCESS;
    int complete;
#ifndef ENABLE_NO_SCHED_YIELD
    int pollcount = 0;
#endif
    MPIDI_STATE_DECL(MPID_STATE_MPIDI_CH3I_PROGRESS);

    MPIDI_FUNC_ENTER(MPID_STATE_MPIDI_CH3I_PROGRESS);

    do
    {
	MPID_Request        *sreq;
	MPID_Request        *rreq;
	MPID_nem_cell_ptr_t  cell;
	int                  in_fbox = 0;
	MPIDI_VC_t          *vc;

	/* make progress receiving */
	/* check queue */
	  
	if (!MPIDI_CH3I_active_send[CH3_NORMAL_QUEUE] && !MPIDI_CH3I_SendQ_head (CH3_NORMAL_QUEUE) && is_blocking)
	{
	    mpi_errno = MPID_nem_mpich2_blocking_recv (&cell, &in_fbox);
	}
	else
	{
#ifndef ENABLE_NO_SCHED_YIELD
            if (pollcount >= MPID_NEM_POLLS_BEFORE_YIELD)
            {
                pollcount = 0;
                sched_yield();
            }
            ++pollcount;
#endif
	    mpi_errno = MPID_nem_mpich2_test_recv (&cell, &in_fbox);
	}
        if (mpi_errno) MPIU_ERR_POP (mpi_errno);

	if (cell)
	{
	    char *cell_buf    = (char *)cell->pkt.mpich2.payload;
	    int   payload_len = cell->pkt.mpich2.datalen;

            /* Empty packets are not allowed */
            MPIU_Assert (payload_len >= 0);
            
	    if (in_fbox)
	    {
                MPIDI_CH3_Pkt_t *pkt = (MPIDI_CH3_Pkt_t *)cell_buf;
                /* This packet must be the first packet of a new message */
                MPIU_DBG_MSG (CH3_CHANNEL, VERBOSE, "Recv new pkt");
                MPIU_Assert (payload_len >= sizeof (MPIDI_CH3_Pkt_t));

		MPIDI_PG_Get_vc (MPIDI_Process.my_pg, MPID_NEM_FBOX_SOURCE (cell), &vc);
                MPIU_Assert (vc->ch.recv_active == NULL);

#if 0
#if 0
		{ MPIDI_CH3_Pkt_t *pkt = (MPIDI_CH3_Pkt_t *)cell_buf;
                    mpi_errno = pktArray[pkt->type]( vc, pkt, &rreq );
		}
#else		    
		mpi_errno = MPIDI_CH3U_Handle_recv_pkt (vc, (MPIDI_CH3_Pkt_t *)cell_buf, &rreq);
#endif
#endif
		mpi_errno = pktArray[pkt->type] (vc, pkt, &rreq);
		if (mpi_errno) MPIU_ERR_POP (mpi_errno);
                
                /* Channel fields don't get initialized on request creation, init them here */
                if (rreq)
                    rreq->ch.iov_offset = 0;
		cell_buf    += sizeof (MPIDI_CH3_Pkt_t);
		payload_len -= sizeof (MPIDI_CH3_Pkt_t);
                MPIU_Assert (!rreq || rreq->ch.iov_offset == 0);
	    }
	    else
	    {
                MPIDI_CH3_Pkt_t *pkt = (MPIDI_CH3_Pkt_t *)cell_buf;
		MPIDI_PG_Get_vc (MPIDI_Process.my_pg, MPID_NEM_CELL_SOURCE (cell), &vc);
		
		rreq = vc->ch.recv_active;
                MPIU_DBG_STMT (CH3_CHANNEL, VERBOSE, {
                        if (rreq)
                        {
                            MPIU_DBG_MSG (CH3_CHANNEL, VERBOSE, "Recv cont pkt");
                        }
                    });

		if (!rreq)
		{
                    /* This packet must be the first packet of a new message */
                    MPIU_DBG_MSG (CH3_CHANNEL, VERBOSE, "Recv new pkt");
                    MPIU_Assert (payload_len >= sizeof (MPIDI_CH3_Pkt_t));
		    
#if 0
#if 0
		    { MPIDI_CH3_Pkt_t *pkt = (MPIDI_CH3_Pkt_t *)cell_buf;
                        mpi_errno = pktArray[pkt->type]( vc, pkt, &rreq );
		    }
#else		    
		    mpi_errno = MPIDI_CH3U_Handle_recv_pkt (vc, (MPIDI_CH3_Pkt_t *)cell_buf, &rreq);
#endif
#endif
                    mpi_errno = pktArray[pkt->type] (vc, pkt, &rreq);
		    if (mpi_errno) MPIU_ERR_POP (mpi_errno);
                    
                    /* Channel fields don't get initialized on request creation, init them here */
                    if (rreq)
                        rreq->ch.iov_offset = 0;
		    cell_buf += sizeof (MPIDI_CH3_Pkt_t);
		    payload_len -= sizeof (MPIDI_CH3_Pkt_t);
                    MPIU_Assert (!rreq || rreq->ch.iov_offset == 0);
		}
	    }            
	    
	    if (rreq)
	    {
		MPID_IOV *iov;
		int n_iov;

                complete = 0;

                MPIU_Assert (rreq->dev.iov_count > 0 && rreq->dev.iov[rreq->ch.iov_offset].MPID_IOV_LEN > 0);
		do
		{
		    iov = &rreq->dev.iov[rreq->ch.iov_offset];
		    n_iov = rreq->dev.iov_count;
		
		    while ((n_iov) && (payload_len >= iov->iov_len))
		    {
			int _iov_len = iov->iov_len;
			MPID_NEM_MEMCPY (iov->iov_base, cell_buf, _iov_len);
			payload_len -= _iov_len;
			cell_buf += _iov_len;
			--n_iov;
			++iov;
		    }
		
		    if (n_iov)
		    {
			if (payload_len > 0)
			{
			    MPID_NEM_MEMCPY (iov->iov_base, cell_buf, payload_len);
			    iov->iov_base = (void *)((char *)iov->iov_base + payload_len);
			    iov->iov_len -= payload_len;
			    payload_len = 0;
			}
		    
			rreq->ch.iov_offset = iov - rreq->dev.iov;
			rreq->dev.iov_count = n_iov;
			vc->ch.recv_active = rreq;
		    }
		    else
		    {				       
                        int (*reqFn)(MPIDI_VC_t *, MPID_Request *, int *);

                        reqFn = rreq->dev.OnDataAvail;
                        if (!reqFn)
                        {
                            MPIU_Assert(MPIDI_Request_get_type(rreq)!=MPIDI_REQUEST_TYPE_GET_RESP);
                            MPIDI_CH3U_Request_complete(rreq);
                            complete = TRUE;
			}
                        else
                        {   
                            mpi_errno = reqFn (vc, rreq, &complete);
                            if (mpi_errno) MPIU_ERR_POP (mpi_errno);
                        }
                        
			if (!complete)
			{
                            rreq->ch.iov_offset = 0;
                            MPIU_Assert (rreq->dev.iov_count > 0 && rreq->dev.iov[rreq->ch.iov_offset].MPID_IOV_LEN > 0);
                        }
			else
			{
                            MPIU_DBG_MSG (CH3_CHANNEL, VERBOSE, ".... complete");
                            MPIU_DBG_STMT (CH3_CHANNEL, VERBOSE, {
                                    int i;
                                    for (i = 0; i < MPID_IOV_LIMIT; ++i)
                                        rreq->dev.iov[i].MPID_IOV_LEN = 0;
                                });
                            
                            vc->ch.recv_active = NULL;
                            
			}
		    }
		}
		while (payload_len && !complete);
                MPIU_DBG_STMT (CH3_CHANNEL, VERBOSE, {
                        if (payload_len)
                            MPIU_DBG_MSG_D (CH3_CHANNEL, VERBOSE, "data left %d", payload_len);
                    });
	    }

	    if (!in_fbox)
	    {
		MPID_nem_mpich2_release_cell (cell, vc);
	    }
	    else
	    {
		MPID_nem_mpich2_release_fbox (cell);
	    }	    
	}

	/* make progress sending */
        
	sreq = MPIDI_CH3I_active_send[CH3_NORMAL_QUEUE];
        MPIU_DBG_STMT (CH3_CHANNEL, VERBOSE, {
                if (sreq)
                    MPIU_DBG_MSG (CH3_CHANNEL, VERBOSE, "Send: cont sreq");
            });
	if (!sreq)
	{
	    sreq = MPIDI_CH3I_SendQ_head (CH3_NORMAL_QUEUE);
            MPIDI_CH3I_active_send[CH3_NORMAL_QUEUE] = sreq;
            MPIU_DBG_STMT (CH3_CHANNEL, VERBOSE, {
                    if (sreq)
                        MPIU_DBG_MSG (CH3_CHANNEL, VERBOSE, "Send: new sreq ");
                });
            
	}
        
	if (sreq)
	{
	    MPID_IOV *_iov;
	    int _n_iov;
	    int again = 0;
	    
            MPIU_Assert (sreq->dev.iov_count > 0 && sreq->dev.iov[sreq->ch.iov_offset].MPID_IOV_LEN > 0);
            
	    _iov = &sreq->dev.iov[sreq->ch.iov_offset];
	    _n_iov = sreq->dev.iov_count;

	    do 
	    {
		mpi_errno = MPID_nem_mpich2_sendv (&_iov, &_n_iov, sreq->ch.vc, &again);
                if (mpi_errno) MPIU_ERR_POP (mpi_errno);
	    }
	    while (!again && _n_iov > 0);

	    if (again)
	    {
		sreq->ch.iov_offset = _iov - sreq->dev.iov;
		sreq->dev.iov_count = _n_iov;
	    }
	    else
	    {
                int (*reqFn)(MPIDI_VC_t *, MPID_Request *, int *);
                
                reqFn = sreq->dev.OnDataAvail;
                if (!reqFn)
                {
                    MPIU_Assert (MPIDI_Request_get_type (sreq) != MPIDI_REQUEST_TYPE_GET_RESP);
                    MPIDI_CH3U_Request_complete (sreq);
                    
                    MPIDI_CH3I_SendQ_dequeue (CH3_NORMAL_QUEUE);
                    MPIDI_CH3I_active_send[CH3_NORMAL_QUEUE] = NULL;
                    MPIU_DBG_MSG (CH3_CHANNEL, VERBOSE, ".... complete");
                    MPIU_DBG_STMT (CH3_CHANNEL, VERBOSE, {
                        int i;
                        for (i = 0; i < MPID_IOV_LIMIT; ++i)
                            sreq->dev.iov[i].MPID_IOV_LEN = 0;
                    });
                }
                else
                {
                    complete = 0;
                    mpi_errno = reqFn (sreq->ch.vc, sreq, &complete);
                    if (mpi_errno) MPIU_ERR_POP (mpi_errno);

                    if (complete)
                    {
                        MPIDI_CH3I_SendQ_dequeue (CH3_NORMAL_QUEUE);
                        MPIDI_CH3I_active_send[CH3_NORMAL_QUEUE] = NULL;
                        MPIU_DBG_MSG (CH3_CHANNEL, VERBOSE, ".... complete");
                        MPIU_DBG_STMT (CH3_CHANNEL, VERBOSE, {
                            int i;
                            for (i = 0; i < MPID_IOV_LIMIT; ++i)
                                sreq->dev.iov[i].MPID_IOV_LEN = 0;
                        });
                    }
                    else
                    {
                        sreq->ch.iov_offset = 0;
                        MPIU_Assert (sreq->dev.iov_count > 0 && sreq->dev.iov[sreq->ch.iov_offset].MPID_IOV_LEN > 0);
                    }
                }            
	    }
	}
    }
    while (completions == MPIDI_CH3I_progress_completion_count && is_blocking);
    

 fn_exit:
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDI_CH3I_PROGRESS);
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}


#define set_request_info(rreq_, pkt_, msg_type_)                \
{                                                               \
    (rreq_)->status.MPI_SOURCE = (pkt_)->match.rank;            \
    (rreq_)->status.MPI_TAG = (pkt_)->match.tag;                \
    (rreq_)->status.count = (pkt_)->data_sz;                    \
    (rreq_)->dev.sender_req_id = (pkt_)->sender_req_id;         \
    (rreq_)->dev.recv_data_sz = (pkt_)->data_sz;                \
    MPIDI_Request_set_seqnum((rreq_), (pkt_)->seqnum);          \
    MPIDI_Request_set_msg_type((rreq_), (msg_type_));           \
}

#ifdef BYPASS_PROGRESS
#undef FUNCNAME
#define FUNCNAME MPIDI_CH3_Progress_poke_with_matching
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
MPID_Request *MPIDI_CH3_Progress_poke_with_matching (int source, int tag, MPID_Comm *comm,int context_id,int *foundp, void *buf, int count, MPI_Datatype datatype,MPI_Status * status)   
{
    int             mpi_errno = MPI_SUCCESS;
    MPID_Request   *rreq  = NULL;
    MPID_nem_cell_ptr_t cell  = NULL;
    int             in_fbox;
    int             dt_contig;
    MPI_Aint        dt_true_lb;
    MPIDI_msg_sz_t  userbuf_sz;
    MPID_Datatype  *dt_ptr;
    

    MPIDI_STATE_DECL(MPID_STATE_MPIDI_CH3I_PROGRESS_POKE_WITH_MATCHING);
    MPIDI_FUNC_ENTER(MPID_STATE_MPIDI_CH3I_PROGRESS_POKE_WITH_MATCHING);    
    MPIDI_DBG_PRINTF((50, FCNAME, "entering, buf=%p, count=%d, dtype=%d",
		      buf,count,datatype));

    *foundp = FALSE ;
    MPIDI_Datatype_get_info(count, datatype, dt_contig, userbuf_sz, dt_ptr, dt_true_lb);

    /* handle only contiguous types (for now) and one-cell packets */
    if((dt_contig) && (( userbuf_sz <= MPID_NEM__BYPASS_Q_MAX_VAL))) 
    {
	//PAPI_reset(PAPI_EventSet);
	MPID_nem_mpich2_blocking_recv (&cell, &in_fbox);
	//PAPI_accum(PAPI_EventSet, PAPI_values2);	

	if (cell)
	{	 
	    char *cell_buf    = cell->pkt.mpich2.payload;

	    switch(((MPIDI_CH3_Pkt_t *)cell_buf)->type)
	    {
	    case MPIDI_CH3_PKT_EAGER_SEND:
		{
		    MPIDI_CH3_Pkt_eager_send_t *eager_pkt =  &((MPIDI_CH3_Pkt_t *)cell_buf)->eager_send;
		    int payload_len = eager_pkt->data_sz; 
		    cell_buf += sizeof (MPIDI_CH3_Pkt_t);
			    
		    if(((eager_pkt->match.tag  == tag   )||(tag    == MPI_ANY_TAG   )) &&
		       ((eager_pkt->match.rank == source)||(source == MPI_ANY_SOURCE)) &&
		       (eager_pkt->match.context_id == context_id))
		    {
			/* cell matches */
			*foundp = TRUE;
				    
			if (payload_len > 0)
			{
			    if (payload_len <= userbuf_sz)
			    {				    
				MPID_NEM_MEMCPY((char *)(buf+ dt_true_lb), cell_buf,payload_len);
			    }
			    else
			    {
				/* error : truncate */
				MPID_NEM_MEMCPY((char *)(buf+dt_true_lb),cell_buf, userbuf_sz);
				status->MPI_ERROR = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE, FCNAME, __LINE__, MPI_ERR_TRUNCATE,"**truncate", "**truncate %d %d %d %d", status->MPI_SOURCE,status->MPI_TAG,payload_len, userbuf_sz );
				mpi_errno = status->MPI_ERROR;
				goto exit_fn;
			    }
			}
		    }
		    else
		    {
			/* create a request for the cell, enqueue it on
			   the unexpected queue */
			rreq  = MPID_Request_create();
			if (rreq != NULL)
			{
			    MPIU_Object_set_ref(rreq, 2);
			    rreq->kind                 = MPID_REQUEST_RECV;
			    rreq->dev.match.tag        = eager_pkt->match.tag ;
			    rreq->dev.match.rank       = eager_pkt->match.rank;
			    rreq->dev.match.context_id = eager_pkt->match.context_id;
			    rreq->dev.tmpbuf           = MPIU_Malloc(userbuf_sz);
			    MPID_NEM_MEMCPY((char *)(rreq->dev.tmpbuf),cell_buf, userbuf_sz);
			    rreq->dev.next             = NULL;
			    if (*MPID_Recvq_unexpected_tail_ptr != NULL)
			    {
				(*MPID_Recvq_unexpected_tail_ptr)->dev.next = rreq;
			    }
			    else
			    {
				*MPID_Recvq_unexpected_head_ptr = rreq;
			    }
			    *MPID_Recvq_unexpected_tail_ptr = rreq;     
			}
		    }
		}
		break;
	    case MPIDI_CH3_PKT_READY_SEND:
		{
		    MPIDI_CH3_Pkt_ready_send_t *ready_pkt =  &((MPIDI_CH3_Pkt_t *)cell_buf)->ready_send;
		    fprintf(stdout,"ERROR : MPIDI_CH3_PKT_READY_SEND not handled (yet) \n");			    
		}
		break;
	    case MPIDI_CH3_PKT_EAGER_SYNC_SEND:
		{
		    MPIDI_CH3_Pkt_eager_send_t *es_pkt =  &((MPIDI_CH3_Pkt_t *)cell_buf)->eager_send;
		    int payload_len = es_pkt->data_sz; 
		    cell_buf += sizeof (MPIDI_CH3_Pkt_t);
			    
		    if(((es_pkt->match.tag  == tag   )||(tag    == MPI_ANY_TAG   )) &&
		       ((es_pkt->match.rank == source)||(source == MPI_ANY_SOURCE)) &&
		       (es_pkt->match.context_id == context_id))
		    {
			MPIDI_CH3_Pkt_t  upkt;
			MPIDI_CH3_Pkt_eager_sync_ack_t * const esa_pkt = &upkt.eager_sync_ack;
			MPID_Request * esa_req = NULL;
			MPIDI_VC_t   *vc;

			/* cell matches */
			*foundp = TRUE;
				    
			if (payload_len > 0)
			{
			    if (payload_len <= userbuf_sz)
			    {				    
				MPID_NEM_MEMCPY((char *)(buf+ dt_true_lb), cell_buf,payload_len);
			    }
			    else
			    {
				/* error : truncate */
				MPID_NEM_MEMCPY((char *)(buf+dt_true_lb),cell_buf, userbuf_sz);
				status->MPI_ERROR = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE, FCNAME, __LINE__, MPI_ERR_TRUNCATE,"**truncate", "**truncate %d %d %d %d", status->MPI_SOURCE,status->MPI_TAG,payload_len, userbuf_sz );
				mpi_errno = status->MPI_ERROR;
				goto exit_fn;
			    }
			}
				    
			/* send Ack back */
			MPIDI_Pkt_init(esa_pkt, MPIDI_CH3_PKT_EAGER_SYNC_ACK);
			esa_pkt->sender_req_id = es_pkt->sender_req_id;
			if (in_fbox)
			{
			    MPIDI_PG_Get_vc (MPIDI_Process.my_pg, MPID_NEM_FBOX_SOURCE (cell), &vc);
			}
			else
			{
			    MPIDI_PG_Get_vc (MPIDI_Process.my_pg, MPID_NEM_CELL_SOURCE (cell), &vc);
			}
				    
			mpi_errno = MPIDI_CH3_iStartMsg(vc, esa_pkt, sizeof(*esa_pkt), &esa_req);
                        MPIU_ERR_CHKANDJUMP (mpi_errno, mpi_errno, MPI_ERR_OTHER, "**ch3|syncack");
	    
			if (esa_req != NULL)
			{
			    MPID_Request_release(esa_req);
			}
		    }
		    else
		    {
			/* create a request for the cell, enqueue it on
			   the unexpected queue */
			rreq  = MPID_Request_create();
			if (rreq != NULL)
			{
			    MPIU_Object_set_ref(rreq, 2);
			    rreq->kind                 = MPID_REQUEST_RECV;
			    rreq->dev.match.tag        = es_pkt->match.tag ;
			    rreq->dev.match.rank       = es_pkt->match.rank;
			    rreq->dev.match.context_id = es_pkt->match.context_id;
			    rreq->dev.tmpbuf           = MPIU_Malloc(userbuf_sz);
			    MPID_NEM_MEMCPY((char *)(rreq->dev.tmpbuf),cell_buf, userbuf_sz);
			    rreq->dev.next             = NULL;
			    if (*MPID_Recvq_unexpected_tail_ptr != NULL)
			    {
				(*MPID_Recvq_unexpected_tail_ptr)->dev.next = rreq; 
			    }
			    else
			    {
				*MPID_Recvq_unexpected_head_ptr = rreq; 
			    }
			    *MPID_Recvq_unexpected_tail_ptr = rreq; 
			    MPIDI_Request_set_sync_send_flag(rreq,TRUE);
			}
		    }
		}
		break;
	    case MPIDI_CH3_PKT_EAGER_SYNC_ACK:
		{
		    MPIDI_CH3_Pkt_eager_sync_ack_t * esa_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->eager_sync_ack;
		    MPID_Request * sreq;
		    MPID_Request_get_ptr(esa_pkt->sender_req_id, sreq);
		    MPIDI_CH3U_Request_complete(sreq);
		}
		break;
	    case MPIDI_CH3_PKT_RNDV_REQ_TO_SEND:
		{
		    /* this case in currently disabled since cells are smaller than eager msgs, but ... */
		    MPIDI_CH3_Pkt_rndv_req_to_send_t *rts_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->rndv_req_to_send;
		    rreq  = MPID_Request_create();
		    if (rreq != NULL)
		    {
			MPIU_Object_set_ref(rreq, 2);
			rreq->kind                 = MPID_REQUEST_RECV;
			rreq->dev.next             = NULL;     
		    }    
			    
		    if(((rts_pkt->match.tag  == tag   )||(tag    == MPI_ANY_TAG   )) &&
		       ((rts_pkt->match.rank == source)||(source == MPI_ANY_SOURCE)) &&
		       (rts_pkt->match.context_id == context_id))
		    {
			*foundp = TRUE;
			rreq->dev.match.tag        = tag;
			rreq->dev.match.rank       = source;
			rreq->dev.match.context_id = context_id;
			rreq->comm                 = comm;
			MPIR_Comm_add_ref(comm);
			rreq->dev.user_buf         = buf;
			rreq->dev.user_count       = count;
			rreq->dev.datatype         = datatype;
			set_request_info(rreq,rts_pkt, MPIDI_REQUEST_RNDV_MSG);
		    }
		    else
		    {
			/* enqueue rreq on the unexp queue */
			rreq->dev.match.tag        = rts_pkt->match.tag;
			rreq->dev.match.rank       = rts_pkt->match.rank;
			rreq->dev.match.context_id = rts_pkt->match.context_id;
			if (*MPID_Recvq_unexpected_tail_ptr != NULL)
			{
			    (*MPID_Recvq_unexpected_tail_ptr)->dev.next = rreq;
			}
			else
			{
			    *MPID_Recvq_unexpected_head_ptr  = rreq;
			}
			*MPID_Recvq_unexpected_tail_ptr  = rreq;
		    }
		}
		break;
	    case MPIDI_CH3_PKT_RNDV_CLR_TO_SEND:
		{
		    MPIDI_CH3_Pkt_rndv_clr_to_send_t *cts_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->rndv_clr_to_send;
		    MPID_Request   *sreq;
		    MPID_Request   *rts_sreq;
		    MPIDI_CH3_Pkt_t upkt;
		    MPIDI_CH3_Pkt_rndv_send_t * rs_pkt = &upkt.rndv_send;
		    int             dt_contig;
		    MPI_Aint        dt_true_lb;
		    MPIDI_msg_sz_t  data_sz;
		    MPID_Datatype  *dt_ptr;
		    MPID_IOV        iov[MPID_IOV_LIMIT];
		    int             iov_n;
		    MPIDI_VC_t     *vc;

		    MPID_Request_get_ptr(cts_pkt->sender_req_id, sreq);
		    MPIDI_Request_fetch_and_clear_rts_sreq(sreq, &rts_sreq);
		    if (rts_sreq != NULL)
		    {
			MPID_Request_release(rts_sreq);
		    }

		    MPIDI_Pkt_init(rs_pkt, MPIDI_CH3_PKT_RNDV_SEND);
		    rs_pkt->receiver_req_id = cts_pkt->receiver_req_id;
		    iov[0].MPID_IOV_BUF = (void*)rs_pkt;
		    iov[0].MPID_IOV_LEN = sizeof(*rs_pkt);
		    MPIDI_Datatype_get_info(sreq->dev.user_count, sreq->dev.datatype, dt_contig, data_sz, dt_ptr, dt_true_lb);

		    if (dt_contig)
		    {
			sreq->dev.OnDataAvail = 0;
			iov[1].MPID_IOV_BUF = (char *)sreq->dev.user_buf + dt_true_lb;
			iov[1].MPID_IOV_LEN = data_sz;
			iov_n = 2;
		    }
		    else
		    {
			MPID_Segment_init(sreq->dev.user_buf, sreq->dev.user_count, sreq->dev.datatype, &sreq->dev.segment,0);
			iov_n = MPID_IOV_LIMIT - 1;
			sreq->dev.segment_first = 0;
			sreq->dev.segment_size = data_sz;
			mpi_errno = MPIDI_CH3U_Request_load_send_iov(sreq, &iov[1], &iov_n);
			/* --BEGIN ERROR HANDLING-- */
			if (mpi_errno != MPI_SUCCESS)
			{
			    mpi_errno = MPIR_Err_create_code(mpi_errno, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER,
							     "**ch3|loadsendiov", 0);
			    goto exit_fn;
			}
			/* --END ERROR HANDLING-- */
			iov_n += 1;
		    }
			    
		    if (in_fbox)
		    {
			MPIDI_PG_Get_vc (MPIDI_Process.my_pg, MPID_NEM_FBOX_SOURCE (cell), &vc);
		    }
		    else
		    {
			MPIDI_PG_Get_vc (MPIDI_Process.my_pg, MPID_NEM_CELL_SOURCE (cell), &vc);
		    }
		    mpi_errno = MPIDI_CH3_iSendv(vc, sreq, iov, iov_n);
		    /* --BEGIN ERROR HANDLING-- */
		    if (mpi_errno != MPI_SUCCESS)
		    {
			mpi_errno = MPIR_Err_create_code(mpi_errno, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**ch3|senddata", 0);
			goto exit_fn;
		    }
		    /* --END ERROR HANDLING-- */
		}
		break;
	    case MPIDI_CH3_PKT_RNDV_SEND:
		{
		    /* this case can't happen since there is a posted request for the recv */
		    /* this code is only active when both queues are empty */
		}
		break;
	    case MPIDI_CH3_PKT_CANCEL_SEND_REQ:
		{
		    MPIDI_CH3_Pkt_cancel_send_req_t * req_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->cancel_send_req;
		    MPID_Request * rreq;
		    int ack;
		    MPIDI_CH3_Pkt_t upkt;
		    MPIDI_CH3_Pkt_cancel_send_resp_t * resp_pkt = &upkt.cancel_send_resp;
		    MPID_Request * resp_sreq;
		    MPIDI_VC_t     *vc;

		    rreq = MPIDI_CH3U_Recvq_FDU(req_pkt->sender_req_id, &req_pkt->match);
		    if (rreq != NULL)
		    {
			if (MPIDI_Request_get_msg_type(rreq) == MPIDI_REQUEST_EAGER_MSG && rreq->dev.recv_data_sz > 0)
			{
			    MPIU_Free(rreq->dev.tmpbuf);
			}
			MPID_Request_release(rreq);
			ack = TRUE;
		    }
		    else
		    {
			ack = FALSE;
		    }
		    MPIDI_Pkt_init(resp_pkt, MPIDI_CH3_PKT_CANCEL_SEND_RESP);
		    resp_pkt->sender_req_id = req_pkt->sender_req_id;
		    resp_pkt->ack = ack;
		    if (in_fbox)
		    {
			MPIDI_PG_Get_vc (MPIDI_Process.my_pg, MPID_NEM_FBOX_SOURCE (cell), &vc);
		    }
		    else
		    {
			MPIDI_PG_Get_vc (MPIDI_Process.my_pg, MPID_NEM_CELL_SOURCE (cell), &vc);
		    }
		    mpi_errno = MPIDI_CH3_iStartMsg(vc, resp_pkt, sizeof(*resp_pkt), &resp_sreq);
		    /* --BEGIN ERROR HANDLING-- */
		    if (mpi_errno != MPI_SUCCESS)
		    {
			mpi_errno = MPIR_Err_create_code(mpi_errno, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**\
ch3|cancelresp", 0);
			goto exit_fn;
		    }
		    /* --END ERROR HANDLING-- */
		    if (resp_sreq != NULL)
		    {
			MPID_Request_release(resp_sreq);
		    }
		}
		break;
	    case MPIDI_CH3_PKT_CANCEL_SEND_RESP:
		{
		    MPIDI_CH3_Pkt_cancel_send_resp_t * resp_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->cancel_send_resp;
		    MPID_Request * sreq;

		    MPID_Request_get_ptr(resp_pkt->sender_req_id, sreq);
		    if (resp_pkt->ack)
		    {
			sreq->status.cancelled = TRUE;
			if (MPIDI_Request_get_msg_type(sreq) == MPIDI_REQUEST_RNDV_MSG ||
			    MPIDI_Request_get_type(sreq) == MPIDI_REQUEST_TYPE_SSEND)
			{
			    int cc;
			    MPIDI_CH3U_Request_decrement_cc(sreq, &cc);
			}
		    }
		    else
		    {
			MPIDI_DBG_PRINTF((35, FCNAME, "unable to cancel message"));
		    }
		    MPIDI_CH3U_Request_complete(sreq);
		}
		break;
	    case MPIDI_CH3_PKT_PUT:
		{
		    MPIDI_CH3_Pkt_put_t * put_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->put;
		    fprintf(stdout,"ERROR : MPIDI_CH3_PKT_PUT not handled (yet) \n");			    
		}
		break;
	    case MPIDI_CH3_PKT_ACCUMULATE:
		{
		    MPIDI_CH3_Pkt_accum_t * accum_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->accum;
		    fprintf(stdout,"ERROR : MPIDI_CH3_PKT_ACCUMULATE not handled (yet) \n");			    
		}
		break;			
	    case MPIDI_CH3_PKT_GET:
		{
		    MPIDI_CH3_Pkt_get_t * get_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->get;
		    fprintf(stdout,"ERROR : MPIDI_CH3_PKT_GET not handled (yet) \n");			    
		}
		break;			
	    case MPIDI_CH3_PKT_GET_RESP:
		{
		    MPIDI_CH3_Pkt_get_resp_t * get_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->get_resp;
		    fprintf(stdout,"ERROR : MPIDI_CH3_PKT_GET_RESP not handled (yet) \n");			    
		}
		break;			
	    case MPIDI_CH3_PKT_LOCK:
		{
		    MPIDI_CH3_Pkt_lock_t * lock_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->lock;
		    MPID_Win *win_ptr;
		    MPIDI_VC_t     *vc;

		    if (in_fbox)
		    {
			MPIDI_PG_Get_vc (MPIDI_Process.my_pg, MPID_NEM_FBOX_SOURCE (cell), &vc);
		    }
		    else
		    {
			MPIDI_PG_Get_vc (MPIDI_Process.my_pg, MPID_NEM_CELL_SOURCE (cell), &vc);
		    }

		    MPID_Win_get_ptr(lock_pkt->target_win_handle, win_ptr);
		    if (MPIDI_CH3I_Try_acquire_win_lock(win_ptr,lock_pkt->lock_type) == 1)
		    {				 
			mpi_errno = MPIDI_CH3I_Send_lock_granted_pkt(vc,lock_pkt->source_win_handle);
		    }			    
		    else 
		    {
			/* queue the lock information */
			MPIDI_Win_lock_queue *curr_ptr, *prev_ptr, *new_ptr;
				    
			/* FIXME: MT: This may need to be done atomically. */
				    
			curr_ptr = (MPIDI_Win_lock_queue *) win_ptr->lock_queue;
			prev_ptr = curr_ptr;
			while (curr_ptr != NULL)
			{
			    prev_ptr = curr_ptr;
			    curr_ptr = curr_ptr->next;
			}
				    
			new_ptr = (MPIDI_Win_lock_queue *) MPIU_Malloc(sizeof(MPIDI_Win_lock_queue));
			/* --BEGIN ERROR HANDLING-- */
			if (!new_ptr)
			{
			    mpi_errno = MPIR_Err_create_code( MPI_SUCCESS, MPIR_ERR_RECOVERABLE, FCNAME, __LINE__, MPI_ERR_OTHER,
							      "**nomem", 0 );
			    goto exit_fn;
			}
			/* --END ERROR HANDLING-- */
			if (prev_ptr != NULL)
			    prev_ptr->next = new_ptr;
			else
			    win_ptr->lock_queue = new_ptr;
				
			new_ptr->next = NULL;
			new_ptr->lock_type = lock_pkt->lock_type;
			new_ptr->source_win_handle = lock_pkt->source_win_handle;
			new_ptr->vc = vc;
			new_ptr->pt_single_op = NULL;
		    }
		}
		break;			
	    case MPIDI_CH3_PKT_LOCK_GRANTED:
		{
		    MPIDI_CH3_Pkt_lock_granted_t * lock_granted_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->lock_granted;
		    MPID_Win *win_ptr;
		    MPID_Win_get_ptr(lock_granted_pkt->source_win_handle, win_ptr);
		    win_ptr->lock_granted = 1;
		    MPIDI_CH3_Progress_signal_completion();
		}
		break;			
	    case MPIDI_CH3_PKT_PT_RMA_DONE:
		{
		    MPIDI_CH3_Pkt_pt_rma_done_t * pt_rma_done_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->pt_rma_done;
		    MPID_Win *win_ptr;
		    MPID_Win_get_ptr(pt_rma_done_pkt->source_win_handle, win_ptr);
		    win_ptr->lock_granted = 0;
		    MPIDI_CH3_Progress_signal_completion();
		}
		break;
	    case MPIDI_CH3_PKT_LOCK_PUT_UNLOCK:
		{
		    MPIDI_CH3_Pkt_lock_put_unlock_t * lock_put_unlock_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->lock_put_unlock;
		    fprintf(stdout,"ERROR : MPIDI_CH3_PKT_LOCK_PUT_UNLOCK not handled (yet) \n");			    
		}
		break;
	    case MPIDI_CH3_PKT_LOCK_GET_UNLOCK:
		{
		    MPIDI_CH3_Pkt_lock_get_unlock_t * lock_get_unlock_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->lock_get_unlock;
		    fprintf(stdout,"ERROR : MPIDI_CH3_PKT_LOCK_GET_UNLOCK not handled (yet) \n");			    
		}
		break;						
	    default:
		{
		    /* nothing */
		}
	    }
		
	    if (!in_fbox)
	    {
		MPIDI_VC_t   *vc;
		MPIDI_PG_Get_vc (MPIDI_Process.my_pg, MPID_NEM_CELL_SOURCE (cell), &vc);
		MPID_nem_mpich2_release_cell (cell, vc);
	    }
	    else
	    {
		MPID_nem_mpich2_release_fbox (cell);
	    }	 

	    if(*foundp == FALSE)
	    {
		/* the cell does not match the request: create one */
		/* this is the request that sould be returned !    */
		goto make_req;
	    }
	}
	else
	{
	make_req:
	    rreq  = MPID_Request_create();
	    if (rreq != NULL)
	    {
		MPIU_Object_set_ref(rreq, 2);
		rreq->kind                 = MPID_REQUEST_RECV;
		rreq->dev.match.tag        = tag;
		rreq->dev.match.rank       = source;
		rreq->dev.match.context_id = context_id;
		rreq->dev.next             = NULL;			  
		if (*MPID_Recvq_posted_tail_ptr != NULL)
		{
		    (*MPID_Recvq_posted_tail_ptr)->dev.next = rreq;
		}
		else
		{
		    *MPID_Recvq_posted_head_ptr = rreq;
		}
		*MPID_Recvq_posted_tail_ptr = rreq;
		MPIDI_POSTED_RECV_ENQUEUE_HOOK (rreq);
	    }
	}	
    }
 exit_fn:
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDI_CH3I_PROGRESS_POKE_WITH_MATCHING);
    return rreq;
}

#undef FUNCNAME
#define FUNCNAME MPIDI_CH3_Progress_ipoke_with_matching
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
MPID_Request *  MPIDI_CH3_Progress_ipoke_with_matching (int source, int tag, MPID_Comm *comm,int context_id,int *foundp, void *buf, int count, MPI_Datatype datatype,MPI_Status * status)   
{
    int             mpi_errno = MPI_SUCCESS;
    MPID_Request   *rreq  = NULL;
    MPID_nem_cell_ptr_t cell  = NULL;
    int             in_fbox;
    int             dt_contig;
    MPI_Aint        dt_true_lb;
    MPIDI_msg_sz_t  userbuf_sz;
    MPID_Datatype  *dt_ptr;
    

    MPIDI_STATE_DECL(MPID_STATE_MPIDI_CH3I_PROGRESS_IPOKE_WITH_MATCHING);
    MPIDI_FUNC_ENTER(MPID_STATE_MPIDI_CH3I_PROGRESS_IPOKE_WITH_MATCHING);    
    MPIDI_DBG_PRINTF((50, FCNAME, "entering, buf=%p, count=%d, dtype=%d",
		      buf,count,datatype));

    *foundp = FALSE ;
    MPIDI_Datatype_get_info(count, datatype, dt_contig, userbuf_sz, dt_ptr, dt_true_lb);

    /* handle only contiguous types (for now) and one-cell packets */
    if((dt_contig) && (( userbuf_sz <= MPID_NEM__BYPASS_Q_MAX_VAL))) 
    {
	MPID_nem_mpich2_test_recv_wait (&cell, &in_fbox,1000);
	
	if (cell)
	    {	 
		char *cell_buf    = cell->pkt.mpich2.payload;

		switch(((MPIDI_CH3_Pkt_t *)cell_buf)->type)
		    {
		    case MPIDI_CH3_PKT_EAGER_SEND:
			{
			    MPIDI_CH3_Pkt_eager_send_t *eager_pkt =  &((MPIDI_CH3_Pkt_t *)cell_buf)->eager_send;
			    int payload_len = eager_pkt->data_sz; 
			    cell_buf += sizeof (MPIDI_CH3_Pkt_t);
			    
			    if(((eager_pkt->match.tag  == tag   )||(tag    == MPI_ANY_TAG   )) &&
			       ((eager_pkt->match.rank == source)||(source == MPI_ANY_SOURCE)) &&
			        (eager_pkt->match.context_id == context_id))
				{
				    /* cell matches */
				    *foundp = TRUE;
				    
				    if (payload_len > 0)
					{
					    if (payload_len <= userbuf_sz)
						{				    
						    MPID_NEM_MEMCPY((char *)(buf+ dt_true_lb), cell_buf,payload_len);
						}
					    else
						{
						    /* error : truncate */
						    MPID_NEM_MEMCPY((char *)(buf+dt_true_lb),cell_buf, userbuf_sz);
						    status->MPI_ERROR = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE, FCNAME, __LINE__, MPI_ERR_TRUNCATE,"**truncate", "**truncate %d %d %d %d", status->MPI_SOURCE,status->MPI_TAG,payload_len, userbuf_sz );
						    mpi_errno = status->MPI_ERROR;
						    goto exit_fn;
						}
					}
				}
			    else
				{
				    /* create a request for the cell, enqueue it on
				       the unexpected queue */
				    rreq  = MPID_Request_create();
				    if (rreq != NULL)
					{
					    MPIU_Object_set_ref(rreq, 2);
					    rreq->kind                 = MPID_REQUEST_RECV;
					    rreq->dev.match.tag        = eager_pkt->match.tag ;
					    rreq->dev.match.rank       = eager_pkt->match.rank;
					    rreq->dev.match.context_id = eager_pkt->match.context_id;
					    rreq->dev.tmpbuf           = MPIU_Malloc(userbuf_sz);
					    MPID_NEM_MEMCPY((char *)(rreq->dev.tmpbuf),cell_buf, userbuf_sz);
					    rreq->dev.next             = NULL;
					    if (MPIDI_Process.recvq_unexpected_tail != NULL)
						{
						    MPIDI_Process.recvq_unexpected_tail->dev.next = rreq;
						}
					    else
						{
						    MPIDI_Process.recvq_unexpected_head = rreq;
						}
					    MPIDI_Process.recvq_unexpected_tail = rreq;
					}
				}
			}
			break;
		    case MPIDI_CH3_PKT_READY_SEND:
			{
			    MPIDI_CH3_Pkt_ready_send_t *ready_pkt =  &((MPIDI_CH3_Pkt_t *)cell_buf)->ready_send;
			    fprintf(stdout,"ERROR : MPIDI_CH3_PKT_READY_SEND not handled (yet) \n");			    
			}
			break;
		    case MPIDI_CH3_PKT_EAGER_SYNC_SEND:
			{
			    MPIDI_CH3_Pkt_eager_send_t *es_pkt =  &((MPIDI_CH3_Pkt_t *)cell_buf)->eager_send;
			    int payload_len = es_pkt->data_sz; 
			    cell_buf += sizeof (MPIDI_CH3_Pkt_t);
			    
			    if(((es_pkt->match.tag  == tag   )||(tag    == MPI_ANY_TAG   )) &&
			       ((es_pkt->match.rank == source)||(source == MPI_ANY_SOURCE)) &&
			        (es_pkt->match.context_id == context_id))
				{
				    MPIDI_CH3_Pkt_t  upkt;
				    MPIDI_CH3_Pkt_eager_sync_ack_t * const esa_pkt = &upkt.eager_sync_ack;
				    MPID_Request * esa_req = NULL;
				    MPIDI_VC_t   *vc;

				    /* cell matches */
				    *foundp = TRUE;
				    
				    if (payload_len > 0)
					{
					    if (payload_len <= userbuf_sz)
						{				    
						    MPID_NEM_MEMCPY((char *)(buf+ dt_true_lb), cell_buf,payload_len);
						}
					    else
						{
						    /* error : truncate */
						    MPID_NEM_MEMCPY((char *)(buf+dt_true_lb),cell_buf, userbuf_sz);
						    status->MPI_ERROR = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE, FCNAME, __LINE__, MPI_ERR_TRUNCATE,"**truncate", "**truncate %d %d %d %d", status->MPI_SOURCE,status->MPI_TAG,payload_len, userbuf_sz );
						    mpi_errno = status->MPI_ERROR;
						    goto exit_fn;
						}
					}
				    
				    /* send Ack back */
				    MPIDI_Pkt_init(esa_pkt, MPIDI_CH3_PKT_EAGER_SYNC_ACK);
				    esa_pkt->sender_req_id = es_pkt->sender_req_id;
				    if (in_fbox)
					{
					    MPIDI_PG_Get_vc (MPIDI_Process.my_pg, MPID_NEM_FBOX_SOURCE (cell), &vc);
					}
				    else
					{
					    MPIDI_PG_Get_vc (MPIDI_Process.my_pg, MPID_NEM_CELL_SOURCE (cell), &vc);
					}
				    
				    mpi_errno = MPIDI_CH3_iStartMsg(vc, esa_pkt, sizeof(*esa_pkt), &esa_req);
				    
				    /* --BEGIN ERROR HANDLING-- */
				    if (mpi_errno != MPI_SUCCESS)
					{
					    mpi_errno = MPIR_Err_create_code(mpi_errno, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER,"**ch3|syncack", 0);
					    goto exit_fn;
					}
				    /* --END ERROR HANDLING-- */
				    if (esa_req != NULL)
					{
					    MPID_Request_release(esa_req);
					}
				}
			    else
				{
				    /* create a request for the cell, enqueue it on
				       the unexpected queue */
				    rreq  = MPID_Request_create();
				    if (rreq != NULL)
					{
					    MPIU_Object_set_ref(rreq, 2);
					    rreq->kind                 = MPID_REQUEST_RECV;
					    rreq->dev.match.tag        = es_pkt->match.tag ;
					    rreq->dev.match.rank       = es_pkt->match.rank;
					    rreq->dev.match.context_id = es_pkt->match.context_id;
					    rreq->dev.tmpbuf           = MPIU_Malloc(userbuf_sz);
					    MPID_NEM_MEMCPY((char *)(rreq->dev.tmpbuf),cell_buf, userbuf_sz);
					    rreq->dev.next             = NULL;
					    if (MPIDI_Process.recvq_unexpected_tail != NULL)
						{
						    MPIDI_Process.recvq_unexpected_tail->dev.next = rreq;
						}
					    else
						{
						    MPIDI_Process.recvq_unexpected_head = rreq;
						}
					    MPIDI_Process.recvq_unexpected_tail = rreq;
					    MPIDI_Request_set_sync_send_flag(rreq,TRUE);
					}
				}
			}
			break;
		    case MPIDI_CH3_PKT_EAGER_SYNC_ACK:
			{
			    MPIDI_CH3_Pkt_eager_sync_ack_t * esa_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->eager_sync_ack;
			    MPID_Request * sreq;
			    MPID_Request_get_ptr(esa_pkt->sender_req_id, sreq);
			    MPIDI_CH3U_Request_complete(sreq);
			}
			break;
		    case MPIDI_CH3_PKT_RNDV_REQ_TO_SEND:
			{
			    /* this case in currently disabled since cells are smaller than eager msgs, but ... */
			    MPIDI_CH3_Pkt_rndv_req_to_send_t *rts_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->rndv_req_to_send;
			    rreq  = MPID_Request_create();
			    if (rreq != NULL)
				{
				    MPIU_Object_set_ref(rreq, 2);
				    rreq->kind                 = MPID_REQUEST_RECV;
				    rreq->dev.next             = NULL;     
				}    
			    
			    if(((rts_pkt->match.tag  == tag   )||(tag    == MPI_ANY_TAG   )) &&
			       ((rts_pkt->match.rank == source)||(source == MPI_ANY_SOURCE)) &&
			        (rts_pkt->match.context_id == context_id))
				{
				    *foundp = TRUE;
				    rreq->dev.match.tag        = tag;
				    rreq->dev.match.rank       = source;
				    rreq->dev.match.context_id = context_id;
				    rreq->comm                 = comm;
				    MPIR_Comm_add_ref(comm);
				    rreq->dev.user_buf         = buf;
				    rreq->dev.user_count       = count;
				    rreq->dev.datatype         = datatype;
				    set_request_info(rreq,rts_pkt, MPIDI_REQUEST_RNDV_MSG);
				}
			    else
				{
				    /* enqueue rreq on the unexp queue */
				    rreq->dev.match.tag        = rts_pkt->match.tag;
				    rreq->dev.match.rank       = rts_pkt->match.rank;
				    rreq->dev.match.context_id = rts_pkt->match.context_id;
				    if (MPIDI_Process.recvq_unexpected_tail != NULL)
					{
					    MPIDI_Process.recvq_unexpected_tail->dev.next = rreq;
						}
				    else
					{
					    MPIDI_Process.recvq_unexpected_head = rreq;
					}
				    MPIDI_Process.recvq_unexpected_tail = rreq;
				}
			}
			break;
		    case MPIDI_CH3_PKT_RNDV_CLR_TO_SEND:
			{
			    MPIDI_CH3_Pkt_rndv_clr_to_send_t *cts_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->rndv_clr_to_send;
			    MPID_Request   *sreq;
			    MPID_Request   *rts_sreq;
			    MPIDI_CH3_Pkt_t upkt;
			    MPIDI_CH3_Pkt_rndv_send_t * rs_pkt = &upkt.rndv_send;
			    int             dt_contig;
			    MPI_Aint        dt_true_lb;
			    MPIDI_msg_sz_t  data_sz;
			    MPID_Datatype  *dt_ptr;
			    MPID_IOV        iov[MPID_IOV_LIMIT];
			    int             iov_n;
			    MPIDI_VC_t     *vc;

			    MPID_Request_get_ptr(cts_pkt->sender_req_id, sreq);
			    MPIDI_Request_fetch_and_clear_rts_sreq(sreq, &rts_sreq);
			    if (rts_sreq != NULL)
				{
				    MPID_Request_release(rts_sreq);
				}

			    MPIDI_Pkt_init(rs_pkt, MPIDI_CH3_PKT_RNDV_SEND);
			    rs_pkt->receiver_req_id = cts_pkt->receiver_req_id;
			    iov[0].MPID_IOV_BUF = (void*)rs_pkt;
			    iov[0].MPID_IOV_LEN = sizeof(*rs_pkt);
			    MPIDI_Datatype_get_info(sreq->dev.user_count, sreq->dev.datatype, dt_contig, data_sz, dt_ptr, dt_true_lb);

			    if (dt_contig)
				{
				    sreq->dev.OnDataAvail = 0;
				    iov[1].MPID_IOV_BUF = (char *)sreq->dev.user_buf + dt_true_lb;
				    iov[1].MPID_IOV_LEN = data_sz;
				    iov_n = 2;
				}
			    else
				{
				    MPID_Segment_init(sreq->dev.user_buf, sreq->dev.user_count, sreq->dev.datatype, &sreq->dev.segment,0);
				    iov_n = MPID_IOV_LIMIT - 1;
				    sreq->dev.segment_first = 0;
				    sreq->dev.segment_size = data_sz;
				    mpi_errno = MPIDI_CH3U_Request_load_send_iov(sreq, &iov[1], &iov_n);
				    /* --BEGIN ERROR HANDLING-- */
				    if (mpi_errno != MPI_SUCCESS)
					{
					    mpi_errno = MPIR_Err_create_code(mpi_errno, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER,
									     "**ch3|loadsendiov", 0);
					    goto exit_fn;
					}
				    /* --END ERROR HANDLING-- */
				    iov_n += 1;
				}
			    
			    if (in_fbox)
				{
				    MPIDI_PG_Get_vc (MPIDI_Process.my_pg, MPID_NEM_FBOX_SOURCE (cell), &vc);
				}
			    else
				{
				    MPIDI_PG_Get_vc (MPIDI_Process.my_pg, MPID_NEM_CELL_SOURCE (cell), &vc);
				}
			    mpi_errno = MPIDI_CH3_iSendv(vc, sreq, iov, iov_n);
			    /* --BEGIN ERROR HANDLING-- */
			    if (mpi_errno != MPI_SUCCESS)
				{
				    mpi_errno = MPIR_Err_create_code(mpi_errno, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**ch3|senddata", 0);
				    goto exit_fn;
				}
			    /* --END ERROR HANDLING-- */
			}
			break;
		    case MPIDI_CH3_PKT_RNDV_SEND:
			{
			    /* this case can't happen since there is a posted request for the recv */
			    /* this code is only active when both queues are empty */
			}
			break;
		    case MPIDI_CH3_PKT_CANCEL_SEND_REQ:
			{
			    MPIDI_CH3_Pkt_cancel_send_req_t * req_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->cancel_send_req;
			    MPID_Request * rreq;
			    int ack;
			    MPIDI_CH3_Pkt_t upkt;
			    MPIDI_CH3_Pkt_cancel_send_resp_t * resp_pkt = &upkt.cancel_send_resp;
			    MPID_Request * resp_sreq;
			    MPIDI_VC_t     *vc;

			    rreq = MPIDI_CH3U_Recvq_FDU(req_pkt->sender_req_id, &req_pkt->match);
			    if (rreq != NULL)
				{
				    if (MPIDI_Request_get_msg_type(rreq) == MPIDI_REQUEST_EAGER_MSG && rreq->dev.recv_data_sz > 0)
					{
					    MPIU_Free(rreq->dev.tmpbuf);
					}
				    MPID_Request_release(rreq);
				    ack = TRUE;
				}
			    else
				{
				    ack = FALSE;
				}
			    MPIDI_Pkt_init(resp_pkt, MPIDI_CH3_PKT_CANCEL_SEND_RESP);
			    resp_pkt->sender_req_id = req_pkt->sender_req_id;
			    resp_pkt->ack = ack;
			    if (in_fbox)
				{
				    MPIDI_PG_Get_vc (MPIDI_Process.my_pg, MPID_NEM_FBOX_SOURCE (cell), &vc);
				}
			    else
				{
				    MPIDI_PG_Get_vc (MPIDI_Process.my_pg, MPID_NEM_CELL_SOURCE (cell), &vc);
				}
			    mpi_errno = MPIDI_CH3_iStartMsg(vc, resp_pkt, sizeof(*resp_pkt), &resp_sreq);
			    /* --BEGIN ERROR HANDLING-- */
			    if (mpi_errno != MPI_SUCCESS)
				{
				    mpi_errno = MPIR_Err_create_code(mpi_errno, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**\
ch3|cancelresp", 0);
				    goto exit_fn;
				}
			    /* --END ERROR HANDLING-- */
			    if (resp_sreq != NULL)
				{
				    MPID_Request_release(resp_sreq);
				}
			}
			break;
		    case MPIDI_CH3_PKT_CANCEL_SEND_RESP:
			{
			    MPIDI_CH3_Pkt_cancel_send_resp_t * resp_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->cancel_send_resp;
			    MPID_Request * sreq;

			    MPID_Request_get_ptr(resp_pkt->sender_req_id, sreq);
			    if (resp_pkt->ack)
				{
				    sreq->status.cancelled = TRUE;
				    if (MPIDI_Request_get_msg_type(sreq) == MPIDI_REQUEST_RNDV_MSG ||
					MPIDI_Request_get_type(sreq) == MPIDI_REQUEST_TYPE_SSEND)
					{
					    int cc;
					    MPIDI_CH3U_Request_decrement_cc(sreq, &cc);
					}
				}
			    else
				{
				    MPIDI_DBG_PRINTF((35, FCNAME, "unable to cancel message"));
				}
			    MPIDI_CH3U_Request_complete(sreq);
			}
			break;
		    case MPIDI_CH3_PKT_PUT:
			{
			    MPIDI_CH3_Pkt_put_t * put_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->put;
			    fprintf(stdout,"ERROR : MPIDI_CH3_PKT_PUT not handled (yet) \n");			    
			}
			break;
		    case MPIDI_CH3_PKT_ACCUMULATE:
			{
			    MPIDI_CH3_Pkt_accum_t * accum_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->accum;
			    fprintf(stdout,"ERROR : MPIDI_CH3_PKT_ACCUMULATE not handled (yet) \n");			    
			}
			break;			
		    case MPIDI_CH3_PKT_GET:
			{
			    MPIDI_CH3_Pkt_get_t * get_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->get;
			    fprintf(stdout,"ERROR : MPIDI_CH3_PKT_GET not handled (yet) \n");			    
			}
			break;			
		    case MPIDI_CH3_PKT_GET_RESP:
			{
			    MPIDI_CH3_Pkt_get_resp_t * get_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->get_resp;
			    fprintf(stdout,"ERROR : MPIDI_CH3_PKT_GET_RESP not handled (yet) \n");			    
			}
			break;			
		    case MPIDI_CH3_PKT_LOCK:
			{
			    MPIDI_CH3_Pkt_lock_t * lock_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->lock;
			    MPID_Win *win_ptr;
			    MPIDI_VC_t     *vc;

			    if (in_fbox)
				{
				    MPIDI_PG_Get_vc (MPIDI_Process.my_pg, MPID_NEM_FBOX_SOURCE (cell), &vc);
				}
			    else
				{
				    MPIDI_PG_Get_vc (MPIDI_Process.my_pg, MPID_NEM_CELL_SOURCE (cell), &vc);
				}

			    MPID_Win_get_ptr(lock_pkt->target_win_handle, win_ptr);
			    if (MPIDI_CH3I_Try_acquire_win_lock(win_ptr,lock_pkt->lock_type) == 1)
				{				 
				    mpi_errno = MPIDI_CH3I_Send_lock_granted_pkt(vc,lock_pkt->source_win_handle);
				}			    
			    else 
				{
				    /* queue the lock information */
				    MPIDI_Win_lock_queue *curr_ptr, *prev_ptr, *new_ptr;
				    
				    /* FIXME: MT: This may need to be done atomically. */
				    
				    curr_ptr = (MPIDI_Win_lock_queue *) win_ptr->lock_queue;
				    prev_ptr = curr_ptr;
				    while (curr_ptr != NULL)
					{
					    prev_ptr = curr_ptr;
					    curr_ptr = curr_ptr->next;
					}
				    
				    new_ptr = (MPIDI_Win_lock_queue *) MPIU_Malloc(sizeof(MPIDI_Win_lock_queue));
				    /* --BEGIN ERROR HANDLING-- */
				    if (!new_ptr)
					{
					    mpi_errno = MPIR_Err_create_code( MPI_SUCCESS, MPIR_ERR_RECOVERABLE, FCNAME, __LINE__, MPI_ERR_OTHER,
									      "**nomem", 0 );
					    goto exit_fn;
					}
				    /* --END ERROR HANDLING-- */
				if (prev_ptr != NULL)
				    prev_ptr->next = new_ptr;
				else
				    win_ptr->lock_queue = new_ptr;
				
				new_ptr->next = NULL;
				new_ptr->lock_type = lock_pkt->lock_type;
				new_ptr->source_win_handle = lock_pkt->source_win_handle;
				new_ptr->vc = vc;
				new_ptr->pt_single_op = NULL;
				}
			}
			break;			
		    case MPIDI_CH3_PKT_LOCK_GRANTED:
			{
			    MPIDI_CH3_Pkt_lock_granted_t * lock_granted_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->lock_granted;
			    MPID_Win *win_ptr;
			    MPID_Win_get_ptr(lock_granted_pkt->source_win_handle, win_ptr);
			    win_ptr->lock_granted = 1;
			    MPIDI_CH3_Progress_signal_completion();
			}
			break;			
		    case MPIDI_CH3_PKT_PT_RMA_DONE:
			{
			    MPIDI_CH3_Pkt_pt_rma_done_t * pt_rma_done_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->pt_rma_done;
			    MPID_Win *win_ptr;
			    MPID_Win_get_ptr(pt_rma_done_pkt->source_win_handle, win_ptr);
			    win_ptr->lock_granted = 0;
			    MPIDI_CH3_Progress_signal_completion();
			}
			break;
		    case MPIDI_CH3_PKT_LOCK_PUT_UNLOCK:
                        {
                            MPIDI_CH3_Pkt_lock_put_unlock_t * lock_put_unlock_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->lock_put_unlock;
			    fprintf(stdout,"ERROR : MPIDI_CH3_PKT_LOCK_PUT_UNLOCK not handled (yet) \n");			    
			}
			break;
		    case MPIDI_CH3_PKT_LOCK_GET_UNLOCK:
                        {
                            MPIDI_CH3_Pkt_lock_get_unlock_t * lock_get_unlock_pkt = &((MPIDI_CH3_Pkt_t *)cell_buf)->lock_get_unlock;
			    fprintf(stdout,"ERROR : MPIDI_CH3_PKT_LOCK_GET_UNLOCK not handled (yet) \n");			    
			}
			break;						
		    default:
			{
			    /* nothing */
			}
		    }
		
		if (!in_fbox)
		    {
			MPIDI_VC_t *vc;
			MPIDI_PG_Get_vc (MPIDI_Process.my_pg, MPID_NEM_CELL_SOURCE (cell), &vc);
			MPID_nem_mpich2_release_cell (cell, vc);
		    }
		else
		    {
			MPID_nem_mpich2_release_fbox (cell);
		    }	 

		if(*foundp == FALSE)
		    {
			/* the cell does not match the request: create one */
			/* this is the request that sould be returned !    */
			goto make_req;
		    }
	    }
	else
	    {
	    make_req:
		rreq  = MPID_Request_create();
		if (rreq != NULL)
		    {
			MPIU_Object_set_ref(rreq, 2);
			rreq->kind                 = MPID_REQUEST_RECV;
			rreq->dev.match.tag        = tag;
			rreq->dev.match.rank       = source;
			rreq->dev.match.context_id = context_id;
			rreq->dev.next             = NULL;			  
			if (MPIDI_Process.recvq_posted_tail != NULL)
			    {
				MPIDI_Process.recvq_posted_tail->dev.next = rreq;
			    }
			else
			    {
				MPIDI_Process.recvq_posted_head = rreq;
			    }
			MPIDI_Process.recvq_posted_tail = rreq;
			MPIDI_POSTED_RECV_ENQUEUE_HOOK (rreq);
		    }
	    }	
    }
 exit_fn:
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDI_CH3I_PROGRESS_IPOKE_WITH_MATCHING);
    return rreq;
}

#endif  /* BYPASS_PROGRESS */

#undef FUNCNAME
#define FUNCNAME MPIDI_CH3_Progress_poke
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3_Progress_poke(void)
{
    int mpi_errno;
    MPIDI_STATE_DECL(MPID_STATE_MPIDI_CH3_PROGRESS_POKE);
    
    MPIDI_FUNC_ENTER(MPID_STATE_MPIDI_CH3_PROGRESS_POKE);
    
    mpi_errno = MPIDI_CH3I_Progress(FALSE);
    
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDI_CH3_PROGRESS_POKE);
    return mpi_errno;
}

#if !defined(MPIDI_CH3_Progress_end)
#undef FUNCNAME
#define FUNCNAME MPIDI_CH3_Progress_end
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
void MPIDI_CH3_Progress_end(MPID_Progress_state * state)
{
    /* MT: This function is empty for the single-threaded implementation */
    MPIDI_STATE_DECL(MPID_STATE_MPIDI_CH3_PROGRESS_END);

    MPIDI_FUNC_ENTER(MPID_STATE_MPIDI_CH3_PROGRESS_END);
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDI_CH3_PROGRESS_END);
}
#endif

#undef FUNCNAME
#define FUNCNAME MPIDI_CH3I_Progress_init
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3I_Progress_init(void)
{
    int mpi_errno = MPI_SUCCESS;
    int i;
    MPIDI_STATE_DECL(MPID_STATE_MPIDI_CH3I_PROGRESS_INIT);

    MPIDI_FUNC_ENTER(MPID_STATE_MPIDI_CH3I_PROGRESS_INIT);

    for (i = 0; i < CH3_NUM_QUEUES; ++i)
    {
	MPIDI_CH3I_sendq_head[i] = NULL;
	MPIDI_CH3I_sendq_tail[i] = NULL;
    }

    /* Initialize the code to handle incoming packets */
    mpi_errno = MPIDI_CH3_PktHandler_Init( pktArray, MPIDI_CH3_PKT_END_CH3+1 );
    if (mpi_errno) MPIU_ERR_POP(mpi_errno);

 fn_exit:
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDI_CH3I_PROGRESS_INIT);
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}

#undef FUNCNAME
#define FUNCNAME MPIDI_CH3I_Progress_finalize
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3I_Progress_finalize(void)
{
    MPIDI_STATE_DECL(MPID_STATE_MPIDI_CH3I_PROGRESS_FINALIZE);

    MPIDI_FUNC_ENTER(MPID_STATE_MPIDI_CH3I_PROGRESS_FINALIZE);

    MPIDI_FUNC_EXIT(MPID_STATE_MPIDI_CH3I_PROGRESS_FINALIZE);
    return MPI_SUCCESS;
}


#undef FUNCNAME
#define FUNCNAME MPIDI_CH3_Connection_terminate
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3_Connection_terminate (MPIDI_VC_t * vc)
{
    int mpi_errno = MPI_SUCCESS;

    mpi_errno = MPID_nem_net_module_vc_terminate(vc);
    if(mpi_errno) MPIU_ERR_POP(mpi_errno);

    mpi_errno = MPIDI_CH3U_Handle_connection (vc, MPIDI_VC_EVENT_TERMINATED);
    if(mpi_errno) MPIU_ERR_POP(mpi_errno);

fn_exit:
    return mpi_errno;
fn_fail:
    goto fn_exit;
}
/* end MPIDI_CH3_Connection_terminate() */



#undef FUNCNAME
#define FUNCNAME MPIDI_CH3I_Posted_recv_enqueued
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3I_Posted_recv_enqueued (MPID_Request *rreq)
{
    int mpi_errno = MPI_SUCCESS;

    /* don't enqueue for anysource */
    if (rreq->dev.match.rank < 0)
	goto fn_exit;
    /* don't enqueue a fastbox for yourself */
    if (rreq->dev.match.rank == MPIDI_CH3I_my_rank)
	goto fn_exit;
    /* don't enqueue non-local processes */
    if (!MPID_NEM_IS_LOCAL (rreq->dev.match.rank))
	goto fn_exit;

    mpi_errno = MPID_nem_mpich2_enqueue_fastbox (MPID_NEM_LOCAL_RANK (rreq->dev.match.rank));
    if (mpi_errno) MPIU_ERR_POP (mpi_errno);

 fn_exit:
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}

#undef FUNCNAME
#define FUNCNAME MPIDI_CH3I_Posted_recv_dequeued
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3I_Posted_recv_dequeued (MPID_Request *rreq)
{
    int mpi_errno = MPI_SUCCESS;
    
    if (rreq->dev.match.rank < 0)
	goto fn_exit;
    if (rreq->dev.match.rank == MPIDI_CH3I_my_rank)
	goto fn_exit;
    if (!MPID_NEM_IS_LOCAL (rreq->dev.match.rank))
	goto fn_exit;

    mpi_errno = MPID_nem_mpich2_dequeue_fastbox (MPID_NEM_LOCAL_RANK (rreq->dev.match.rank));
    if (mpi_errno) MPIU_ERR_POP (mpi_errno);

 fn_exit:
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}
