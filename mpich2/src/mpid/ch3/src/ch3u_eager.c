/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpidimpl.h"

/*
 * Send an eager message.  To optimize for the important, short contiguous
 * message case, there are separate routines for the contig and non-contig
 * datatype cases.
 */

#undef FUNCNAME
#define FUNCNAME MPIDI_EagerNoncontigSend
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
/* MPIDI_CH3_EagerNoncontigSend - Eagerly send noncontiguous data */
int MPIDI_CH3_EagerNoncontigSend( MPID_Request **sreq_p, 
				  MPIDI_CH3_Pkt_type_t reqtype, 
				  const void * buf, int count, 
				  MPI_Datatype datatype, int data_sz, 
				  int rank, 
				  int tag, MPID_Comm * comm, 
				  int context_offset )
{
    int mpi_errno = MPI_SUCCESS;
    int iov_n;
    MPIDI_VC_t * vc;
    MPID_Request *sreq = *sreq_p;
    MPIDI_CH3_Pkt_t upkt;
    MPIDI_CH3_Pkt_eager_send_t * const eager_pkt = &upkt.eager_send;
    MPID_IOV iov[MPID_IOV_LIMIT];
    
    MPIDI_Pkt_init(eager_pkt, reqtype);
    eager_pkt->match.rank	= comm->rank;
    eager_pkt->match.tag	= tag;
    eager_pkt->match.context_id	= comm->context_id + context_offset;
    eager_pkt->sender_req_id	= MPI_REQUEST_NULL;
    eager_pkt->data_sz		= data_sz;
    
    iov[0].MPID_IOV_BUF = (MPID_IOV_BUF_CAST)eager_pkt;
    iov[0].MPID_IOV_LEN = sizeof(*eager_pkt);
    
    MPIU_DBG_MSG_FMT(CH3_OTHER,VERBOSE,(MPIU_DBG_FDEST,
             "sending non-contiguous eager message, data_sz=" MPIDI_MSG_SZ_FMT,
					data_sz));
	    
    MPID_Segment_init(buf, count, datatype, &sreq->dev.segment, 0);
    sreq->dev.segment_first = 0;
    sreq->dev.segment_size = data_sz;
	    
    iov_n = MPID_IOV_LIMIT - 1;

    /* One the initial load of a send iov req, set the OnFinal action (null
       for point-to-point) */
    sreq->dev.OnFinal = 0;
    mpi_errno = MPIDI_CH3U_Request_load_send_iov(sreq, &iov[1], &iov_n);
    if (mpi_errno == MPI_SUCCESS)
    {
	iov_n += 1;
	
	MPIDI_Comm_get_vc(comm, rank, &vc);
	MPIDI_VC_FAI_send_seqnum(vc, seqnum);
	MPIDI_Pkt_set_seqnum(eager_pkt, seqnum);
	MPIDI_Request_set_seqnum(sreq, seqnum);
	
	mpi_errno = MPIDI_CH3_iSendv(vc, sreq, iov, iov_n);
	/* --BEGIN ERROR HANDLING-- */
	if (mpi_errno != MPI_SUCCESS)
	{
	    MPIU_Object_set_ref(sreq, 0);
	    MPIDI_CH3_Request_destroy(sreq);
	    *sreq_p = NULL;
	    mpi_errno = MPIR_Err_create_code(mpi_errno, MPIR_ERR_FATAL, 
		         FCNAME, __LINE__, MPI_ERR_OTHER, "**ch3|eagermsg", 0);
	    goto fn_exit;
	}
	/* --END ERROR HANDLING-- */

	/* Note that in the non-blocking case, we need to add a ref to the
	   datatypes */
    }
    else
    {
	/* --BEGIN ERROR HANDLING-- */
	MPIU_Object_set_ref(sreq, 0);
	MPIDI_CH3_Request_destroy(sreq);
	*sreq_p = NULL;
	mpi_errno = MPIR_Err_create_code(mpi_errno, MPIR_ERR_RECOVERABLE, 
		    FCNAME, __LINE__, MPI_ERR_OTHER, "**ch3|loadsendiov", 0);
	goto fn_exit;
	/* --END ERROR HANDLING-- */
    }

 fn_exit:
    return mpi_errno;
}

/* Send a contiguous eager message.  We'll want to optimize (and possibly
   inline) this.

   Make sure that buf is at the beginning of the data to send; 
   adjust by adding dt_true_lb if necessary 
*/
#undef FUNCNAME
#define FUNCNAME MPIDI_EagerContigSend
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3_EagerContigSend( MPID_Request **sreq_p, 
			       MPIDI_CH3_Pkt_type_t reqtype, 
			       const void * buf, int data_sz, int rank, 
			       int tag, MPID_Comm * comm, int context_offset )
{
    int mpi_errno = MPI_SUCCESS;
    MPIDI_VC_t * vc;
    MPIDI_CH3_Pkt_t upkt;
    MPIDI_CH3_Pkt_eager_send_t * const eager_pkt = &upkt.eager_send;
    MPID_Request *sreq = *sreq_p;
    MPID_IOV iov[MPID_IOV_LIMIT];
    
    MPIDI_Pkt_init(eager_pkt, reqtype);
    eager_pkt->match.rank	= comm->rank;
    eager_pkt->match.tag	= tag;
    eager_pkt->match.context_id	= comm->context_id + context_offset;
    eager_pkt->sender_req_id	= MPI_REQUEST_NULL;
    eager_pkt->data_sz		= data_sz;
    
    iov[0].MPID_IOV_BUF = (MPID_IOV_BUF_CAST)eager_pkt;
    iov[0].MPID_IOV_LEN = sizeof(*eager_pkt);
    
    MPIU_DBG_MSG_FMT(CH3_OTHER,VERBOSE,(MPIU_DBG_FDEST,
	       "sending contiguous eager message, data_sz=" MPIDI_MSG_SZ_FMT,
					data_sz));
	    
    iov[1].MPID_IOV_BUF = (MPID_IOV_BUF_CAST) buf;
    iov[1].MPID_IOV_LEN = data_sz;
    
    MPIDI_Comm_get_vc(comm, rank, &vc);
    MPIDI_VC_FAI_send_seqnum(vc, seqnum);
    MPIDI_Pkt_set_seqnum(eager_pkt, seqnum);
    
    mpi_errno = MPIDI_CH3_iStartMsgv(vc, iov, 2, sreq_p);
    if (mpi_errno != MPI_SUCCESS) {
	MPIU_ERR_SETFATALANDJUMP(mpi_errno,MPI_ERR_OTHER,"**ch3|eagermsg");
    }

    sreq = *sreq_p;
    if (sreq != NULL)
    {
	MPIDI_Request_set_seqnum(sreq, seqnum);
	MPIDI_Request_set_type(sreq, MPIDI_REQUEST_TYPE_SEND);
    }

 fn_fail:
    return mpi_errno;
}

#ifdef USE_EAGER_SHORT
/* Send a short contiguous eager message.  We'll want to optimize (and possibly
   inline) this 

   Make sure that buf is at the beginning of the data to send; 
   adjust by adding dt_true_lb if necessary 

   We may need a nonblocking (cancellable) version of this, which will 
   have a smaller payload.
*/
#undef FUNCNAME
#define FUNCNAME MPIDI_EagerContigShortSend
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3_EagerContigShortSend( MPID_Request **sreq_p, 
				    MPIDI_CH3_Pkt_type_t reqtype, 
				    const void * buf, int data_sz, int rank, 
				    int tag, MPID_Comm * comm, 
				    int context_offset )
{
    int mpi_errno = MPI_SUCCESS;
    MPIDI_VC_t * vc;
    MPIDI_CH3_Pkt_t upkt;
    MPIDI_CH3_Pkt_eagershort_send_t * const eagershort_pkt = 
	&upkt.eagershort_send;
    MPID_Request *sreq = *sreq_p;
    
    printf( "Sending short eager\n"); fflush(stdout);
    MPIDI_Pkt_init(eagershort_pkt, reqtype);
    eagershort_pkt->match.rank	     = comm->rank;
    eagershort_pkt->match.tag	     = tag;
    eagershort_pkt->match.context_id = comm->context_id + context_offset;
    eagershort_pkt->data_sz	     = data_sz;
    
    MPIU_DBG_MSG_FMT(CH3_OTHER,VERBOSE,(MPIU_DBG_FDEST,
       "sending contiguous short eager message, data_sz=" MPIDI_MSG_SZ_FMT,
					data_sz));
	    
    MPIDI_Comm_get_vc(comm, rank, &vc);
    MPIDI_VC_FAI_send_seqnum(vc, seqnum);
    MPIDI_Pkt_set_seqnum(eager_pkt, seqnum);

    /* Copy the payload. We could optimize this if data_sz & 0x3 == 0 
       (copy (data_sz >> 2) ints, inline that since data size is 
       currently limited to 4 ints */
    {
	unsigned char * restrict p = 
	    (unsigned char *)eagershort_pkt->data;
	unsigned char const * restrict bufp = (unsigned char *)buf;
	int i;
	for (i=0; i<data_sz; i++) {
	    *p++ = *bufp++;
	}
    }

    mpi_errno = MPIDI_CH3_iStartMsg(vc, eagershort_pkt, sizeof(*eagershort_pkt),
				    &sreq );
    if (mpi_errno != MPI_SUCCESS) {
	MPIU_ERR_SETFATALANDJUMP(mpi_errno,MPI_ERR_OTHER,"**ch3|eagermsg");
    }
    if (sreq != NULL) {
	printf( "Surprise, did not complete send of eagershort (starting connection?)\n" ); fflush(stdout);
	MPIDI_Request_set_seqnum(sreq, seqnum);
	MPIDI_Request_set_type(sreq, MPIDI_REQUEST_TYPE_SEND);
	/*	sreq->OnDataAvail = 0; */
    }

 fn_fail:    
    return mpi_errno;
}

/* This is the matching handler for the EagerShort message defined above */

#undef FUNCNAME
#define FUNCNAME MPIDI_CH3_PktHandler_EagerShortSend
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3_PktHandler_EagerShortSend( MPIDI_VC_t *vc, MPIDI_CH3_Pkt_t *pkt, 
					 MPID_Request **rreqp )
{
    MPIDI_CH3_Pkt_eagershort_send_t * eagershort_pkt = &pkt->eagershort_send;
    MPID_Request * rreq;
    int found;
    int mpi_errno = MPI_SUCCESS;

    printf( "Receiving short eager!\n" ); fflush(stdout);
    MPIU_DBG_MSG_FMT(CH3_OTHER,VERBOSE,(MPIU_DBG_FDEST,
	"received eagershort send pkt, rank=%d, tag=%d, context=%d",
	eagershort_pkt->match.rank, 
	eagershort_pkt->match.tag, eagershort_pkt->match.context_id));
	    
    rreq = MPIDI_CH3U_Recvq_FDP_or_AEU(&eagershort_pkt->match, &found);
    if (rreq == NULL) {
	MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER, "**nomemreq");
    }
    
    (rreq)->status.MPI_SOURCE = (eagershort_pkt)->match.rank;
    (rreq)->status.MPI_TAG    = (eagershort_pkt)->match.tag;
    (rreq)->status.count      = (eagershort_pkt)->data_sz;
    (rreq)->dev.recv_data_sz  = (eagershort_pkt)->data_sz;
    MPIDI_Request_set_seqnum((rreq), (eagershort_pkt)->seqnum);
    MPIDI_Request_set_msg_type((rreq), MPIDI_REQUEST_EAGER_MSG);

    *rreqp = rreq;

    /* Extract the data from the packet */

    if (rreq->dev.recv_data_sz == 0) {
	MPIDI_CH3U_Request_complete(rreq);
	*rreqp = NULL;
    }
    else {
	if (found) {
	    int dt_contig;
	    MPI_Aint dt_true_lb;
	    MPIDI_msg_sz_t userbuf_sz;
	    MPID_Datatype * dt_ptr;
	    MPIDI_msg_sz_t data_sz;

	    printf( "Found eager short message\n" ); fflush(stdout);

	    /* Make sure that we handle the general (non-contiguous)
	       datatypes correctly while optimizing for the 
	       special case */
 	    /* mpi_errno = MPIDI_CH3U_Post_data_receive_found( rreq ); */
	    /* Here begins the code from receive_found */
	    MPIDI_Datatype_get_info(rreq->dev.user_count, rreq->dev.datatype, 
				    dt_contig, userbuf_sz, dt_ptr, dt_true_lb);
		
	    if (rreq->dev.recv_data_sz <= userbuf_sz) {
		data_sz = rreq->dev.recv_data_sz;
	    }
	    else {
		MPIU_DBG_MSG_FMT(CH3_OTHER,VERBOSE,(MPIU_DBG_FDEST,
		    "receive buffer too small; message truncated, msg_sz=" MPIDI_MSG_SZ_FMT ", userbuf_sz="
						    MPIDI_MSG_SZ_FMT,
				 rreq->dev.recv_data_sz, userbuf_sz));
		rreq->status.MPI_ERROR = MPIR_Err_create_code(MPI_SUCCESS, 
                     MPIR_ERR_RECOVERABLE, FCNAME, __LINE__, MPI_ERR_TRUNCATE,
		     "**truncate", "**truncate %d %d %d %d", 
		     rreq->status.MPI_SOURCE, rreq->status.MPI_TAG, 
		     rreq->dev.recv_data_sz, userbuf_sz );
		rreq->status.count = userbuf_sz;
		data_sz = userbuf_sz;
	    }

	    if (dt_contig && data_sz == rreq->dev.recv_data_sz) {
		/* user buffer is contiguous and large enough to store the
		   entire message.  We can just copy the code */

		printf( "Copying %d bytes\n", data_sz ); fflush(stdout);
		/* Copy the payload. We could optimize this 
		   if data_sz & 0x3 == 0 
		   (copy (data_sz >> 2) ints, inline that since data size is 
		   currently limited to 4 ints */
		{
		    unsigned char const * restrict p = 
			(unsigned char *)eagershort_pkt->data;
		    unsigned char * restrict bufp = 
			(unsigned char *)(char*)(rreq->dev.user_buf) + dt_true_lb;
		    int i;
		    for (i=0; i<data_sz; i++) {
			*bufp++ = *p++;
		    }
		}
		/* FIXME: We want to set the OnDataAvail to the appropriate 
		   function, which depends on whether this is an RMA 
		   request or a pt-to-pt request. */
		rreq->dev.OnDataAvail = 0;
		/* rreq->dev.recv_pending_count = 1; */
		MPIDI_CH3U_Request_complete(rreq);
	    }
	    else {
		MPIDI_msg_sz_t data_sz, last;
		/* user buffer is not contiguous.  Use the segment
		   code to unpack it, handling various errors and 
		   exceptional cases */
		printf( "Surprise!\n" ); fflush(stdout);
		MPID_Segment_init(rreq->dev.user_buf, rreq->dev.user_count, 
				  rreq->dev.datatype, &rreq->dev.segment, 0);

		data_sz = rreq->dev.recv_data_sz;
		last    = data_sz;
		MPID_Segment_unpack( &rreq->dev.segment, 0, 
				     &last, eagershort_pkt->data );
		if (last != data_sz) {
		    /* --BEGIN ERROR HANDLING-- */
		    /* received data was not entirely consumed by unpack() 
		       because too few bytes remained to fill the next basic
		       datatype */
		    rreq->status.count = (int)last;
		    rreq->status.MPI_ERROR = MPIR_Err_create_code(MPI_SUCCESS, 
                         MPIR_ERR_RECOVERABLE, FCNAME, __LINE__, MPI_ERR_TYPE,
			 "**dtypemismatch", 0);
		    /* --END ERROR HANDLING-- */
		}
		/* FIXME: We want to set the OnDataAvail to the appropriate 
		   function, which depends on whether this is an RMA 
		   request or a pt-to-pt request. */
		rreq->dev.OnDataAvail = 0;
		MPIDI_CH3U_Request_complete(rreq);
	    }
	}
	else {
	    MPIDI_msg_sz_t data_sz;
	    /* This is easy; copy the data into a temporary buffer.
	       To begin with, we use the same temporary location as
	       is used in receiving eager unexpected data.
	     */
	    /* FIXME: When eagershort is enabled, provide a preallocated
               space for short messages (which is used even if eager short
	       is not used), since we don't want to have a separate check
	       to figure out which buffer we're using (or perhaps we should 
	       have a free-buffer-pointer, which can be null if it isn't
               a buffer that we've allocated). */
	    data_sz = rreq->dev.recv_data_sz;
	    rreq->dev.tmpbuf = MPIU_Malloc(data_sz);
	    if (!rreq->dev.tmpbuf) {
		MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER,"**nomem");
	    }
	    rreq->dev.tmpbuf_sz = data_sz;
 	    /* Copy the payload. We could optimize this if data_sz & 0x3 == 0 
	       (copy (data_sz >> 2) ints, inline that since data size is 
	       currently limited to 4 ints */
	    {
		unsigned char const * restrict p = 
		    (unsigned char *)eagershort_pkt->data;
		unsigned char * restrict bufp = 
		    (unsigned char *)rreq->dev.tmpbuf;
		int i;
		for (i=0; i<data_sz; i++) {
		    *bufp++ = *p++;
		}
	    }
	    printf( "Unexpected eager short\n" ); fflush(stdout);
	    /* The request is still complete (in the sense of 
	       having all data) */
	    MPIDI_CH3U_Request_complete(rreq);
	    
	}

	if (mpi_errno != MPI_SUCCESS) {
	    MPIU_ERR_SETANDJUMP1(mpi_errno,MPI_ERR_OTHER, "**ch3|postrecv",
		     "**ch3|postrecv %s", "MPIDI_CH3_PKT_EAGERSHORT_SEND");
	}
    }

 fn_fail:
    return mpi_errno;
}

#endif

/* Send a contiguous eager message that can be cancelled (e.g., 
   a nonblocking eager send).  We'll want to optimize (and possibly
   inline) this 

   Make sure that buf is at the beginning of the data to send; 
   adjust by adding dt_true_lb if necessary 
*/
#undef FUNCNAME
#define FUNCNAME MPIDI_EagerContigIsend
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3_EagerContigIsend( MPID_Request **sreq_p, 
				MPIDI_CH3_Pkt_type_t reqtype, 
				const void * buf, int data_sz, int rank, 
				int tag, MPID_Comm * comm, int context_offset )
{
    int mpi_errno = MPI_SUCCESS;
    MPIDI_VC_t * vc;
    MPIDI_CH3_Pkt_t upkt;
    MPIDI_CH3_Pkt_eager_send_t * const eager_pkt = &upkt.eager_send;
    MPID_Request *sreq = *sreq_p;
    MPID_IOV iov[MPID_IOV_LIMIT];

    MPIU_DBG_MSG_FMT(CH3_OTHER,VERBOSE,(MPIU_DBG_FDEST,
	       "sending contiguous eager message, data_sz=" MPIDI_MSG_SZ_FMT,
					data_sz));
	    
    sreq->dev.OnDataAvail = 0;
    
    MPIDI_Pkt_init(eager_pkt, reqtype);
    eager_pkt->match.rank	= comm->rank;
    eager_pkt->match.tag	= tag;
    eager_pkt->match.context_id	= comm->context_id + context_offset;
    eager_pkt->sender_req_id	= sreq->handle;
    eager_pkt->data_sz		= data_sz;
    
    iov[0].MPID_IOV_BUF = (MPID_IOV_BUF_CAST)eager_pkt;
    iov[0].MPID_IOV_LEN = sizeof(*eager_pkt);
    
    iov[1].MPID_IOV_BUF = (MPID_IOV_BUF_CAST) buf;
    iov[1].MPID_IOV_LEN = data_sz;
    
    MPIDI_Comm_get_vc(comm, rank, &vc);
    MPIDI_VC_FAI_send_seqnum(vc, seqnum);
    MPIDI_Pkt_set_seqnum(eager_pkt, seqnum);
    MPIDI_Request_set_seqnum(sreq, seqnum);
    
    mpi_errno = MPIDI_CH3_iSendv(vc, sreq, iov, 2 );
    /* --BEGIN ERROR HANDLING-- */
    if (mpi_errno != MPI_SUCCESS)
    {
	MPIU_Object_set_ref(sreq, 0);
	MPIDI_CH3_Request_destroy(sreq);
	*sreq_p = NULL;
	mpi_errno = MPIR_Err_create_code(mpi_errno, MPIR_ERR_FATAL, FCNAME, 
			    __LINE__, MPI_ERR_OTHER, "**ch3|eagermsg", 0);
	goto fn_exit;
    }
    /* --END ERROR HANDLING-- */
    
 fn_exit:
    return mpi_errno;
}

/* 
 * Here are the routines that are called by the progress engine to handle
 * the various rendezvous message requests (cancel of sends is in 
 * mpid_cancel_send.c).
 */    

#define set_request_info(rreq_, pkt_, msg_type_)		\
{								\
    (rreq_)->status.MPI_SOURCE = (pkt_)->match.rank;		\
    (rreq_)->status.MPI_TAG = (pkt_)->match.tag;		\
    (rreq_)->status.count = (pkt_)->data_sz;			\
    (rreq_)->dev.sender_req_id = (pkt_)->sender_req_id;		\
    (rreq_)->dev.recv_data_sz = (pkt_)->data_sz;		\
    MPIDI_Request_set_seqnum((rreq_), (pkt_)->seqnum);		\
    MPIDI_Request_set_msg_type((rreq_), (msg_type_));		\
}

/* FIXME: This is not optimized for short messages, which 
   should have the data in the same packet when the data is
   particularly short (e.g., one 8 byte long word) */
int MPIDI_CH3_PktHandler_EagerSend( MPIDI_VC_t *vc, MPIDI_CH3_Pkt_t *pkt, 
				    MPID_Request **rreqp )
{
    MPIDI_CH3_Pkt_eager_send_t * eager_pkt = &pkt->eager_send;
    MPID_Request * rreq;
    int found;
    int mpi_errno = MPI_SUCCESS;
    
    MPIU_DBG_MSG_FMT(CH3_OTHER,VERBOSE,(MPIU_DBG_FDEST,
	"received eager send pkt, sreq=0x%08x, rank=%d, tag=%d, context=%d",
	eager_pkt->sender_req_id, eager_pkt->match.rank, 
	eager_pkt->match.tag, eager_pkt->match.context_id));
	    
    rreq = MPIDI_CH3U_Recvq_FDP_or_AEU(&eager_pkt->match, &found);
    if (rreq == NULL) {
	MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER, "**nomemreq");
    }
    
    set_request_info(rreq, eager_pkt, MPIDI_REQUEST_EAGER_MSG);
    if (rreq->dev.recv_data_sz == 0) {
	MPIDI_CH3U_Request_complete(rreq);
	*rreqp = NULL;
    }
    else {
	/* FIXME: What is the logic here?  On an eager receive, the data
	   should be available already, and we should be optimizing
	   for short messages */
	*rreqp = rreq;
	if (found) {
	    mpi_errno = MPIDI_CH3U_Post_data_receive_found( rreq );
	}
	else {
	    mpi_errno = MPIDI_CH3U_Post_data_receive_unexpected( rreq );
	}

	if (mpi_errno != MPI_SUCCESS) {
	    MPIU_ERR_SETANDJUMP1(mpi_errno,MPI_ERR_OTHER, "**ch3|postrecv",
			     "**ch3|postrecv %s", "MPIDI_CH3_PKT_EAGER_SEND");
	}
    }

 fn_fail:
    return mpi_errno;
}


int MPIDI_CH3_PktHandler_ReadySend( MPIDI_VC_t *vc, MPIDI_CH3_Pkt_t *pkt,
				    MPID_Request **rreqp )
{
    MPIDI_CH3_Pkt_ready_send_t * ready_pkt = &pkt->ready_send;
    MPID_Request * rreq;
    int found;
    int mpi_errno = MPI_SUCCESS;
    
    MPIU_DBG_MSG_FMT(CH3_OTHER,VERBOSE,(MPIU_DBG_FDEST,
	"received ready send pkt, sreq=0x%08x, rank=%d, tag=%d, context=%d",
			   ready_pkt->sender_req_id, ready_pkt->match.rank, 
                           ready_pkt->match.tag, ready_pkt->match.context_id));
	    
    rreq = MPIDI_CH3U_Recvq_FDP_or_AEU(&ready_pkt->match, &found);
    if (rreq == NULL) {
	MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER, "**nomemreq");
    }
    
    set_request_info(rreq, ready_pkt, MPIDI_REQUEST_EAGER_MSG);
    if (found) {
	if (rreq->dev.recv_data_sz == 0) {
	    MPIDI_CH3U_Request_complete(rreq);
	    *rreqp = NULL;
	}
	else {
	    *rreqp = rreq;
	    mpi_errno = MPIDI_CH3U_Post_data_receive_found(rreq);
	    if (mpi_errno != MPI_SUCCESS) {
		MPIU_ERR_SETANDJUMP1(mpi_errno,MPI_ERR_OTHER, 
				     "**ch3|postrecv",
				     "**ch3|postrecv %s", 
				     "MPIDI_CH3_PKT_READY_SEND");
	    }
	}
    }
    else
    {
	/* FIXME: an error packet should be sent back to the sender 
	   indicating that the ready-send failed.  On the send
	   side, the error handler for the communicator can be invoked
	   even if the ready-send request has already
	   completed. */
	
	/* We need to consume any outstanding associated data and 
	   mark the request with an error. */
	
	rreq->status.MPI_ERROR = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_RECOVERABLE, FCNAME, __LINE__, MPI_ERR_OTHER,
						      "**rsendnomatch", "**rsendnomatch %d %d", ready_pkt->match.rank,
						      ready_pkt->match.tag);
	rreq->status.count = 0;
	if (rreq->dev.recv_data_sz > 0)
	{
	    /* force read of extra data */
	    *rreqp = rreq;
	    rreq->dev.segment_first = 0;
	    rreq->dev.segment_size = 0;
	    mpi_errno = MPIDI_CH3U_Request_load_recv_iov(rreq);
	    if (mpi_errno != MPI_SUCCESS) {
		MPIU_ERR_SETANDJUMP(mpi_errno,MPI_ERR_OTHER,
				    "**ch3|loadrecviov");
	    }
	}
	else
	{
	    /* mark data transfer as complete and decrement CC */
	    MPIDI_CH3U_Request_complete(rreq);
	    *rreqp = NULL;
	}
    }
 fn_fail:
    return mpi_errno;
}


/*
 * Define the routines that can print out the cancel packets if 
 * debugging is enabled.
 */
#ifdef MPICH_DBG_OUTPUT
int MPIDI_CH3_PktPrint_EagerSend( FILE *fp, MPIDI_CH3_Pkt_t *pkt )
{
    MPIU_DBG_PRINTF((" type ......... EAGER_SEND\n"));
    MPIU_DBG_PRINTF((" sender_reqid . 0x%08X\n", pkt->eager_send.sender_req_id));
    MPIU_DBG_PRINTF((" context_id ... %d\n", pkt->eager_send.match.context_id));
    MPIU_DBG_PRINTF((" tag .......... %d\n", pkt->eager_send.match.tag));
    MPIU_DBG_PRINTF((" rank ......... %d\n", pkt->eager_send.match.rank));
    MPIU_DBG_PRINTF((" data_sz ...... %d\n", pkt->eager_send.data_sz));
#ifdef MPID_USE_SEQUENCE_NUMBERS
    MPIU_DBG_PRINTF((" seqnum ....... %d\n", pkt->eager_send.seqnum));
#endif
}

int MPIDI_CH3_PktPrint_EagerShortSend( FILE *fp, MPIDI_CH3_Pkt_t *pkt )
{
    int datalen;
    unsigned char *p = (unsigned char *)pkt->eagershort_send.data;
    MPIU_DBG_PRINTF((" type ......... EAGERSHORT_SEND\n"));
    MPIU_DBG_PRINTF((" context_id ... %d\n", pkt->eagershort_send.match.context_id));
    MPIU_DBG_PRINTF((" tag .......... %d\n", pkt->eagershort_send.match.tag));
    MPIU_DBG_PRINTF((" rank ......... %d\n", pkt->eagershort_send.match.rank));
    MPIU_DBG_PRINTF((" data_sz ...... %d\n", pkt->eagershort_send.data_sz));
#ifdef MPID_USE_SEQUENCE_NUMBERS
    MPIU_DBG_PRINTF((" seqnum ....... %d\n", pkt->eagershort_send.seqnum));
#endif
    datalen = pkt->eagershort_send.data_sz;
    if (datalen > 0) {
	char databytes[64+1];
	int i;
	if (datalen > 32) datalen = 32;
	for (i=0; i<datalen; i++) {
	    MPIU_Snprintf( &databytes[2*i], 64 - 2*i, "%2x", p[i] );
	}
	MPIU_DBG_PRINTF((" data ......... %s\n", databytes));
    }
}

int MPIDI_CH3_PktPrint_ReadySend( FILE *fp, MPIDI_CH3_Pkt_t *pkt )
{
    MPIU_DBG_PRINTF((" type ......... READY_SEND\n"));
    MPIU_DBG_PRINTF((" sender_reqid . 0x%08X\n", pkt->ready_send.sender_req_id));
    MPIU_DBG_PRINTF((" context_id ... %d\n", pkt->ready_send.match.context_id));
    MPIU_DBG_PRINTF((" tag .......... %d\n", pkt->ready_send.match.tag));
    MPIU_DBG_PRINTF((" rank ......... %d\n", pkt->ready_send.match.rank));
    MPIU_DBG_PRINTF((" data_sz ...... %d\n", pkt->ready_send.data_sz));
#ifdef MPID_USE_SEQUENCE_NUMBERS
    MPIU_DBG_PRINTF((" seqnum ....... %d\n", pkt->ready_send.seqnum));
#endif
}

#endif /* MPICH_DBG_OUTPUT */
