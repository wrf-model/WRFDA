/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2006 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpid_nem_impl.h"

#undef FUNCNAME
#define FUNCNAME MPID_nem_lmt_pre_send
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPID_nem_lmt_pre_send (MPIDI_VC_t *vc, MPID_Request *req, MPID_IOV *cookie)
{
    int mpi_errno = MPI_SUCCESS;
    
    if (vc->ch.lmt_pre_send)
    {
        mpi_errno = vc->ch.lmt_pre_send (vc, req, cookie);
        if (mpi_errno) MPIU_ERR_POP (mpi_errno);
        goto fn_exit;
    }

    /* default method: using message queues */

    /* send a NULL RTS message */
    req->ch.s_cookie.MPID_IOV_BUF = 0;
    req->ch.s_cookie.MPID_IOV_LEN = 0;
    *cookie = req->ch.s_cookie;

 fn_exit:
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}

#undef FUNCNAME
#define FUNCNAME MPID_nem_lmt_pre_recv
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPID_nem_lmt_pre_recv (MPIDI_VC_t *vc, MPID_Request *req, MPID_IOV s_cookie, MPID_IOV *r_cookie, int *send_cts)
{
    int mpi_errno = MPI_SUCCESS;

    if (vc->ch.lmt_pre_recv)
    {
        mpi_errno = vc->ch.lmt_pre_recv (vc, req, s_cookie, r_cookie, send_cts);
        if (mpi_errno) MPIU_ERR_POP (mpi_errno);
        goto fn_exit;
    }

    /* default method: using message queues */

    req->ch.r_cookie.MPID_IOV_BUF = &req->handle;
    req->ch.r_cookie.MPID_IOV_LEN = sizeof (req->handle);
    *r_cookie = req->ch.r_cookie;
    *send_cts = 1;

 fn_exit:
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}

#undef FUNCNAME
#define FUNCNAME MPID_nem_lmt_start_send
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPID_nem_lmt_start_send (MPIDI_VC_t *vc, MPID_Request *sreq, MPID_IOV r_cookie)
{
    int mpi_errno = MPI_SUCCESS;
    MPIDI_CH3_Pkt_t upkt;
    MPIDI_CH3_Pkt_rndv_send_t * rs_pkt = &upkt.rndv_send;
    MPIDI_msg_sz_t data_sz;
    int dt_contig;
    MPI_Aint dt_true_lb;
    MPID_Datatype * dt_ptr;
    MPID_IOV iov[MPID_IOV_LIMIT];
    int iov_n;

    if (vc->ch.lmt_start_send)
    {
        mpi_errno = vc->ch.lmt_start_send (vc, sreq, r_cookie);
        if (mpi_errno) MPIU_ERR_POP (mpi_errno);
        goto fn_exit;
    }
    
    /* default method: using message queues */

    MPIU_ERR_SETANDJUMP (mpi_errno, MPI_ERR_OTHER, "**notimpl");
    
 fn_exit:
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}

#undef FUNCNAME
#define FUNCNAME MPID_nem_lmt_start_recv
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPID_nem_lmt_start_recv (MPIDI_VC_t *vc, MPID_Request *req)
{
    int mpi_errno = MPI_SUCCESS;
    int done;

    if (vc->ch.lmt_start_recv)
    {
        mpi_errno = vc->ch.lmt_start_recv (vc, req);
        if (mpi_errno) MPIU_ERR_POP (mpi_errno);
        goto fn_exit;
    }

    /* default method: using message queues */
    /* do nothing */
    
 fn_exit:
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}

#undef FUNCNAME
#define FUNCNAME MPID_nem_lmt_post_send
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPID_nem_lmt_post_send (MPIDI_VC_t *vc, MPID_Request *req)
{
    int mpi_errno = MPI_SUCCESS;

    if (vc->ch.lmt_post_send)
    {
        mpi_errno = vc->ch.lmt_post_send (vc, req);
        if (mpi_errno) MPIU_ERR_POP (mpi_errno);
        goto fn_exit;
    }

    /* default method: using message queues */

 fn_exit:
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}

#undef FUNCNAME
#define FUNCNAME MPID_nem_lmt_post_recv
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPID_nem_lmt_post_recv (MPIDI_VC_t *vc, MPID_Request *req)
{
    int mpi_errno = MPI_SUCCESS;

    if (vc->ch.lmt_post_recv)
    {
        mpi_errno = vc->ch.lmt_post_recv (vc, req);
        if (mpi_errno) MPIU_ERR_POP (mpi_errno);
        goto fn_exit;
    }
    
    /* default method: using message queues */

    MPIDI_CH3U_Request_complete (req);

 fn_exit:
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}


