/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2006 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpid_nem_impl.h"
#include "mpid_nem_datatypes.h"
#include <sys/ipc.h>
#include <sys/shm.h>
#include <errno.h>

#include <sched.h>
#define sched_yield() do { } while(0)

int MPID_nem_allocate_shm_region (volatile MPID_nem_copy_buf_t **buf_p, int *handle);
int MPID_nem_attach_shm_region (volatile MPID_nem_copy_buf_t **buf_p, int handle);
int MPID_nem_detach_shm_region (volatile MPID_nem_copy_buf_t *buf);

#define BUF_EMPTY 0
#define BUF_FULL 1

#undef FUNCNAME
#define FUNCNAME MPID_nem_lmt_shm_pre_send
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPID_nem_lmt_shm_pre_send (MPIDI_VC_t *vc, MPID_Request *req, MPID_IOV *cookie)
{
    int mpi_errno = MPI_SUCCESS;

    cookie->MPID_IOV_BUF = 0;
    cookie->MPID_IOV_LEN = 0;

 fn_exit:
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}

#undef FUNCNAME
#define FUNCNAME MPID_nem_lmt_shm_pre_recv
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPID_nem_lmt_shm_pre_recv (MPIDI_VC_t *vc, MPID_Request *req, MPID_IOV s_cookie, MPID_IOV *r_cookie, int *send_cts)
{
    int mpi_errno = MPI_SUCCESS;

    *send_cts = 1;

    mpi_errno = MPID_nem_allocate_shm_region (&vc->ch.copy_buf, &vc->ch.copy_buf_handle);
    if (mpi_errno) MPIU_ERR_POP (mpi_errno);
    
    vc->ch.copy_buf->s_len = 0;
    vc->ch.copy_buf->r_len = 0;
    
    vc->ch.copy_buf->flag[0] = BUF_EMPTY;
    vc->ch.copy_buf->flag[1] = BUF_EMPTY;

    r_cookie->MPID_IOV_BUF = &vc->ch.copy_buf_handle;
    r_cookie->MPID_IOV_LEN = sizeof (vc->ch.copy_buf_handle);
        

 fn_exit:
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}

#undef FUNCNAME
#define FUNCNAME MPID_nem_lmt_shm_start_send
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPID_nem_lmt_shm_start_send (MPIDI_VC_t *vc, MPID_Request *req, MPID_IOV r_cookie)
{
    int mpi_errno = MPI_SUCCESS;
    MPIDI_msg_sz_t data_sz;
    int dt_contig;
    MPI_Aint dt_true_lb;
    MPID_Datatype * dt_ptr;
    volatile MPID_nem_copy_buf_t *copy_buf;
    int first;
    int last;
    int buf_num;
    MPID_Segment segment;
    int s_len = 0;
    int r_len;
    
    MPIU_Assert (r_cookie.MPID_IOV_LEN == sizeof (vc->ch.copy_buf_handle));
    MPID_NEM_MEMCPY (&vc->ch.copy_buf_handle, r_cookie.MPID_IOV_BUF, sizeof (vc->ch.copy_buf_handle));

    mpi_errno = MPID_nem_attach_shm_region (&vc->ch.copy_buf, vc->ch.copy_buf_handle);
    if (mpi_errno) MPIU_ERR_POP (mpi_errno);
    copy_buf = vc->ch.copy_buf;
    
    MPIDI_Datatype_get_info (req->dev.user_count, req->dev.datatype, dt_contig, data_sz, dt_ptr, dt_true_lb);

    copy_buf->s_len = data_sz;
    while ((r_len = copy_buf->r_len) == 0)
        /* FIXME: add occasional yields */
        sched_yield();
    
    if (r_len < data_sz)
    {
        /* message will be truncated */
        s_len = data_sz;
        data_sz = r_len;
 	req->status.MPI_ERROR = MPIU_ERR_SET2 (mpi_errno, MPI_ERR_TRUNCATE, "**truncate", "**truncate %d %d", s_len, r_len);
    }    
    
    MPID_Segment_init (req->dev.user_buf, req->dev.user_count, req->dev.datatype, &segment, 0);

    buf_num = 0;
    first = 0;
    
    do
    {
        while (copy_buf->flag[buf_num] != BUF_EMPTY)
            /* FIXME: add occasional yields */
         sched_yield();
        
        last = (data_sz - first <= MPID_NEM_COPY_BUF_LEN) ? data_sz : first + MPID_NEM_COPY_BUF_LEN;
	MPID_Segment_pack (&segment, first, &last, (void *)copy_buf->buf[buf_num]); /* cast away volatile */
        copy_buf->flag[buf_num] = BUF_FULL;
        MPID_NEM_WRITE_FENCE();
        first = last;
        buf_num = 1 - buf_num;
    }
    while (last < data_sz);

    MPIDI_CH3U_Request_complete (req);
    
    mpi_errno = MPID_nem_detach_shm_region (copy_buf);
    if (mpi_errno) MPIU_ERR_POP (mpi_errno);

fn_exit:
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}

#undef FUNCNAME
#define FUNCNAME MPID_nem_lmt_shm_start_recv
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPID_nem_lmt_shm_start_recv (MPIDI_VC_t *vc, MPID_Request *req)
{
    int mpi_errno = MPI_SUCCESS;
    MPIDI_msg_sz_t data_sz;
    int dt_contig;
    MPI_Aint dt_true_lb;
    MPID_Datatype * dt_ptr;
    volatile MPID_nem_copy_buf_t *copy_buf;
    int first;
    int last;
    int buf_num;
    int buf_len;
    int copy_len;
    MPID_Segment segment;
    int s_len;
    int r_len = 0;

    copy_buf = vc->ch.copy_buf;
    
    MPIDI_Datatype_get_info (req->dev.user_count, req->dev.datatype, dt_contig, data_sz, dt_ptr, dt_true_lb);

    copy_buf->r_len = data_sz;
    while ((s_len = copy_buf->s_len) == 0)
        /* FIXME: add occasional yields */
        sched_yield();

    if (data_sz > s_len)
    {
        data_sz = s_len;
    }
    else if (data_sz < s_len)
    {
        /* message will be truncated */
        r_len = data_sz;
 	req->status.MPI_ERROR = MPIU_ERR_SET2 (mpi_errno, MPI_ERR_TRUNCATE, "**truncate", "**truncate %d %d", s_len, r_len);
    }    
    
    MPID_Segment_init (req->dev.user_buf, req->dev.user_count, req->dev.datatype, &segment, 0);

    buf_num = 0;
    first = 0;    

    do
    {
        while ((buf_len = copy_buf->flag[buf_num]) != BUF_FULL)
            /* FIXME: add occasional yields */
            sched_yield();

        last = (data_sz - first <= MPID_NEM_COPY_BUF_LEN) ? data_sz : first + MPID_NEM_COPY_BUF_LEN;
	MPID_Segment_unpack (&segment, first, &last, (void *)copy_buf->buf[buf_num]); /* cast away volatile */
        copy_buf->flag[buf_num] = BUF_EMPTY;
        MPID_NEM_WRITE_FENCE();
        first = last;
        buf_num = 1 - buf_num;
    }
    while (last < data_sz);

    MPIDI_CH3U_Request_complete (req);

    mpi_errno = MPID_nem_detach_shm_region (vc->ch.copy_buf);
    if (mpi_errno) MPIU_ERR_POP (mpi_errno);

 fn_exit:
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}

#undef FUNCNAME
#define FUNCNAME MPID_nem_lmt_shm_post_send
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPID_nem_lmt_shm_post_send (MPIDI_VC_t *vc, MPID_Request *req)
{
    return MPI_SUCCESS;
}

#undef FUNCNAME
#define FUNCNAME MPID_nem_lmt_shm_post_recv
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPID_nem_lmt_shm_post_recv (MPIDI_VC_t *vc, MPID_Request *req)
{
    return MPI_SUCCESS;
}


#undef FUNCNAME
#define FUNCNAME MPID_nem_allocate_shm_region
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPID_nem_allocate_shm_region (volatile MPID_nem_copy_buf_t **buf_p, int *handle)
{
    int mpi_errno = MPI_SUCCESS;
    int shmid;
    static int key = 0;
    void *buf;
    struct shmid_ds ds;

    if (*buf_p)
    {
        /* we're already attached */
        goto fn_exit;
    } 
   
    do
    {
        ++key;
        shmid = shmget (key, sizeof (MPID_nem_copy_buf_t), IPC_CREAT | IPC_EXCL | S_IRWXU);
    }
    while (shmid == -1 && errno == EEXIST);

    if (shmid == -1)
    {
        perror ("shmget");
        mpi_errno = MPI_ERR_INTERN;
        MPIU_ERR_POP (mpi_errno);
    }

    buf = 0;
    buf = shmat (shmid, buf, 0);

    if ((int)buf == -1)
    {
        perror ("shmat");
        mpi_errno = MPI_ERR_INTERN;
        MPIU_ERR_POP (mpi_errno);
    }
    
    *buf_p = buf;
    *handle = shmid;

 fn_exit:
    return mpi_errno;
 fn_fail:
    shmctl (shmid, IPC_RMID, &ds); /* try to remove region */
    goto fn_exit;
}

#undef FUNCNAME
#define FUNCNAME MPID_nem_attach_shm_region
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPID_nem_attach_shm_region (volatile MPID_nem_copy_buf_t **buf_p, int handle)
{
    int mpi_errno = MPI_SUCCESS;
    void *buf;
    int ret;
    struct shmid_ds ds;
    
    if (*buf_p)
    {
        /* we're already attached */
        goto fn_exit;
    } 

    buf = 0;
    buf = shmat (handle, buf, 0);

    if ((int)buf == -1)
    {
        perror ("shmat");
        mpi_errno = MPI_ERR_INTERN;
        MPIU_ERR_POP (mpi_errno);
    }
    
    *buf_p = buf;

    ret = shmctl (handle, IPC_RMID, &ds);
    if (ret == -1)
    {
        perror ("shmctl");
        mpi_errno = MPI_ERR_INTERN;
        MPIU_ERR_POP (mpi_errno);
    }

 fn_exit:
    return mpi_errno;
 fn_fail:
    shmctl (handle, IPC_RMID, &ds); /* try to remove region */
    goto fn_exit;
}

#undef FUNCNAME
#define FUNCNAME MPID_nem_detach_shm_region
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPID_nem_detach_shm_region (volatile MPID_nem_copy_buf_t *buf)
{
    int mpi_errno = MPI_SUCCESS;
    int ret;
    struct shmid_ds ds;

    /* for now never detach */
    goto fn_exit;
    
    ret = shmdt ((void *)buf); /* cast away volatile */
    if (ret != 0)
    {
        perror ("shmdt");
        mpi_errno = MPI_ERR_INTERN;
        MPIU_ERR_POP (mpi_errno);
    }

 fn_exit:
    return mpi_errno;
 fn_fail:
    goto fn_exit;
}
