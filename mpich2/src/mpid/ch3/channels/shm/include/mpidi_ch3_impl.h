/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#if !defined(MPICH_MPIDI_CH3_IMPL_H_INCLUDED)
#define MPICH_MPIDI_CH3_IMPL_H_INCLUDED

#include "mpidi_ch3i_shm_conf.h"
#include "mpidimpl.h"
#include "mpidu_process_locks.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <stdlib.h>
#include <assert.h>

#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_ERRNO_H
#include <errno.h>
#endif
#ifdef HAVE_PTHREAD_H
#include <pthread.h>
#endif
#ifdef HAVE_SYS_IPC_H
#include <sys/ipc.h>
#endif
#ifdef HAVE_SYS_SHM_H
#include <sys/shm.h>
#endif
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_FCNTL_H
#include <fcntl.h>
#endif
#ifdef HAVE_SYS_MMAN_H
#include <sys/mman.h>
#endif

#ifdef HAVE_GCC_AND_PENTIUM_ASM
#ifdef HAVE_GCC_ASM_AND_X86_SFENCE
/*#define MPID_WRITE_BARRIER() __asm__ __volatile__  ( "sfence" ::: "memory" )*/
#define MPID_WRITE_BARRIER()
#else
#define MPID_WRITE_BARRIER()
#endif
#ifdef HAVE_GCC_ASM_AND_X86_LFENCE
#define MPID_READ_BARRIER() __asm__ __volatile__  ( ".byte 0x0f, 0xae, 0xe8" ::: "memory" )
#else
#define MPID_READ_BARRIER()
#endif
#ifdef HAVE_GCC_ASM_AND_X86_MFENCE
/*#define MPID_READ_WRITE_BARRIER() __asm__ __volatile__  ( ".byte 0x0f, 0xae, 0xf0" ::: "memory" )*/
#define MPID_READ_WRITE_BARRIER()
#else
#define MPID_READ_WRITE_BARRIER()
#endif

#elif defined(HAVE___ASM_AND_PENTIUM_ASM)
#ifdef HAVE___ASM_AND_X86_SFENCE
/*#define MPID_WRITE_BARRIER() __asm sfence */
#define MPID_WRITE_BARRIER()
#else
#define MPID_WRITE_BARRIER()
#endif
#ifdef HAVE___ASM_AND_X86_LFENCE
#define MPID_READ_BARRIER() __asm __emit 0x0f __asm __emit 0xae __asm __emit 0xe8
#else
#define MPID_READ_BARRIER()
#endif
#ifdef HAVE___ASM_AND_X86_MFENCE
/*#define MPID_READ_WRITE_BARRIER() __asm __emit 0x0f __asm __emit 0xae __asm __emit 0xf0*/
#define MPID_READ_WRITE_BARRIER()
#else
#define MPID_READ_WRITE_BARRIER()
#endif

#else
#define MPID_WRITE_BARRIER()
#define MPID_READ_BARRIER()
#define MPID_READ_WRITE_BARRIER()
#endif

#define MPIDI_CH3I_NUM_PACKETS          16
#define MPIDI_CH3I_PACKET_SIZE         (16*1024)
#define MPIDI_CH3I_PKT_AVAILABLE        0
#define MPIDI_CH3I_PKT_USED             1
#define MPIDI_CH3I_SPIN_COUNT_DEFAULT   100
#define MPIDI_CH3I_YIELD_COUNT_DEFAULT  5000
#define MPIDI_SHM_EAGER_LIMIT           10240
#ifdef HAVE_SHARED_PROCESS_READ
#define MPIDI_SHM_RNDV_LIMIT            10240
#endif
#define MPID_SHMEM_PER_PROCESS          1048576

/* FIXME: The ssm channel uses these names instead.  We'll switch to them 
   eventually (since there is a great deal of commonality between the ssm and
   shm code, and that commonality should be reflected in common support 
   routines) */
#define MPIDI_CH3I_PKT_EMPTY            MPIDI_CH3I_PKT_AVAILABLE
#define MPIDI_CH3I_PKT_FILLED           MPIDI_CH3I_PKT_USED


/* This structure uses the avail field to signal that the data is available for reading.
   The code fills the data and then sets the avail field.
   This assumes that declaring avail to be volatile causes the compiler to insert a
   write barrier when the avail location is written to.
   */
typedef struct MPIDI_CH3I_SHM_Packet_t
{
    volatile int avail;
    /*char pad_avail[60];*/ /* keep the avail flag on a separate cache line */
    int num_bytes;
    int offset;
    /* insert stuff here to align data? */
    int pad;/*char pad_data[56];*/ /* cache align the data */
    char data[MPIDI_CH3I_PACKET_SIZE];
    /* insert stuff here to pad the packet up to an aligned boundary? */
} MPIDI_CH3I_SHM_Packet_t;

typedef struct MPIDI_CH3I_SHM_Queue_t
{
    int head_index;
    int tail_index;
    /* insert stuff here to align the packets? */
    /*char pad[56];*/ /* cache align the data */
    MPIDI_CH3I_SHM_Packet_t packet[MPIDI_CH3I_NUM_PACKETS];
} MPIDI_CH3I_SHM_Queue_t;

typedef struct MPIDI_CH3I_Process_s
{
    MPIDI_VC_t *unex_finished_list, *vc;
}
MPIDI_CH3I_Process_t;

extern MPIDI_CH3I_Process_t MPIDI_CH3I_Process;

#define MPIDI_CH3I_SendQ_enqueue(vc, req)				\
{									\
    /* MT - not thread safe! */						\
    MPIDI_DBG_PRINTF((50, FCNAME, "SendQ_enqueue vc=0x%08x req=0x%08x",	\
	              (unsigned long) vc, req->handle));		\
    req->dev.next = NULL;						\
    if (vc->ch.sendq_tail != NULL)					\
    {									\
	vc->ch.sendq_tail->dev.next = req;				\
    }									\
    else								\
    {									\
	vc->ch.sendq_head = req;					\
    }									\
    vc->ch.sendq_tail = req;						\
}

#define MPIDI_CH3I_SendQ_enqueue_head(vc, req)				     \
{									     \
    /* MT - not thread safe! */						     \
    MPIDI_DBG_PRINTF((50, FCNAME, "SendQ_enqueue_head vc=0x%08x req=0x%08x", \
	              (unsigned long) vc, req->handle));		     \
    req->dev.next = vc->ch.sendq_head;					     \
    if (vc->ch.sendq_tail == NULL)					     \
    {									     \
	vc->ch.sendq_tail = req;					     \
    }									     \
    vc->ch.sendq_head = req;						     \
}

#define MPIDI_CH3I_SendQ_dequeue(vc)					\
{									\
    /* MT - not thread safe! */						\
    MPIDI_DBG_PRINTF((50, FCNAME, "SendQ_dequeue vc=0x%08x req=0x%08x",	\
	              (unsigned long) vc, vc->ch.sendq_head));		\
    vc->ch.sendq_head = vc->ch.sendq_head->dev.next;			\
    if (vc->ch.sendq_head == NULL)					\
    {									\
	vc->ch.sendq_tail = NULL;					\
    }									\
}

#define MPIDI_CH3I_SendQ_head(vc) (vc->ch.sendq_head)

#define MPIDI_CH3I_SendQ_empty(vc) (vc->ch.sendq_head == NULL)

typedef enum shm_wait_e
{
SHM_WAIT_TIMEOUT,
SHM_WAIT_READ,
SHM_WAIT_WRITE,
SHM_WAIT_WAKEUP
} shm_wait_t;

int MPIDI_CH3I_Progress_init(void);
int MPIDI_CH3I_Progress_finalize(void);
int MPIDI_CH3I_Request_adjust_iov(MPID_Request *, MPIDI_msg_sz_t);

int MPIDI_CH3I_SHM_Get_mem(MPIDI_PG_t *pg, int nTotalSize, int nRank, int nNproc, BOOL bUseShm);
int MPIDI_CH3I_SHM_Release_mem(MPIDI_PG_t *pg, BOOL bUseShm);

int MPIDI_CH3I_SHM_read_progress(MPIDI_VC_t *vc, int millisecond_timeout, MPIDI_VC_t **vc_pptr, int *num_bytes_ptr, shm_wait_t *shm_out);
int MPIDI_CH3I_SHM_post_read(MPIDI_VC_t *vc, void *buf, int len, int (*read_progress_update)(int, void*));
int MPIDI_CH3I_SHM_post_readv(MPIDI_VC_t *vc, MPID_IOV *iov, int n, int (*read_progress_update)(int, void*));
int MPIDI_CH3I_SHM_write(MPIDI_VC_t *vc, void *buf, int len, int *num_bytes_ptr);
int MPIDI_CH3I_SHM_writev(MPIDI_VC_t *vc, MPID_IOV *iov, int n, int *num_bytes_ptr);
int MPIDI_CH3I_SHM_rdma_readv(MPIDI_VC_t *vc, MPID_Request *rreq);
int MPIDI_CH3I_SHM_rdma_writev(MPIDI_VC_t *vc, MPID_Request *sreq);

int MPIDI_CH3I_SHM_Progress_init(void);

#endif /* !defined(MPICH_MPIDI_CH3_IMPL_H_INCLUDED) */
