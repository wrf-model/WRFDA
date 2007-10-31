/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

/* FIXME: This header should contain only the definitions exported to the
   mpiimpl.h level */

#if !defined(MPICH_MPIDPRE_H_INCLUDED)
#define MPICH_MPIDPRE_H_INCLUDED

#include "mpidi_ch3_conf.h"

#ifdef HAVE_ASSERT_H
#include <assert.h>
#endif

#if defined(HAVE_SYS_TYPES_H)
#include <sys/types.h>
#endif

#include "mpid_dataloop.h"
struct MPID_Datatype;

typedef MPI_Aint MPIDI_msg_sz_t;
/* We need to match the size of MPI_Aint to the relevant Format control
 */
#ifdef MPI_AINT_IS_LONG_INT
#define MPIDI_MSG_SZ_FMT "%ld"
#elif defined(MPI_AINT_IS_LONG_LONG_INT)
#define MPIDI_MSG_SZ_FMT "%lld"
#else
#define MPIDI_MSG_SZ_FMT "%d"
#endif

/* Include definitions from the channel which must exist before items in this 
   file (mpidpre.h) or the file it includes (mpiimpl.h) can be defined. */
#include "mpidi_ch3_pre.h"

/* FIXME: Who defines this name */
/* As of 8/1/06, no-one defined MSGS_UNORDERED.  We should consider 
   moving support for unordered messages to a different part of the code
   However, note that sequence numbers may be useful in other contexts, 
   including identifying messages when multithreaded (for better profiling
   tools) and handling cancellations (rather than relying on unique 
   request ids) 
*/
#if defined (MPIDI_CH3_MSGS_UNORDERED)
#define MPID_USE_SEQUENCE_NUMBERS
#endif

#if defined(MPID_USE_SEQUENCE_NUMBERS)
typedef unsigned long MPID_Seqnum_t;
#endif

#include "mpichconf.h"

typedef struct MPIDI_Message_match
{
    int32_t tag;
    int16_t rank;
    int16_t context_id;
}
MPIDI_Message_match;
#define MPIDI_TAG_UB (0x7fffffff)

/* Packet types are defined in mpidpkt.h .  The intent is to remove the
   need for packet definitions outside of the device directories.
   Currently, the request contains a block of storage in which a 
   packet header can be copied in the event that a message cannot be
   send immediately.  
*/
#include "mpidpkt.h"
/*
 * THIS IS OBSOLETE AND UNUSED, BUT RETAINED FOR ITS DESCRIPTIONS OF THE
 * VARIOUS STATES.  Note that this is not entirely accurate, as the 
 * CA_COMPLETE state could depend on the packet type (e.g., for RMA 
 * operations).
 *
 * MPIDI_CA_t
 *
 * An enumeration of the actions to perform when the requested I/O operation 
 * has completed.
 *
 * MPIDI_CH3_CA_COMPLETE - The last operation for this request has completed.
 * The completion counter should be decremented.  If
 * it has reached zero, then the request should be released by calling 
 * MPID_Request_release().
 *
 * MPIDI_CH3_CA_UNPACK_UEBUF_AND_COMPLETE - This is a special case of the 
 * MPIDI_CH3_CA_COMPLETE.  The data for an unexpected
 * eager messaage has been stored into a temporary buffer and needs to be 
 * copied/unpacked into the user buffer before the
 * completion counter can be decremented, etc.
 *
 * MPIDI_CH3_CA_UNPACK_SRBUF_AND_COMPLETE - This is a special case of the 
 * MPIDI_CH3_CA_COMPLETE.  The data from the completing
 * read has been stored into a temporary send/receive buffer and needs to be 
 * copied/unpacked into the user buffer before the
 * completion counter can be decremented, etc.
 *
 * MPIDI_CH3_CA_RELOAD_IOV - This request contains more segments of data than 
 * the IOV or buffer space allow.  Since the
 * previously request operation has completed, the IOV in the request should 
 * be reload at this time.
 *
 * MPIDI_CH3_CA_UNPACK_SRBUF_AND_RELOAD_IOV - This is a special case of the 
 * MPIDI_CH3_CA_RELOAD_IOV.  The data from the
 * completing read operation has been stored into a temporary send/receive 
 * buffer and needs to be copied/unpacked into the user
 * buffer before the IOV is reloaded.
 *
 * MPIDI_CH3_CA_END_CH3 - This not a real action, but rather a marker.  
 * All actions numerically less than MPID_CA_END are defined
 * by channel device.  Any actions numerically greater than MPIDI_CA_END are 
 * internal to the channel instance and must be handled
 * by the channel instance.
 */

/*S
  MPIDI_PG_t - Process group description

  Notes:
  Every 'MPI_COMM_WORLD' known to this process has an associated process 
  group.  
  S*/
typedef struct MPIDI_PG
{
    /* MPIU_Object field.  MPIDI_PG_t objects are not allocated using the 
       MPIU_Object system, but we do use the associated reference counting 
       routines.  Therefore, handle must be present, but is not used 
       except by debugging routines */
    int handle;
    volatile int ref_count;

    /* Next pointer used to maintain a list of all process groups known to 
       this process */
    struct MPIDI_PG * next;

    /* Number of processes in the process group */
    int size;

    /* VC table.  At present this is a pointer to an array of VC structures. 
       Someday we may want make this a pointer to an array
       of VC references.  Thus, it is important to use MPIDI_PG_Get_vc() 
       instead of directly referencing this field. */
    struct MPIDI_VC * vct;

    /* Pointer to the process group ID.  The actual ID is defined and 
       allocated by the process group.  The pointer is kept in the
       device space because it is necessary for the device to be able to 
       find a particular process group. */
    void * id;

    /* Replacement abstraction for connection information */
    /* Connection information needed to access processes in this process 
       group and to share the data with other processes.  The items are
       connData - pointer for data used to implement these functions 
                  (e.g., a pointer to an array of process group info)
       getConnInfo( rank, buf, bufsize, self ) - function to store into
                  buf the connection information for rank in this process 
                  group
       connInfoToString( buf_p, size, self ) - return in buf_p a string
                  that can be sent to another process to recreate the
                  connection information (the info needed to support
                  getConnInfo)
       connInfoFromString( buf, self ) - setup the information needed
                  to implement getConnInfo
       freeConnInfo( self ) - free any storage or resources associated
                  with the connection information.

       See ch3/src/mpidi_pg.c 
    */
    void *connData;
    int  (*getConnInfo)( int, char *, int, struct MPIDI_PG * );
    int  (*connInfoToString)( char **, int *, struct MPIDI_PG * );
    int  (*connInfoFromString)( const char *,  struct MPIDI_PG * );
    int  (*freeConnInfo)( struct MPIDI_PG * );

#if defined(MPIDI_CH3_PG_DECL)
    MPIDI_CH3_PG_DECL
#endif    
}
MPIDI_PG_t;



/*E
  MPIDI_VC_State - States for a virtual connection.
 
  Notes:
  A closed connection is placed into 'STATE_INACTIVE'. (is this true?)
 E*/
typedef enum MPIDI_VC_State
{
    MPIDI_VC_STATE_INACTIVE=1,
    MPIDI_VC_STATE_ACTIVE,
    MPIDI_VC_STATE_LOCAL_CLOSE,
    MPIDI_VC_STATE_REMOTE_CLOSE,
    MPIDI_VC_STATE_CLOSE_ACKED
} MPIDI_VC_State_t;

typedef struct MPIDI_VC
{
    /* XXX - need better comment */
    /* MPIU_Object fields.  MPIDI_VC_t objects are not allocated using the 
       MPIU_Object system, but we do use the associated
       reference counting routines.  The handle value is required 
       when debugging objects (the handle kind is used in reporting
       on changes to the object).
    */
    int handle;
    volatile int ref_count;

    /* state of the VC */
    MPIDI_VC_State_t state;

    /* Process group to which this VC belongs */
    MPIDI_PG_t * pg;

    /* Rank of the process in that process group associated with this VC */
    int pg_rank;

    /* Local process ID */
    int lpid;
    
#if defined(MPID_USE_SEQUENCE_NUMBERS)
    /* Sequence number of the next packet to be sent */
    MPID_Seqnum_t seqnum_send;
#endif
    
#if defined(MPIDI_CH3_MSGS_UNORDERED)
    /* Sequence number of the next packet we expect to receive */
    MPID_Seqnum_t seqnum_recv;

    /* Queue for holding packets received out of order.  NOTE: the CH3 device 
       only orders packets.  Handling of out-of-order data
       is the responsibility of the channel. */
    MPIDI_CH3_Pkt_send_container_t * msg_reorder_queue;
#endif
    
# if defined(MPIDI_CH3_VC_DECL)
    MPIDI_CH3_VC_DECL
# endif
}
MPIDI_VC_t;

typedef enum MPIDI_VC_Event
{
    MPIDI_VC_EVENT_TERMINATED
}
MPIDI_VC_Event_t;

#ifndef HAVE_MPIDI_VCRT
#define HAVE_MPIDI_VCRT
typedef struct MPIDI_VCRT * MPID_VCRT;
typedef struct MPIDI_VC * MPID_VCR;
#endif

#ifndef DEFINED_REQ
#define DEFINED_REQ
#if defined(MPID_USE_SEQUENCE_NUMBERS)
#   define MPIDI_REQUEST_SEQNUM	\
        MPID_Seqnum_t seqnum;
#else
#   define MPIDI_REQUEST_SEQNUM
#endif

#define MPIDI_DEV_WIN_DECL                                               \
    volatile int my_counter;  /* completion counter for operations       \
                                 targeting this window */                \
    void **base_addrs;     /* array of base addresses of the windows of  \
                              all processes */                           \
    int *disp_units;      /* array of displacement units of all windows */\
    MPI_Win *all_win_handles;    /* array of handles to the window objects\
                                          of all processes */            \
    struct MPIDI_RMA_ops *rma_ops_list; /* list of outstanding RMA requests */  \
    volatile int lock_granted;  /* flag to indicate whether lock has     \
                                   been granted to this process (as source) for         \
                                   passive target rma */                 \
    volatile int current_lock_type;   /* current lock type on this window (as target)   \
                              * (none, shared, exclusive) */             \
    volatile int shared_lock_ref_cnt;                                    \
    struct MPIDI_Win_lock_queue volatile *lock_queue;  /* list of unsatisfied locks */  \
                                                                         \
    int *pt_rma_puts_accs;  /* array containing the no. of passive target\
                               puts/accums issued from this process to other \
                               processes. */                             \
    volatile int my_pt_rma_puts_accs;  /* no. of passive target puts/accums  \
                                          that this process has          \
                                          completed as target */
 
#ifdef MPIDI_CH3_WIN_DECL
#define MPID_DEV_WIN_DECL \
MPIDI_DEV_WIN_DECL \
MPIDI_CH3_WIN_DECL
#else
#define MPID_DEV_WIN_DECL \
MPIDI_DEV_WIN_DECL
#endif


typedef struct MPIDI_Request {
    MPIDI_Message_match match;

    /* user_buf, user_count, and datatype needed to process 
       rendezvous messages. */
    void * user_buf;
    int user_count;
    MPI_Datatype datatype;

    /* segment, segment_first, and segment_size are used when processing 
       non-contiguous datatypes */
    MPID_Segment segment;
    MPIDI_msg_sz_t segment_first;
    MPIDI_msg_sz_t segment_size;

    /* Pointer to datatype for reference counting purposes */
    struct MPID_Datatype * datatype_ptr;

    /* iov and iov_count define the data to be transferred/received */
    MPID_IOV iov[MPID_IOV_LIMIT];
    int iov_count;

    /* FIXME: RDMA values are specific to some channels? */
    MPID_IOV rdma_iov[MPID_IOV_LIMIT];
    int rdma_iov_count;
    int rdma_iov_offset;
    MPI_Request rdma_request;

    /* OnDataAvail is the action to take when data is now available.
       For example, when an operation described by an iov has 
       completed.  This replaces the MPIDI_CA_t (completion action)
       field used through MPICH2 1.0.4. */
    int (*OnDataAvail)( MPIDI_VC_t *, struct MPID_Request *, int * );
    /* OnFinal is used in the following case:
       OnDataAvail is set to a function, and that function has processed
       all of the data.  At that point, the OnDataAvail function can
       reset OnDataAvail to OnFinal.  This is normally used when processing
       non-contiguous data, where there is one more action to take (such
       as a get-response) when processing of the non-contiguous data 
       completes. This value need not be initialized unless OnDataAvail
       is set to a non-null value (and then only in certain cases) */
    int (*OnFinal)( MPIDI_VC_t *, struct MPID_Request *, int * );

    /* tmpbuf and tmpbuf_sz describe temporary storage used for things like 
       unexpected eager messages and packing/unpacking
       buffers.  tmpuf_off is the current offset into the temporary buffer. */
    void * tmpbuf;
    int tmpbuf_off;
    MPIDI_msg_sz_t tmpbuf_sz;

    MPIDI_msg_sz_t recv_data_sz;
    MPI_Request sender_req_id;

    unsigned state;
    int cancel_pending;
    int recv_pending_count;

    /* The next 8 are for RMA */
    MPI_Op op;
    /* For accumulate, since data is first read into a tmp_buf */
    void *real_user_buf;
    /* For derived datatypes at target */
    struct MPIDI_RMA_dtype_info *dtype_info;
    void *dataloop;
    /* req. handle needed to implement derived datatype gets  */
    MPI_Request request_handle;
    MPI_Win target_win_handle;
    MPI_Win source_win_handle;
    int single_op_opt;   /* to indicate a lock-put-unlock optimization case */
    struct MPIDI_Win_lock_queue *lock_queue_entry; /* for single lock-put-unlock optimization */

    MPIDI_REQUEST_SEQNUM

    struct MPID_Request * next;
} MPIDI_Request;
#define MPID_REQUEST_DECL MPIDI_Request dev;

#if defined(MPIDI_CH3_REQUEST_DECL)
#define MPID_DEV_REQUEST_DECL			\
MPID_REQUEST_DECL				\
MPIDI_CH3_REQUEST_DECL
#else
#define MPID_DEV_REQUEST_DECL			\
MPID_REQUEST_DECL
#endif

#ifdef MPIDI_CH3_REQUEST_KIND_DECL
#define MPID_DEV_REQUEST_KIND_DECL MPIDI_CH3_REQUEST_KIND_DECL
#endif

#endif

/* FIXME: This ifndef test is a temp until mpidpre is cleaned of
   all items that do not belong (e.g., all items not needed by the
   top layers of MPICH2) */
#ifndef MPID_PROGRESS_STATE_DECL
#if defined(MPIDI_CH3_PROGRESS_STATE_DECL)
#   define MPID_PROGRESS_STATE_DECL MPIDI_CH3_PROGRESS_STATE_DECL
#else
#   define MPID_PROGRESS_STATE_DECL int foo;
#endif
#endif


/* Tell Intercomm create and friends that the GPID routines have been
   implemented */
#define HAVE_GPID_ROUTINES

#endif /* !defined(MPICH_MPIDPRE_H_INCLUDED) */
