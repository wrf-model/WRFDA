/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#if !defined(MPICH_MPIDI_CH3_IMPL_H_INCLUDED)
#define MPICH_MPIDI_CH3_IMPL_H_INCLUDED

#include "mpidi_ch3i_sock_conf.h"
#include "mpidi_ch3_conf.h"
#include "mpidimpl.h"
#include "ch3usock.h"

/* This is all socket connection definitions */

    /* MT - not thread safe! */
#define MPIDI_CH3I_SendQ_enqueue(vc, req)				\
{									\
    MPIDI_DBG_PRINTF((50, FCNAME, "SendQ_enqueue vc=%p req=0x%08x", vc, req->handle));  \
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

    /* MT - not thread safe! */
#define MPIDI_CH3I_SendQ_enqueue_head(vc, req)				\
{									\
    MPIDI_DBG_PRINTF((50, FCNAME, "SendQ_enqueue_head vc=%p req=0x%08x", vc, req->handle));\
    req->dev.next = vc->ch.sendq_head;					\
    if (vc->ch.sendq_tail == NULL)					\
    {									\
	vc->ch.sendq_tail = req;					\
    }									\
    vc->ch.sendq_head = req;						\
}

    /* MT - not thread safe! */
#define MPIDI_CH3I_SendQ_dequeue(vc)					\
{									\
    MPIDI_DBG_PRINTF((50, FCNAME, "SendQ_dequeue vc=%p req=0x%08x", vc, vc->ch.sendq_head->handle));\
    vc->ch.sendq_head = vc->ch.sendq_head->dev.next;			\
    if (vc->ch.sendq_head == NULL)					\
    {									\
	vc->ch.sendq_tail = NULL;					\
    }									\
}


#define MPIDI_CH3I_SendQ_head(vc) (vc->ch.sendq_head)

#define MPIDI_CH3I_SendQ_empty(vc) (vc->ch.sendq_head == NULL)

/* End of connection-related macros */

/* FIXME: Any of these used in the ch3->channel interface should be
   defined in a header file in ch3/include that defines the 
   channel interface */
int MPIDI_CH3I_Progress_init(void);
int MPIDI_CH3I_Progress_finalize(void);
int MPIDI_CH3I_VC_post_connect(MPIDI_VC_t *);

#endif /* !defined(MPICH_MPIDI_CH3_IMPL_H_INCLUDED) */
