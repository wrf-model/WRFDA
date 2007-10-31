/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include <windows.h>

typedef HANDLE MPE_Thread_mutex_t;
typedef HANDLE MPE_Thread_id_t;
typedef DWORD MPE_Thread_tls_t;

typedef struct MPE_Thread_cond_fifo_t
{
    HANDLE event;
    struct MPE_Thread_cond_fifo_t *next;
} MPE_Thread_cond_fifo_t;
typedef struct MPE_Thread_cond_t
{
    MPE_Thread_tls_t tls;
    MPE_Thread_mutex_t fifo_mutex;
    MPE_Thread_cond_fifo_t *fifo_head, *fifo_tail;
} MPE_Thread_cond_t;
