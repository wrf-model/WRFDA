/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */
#include "mpidu_process_locks.h"

#ifdef USE_PROCESS_LOCKS

#include "mpidu_process_locks.h"
#include <errno.h>
#ifdef HAVE_WINDOWS_H
#include <winsock2.h>
#include <windows.h>
#endif

int g_nLockSpinCount = 100;

#if !defined(USE_BUSY_LOCKS) && !defined(HAVE_MUTEX_INIT) && !defined(HAVE_SPARC_INLINE_PROCESS_LOCKS)
#ifndef MPIDU_Process_lock_init
#undef FUNCNAME
#define FUNCNAME MPIDU_Process_lock_init
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
/*@
   MPIDU_Process_lock_init - 

   Parameters:
+  MPIDU_Process_lock_t *lock

   Notes:
@*/
void MPIDU_Process_lock_init( MPIDU_Process_lock_t *lock )
{
#ifdef HAVE_NT_LOCKS
    MPIDI_STATE_DECL(MPID_STATE_MPIDU_PROCESS_LOCK_INIT);
    MPIDI_FUNC_ENTER(MPID_STATE_MPIDU_PROCESS_LOCK_INIT);
    *lock = CreateMutex(NULL, FALSE, NULL);
    if (*lock == NULL)
    {
        MPIU_Error_printf("error in mutex_init: %d\n", GetLastError());
    }
#elif defined(HAVE_PTHREAD_H)
    /* should be called by one process only */
    int err;
    pthread_mutexattr_t attr;
    MPIDI_STATE_DECL(MPID_STATE_MPIDU_PROCESS_LOCK_INIT);
    MPIDI_FUNC_ENTER(MPID_STATE_MPIDU_PROCESS_LOCK_INIT);
#ifdef HAVE_PTHREAD_MUTEXATTR_INIT
    err = pthread_mutexattr_init(&attr);
    if (err != 0)
      MPIU_Error_printf("error in pthread_mutexattr_init: %s\n", strerror(err));
#endif
#ifdef HAVE_PTHREAD_MUTEXATTR_SETPSHARED
    err = pthread_mutexattr_setpshared(&attr, PTHREAD_PROCESS_SHARED);
    if (err != 0)
      MPIU_Error_printf("error in pthread_mutexattr_setpshared: %s\n", strerror(err));

    err = pthread_mutex_init( lock, &attr );
#else
    err = pthread_mutex_init( lock, NULL );
#endif
    if ( err != 0 ) 
        MPIU_Error_printf( "error in mutex_init: %s\n", strerror(err) );
#else
#error Locking functions not defined
#endif
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDU_PROCESS_LOCK_INIT);
}
#endif /* MPIDU_Process_lock_init */

#ifndef MPIDU_Process_lock
#undef FUNCNAME
#define FUNCNAME MPIDU_Process_lock
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
/*@
   MPIDU_Process_lock - 

   Parameters:
+  MPIDU_Process_lock_t *lock

   Notes:
@*/
void MPIDU_Process_lock( MPIDU_Process_lock_t *lock )
{
#ifdef HAVE_NT_LOCKS
    DWORD dwRetVal;
    MPIDI_STATE_DECL(MPID_STATE_MPIDU_PROCESS_LOCK);
    MPIDI_FUNC_ENTER(MPID_STATE_MPIDU_PROCESS_LOCK);
    /*printf("nt lock %x\n", lock);fflush(stdout);*/
    dwRetVal = WaitForSingleObject(*lock, INFINITE);
    if (dwRetVal != WAIT_OBJECT_0)
    {
        if (dwRetVal == WAIT_FAILED)
            MPIU_Error_printf("error in mutex_lock: %s\n", strerror(GetLastError()));
        else
            MPIU_Error_printf("error in mutex_lock: %d\n", GetLastError());
    }
    /*printf("lock: Handle = %u\n", (unsigned long)*lock);*/
#elif defined(HAVE_PTHREAD_H)
    int err;
    MPIDI_STATE_DECL(MPID_STATE_MPIDU_PROCESS_LOCK);

    MPIDI_FUNC_ENTER(MPID_STATE_MPIDU_PROCESS_LOCK);
    err = pthread_mutex_lock( lock );
    if ( err != 0 ) 
        MPIU_Error_printf( "error in mutex_lock: %s\n", strerror(err) );
#else
#error Locking functions not defined
#endif
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDU_PROCESS_LOCK);
}
#endif /* MPIDU_Process_lock */

#ifndef MPIDU_Process_unlock
#undef FUNCNAME
#define FUNCNAME MPIDU_Process_unlock
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
/*@
   MPIDU_Process_unlock - 

   Parameters:
+  MPIDU_Process_lock_t *lock

   Notes:
@*/
void MPIDU_Process_unlock( MPIDU_Process_lock_t *lock )
{
#ifdef HAVE_NT_LOCKS
    MPIDI_STATE_DECL(MPID_STATE_MPIDU_PROCESS_UNLOCK);
    MPIDI_FUNC_ENTER(MPID_STATE_MPIDU_PROCESS_UNLOCK);
    if (!ReleaseMutex(*lock))
    {
        MPIU_Error_printf("error in mutex_unlock: %d\n", GetLastError());
        MPIU_Error_printf("Handle = %p\n", *lock);
    }
    /*printf("unlock: Handle = %u\n", (unsigned long)*lock);*/
#elif defined(HAVE_PTHREAD_H)
    int err;
    MPIDI_STATE_DECL(MPID_STATE_MPIDU_PROCESS_UNLOCK);
    MPIDI_FUNC_ENTER(MPID_STATE_MPIDU_PROCESS_UNLOCK);
    err = pthread_mutex_unlock( lock );
    if ( err != 0 ) 
        MPIU_Error_printf( "error in mutex_unlock: %s\n", strerror(err) );
#else
#error Locking functions not defined
#endif
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDU_PROCESS_UNLOCK);
}
#endif /* MPIDU_Process_unlock */

#undef FUNCNAME
#define FUNCNAME MPIDU_Process_lock_busy_wait
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
/*@
   MPIDU_Process_lock_busy_wait - 

   Parameters:
+  MPIDU_Process_lock_t *lock

   Notes:
@*/
void MPIDU_Process_lock_busy_wait( MPIDU_Process_lock_t *lock )
{
    MPIDI_STATE_DECL(MPID_STATE_MPIDU_PROCESS_LOCK_BUSY_WAIT);
    MPIDI_FUNC_ENTER(MPID_STATE_MPIDU_PROCESS_LOCK_BUSY_WAIT);
    MPIDU_Process_lock(lock);
    MPIDU_Process_unlock(lock);
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDU_PROCESS_LOCK_BUSY_WAIT);
}

#ifndef MPIDU_Process_lock_free
#undef FUNCNAME
#define FUNCNAME MPIDU_Process_lock_free
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
/*@
   MPIDU_Process_lock_free - 

   Parameters:
+  MPIDU_Process_lock_t *lock

   Notes:
@*/
void MPIDU_Process_lock_free( MPIDU_Process_lock_t *lock )
{
#ifdef HAVE_NT_LOCKS
    MPIDI_STATE_DECL(MPID_STATE_MPIDU_PROCESS_LOCK_FREE);
    MPIDI_FUNC_ENTER(MPID_STATE_MPIDU_PROCESS_LOCK_FREE);
    /*printf("Free_lock: Handle = %u\n", (unsigned long)*lock);*/
    CloseHandle(*lock);
    *lock = NULL;
#elif defined(HAVE_PTHREAD_H)
    int err;
    MPIDI_STATE_DECL(MPID_STATE_MPIDU_PROCESS_LOCK_FREE);
    MPIDI_FUNC_ENTER(MPID_STATE_MPIDU_PROCESS_LOCK_FREE);
    err = pthread_mutex_destroy( lock );
    if ( err != 0 ) 
	MPIU_Error_printf( "error in mutex_destroy: %s\n", strerror(err) );
#else
#error Locking functions not defined
#endif
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDU_PROCESS_LOCK_FREE);
}
#endif /* MPIDU_Process_lock_free */

#endif /* #ifndef USE_BUSY_LOCKS */

#endif /* USE_PROCESS_LOCKS */
