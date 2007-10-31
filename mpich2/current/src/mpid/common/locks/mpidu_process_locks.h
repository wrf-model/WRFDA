/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */
#ifndef MPIDU_PROCESS_LOCKS_H
#define MPIDU_PROCESS_LOCKS_H

#include "mpiimpl.h"
#include "mpid_locksconf.h"

#include <stdio.h>

/* FIXME: First use the configure ifdefs to decide on an approach for 
   locks.  Then put all lock code in one place, or at least guarded by
   the same "USE_xxx" ifdef.  It is nearly impossible with the current code
   to determine, for example, what is the definition of MPIDU_Process_lock_t.
   (Specifically, for the Intel compiler on an x86, it appears to be
   missing a volatile, needed when using the _InterlockedExchange inline 
   function
*/

#ifdef HAVE_GCC_AND_PENTIUM_ASM
#define HAVE_COMPARE_AND_SWAP
static inline char
__attribute__ ((unused))
     compare_and_swap (volatile long int *p, long int oldval, long int newval)
{
  char ret;
  long int readval;

  __asm__ __volatile__ ("lock; cmpxchgl %3, %1; sete %0"
                : "=q" (ret), "=m" (*p), "=a" (readval)
            : "r" (newval), "m" (*p), "a" (oldval) : "memory");
  return ret;
}
#endif

#ifdef HAVE_GCC_AND_X86_64_ASM
#define HAVE_COMPARE_AND_SWAP
static inline char
__attribute__ ((unused))
     compare_and_swap (volatile long int *p, long int oldval, long int newval)
{
  char ret;
  long int readval;

  __asm__ __volatile__ ("lock; cmpxchgq %3, %1; sete %0"
                : "=q" (ret), "=m" (*p), "=a" (readval)
            : "r" (newval), "m" (*p), "a" (oldval) : "memory");
  return ret;
}
#endif

#ifdef HAVE_GCC_AND_IA64_ASM
#define HAVE__INTERLOCKEDEXCHANGE 1
/* Make sure that the type here is compatible with the use and with 
 * MPIDU_Process_lock_t */
static inline unsigned long _InterlockedExchange(volatile long *ptr, unsigned long x)
{
   unsigned long result;
   __asm__ __volatile ("xchg4 %0=[%1],%2" : "=r" (result) : "r" (ptr), "r" (x) : "memory");
   return result;
}
#endif

#ifdef HAVE_ICC_AND_IA64
#define HAVE__INTERLOCKEDEXCHANGE 1
#include <ia64intrin.h>
#define _InterlockedExchange(ptr,x) _InterlockedExchange(ptr,x)
#endif

extern int g_nLockSpinCount;

/* Define MPIDU_Yield() */
#ifdef HAVE_YIELD
#define MPIDU_Yield() yield()
#elif defined(HAVE_WIN32_SLEEP)
#define MPIDU_Yield() Sleep(0)
#elif defined (HAVE_SCHED_YIELD)
#ifdef HAVE_SCHED_H
#include <sched.h>
#endif
#define MPIDU_Yield() sched_yield()
#elif defined (HAVE_SELECT)
#define MPIDU_Yield() { struct timeval t; t.tv_sec = 0; t.tv_usec = 0; select(0,0,0,0,&t); }
#elif defined (HAVE_USLEEP)
#define MPIDU_Yield() usleep(0)
#elif defined (HAVE_SLEEP)
#define MPIDU_Yield() sleep(0)
#else
#error *** No yield function specified ***
#endif

#if defined(HAVE_SPARC_INLINE_PROCESS_LOCKS)
typedef int MPIDU_Process_lock_t;
#error 'process locks are not supported for Solaris.  Developers should see req 2033'

#else

#ifdef HAVE_MUTEX_INIT
/*   Only known system is Solaris */
#include <sys/systeminfo.h>
#include <sys/processor.h>
#include <sys/procset.h>
#include <synch.h>
#include <string.h>

typedef mutex_t                 MPIDU_Process_lock_t;
#define MPIDU_Process_lock_init(lock)   mutex_init(lock,USYNC_PROCESS,(void *)NULL)
#define MPIDU_Process_lock(lock)        mutex_lock(lock)
#define MPIDU_Process_unlock(lock)      mutex_unlock(lock)
#define MPIDU_Process_lock_free(lock)   mutex_destroy(lock)

#undef FUNCNAME
#define FUNCNAME MPIDU_Process_lock_busy_wait
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
static inline void MPIDU_Process_lock_busy_wait( MPIDU_Process_lock_t *lock )
{
    int i;
    MPIDI_STATE_DECL(MPID_STATE_MPIDU_PROCESS_LOCK_BUSY_WAIT);
    MPIDI_FUNC_ENTER(MPID_STATE_MPIDU_PROCESS_LOCK_BUSY_WAIT);
    mutex_lock(lock);
    mutex_unlock(lock);
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDU_PROCESS_LOCK_BUSY_WAIT);
}

#else

#ifdef USE_BUSY_LOCKS
#ifdef HAVE_MUTEX_INIT
typedef mutex_t MPIDU_Process_lock_t;
#else
typedef volatile long MPIDU_Process_lock_t;
#endif
#else
#ifdef HAVE_NT_LOCKS
typedef HANDLE MPIDU_Process_lock_t;
#elif defined(HAVE_PTHREAD_H)
#include <pthread.h>
typedef pthread_mutex_t MPIDU_Process_lock_t;  
#else
#error *** No locking mechanism for shared memory.specified ***
#endif
#endif

#include <errno.h>
#ifdef HAVE_WINDOWS_H
#include <winsock2.h>
#include <windows.h>
#endif

#ifdef USE_BUSY_LOCKS

/* FIXME: only the lock/unlock/wait routines should be considered for 
   static inline functions */
#undef FUNCNAME
#define FUNCNAME MPIDU_Process_lock_init
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
static inline void MPIDU_Process_lock_init( MPIDU_Process_lock_t *lock )
{
#ifdef HAVE_MUTEX_INIT
    int err;
#endif
    MPIDI_STATE_DECL(MPID_STATE_MPIDU_PROCESS_LOCK_INIT);
    MPIDI_FUNC_ENTER(MPID_STATE_MPIDU_PROCESS_LOCK_INIT);
#ifdef HAVE_MUTEX_INIT
    memset(lock, 0, sizeof(MPIDU_Process_lock_t));
    err = mutex_init(lock, USYNC_PROCESS, 0);
    if (err)
	MPIU_Error_printf("mutex_init error: %d\n", err);
#else
    *(lock) = 0;
#endif
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDU_PROCESS_LOCK_INIT);
}

#undef FUNCNAME
#define FUNCNAME MPIDU_Process_lock
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
static inline void MPIDU_Process_lock( MPIDU_Process_lock_t *lock )
{
#ifdef HAVE_MUTEX_INIT
    int err;
    err = mutex_lock(lock);
    if (err)
	MPIU_Error_printf("mutex_lock error: %d\n", err);
#else
    int i;
    MPIDI_STATE_DECL(MPID_STATE_MPIDU_PROCESS_LOCK);
    MPIDI_FUNC_ENTER(MPID_STATE_MPIDU_PROCESS_LOCK);
    for (;;)
    {
        for (i=0; i<g_nLockSpinCount; i++)
        {
            if (*lock == 0)
            {
#ifdef HAVE_INTERLOCKEDEXCHANGE
                if (InterlockedExchange((LPLONG)lock, 1) == 0)
                {
                    MPIDI_FUNC_EXIT(MPID_STATE_MPIDU_PROCESS_LOCK);
                    return;
                }
#elif defined(HAVE__INTERLOCKEDEXCHANGE)
		/* The Intel compiler complains if the lock is cast to
		 * volatile void * (the type of lock is probably
		 * volatile long *).  The void * works for the Intel 
		 * compiler. */
                if (_InterlockedExchange((void *)lock, 1) == 0)
                {
                    MPIDI_FUNC_EXIT(MPID_STATE_MPIDU_PROCESS_LOCK);
                    return;
                }
#elif defined(HAVE_COMPARE_AND_SWAP)
                if (compare_and_swap(lock, 0, 1) == 1)
                {
                    MPIDI_FUNC_EXIT(MPID_STATE_MPIDU_PROCESS_LOCK);
                    return;
                }
#else
#error *** No atomic memory operation specified to implement busy locks ***
#endif
            }
        }
        MPIDU_Yield();
    }
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDU_PROCESS_LOCK);
#endif
}

#undef FUNCNAME
#define FUNCNAME MPIDU_Process_unlock
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
static inline void MPIDU_Process_unlock( MPIDU_Process_lock_t *lock )
{
#ifdef HAVE_MUTEX_INIT
    int err;
#endif
    MPIDI_STATE_DECL(MPID_STATE_MPIDU_PROCESS_UNLOCK);
    MPIDI_FUNC_ENTER(MPID_STATE_MPIDU_PROCESS_UNLOCK);
#ifdef HAVE_MUTEX_INIT
    err = mutex_lock(lock);
    if (err)
	MPIU_Error_printf("mutex_unlock error: %d\n", err);
#else
    *(lock) = 0;
#endif
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDU_PROCESS_UNLOCK);
}

#undef FUNCNAME
#define FUNCNAME MPIDU_Process_lock_busy_wait
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
static inline void MPIDU_Process_lock_busy_wait( MPIDU_Process_lock_t *lock )
{
    int i;
#ifdef HAVE_MUTEX_INIT
    int err;
#endif
    MPIDI_STATE_DECL(MPID_STATE_MPIDU_PROCESS_LOCK_BUSY_WAIT);
    MPIDI_FUNC_ENTER(MPID_STATE_MPIDU_PROCESS_LOCK_BUSY_WAIT);
#ifdef HAVE_MUTEX_INIT
    err = mutex_lock(lock);
    if (err)
	MPIU_Error_printf("mutex_lock error: %d\n", err);
    err = mutex_unlock(lock);
    if (err)
	MPIU_Error_printf("mutex_unlock error: %d\n", err);
#else
    for (;;)
    {
        for (i=0; i<g_nLockSpinCount; i++)
            if (!*lock)
            {
		MPIDI_FUNC_EXIT(MPID_STATE_MPIDU_PROCESS_LOCK_BUSY_WAIT);
                return;
            }
        MPIDU_Yield();
    }
#endif
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDU_PROCESS_LOCK_BUSY_WAIT);
}

#undef FUNCNAME
#define FUNCNAME MPIDU_Process_lock_free
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
static inline void MPIDU_Process_lock_free( MPIDU_Process_lock_t *lock )
{
#ifdef HAVE_MUTEX_INIT
    int err;
#endif
    MPIDI_STATE_DECL(MPID_STATE_MPIDU_PROCESS_LOCK_FREE);
    MPIDI_FUNC_ENTER(MPID_STATE_MPIDU_PROCESS_LOCK_FREE);
#ifdef HAVE_MUTEX_INIT
    err = mutex_destroy(lock);
    if (err)
	MPIU_Error_printf("mutex_destroy error: %d\n", err);
#endif
    MPIDI_FUNC_EXIT(MPID_STATE_MPIDU_PROCESS_LOCK_FREE);
}

#else

void MPIDU_Process_lock_init( MPIDU_Process_lock_t *lock );
void MPIDU_Process_lock( MPIDU_Process_lock_t *lock );
void MPIDU_Process_unlock( MPIDU_Process_lock_t *lock );
void MPIDU_Process_lock_free( MPIDU_Process_lock_t *lock );
void MPIDU_Process_lock_busy_wait( MPIDU_Process_lock_t *lock );

#endif /* #ifdef USE_BUSY_LOCKS */
#endif /* #ifdef HAVE_MUTEX_INIT */
#endif /* defined(HAVE_SPARC_INLINE_PROCESS_LOCKS) */


#undef FUNCNAME
#define FUNCNAME MPIDU_Compare_swap
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
/*@
   MPIDU_Compare_swap - 

   Parameters:
+  void **dest
.  void *new_val
.  void *compare_val
.  MPIDU_Process_lock_t *lock
-  void **original_val

   Notes:
@*/
static inline int MPIDU_Compare_swap( void **dest, void *new_val, void *compare_val,            
                        MPIDU_Process_lock_t *lock, void **original_val )
{
    /* dest = pointer to value to be checked (address size)
       new_val = value to set dest to if *dest == compare_val
       original_val = value of dest prior to this operation */

    MPIDI_STATE_DECL(MPID_STATE_MPIDU_COMPARE_SWAP);
    MPIDI_FUNC_ENTER(MPID_STATE_MPIDU_COMPARE_SWAP);
#ifdef HAVE_NT_LOCKS
    MPIU_UNREFERENCED_ARG(lock);
    /**original_val = InterlockedCompareExchange(dest, new_val, compare_val);*/
    /**original_val = (void*)InterlockedCompareExchange((LONG*)dest, (LONG)new_val, (LONG)compare_val);*/
    *original_val = (void*)InterlockedCompareExchangePointer(dest, new_val, compare_val);
#elif defined(HAVE_COMPARE_AND_SWAP)
    MPIU_UNREFERENCED_ARG(lock);
    if (compare_and_swap((volatile long *)dest, (long)compare_val, (long)new_val))
        *original_val = new_val;
#elif defined(HAVE_SPARC_INLINE_PROCESS_LOCKS) || defined(HAVE_PTHREAD_H) || defined(HAVE_MUTEX_INIT)
    MPIDU_Process_lock( lock );

    *original_val = *dest;
    
    if ( *dest == compare_val )
        *dest = new_val;

    MPIDU_Process_unlock( lock );
#else
#error *** No locking functions specified ***
#endif

    MPIDI_FUNC_EXIT(MPID_STATE_MPIDU_COMPARE_SWAP);
    return 0;
}

#endif
