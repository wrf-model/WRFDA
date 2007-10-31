/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

/*
 * Threads
 */

/* MPE_Thread_create() defined in mpe_thread_posix.c */

#define MPE_Thread_exit()			\
{						\
    pthread_exit(NULL);				\
}

#define MPE_Thread_self(id_)			\
{						\
    *(id_) = pthread_self();			\
}

#define MPE_Thread_same(id1_, id2_, same_)			\
{								\
    *(same_) = pthread_equal(*(id1_), *(id2_)) ? TRUE : FALSE;	\
}

#define MPE_Thread_yield()						\
{									\
    /* FIXME: need to check for different types of yield */	\
    sched_yield();							\
}


/*
 *    Mutexes
 */

/* FIXME: using constant initializer if available */
#define MPE_Thread_mutex_create(mutex_ptr_, err_ptr_)		\
{								\
    int err__;							\
								\
    err__ = pthread_mutex_init((mutex_ptr_), NULL);		\
    if ((err_ptr_) != NULL)					\
    {								\
	/* FIXME: convert error to an MPE_THREAD_ERR value */	\
	*(int *)(err_ptr_) = err__;				\
    }								\
}

#define MPE_Thread_mutex_destroy(mutex_ptr_, err_ptr_)		\
{								\
    int err__;							\
								\
    err__ = pthread_mutex_destroy(mutex_ptr_);			\
    if ((err_ptr_) != NULL)					\
    {								\
	/* FIXME: convert error to an MPE_THREAD_ERR value */	\
	*(int *)(err_ptr_) = err__;				\
    }								\
}

#define MPE_Thread_mutex_lock(mutex_ptr_, err_ptr_)		\
{								\
    int err__;							\
    MPIU_DBG_MSG(THREAD,TYPICAL,"Enter MPE_Thread_mutex");      \
    err__ = pthread_mutex_lock(mutex_ptr_);			\
    if ((err_ptr_) != NULL)					\
    {								\
	/* FIXME: convert error to an MPE_THREAD_ERR value */	\
	*(int *)(err_ptr_) = err__;				\
    }								\
}

#define MPE_Thread_mutex_unlock(mutex_ptr_, err_ptr_)		\
{								\
    int err__;							\
								\
    MPIU_DBG_MSG(THREAD,TYPICAL,"Exiting MPE_Thread_mutex");    \
    err__ = pthread_mutex_unlock(mutex_ptr_);			\
    if ((err_ptr_) != NULL)					\
    {								\
	/* FIXME: convert error to an MPE_THREAD_ERR value */	\
	*(int *)(err_ptr_) = err__;				\
    }								\
}

#define MPE_Thread_mutex_trylock(mutex_ptr_, flag_ptr_, err_ptr_)		\
{										\
    int err__;									\
										\
    err__ = pthread_mutex_trylock(mutex_ptr_);					\
    *(flag_ptr_) = (err__ == 0) ? TRUE : FALSE;					\
    if ((err_ptr_) != NULL)							\
    {										\
	*(int *)(err_ptr_) = (err__ == EBUSY) : MPE_THREAD_SUCCESS ? err__;	\
	/* FIXME: convert error to an MPE_THREAD_ERR value */			\
    }										\
}


/*
 * Condition Variables
 */

#define MPE_Thread_cond_create(cond_ptr_, err_ptr_)		\
{								\
    int err__;							\
    								\
    err__ = pthread_cond_init((cond_ptr_), NULL);		\
    if ((err_ptr_) != NULL)					\
    {								\
	/* FIXME: convert error to an MPE_THREAD_ERR value */	\
	*(int *)(err_ptr_) = err__;				\
    }								\
}

#define MPE_Thread_cond_destroy(cond_ptr_, err_ptr_)		\
{								\
    int err__;							\
    								\
    err__ = pthread_cond_destroy(cond_ptr_);			\
    if ((err_ptr_) != NULL)					\
    {								\
	/* FIXME: convert error to an MPE_THREAD_ERR value */	\
	*(int *)(err_ptr_) = err__;				\
    }								\
}

#define MPE_Thread_cond_wait(cond_ptr_, mutex_ptr_, err_ptr_)						\
{													\
    int err__;												\
    													\
    /* The latest pthread specification says that cond_wait routines aren't allowed to return EINTR,	\
       but some of the older implementations still do. */						\
    do													\
    {													\
	err__ = pthread_cond_wait((cond_ptr_), (mutex_ptr_));						\
    }													\
    while (err__ == EINTR);										\
													\
    if ((err_ptr_) != NULL)										\
    {													\
	/* FIXME: convert error to an MPE_THREAD_ERR value */						\
	*(int *)(err_ptr_) = err__;									\
    }													\
}

#define MPE_Thread_cond_broadcast(cond_ptr_, err_ptr_)		\
{								\
    int err__;							\
    								\
    err__ = pthread_cond_broadcast(cond_ptr_);			\
								\
    if ((err_ptr_) != NULL)					\
    {								\
	/* FIXME: convert error to an MPE_THREAD_ERR value */	\
	*(int *)(err_ptr_) = err__;				\
    }								\
}

#define MPE_Thread_cond_signal(cond_ptr_, err_ptr_)		\
{								\
    int err__;							\
								\
    err__ = pthread_cond_signal(cond_ptr_);			\
								\
    if ((err_ptr_) != NULL)					\
    {								\
	/* FIXME: convert error to an MPE_THREAD_ERR value */	\
	*(int *)(err_ptr_) = err__;				\
    }								\
}


/*
 * Thread Local Storage
 */

#define MPE_Thread_tls_create(exit_func_ptr_, tls_ptr_, err_ptr_)	\
{									\
    int err__;								\
    									\
    err__ = pthread_key_create((tls_ptr_), (exit_func_ptr_));		\
									\
    if ((err_ptr_) != NULL)						\
    {									\
	/* FIXME: convert error to an MPE_THREAD_ERR value */		\
	*(int *)(err_ptr_) = err__;					\
    }									\
}

#define MPE_Thread_tls_destroy(tls_ptr_, err_ptr_)		\
{								\
    int err__;							\
    								\
    err__ = pthread_key_delete(*(tls_ptr_));			\
								\
    if ((err_ptr_) != NULL)					\
    {								\
	/* FIXME: convert error to an MPE_THREAD_ERR value */	\
	*(int *)(err_ptr_) = err__;				\
    }								\
}

#define MPE_Thread_tls_set(tls_ptr_, value_, err_ptr_)		\
{								\
    int err__;							\
								\
    err__ = pthread_setspecific(*(tls_ptr_), (value_));		\
								\
    if ((err_ptr_) != NULL)					\
    {								\
	/* FIXME: convert error to an MPE_THREAD_ERR value */	\
	*(int *)(err_ptr_) = err__;				\
    }								\
}

#define MPE_Thread_tls_get(tls_ptr_, value_ptr_, err_ptr_)	\
{								\
    *(value_ptr_) = pthread_getspecific(*(tls_ptr_));		\
								\
    if ((err_ptr_) != NULL)					\
    {								\
	/* FIXME: convert error to an MPE_THREAD_ERR value */	\
	*(int *)(err_ptr_) = MPE_THREAD_SUCCESS;		\
    }								\
}
