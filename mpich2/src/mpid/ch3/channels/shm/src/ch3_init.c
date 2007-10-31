/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpidi_ch3_impl.h"
#include "pmi.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

MPIDI_CH3I_Process_t MPIDI_CH3I_Process = {NULL};

static void generate_shm_string(char *str)
{
#ifdef USE_WINDOWS_SHM
    UUID guid;
    UuidCreate(&guid);
    MPIU_Snprintf(str, MPIDI_MAX_SHM_NAME_LENGTH, 
	"%08lX-%04X-%04x-%02X%02X-%02X%02X%02X%02X%02X%02X",
	guid.Data1, guid.Data2, guid.Data3,
	guid.Data4[0], guid.Data4[1], guid.Data4[2], guid.Data4[3],
	guid.Data4[4], guid.Data4[5], guid.Data4[6], guid.Data4[7]);
    MPIU_DBG_PRINTF(("GUID = %s\n", str));
#elif defined (USE_POSIX_SHM)
    MPIU_Snprintf(str, MPIDI_MAX_SHM_NAME_LENGTH, "/mpich_shm_%d", getpid());
#elif defined (USE_SYSV_SHM)
    MPIU_Snprintf(str, MPIDI_MAX_SHM_NAME_LENGTH, "%d", getpid());
#else
#error No shared memory subsystem defined
#endif
}

#undef FUNCNAME
#define FUNCNAME MPIDI_CH3_Init
#undef FCNAME
#define FCNAME MPIDI_QUOTE(FUNCNAME)
int MPIDI_CH3_Init(int has_parent, MPIDI_PG_t * pg, int pg_rank )
{
    int mpi_errno = MPI_SUCCESS;
    int pmi_errno = PMI_SUCCESS;
    MPIDI_VC_t * vc;
    int pg_size;
    int p;
    char * key;
    char * val;
    char *kvsname = NULL;
    int key_max_sz;
    int val_max_sz;
    char shmemkey[MPIDI_MAX_SHM_NAME_LENGTH];
    int i, j, k;
    int shm_block;
    char local_host[100];

    /*
     * Extract process group related information from PMI and initialize
     * structures that track the process group connections, MPI_COMM_WORLD, and
     * MPI_COMM_SELF
     */
    /* MPID_Init in mpid_init.c handles the process group initialization. */

    /* Get the kvsname associated with MPI_COMM_WORLD */
    MPIDI_PG_GetConnKVSname( &kvsname );

    /* set the global variable defaults */
    pg->ch.nShmEagerLimit = MPIDI_SHM_EAGER_LIMIT;
#ifdef HAVE_SHARED_PROCESS_READ
    pg->ch.nShmRndvLimit = MPIDI_SHM_RNDV_LIMIT;
#ifdef HAVE_WINDOWS_H
    pg->ch.pSharedProcessHandles = NULL;
#else
    pg->ch.pSharedProcessIDs = NULL;
    pg->ch.pSharedProcessFileDescriptors = NULL;
#endif
#endif
    pg->ch.addr = NULL;
#ifdef USE_POSIX_SHM
    pg->ch.key[0] = '\0';
    pg->ch.id = -1;
#elif defined (USE_SYSV_SHM)
    pg->ch.key = -1;
    pg->ch.id = -1;
#elif defined (USE_WINDOWS_SHM)
    pg->ch.key[0] = '\0';
    pg->ch.id = NULL;
#else
#error No shared memory subsystem defined
#endif
    pg->ch.nShmWaitSpinCount = MPIDI_CH3I_SPIN_COUNT_DEFAULT;
    pg->ch.nShmWaitYieldCount = MPIDI_CH3I_YIELD_COUNT_DEFAULT;

    /* Initialize the VC table associated with this process
       group (and thus COMM_WORLD) */
    pg_size = pg->size;

    for (p = 0; p < pg_size; p++)
    {
	/* FIXME: the vc's must be set to active for the close protocol to 
	   work in the shm channel */
	pg->vct[p].state = MPIDI_VC_STATE_ACTIVE;
	MPIDI_CH3_VC_Init( &pg->vct[p] );
	pg->vct[p].ch.req = (MPID_Request*)MPIU_Malloc(sizeof(MPID_Request));
	/* FIXME: Should these also be set in the VC_Init, or 
	   is VC_Init moot (never called because this channel does not
	   support dynamic processes?) */
	pg->vct[p].ch.shm_reading_pkt = TRUE;
#ifdef USE_SHM_UNEX
	pg->vct[p].ch.unex_finished_next = NULL;
	pg->vct[p].ch.unex_list = NULL;
#endif
    }
    
    /* save my vc_ptr for easy access */
    /* MPIDI_PG_Get_vcr(pg, pg_rank, &MPIDI_CH3I_Process.vc); */
    /* FIXME: Figure out whether this is a common feature of process 
       groups (and thus make it part of the general PG_Init) or 
       something else.  Avoid a "get" routine because of the danger in
       using "get" where "dup" is required. */
    MPIDI_CH3I_Process.vc = &pg->vct[pg_rank];

    /* Initialize Progress Engine */
    mpi_errno = MPIDI_CH3I_Progress_init();
    if (mpi_errno != MPI_SUCCESS)
    {
	mpi_errno = MPIR_Err_create_code(mpi_errno, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**init_progress", 0);
	return mpi_errno;
    }

    /* Allocate space for pmi keys and values */
    mpi_errno = PMI_KVS_Get_key_length_max(&key_max_sz);
    if (mpi_errno != PMI_SUCCESS)
    {
	mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**fail", "**fail %d", mpi_errno);
	return mpi_errno;
    }
    key_max_sz++;
    key = MPIU_Malloc(key_max_sz);
    if (key == NULL)
    {
	mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**nomem", 0);
	return mpi_errno;
    }
    mpi_errno = PMI_KVS_Get_value_length_max(&val_max_sz);
    if (mpi_errno != PMI_SUCCESS)
    {
	mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**fail", "**fail %d", mpi_errno);
	return mpi_errno;
    }
    val_max_sz++;
    val = MPIU_Malloc(val_max_sz);
    if (val == NULL)
    {
	mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**nomem", 0);
	return mpi_errno;
    }

    /* initialize the shared memory */
    shm_block = sizeof(MPIDI_CH3I_SHM_Queue_t) * pg_size; 

    if (pg_size > 1)
    {
	if (pg_rank == 0)
	{
	    /* Put the shared memory key */
	    generate_shm_string(shmemkey);
	    if (MPIU_Strncpy(key, "SHMEMKEY", key_max_sz))
	    {
		mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**strncpy", 0);
		return mpi_errno;
	    }
	    if (MPIU_Strncpy(val, shmemkey, val_max_sz))
	    {
		mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**strncpy", 0);
		return mpi_errno;
	    }
	    mpi_errno = PMI_KVS_Put(kvsname, key, val);
	    if (mpi_errno != 0)
	    {
		mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**pmi_kvs_put", "**pmi_kvs_put %d", mpi_errno);
		return mpi_errno;
	    }

	    /* Put the hostname to make sure everyone is on the same host */
	    if (MPIU_Strncpy(key, "SHMHOST", key_max_sz))
	    {
		mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**strncpy", 0);
		return mpi_errno;
	    }
	    MPID_Get_processor_name( val, val_max_sz, 0 );
	    mpi_errno = PMI_KVS_Put(kvsname, key, val);
	    if (mpi_errno != 0)
	    {
		mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**pmi_kvs_put", "**pmi_kvs_put %d", mpi_errno);
		return mpi_errno;
	    }

	    mpi_errno = PMI_KVS_Commit(kvsname);
	    if (mpi_errno != 0)
	    {
		mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**pmi_kvs_commit", "**pmi_kvs_commit %d", mpi_errno);
		return mpi_errno;
	    }
	    mpi_errno = PMI_Barrier();
	    if (mpi_errno != 0)
	    {
		mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**pmi_barrier", "**pmi_barrier %d", mpi_errno);
		return mpi_errno;
	    }
	}
	else
	{
	    /* Get the shared memory key */
	    if (MPIU_Strncpy(key, "SHMEMKEY", key_max_sz))
	    {
		mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**strncpy", 0);
		return mpi_errno;
	    }
	    mpi_errno = PMI_Barrier();
	    if (mpi_errno != 0)
	    {
		mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**pmi_barrier", "**pmi_barrier %d", mpi_errno);
		return mpi_errno;
	    }
	    mpi_errno = PMI_KVS_Get(kvsname, key, val, val_max_sz);
	    if (mpi_errno != 0)
	    {
		mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**pmi_kvs_get", "**pmi_kvs_get %d", mpi_errno);
		return mpi_errno;
	    }
	    if (MPIU_Strncpy(shmemkey, val, val_max_sz))
	    {
		mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**strncpy", 0);
		return mpi_errno;
	    }
	    /* Get the root host and make sure local process is on the same node */
	    if (MPIU_Strncpy(key, "SHMHOST", key_max_sz))
	    {
		mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**strncpy", 0);
		return mpi_errno;
	    }
	    mpi_errno = PMI_KVS_Get(kvsname, key, val, val_max_sz);
	    if (mpi_errno != 0)
	    {
		mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**pmi_kvs_get", "**pmi_kvs_get %d", mpi_errno);
		return mpi_errno;
	    }
	    MPID_Get_processor_name( local_host, sizeof(local_host), NULL );
	    if (strcmp(val, local_host))
	    {
		mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**shmhost", "**shmhost %s %s", local_host, val);
		return mpi_errno;
	    }
	}

	MPIU_DBG_PRINTF(("KEY = %s\n", shmemkey));
#if defined(USE_POSIX_SHM) || defined(USE_WINDOWS_SHM)
	if (MPIU_Strncpy(pg->ch.key, shmemkey, MPIDI_MAX_SHM_NAME_LENGTH))
	{
	    mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**strncpy", 0);
	    return mpi_errno;
	}
#elif defined (USE_SYSV_SHM)
	pg->ch.key = atoi(shmemkey);
#else
#error No shared memory subsystem defined
#endif

	mpi_errno = MPIDI_CH3I_SHM_Get_mem( pg, pg_size * shm_block, pg_rank, pg_size, TRUE );
	if (mpi_errno != MPI_SUCCESS)
	{
	    mpi_errno = MPIR_Err_create_code(mpi_errno, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**shmgetmem", 0);
	    return mpi_errno;
	}
    }
    else
    {
	mpi_errno = MPIDI_CH3I_SHM_Get_mem( pg, shm_block, 0, 1, FALSE );
	if (mpi_errno != MPI_SUCCESS)
	{
	    mpi_errno = MPIR_Err_create_code(mpi_errno, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**shmgetmem", 0);
	    return mpi_errno;
	}
    }

    /* initialize each shared memory queue */
    for (i=0; i<pg_size; i++)
    {
	/* MPIDI_PG_Get_vcr(pg, i, &vc); */
	/* FIXME: Move this code to the general init pg for shared
	   memory */
	vc = &pg->vct[i];
#ifdef HAVE_SHARED_PROCESS_READ
#ifdef HAVE_WINDOWS_H
	if (pg->ch.pSharedProcessHandles)
	    vc->ch.hSharedProcessHandle = pg->ch.pSharedProcessHandles[i];
#else
	if (pg->ch.pSharedProcessIDs)
	{
	    vc->ch.nSharedProcessID = pg->ch.pSharedProcessIDs[i];
	    vc->ch.nSharedProcessFileDescriptor = pg->ch.pSharedProcessFileDescriptors[i];
	}
#endif
#endif
	if (i == pg_rank)
	{
	    vc->ch.shm = (MPIDI_CH3I_SHM_Queue_t*)((char*)pg->ch.addr + (shm_block * i));
	    for (j=0; j<pg_size; j++)
	    {
		vc->ch.shm[j].head_index = 0;
		vc->ch.shm[j].tail_index = 0;
		for (k=0; k<MPIDI_CH3I_NUM_PACKETS; k++)
		{
		    vc->ch.shm[j].packet[k].offset = 0;
		    vc->ch.shm[j].packet[k].avail = MPIDI_CH3I_PKT_AVAILABLE;
		}
	    }
	}
	else
	{
	    /*vc->ch.shm += pg_rank;*/
	    vc->ch.shm = NULL;
	    vc->ch.write_shmq = (MPIDI_CH3I_SHM_Queue_t*)((char*)pg->ch.addr + (shm_block * i)) + pg_rank;
	    vc->ch.read_shmq = (MPIDI_CH3I_SHM_Queue_t*)((char*)pg->ch.addr + (shm_block * pg_rank)) + i;
	    /* post a read of the first packet header */
	    vc->ch.shm_reading_pkt = TRUE;
	}
    }

#ifdef HAVE_WINDOWS_H
    {
	/* if you know the number of processors, calculate the spin count relative to that number */
        SYSTEM_INFO info;
        GetSystemInfo(&info);
        if (info.dwNumberOfProcessors == 1)
            pg->ch.nShmWaitSpinCount = 1;
        else if (info.dwNumberOfProcessors < (DWORD) pg_size)
            pg->ch.nShmWaitSpinCount = ( MPIDI_CH3I_SPIN_COUNT_DEFAULT * info.dwNumberOfProcessors ) / pg_size;
    }
#else
    /* figure out how many processors are available and set the spin count accordingly */
#if defined(HAVE_SYSCONF) && defined(_SC_NPROCESSORS_ONLN)
    {
	int num_cpus;
	num_cpus = sysconf(_SC_NPROCESSORS_ONLN);
	if (num_cpus == 1)
	    pg->ch.nShmWaitSpinCount = 1;
	else if (num_cpus > 0 && num_cpus < pg_size)
            pg->ch.nShmWaitSpinCount = 1;
	    /* pg->ch.nShmWaitSpinCount = ( MPIDI_CH3I_SPIN_COUNT_DEFAULT * num_cpus ) / pg_size; */
    }
#else
    /* if the number of cpus cannot be determined, set the spin count to 1 */
    pg->ch.nShmWaitSpinCount = 1;
#endif
#endif

    mpi_errno = PMI_KVS_Commit(kvsname);
    if (mpi_errno != 0)
    {
	mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**pmi_kvs_commit", "**pmi_kvs_commit %d", mpi_errno);
	return mpi_errno;
    }
    mpi_errno = PMI_Barrier();
    if (mpi_errno != 0)
    {
	mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**pmi_barrier", "**pmi_barrier %d", mpi_errno);
	return mpi_errno;
    }
#ifdef USE_POSIX_SHM
    if (shm_unlink(pg->ch.key))
    {
	/* Everyone is unlinking the same object so failure is ok? */
	/*
	mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**shm_unlink", "**shm_unlink %s %d", pg->ch.key, errno);
	return mpi_errno;
	*/
    }
#elif defined (USE_SYSV_SHM)
    if (shmctl(pg->ch.id, IPC_RMID, NULL))
    {
	/* Everyone is removing the same object so failure is ok? */
	if (errno != EINVAL)
	{
	    mpi_errno = MPIR_Err_create_code(MPI_SUCCESS, MPIR_ERR_FATAL, FCNAME, __LINE__, MPI_ERR_OTHER, "**shmctl", "**shmctl %d %d", pg->ch.id, errno);
	    return mpi_errno;
	}
    }
#endif

fn_exit:
    if (val != NULL)
    { 
	MPIU_Free(val);
    }
    if (key != NULL)
    { 
	MPIU_Free(key);
    }
    
    return mpi_errno;
}

/* Perform the channel-specific vc initialization.  This routine is used
   in MPIDI_CH3_Init and in routines that create and initialize connections */
int MPIDI_CH3_VC_Init( MPIDI_VC_t *vc )
{
    vc->ch.sendq_head         = NULL;
    vc->ch.sendq_tail         = NULL;

    /* Which of these do we need? */
    vc->ch.recv_active        = NULL;
    vc->ch.send_active        = NULL;
    vc->ch.req                = NULL;
    vc->ch.read_shmq          = NULL;
    vc->ch.write_shmq         = NULL;
    vc->ch.shm                = NULL;
    vc->ch.shm_state          = 0;
    return 0;
}

/* This function simply tells the CH3 device to use the defaults for the 
   MPI-2 RMA functions */
int MPIDI_CH3_RMAFnsInit( MPIDI_RMAFns *a ) 
{ 
    return 0;
}

/* This routine is a hook for initializing information for a process
   group before the MPIDI_CH3_VC_Init routine is called */
int MPIDI_CH3_PG_Init( MPIDI_PG_t *pg )
{
    /* FIXME: This should call a routine from the ch3/util/shm directory
       to initialize the use of shared memory for processes WITHIN this 
       process group */
    return MPI_SUCCESS;
}
