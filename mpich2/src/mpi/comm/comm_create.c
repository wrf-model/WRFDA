/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 *
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpiimpl.h"
#include "mpicomm.h"

/* -- Begin Profiling Symbol Block for routine MPI_Comm_create */
#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_Comm_create = PMPI_Comm_create
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_Comm_create  MPI_Comm_create
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_Comm_create as PMPI_Comm_create
#endif
/* -- End Profiling Symbol Block */

/* Define MPICH_MPI_FROM_PMPI if weak symbols are not supported to build
   the MPI routines */
#ifndef MPICH_MPI_FROM_PMPI
#undef MPI_Comm_create
#define MPI_Comm_create PMPI_Comm_create

#endif

#undef FUNCNAME
#define FUNCNAME MPI_Comm_create

/*@

MPI_Comm_create - Creates a new communicator

Input Parameters:
+ comm - communicator (handle) 
- group - group, which is a subset of the group of 'comm'  (handle) 

Output Parameter:
. comm_out - new communicator (handle) 

.N ThreadSafe

.N Fortran

.N Errors
.N MPI_SUCCESS
.N MPI_ERR_COMM
.N MPI_ERR_GROUP

.seealso: MPI_Comm_free
@*/
int MPI_Comm_create(MPI_Comm comm, MPI_Group group, MPI_Comm *newcomm)
{
    static const char FCNAME[] = "MPI_Comm_create";
    int mpi_errno = MPI_SUCCESS;
    MPID_Comm *comm_ptr = NULL;
    int i, j, n, *mapping = 0, new_context_id;
    MPID_Comm *newcomm_ptr;
    MPID_Group *group_ptr;
    MPIU_CHKLMEM_DECL(1);
    MPID_MPI_STATE_DECL(MPID_STATE_MPI_COMM_CREATE);

    MPIR_ERRTEST_INITIALIZED_ORDIE();
    
    MPIU_THREAD_SINGLE_CS_ENTER("comm");
    MPID_MPI_FUNC_ENTER(MPID_STATE_MPI_COMM_CREATE);

    /* Validate parameters, and convert MPI object handles to object pointers */
#   ifdef HAVE_ERROR_CHECKING
    {
        MPID_BEGIN_ERROR_CHECKS;
        {
	    MPIR_ERRTEST_COMM(comm, mpi_errno);
            if (mpi_errno) goto fn_fail;
	}
        MPID_END_ERROR_CHECKS;
	
	MPID_Comm_get_ptr( comm, comm_ptr );
	
        MPID_BEGIN_ERROR_CHECKS;
        {
            /* Validate comm_ptr */
            MPID_Comm_valid_ptr( comm_ptr, mpi_errno );
	    /* If comm_ptr is not valid, it will be reset to null */

	    MPIR_ERRTEST_GROUP(group, mpi_errno);
            if (mpi_errno) goto fn_fail;
	}
        MPID_END_ERROR_CHECKS;
	
	MPID_Group_get_ptr( group, group_ptr );
    
        MPID_BEGIN_ERROR_CHECKS;
        {
	    /* Check the group ptr */
	    MPID_Group_valid_ptr( group_ptr, mpi_errno );
            if (mpi_errno) goto fn_fail;
        }
        MPID_END_ERROR_CHECKS;
    }
#   else
    {
	MPID_Comm_get_ptr( comm, comm_ptr );
	MPID_Group_get_ptr( group, group_ptr );
    }
#   endif

    /* ... body of routine ...  */
    
    /* Create a new communicator from the specified group members */

    /* If there is a context id cache in oldcomm, use it here.  Otherwise,
       use the appropriate algorithm to get a new context id. 
       Creating the context id is collective over the *input* communicator,
       so it must be created before we decide if this process is a 
       member of the group */
    /* In the multi-threaded case, MPIR_Get_contextid assumes that the
       calling routine already holds the single criticial section */
    new_context_id = MPIR_Get_contextid( comm_ptr );
    MPIU_ERR_CHKANDJUMP(new_context_id == 0, mpi_errno, MPI_ERR_OTHER, 
			"**toomanycomm" );

    /* Make sure that the processes for this group are contained within
       the input communicator.  Also identify the mapping from the ranks of 
       the old communicator to the new communicator.
       We do this by matching the lpids of the members of the group
       with the lpids of the members of the input communicator.
       It is an error if the group contains a reference to an lpid that 
       does not exist in the communicator.
       
       An important special case is groups (and communicators) that
       are subsets of MPI_COMM_WORLD.  This this case, the lpids are
       exactly the same as the ranks in comm world.  Currently, we
       don't take this into account, but if the code to handle the general 
       case is too messy, we'll add this in.
    */
    if (group_ptr->rank != MPI_UNDEFINED) {
	n = group_ptr->size;
	/*printf( "group size = %d comm size = %d\n", n, 
	  comm_ptr->remote_size ); */
	MPIU_CHKLMEM_MALLOC(mapping,int*,n*sizeof(int),mpi_errno,"mapping");
	for (i=0; i<n; i++) {
	    /* Mapping[i] is the rank in the communicator of the process that
	       is the ith element of the group */
	    /* FIXME : BUBBLE SORT */
	    /* FIXME : NEEDS COMM_WORLD SPECIALIZATION */
	    mapping[i] = -1;
	    for (j=0; j<comm_ptr->remote_size; j++) {
		int comm_lpid;
		MPID_VCR_Get_lpid( comm_ptr->vcr[j], &comm_lpid );
		/*printf( "commlpid = %d, group[%d]lpid = %d\n",
		  comm_lpid, i, group_ptr->lrank_to_lpid[i].lpid ); */
		if (comm_lpid == group_ptr->lrank_to_lpid[i].lpid) {
		    mapping[i] = j;
		    break;
		}
	    }
	    MPIU_ERR_CHKANDJUMP1(mapping[i] == -1,mpi_errno,MPI_ERR_GROUP,
			 "**groupnotincomm", "**groupnotincomm %d", i );
	}

	/* Get the new communicator structure and context id */
	
	mpi_errno = MPIR_Comm_create( &newcomm_ptr );
	if (mpi_errno) goto fn_fail;

	newcomm_ptr->context_id	    = new_context_id;
	newcomm_ptr->recvcontext_id = new_context_id;
	newcomm_ptr->remote_size    = newcomm_ptr->local_size = n;
	newcomm_ptr->rank	    = group_ptr->rank;
	newcomm_ptr->comm_kind	    = MPID_INTRACOMM;
	/* Since the group has been provided, let the new communicator know
	   about the group */
        newcomm_ptr->local_comm	    = 0;
	newcomm_ptr->local_group    = group_ptr;
	newcomm_ptr->remote_group   = group_ptr;
	MPIR_Group_add_ref( group_ptr );
	MPIR_Group_add_ref( group_ptr );

	/* Setup the communicator's vc table */
	MPID_VCRT_Create( n, &newcomm_ptr->vcrt );
	MPID_VCRT_Get_ptr( newcomm_ptr->vcrt, &newcomm_ptr->vcr );
	for (i=0; i<n; i++) {
	    /* For rank i in the new communicator, find the corresponding
	       rank in the input communicator */
	    MPID_VCR_Dup( comm_ptr->vcr[mapping[i]], &newcomm_ptr->vcr[i] );
	    
	    /* printf( "[%d] mapping[%d] = %d\n", comm_ptr->rank, i, mapping[i] ); */
	}

	/* Notify the device of this new communicator */
	/*printf( "about to notify device\n" ); */
	MPID_Dev_comm_create_hook( newcomm_ptr );
	/*printf( "about to return from comm_create\n" ); */
	
	*newcomm = newcomm_ptr->handle;
    }
    else {
	/* This process is not in the group */
	MPIR_Free_contextid( new_context_id );
	*newcomm = MPI_COMM_NULL;
    }
    
    /* ... end of body of routine ... */

    /* mpi_errno = MPID_Comm_create(); */
    if (mpi_errno != MPI_SUCCESS) goto fn_fail;

  fn_exit:
    MPIU_CHKLMEM_FREEALL();
    MPID_MPI_FUNC_EXIT(MPID_STATE_MPI_COMM_CREATE);
    MPIU_THREAD_SINGLE_CS_EXIT("comm");
    return mpi_errno;

  fn_fail:
    /* --BEGIN ERROR HANDLING-- */
#   ifdef HAVE_ERROR_CHECKING
    {
	mpi_errno = MPIR_Err_create_code(
	    mpi_errno, MPIR_ERR_RECOVERABLE, FCNAME, __LINE__, MPI_ERR_OTHER, "**mpi_comm_create",
	    "**mpi_comm_create %C %G %p", comm, group, newcomm);
    }
#   endif
    mpi_errno = MPIR_Err_return_comm( comm_ptr, FCNAME, mpi_errno );
    goto fn_exit;
    /* --END ERROR HANDLING-- */
}

