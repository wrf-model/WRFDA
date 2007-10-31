/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*  
 *  (C) 2004 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpiimpl.h"
/* 
   mpif90conf.h contains definitions that define the data types that are
   available to Fortran 90.  These define a sequence of integers
   that define precision and range (for real and complex), and 
   range for integers.  It also defines a mapping of range to 
   Fortran 90 "kind" values for integers.
*/
#include "mpif90conf.h"

/* This gives the maximum number of distinct types returned by any one of the
   MPI_Type_create_f90_xxx routines */
#define MAX_F90_TYPES 16
typedef struct { int r, p;
    MPI_Datatype d; } F90Predefined;

static int nAllocReal    = 0;
static int nAllocComplex = 0;
static int nAllocInteger = 0;
static F90Predefined f90IntTypes[MAX_F90_TYPES];
static F90Predefined f90RealTypes[MAX_F90_TYPES];
static F90Predefined f90ComplexTypes[MAX_F90_TYPES];

static int MPIR_Create_unnamed_predefined( MPI_Datatype old, int combiner, 
					   int r, int p, MPI_Datatype *new_ptr );

/*
   The simple approach used here is to store (unordered) the precision and 
   range of each type that is created as a result of a user-call to
   one of the MPI_Type_create_f90_xxx routines.  The standard requires
   that the *same* handle be returned to allow simple == comparisons.
   A finalize callback is added to free up any remaining storage
*/

/* Given the requested range and the length of the type in bytes,
   return the matching datatype */
int MPIR_Match_f90_int( int range, int length, MPI_Datatype *newtype )
{
    int i;

    /* Has this type been requested before? */
    for (i=0; i<nAllocInteger; i++) {
	if (f90IntTypes[i].r == range) {
	    *newtype = f90IntTypes[i].d;
	    return 0;
	}
    }
    
    /* No.  Try to add it */
    if (nAllocInteger >= MAX_F90_TYPES) {
	int line = -1;  /* Hack to suppress warning message from 
			   extracterrmsgs, since this code should
			   reflect the routine from which it was called,
			   since the decomposition of the internal routine
			   is of no relevance to either the user or 
			   developer */
	return MPIR_Err_create_code( MPI_SUCCESS, MPIR_ERR_RECOVERABLE, 
				     "MPI_Type_create_f90_integer", line,
				     MPI_ERR_INTERN, "**f90typetoomany",
				     "**f90typetoomany %s %d", 
				     "integer", MAX_F90_TYPES );
    }
    
    /* Temporary */
    *newtype = MPI_DATATYPE_NULL;

    f90IntTypes[nAllocInteger].r   = range;
    f90IntTypes[nAllocInteger].p   = -1;
    f90IntTypes[nAllocInteger++].d = *newtype;
    
    return 0;
}

/* Build a new type */
static int MPIR_Create_unnamed_predefined( MPI_Datatype old, int combiner, 
					   int r, int p, MPI_Datatype *new_ptr )
{
    int mpi_errno;

    /* Create a contiguous type from one instance of the named type */
    mpi_errno = MPID_Type_contiguous( 1, old, new_ptr );

    /* Initialize the contents data */
    if (mpi_errno == MPI_SUCCESS) {
	MPID_Datatype *new_dtp;
	int vals[2];
	int nvals=0;

	switch (combiner) {
	case MPI_COMBINER_F90_INTEGER:
	    nvals   = 1;
	    vals[0] = r;
	    break;

	case MPI_COMBINER_F90_REAL:
	case MPI_COMBINER_F90_COMPLEX:
	    nvals   = 2;
	    vals[0] = p;
	    vals[1] = r;
	    break;
	}

	MPID_Datatype_get_ptr(*new_ptr, new_dtp);
	mpi_errno = MPID_Datatype_set_contents(new_dtp,
					       combiner,
					       nvals,
					       0,
					       0,
					       vals,
					       NULL,
					       NULL );
    }
    
    return mpi_errno;
}
