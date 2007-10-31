/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*  
 *  (C) 2004 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include "mpiimpl.h"
#include "mpif90conf.h"

/* -- Begin Profiling Symbol Block for routine MPI_Type_create_f90_real */
#if defined(HAVE_PRAGMA_WEAK)
#pragma weak MPI_Type_create_f90_real = PMPI_Type_create_f90_real
#elif defined(HAVE_PRAGMA_HP_SEC_DEF)
#pragma _HP_SECONDARY_DEF PMPI_Type_create_f90_real  MPI_Type_create_f90_real
#elif defined(HAVE_PRAGMA_CRI_DUP)
#pragma _CRI duplicate MPI_Type_create_f90_real as PMPI_Type_create_f90_real
#endif
/* -- End Profiling Symbol Block */

/* Define MPICH_MPI_FROM_PMPI if weak symbols are not supported to build
   the MPI routines.  You can use USE_WEAK_SYMBOLS to see if MPICH is
   using weak symbols to implement the MPI routines. */
#ifndef MPICH_MPI_FROM_PMPI
#define MPI_Type_create_f90_real PMPI_Type_create_f90_real

#endif

#undef FUNCNAME
#define FUNCNAME MPI_Type_create_f90_real

/*@
   MPI_Type_create_f90_real - Return a predefined type that matches 
   the specified range

   Input Arguments:
+  precision - Number of decimal digits in mantissa
-  range - Decimal exponent range desired

   Output Arguments:
.  newtype - A predefine MPI Datatype that matches the range.

   Notes:
If there is no corresponding type for the specified range, the call is 
erroneous.  This implementation sets 'newtype' to 'MPI_DATATYPE_NULL' and
returns an error of class 'MPI_ERR_ARG'.

.N Fortran

.N Errors
.N MPI_SUCCESS
.N MPI_ERR_ARG
@*/
int MPI_Type_create_f90_real( int precision, int range, MPI_Datatype *newtype )
{
    static const char FCNAME[] = "MPI_Type_create_f90_real";
    int mpi_errno = MPI_SUCCESS;
    static int f90_real_model[4] = { MPIR_F90_REAL_MODEL,
                                     MPIR_F90_DOUBLE_MODEL };
    MPID_MPI_STATE_DECL(MPID_STATE_MPI_TYPE_CREATE_F90_REAL);

    MPID_MPI_FUNC_ENTER(MPID_STATE_MPI_TYPE_CREATE_F90_REAL);
#   ifdef HAVE_ERROR_CHECKING
    {
        MPID_BEGIN_ERROR_CHECKS;
        {
            MPIR_ERRTEST_INITIALIZED(mpi_errno);
	    if (mpi_errno) {
		return MPIR_Err_return_comm( 0, FCNAME, mpi_errno );
	    }
        }
        MPID_END_ERROR_CHECKS;
    }
#   endif /* HAVE_ERROR_CHECKING */

    /* ... body of routine ...  */

    /* ... end of body of routine ... */

    MPID_MPI_FUNC_EXIT(MPID_STATE_MPI_TYPE_CREATE_F90_REAL);
    return MPI_SUCCESS;
}
