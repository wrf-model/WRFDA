/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include <mpiimpl.h>
#include <mpid_dataloop.h>
#include <stdlib.h>
#include <limits.h>

/*@
  MPID_Type_create_pairtype - create necessary data structures for certain
  pair types (all but MPI_2INT etc., which never have the size != extent
  issue).

  This function is different from the other MPID_Type_create functions in that
  it fills in an already- allocated MPID_Datatype.  This is important for
  allowing configure-time determination of the MPI type values (these types
  are stored in the "direct" space, for those familiar with how MPICH2 deals
  with type allocation).

  Input Parameters:
+ type - name of pair type (e.g. MPI_FLOAT_INT)
- new_dtp - pointer to previously allocated datatype structure, which is
            filled in by this function

  Return Value:
  MPI_SUCCESS on success, MPI errno on failure.

  Note:
  Section 4.9.3 (MINLOC and MAXLOC) of the MPI-1 standard specifies that
  these types should be built as if by the following (e.g. MPI_FLOAT_INT):

    type[0] = MPI_FLOAT
    type[1] = MPI_INT
    disp[0] = 0
    disp[1] = sizeof(float) <---- questionable displacement!
    block[0] = 1
    block[1] = 1
    MPI_TYPE_STRUCT(2, block, disp, type, MPI_FLOAT_INT)

  However, this is a relatively naive approach that does not take struct
  padding into account when setting the displacement of the second element.
  Thus in our implementation we have chosen to instead use the actual
  difference in starting locations of the two types in an actual struct.
@*/
int MPID_Type_create_pairtype(MPI_Datatype type,
			      MPID_Datatype *new_dtp)
{
    int err, mpi_errno = MPI_SUCCESS;
    int type_size, el_size;
    MPI_Aint type_extent, true_ub;

    int blocks[2] = {1, 1};
    MPI_Aint disps[2];
    MPI_Datatype types[2];

    /* handle is filled in by MPIU_Handle_obj_alloc() */
    MPIU_Object_set_ref(new_dtp, 1);
    new_dtp->is_permanent = 1;
    new_dtp->is_committed = 1; /* predefined types are pre-committed */
    new_dtp->attributes   = NULL;
    new_dtp->cache_id     = 0;
    new_dtp->name[0]      = 0;
    new_dtp->contents     = NULL;

    new_dtp->dataloop_size  = -1;
    new_dtp->dataloop       = NULL;
    new_dtp->dataloop_depth = -1;

    /* TODO: perhaps clean this up with a macro? */
    switch(type) {
	case MPI_FLOAT_INT:
	    {
		struct { float a; int b; } foo;

		disps[0]    = 0;
		disps[1]    = (MPI_Aint) ((char *) &foo.b - (char *) &foo.a);
		types[0]    = MPI_FLOAT;
		types[1]    = MPI_INT;
		type_size   = sizeof(foo.a) + sizeof(foo.b);
		type_extent = (MPI_Aint) sizeof(foo);
		el_size     = (sizeof(foo.a) == sizeof(foo.b)) ?
		    (int) sizeof(foo.a) : -1;
		true_ub     = disps[1] + (MPI_Aint) sizeof(foo.b);
	    }
	    break;
	case MPI_DOUBLE_INT:
	    {
		struct { double a; int b; } foo;

		disps[0]    = 0;
		disps[1]    = (MPI_Aint) ((char *) &foo.b - (char *) &foo.a);
		types[0]    = MPI_DOUBLE;
		types[1]    = MPI_INT;
		type_size   = sizeof(foo.a) + sizeof(foo.b);
		type_extent = (MPI_Aint) sizeof(foo);
		el_size     = (sizeof(foo.a) == sizeof(foo.b)) ?
		    (int) sizeof(foo.a) : -1;
		true_ub     = disps[1] + (MPI_Aint) sizeof(foo.b);
	    }
	    break;
	case MPI_LONG_INT:
	    {
		struct { long a; int b; } foo;

		disps[0]    = 0;
		disps[1]    = (MPI_Aint) ((char *) &foo.b - (char *) &foo.a);
		types[0]    = MPI_LONG;
		types[1]    = MPI_INT;
		type_size   = sizeof(foo.a) + sizeof(foo.b);
		type_extent = (MPI_Aint) sizeof(foo);
		el_size     = (sizeof(foo.a) == sizeof(foo.b)) ?
		    (int) sizeof(foo.a) : -1;
		true_ub     = disps[1] + (MPI_Aint) sizeof(foo.b);
	    }
	    break;
	case MPI_SHORT_INT:
	    {
		struct { short a; int b; } foo;

		disps[0]    = 0;
		disps[1]    = (MPI_Aint) ((char *) &foo.b - (char *) &foo.a);
		types[0]    = MPI_SHORT;
		types[1]    = MPI_INT;
		type_size   = sizeof(foo.a) + sizeof(foo.b);
		type_extent = (MPI_Aint) sizeof(foo);
		el_size     = (sizeof(foo.a) == sizeof(foo.b)) ?
		    (int) sizeof(foo.a) : -1;
		true_ub     = disps[1] + (MPI_Aint) sizeof(foo.b);
	    }
	    break;
	case MPI_LONG_DOUBLE_INT:
	    {
		struct { long double a; int b; } foo;

		disps[0]    = 0;
		disps[1]    = (MPI_Aint) ((char *) &foo.b - (char *) &foo.a);
		types[0]    = MPI_LONG_DOUBLE;
		types[1]    = MPI_INT;
		type_size   = sizeof(foo.a) + sizeof(foo.b);
		type_extent = (MPI_Aint) sizeof(foo);
		el_size     = (sizeof(foo.a) == sizeof(foo.b)) ?
		    (int) sizeof(foo.a) : -1;
		true_ub     = disps[1] + (MPI_Aint) sizeof(foo.b);
	    }
	    break;
	default:
	    /* --BEGIN ERROR HANDLING-- */
	    mpi_errno = MPIR_Err_create_code(MPI_SUCCESS,
					     MPIR_ERR_RECOVERABLE,
					     "MPID_Type_create_pairtype",
					     __LINE__,
					     MPI_ERR_OTHER,
					     "**dtype", 0);
	    return mpi_errno;
	    /* --END ERROR HANDLING-- */
    }

    new_dtp->n_elements      = 2;
    new_dtp->element_size    = el_size;
    new_dtp->eltype          = MPI_DATATYPE_NULL;

    new_dtp->has_sticky_lb   = 0;
    new_dtp->true_lb         = 0;
    new_dtp->lb              = 0;

    new_dtp->has_sticky_ub   = 0;
    new_dtp->true_ub         = true_ub;

    new_dtp->size            = type_size;
    new_dtp->ub              = type_extent; /* possible padding */
    new_dtp->extent          = type_extent;

    new_dtp->alignsize       = MPIR_MAX(MPID_Datatype_get_basic_size(types[0]),
					MPID_Datatype_get_basic_size(types[1]));
    /* place maximum on alignment based on padding rules */
    /* There are some really wierd rules for structure alignment; 
       these capture the ones of which we are aware. */
    switch(type) {
	case MPI_SHORT_INT:
	case MPI_LONG_INT:
#ifdef HAVE_MAX_INTEGER_ALIGNMENT
	    new_dtp->alignsize       = MPIR_MIN(new_dtp->alignsize,
						HAVE_MAX_INTEGER_ALIGNMENT);
#endif
	    break;
	case MPI_FLOAT_INT:
#ifdef HAVE_MAX_FP_ALIGNMENT
	    new_dtp->alignsize       = MPIR_MIN(new_dtp->alignsize,
						HAVE_MAX_FP_ALIGNMENT);
#endif
	    break;
	case MPI_DOUBLE_INT:
#ifdef HAVE_MAX_DOUBLE_FP_ALIGNMENT
	    new_dtp->alignsize       = MPIR_MIN(new_dtp->alignsize,
						HAVE_MAX_DOUBLE_FP_ALIGNMENT);
#elif defined(HAVE_MAX_FP_ALIGNMENT)
	    new_dtp->alignsize       = MPIR_MIN(new_dtp->alignsize,
						HAVE_MAX_FP_ALIGNMENT);
#endif
	    break;
	case MPI_LONG_DOUBLE_INT:
#ifdef HAVE_MAX_LONG_DOUBLE_FP_ALIGNMENT
	    new_dtp->alignsize       = MPIR_MIN(new_dtp->alignsize,
					HAVE_MAX_LONG_DOUBLE_FP_ALIGNMENT);
#else
	    new_dtp->alignsize       = MPIR_MIN(new_dtp->alignsize,
						HAVE_MAX_FP_ALIGNMENT);
#endif
	    break;
    }
				   

    new_dtp->is_contig       = (type_size == type_extent) ? 1 : 0;
    new_dtp->n_contig_blocks = (type_size == type_extent) ? 1 : 2;

    /* fill in dataloop(s) */
    err = MPID_Dataloop_create_struct(2,
				      blocks,
				      disps,
				      types,
				      &(new_dtp->dataloop),
				      &(new_dtp->dataloop_size),
				      &(new_dtp->dataloop_depth),
				      MPID_DATALOOP_HOMOGENEOUS);
#if defined(MPID_HAS_HETERO) || 1
    if (!err) {
	/* heterogeneous dataloop representation */
	err = MPID_Dataloop_create_struct(2,
					  blocks,
					  disps,
					  types,
					  &(new_dtp->hetero_dloop),
					  &(new_dtp->hetero_dloop_size),
					  &(new_dtp->hetero_dloop_depth),
					  0);
    }
#endif /* MPID_HAS_HETERO */
    /* --BEGIN ERROR HANDLING-- */
    if (err) {
	mpi_errno = MPIR_Err_create_code(MPI_SUCCESS,
					 MPIR_ERR_RECOVERABLE,
					 "MPID_Dataloop_create_struct",
					 __LINE__,
					 MPI_ERR_OTHER,
					 "**nomem",
					 0);
	return mpi_errno;

    }
    /* --END ERROR HANDLING-- */

    return mpi_errno;
}
