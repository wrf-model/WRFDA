/* -*- Mode: C; c-basic-offset:4 ; -*- */

/*
 *  (C) 2001 by Argonne National Laboratory.
 *      See COPYRIGHT in top-level directory.
 */

#include <mpiimpl.h>
#include <mpid_dataloop.h>
#include <stdlib.h>
#include <limits.h>

void MPID_Datatype_create_dataloop(MPID_Datatype *dtp,
				   MPID_Dataloop **dlp_p,
				   int *dlsz_p,
				   int *dldepth_p,
				   int flags)
{
    MPI_Datatype *types;
    int *ints;
    MPI_Aint *aints;

    int i;
    MPID_Datatype_contents *cp;
    MPID_Datatype old_dtp;
    MPI_Aint stride;
    MPI_Aint *disps;

    MPIU_Assert(dtp->contents);

    cp = dtp->contents;

    /* TODO: THINK ABOUT PADDING... */
    types = (MPI_Datatype *) cp + sizeof(MPID_Datatype_contents);
    ints  = ((char *) types) + cp->nr_types * sizeof(MPI_Datatype);
    aints = ((char *) ints) + cp->nr_ints * sizeof(int);

    MPIU_Assert(cp->combiner != MPI_COMBINER_NAMED);

    /* first check for zero count on types where that makes sense */
    switch(cp->combiner) {
	case MPI_COMBINER_CONTIGUOUS:
	case MPI_COMBINER_VECTOR:
	case MPI_COMBINER_HVECTOR_INTEGER:
	case MPI_COMBINER_HVECTOR:
	case MPI_COMBINER_INDEXED_BLOCK:
	case MPI_COMBINER_INDEXED:
	case MPI_COMBINER_HINDEXED_INTEGER:
	case MPI_COMBINER_HINDEXED:
	case MPI_COMBINER_STRUCT_INTEGER:
	case MPI_COMBINER_STRUCT:
	    if (ints[0] == 0) {
		MPID_Dataloop_create_contiguous(0,
						MPI_INT,
						dlp_p,
						dlsz_p,
						dldepth_p,
						flags);
		return;
	    }
	    break;
	default:
	    break;
    }

    switch(cp->combiner) {
	case MPI_COMBINER_CONTIGUOUS:
	    MPID_Dataloop_create_contiguous(ints[0] /* count */,
					    types[0] /* oldtype */,
					    dlp_p, dlsz_p, dldepth_p,
					    flags);
	    break;
	case MPI_COMBINER_VECTOR:
	    MPID_Dataloop_create_vector(ints[0] /* count */,
					ints[1] /* blklen */,
					ints[2] /* stride */,
					types[0] /* oldtype */,
					0 /* stride not bytes */,
					dlp_p, dlsz_p, dldepth_p, flags);
	    break;
	case MPI_COMBINER_HVECTOR_INTEGER:
	    /* fortran hvector has integer stride in bytes */
	    stride = (MPI_Aint) ints[2];

	    MPID_Dataloop_create_vector(ints[0] /* count */,
					ints[1] /* blklen */,
					stride,
					types[0] /* oldtype */,
					1 /* stride in bytes */,
					dlp_p, dlsz_p, dldepth_p, flags);
	    break;
	case MPI_COMBINER_HVECTOR:
	    MPID_Dataloop_create_vector(ints[0] /* count */,
					ints[1] /* blklen */,
					aints[0] /* stride */,
					types[0] /* oldtype */,
					1 /* stride in bytes */,
					dlp_p, dlsz_p, dldepth_p, flags);
	    break;
	case MPI_COMBINER_INDEXED_BLOCK:
	    MPID_Dataloop_create_blockindexed(ints[0] /* count */,
					      ints[1] /* blklen */,
					      &ints[2] /* disps */,
					      0 /* disp not bytes */,
					      types[0] /* oldtype */,
					      dlp_p, dlsz_p, dldepth_p,
					      flags);
	    break;
	case MPI_COMBINER_INDEXED:
	    MPID_Dataloop_create_indexed(ints[0] /* count */,
					 &ints[1] /* blklens */,
					 &ints[ints[0]+1] /* disps */,
					 0 /* disp not in bytes */,
					 types[0] /* oldtype */,
					 dlp_p, dlsz_p, dldepth_p, flags);
	    break;
	case MPI_COMBINER_HINDEXED_INTEGER:
	    disps = (MPI_Aint *) MPIU_Malloc(ints[0] * sizeof(MPI_Aint));

	    for (i=0; i < ints[0]; i++) {
		disps[i] = (MPI_Aint) ints[ints[0] + 1 + i];
	    }

	    MPID_Dataloop_create_hindexed(ints[0] /* count */,
					  &ints[1] /* blklens */,
					  disps,
					  1 /* disp in bytes */,
					  types[0] /* oldtype */,
					  dlp_p, dlsz_p, dldepth_p, flags);

	    MPIU_Free(disps);

	    break;
	case MPI_COMBINER_HINDEXED:
	    MPID_Dataloop_create_hindexed(ints[0] /* count */,
					  &ints[1] /* blklens */,
					  aints /* disps */,
					  1 /* disp in bytes */,
					  types[0] /* oldtype */,
					  dlp_p, dlsz_p, dldepth_p, flags);

	    break;
	case MPI_COMBINER_STRUCT_INTEGER:
	    disps = (MPI_Aint *) MPIU_Malloc(ints[0] * sizeof(MPI_Aint));

	    for (i=0; i < ints[0]; i++) {
		disps[i] = (MPI_Aint) ints[ints[0] + 1 + i];
	    }

	    MPID_Dataloop_create_struct(ints[0] /* count */,
					&ints[1] /* blklens */,
					disps,
					types /* oldtype array */,
					dlp_p, dlsz_p, dldepth_p, flags);

	    MPIU_Free(disps);

	    break;
	case MPI_COMBINER_STRUCT:
	    MPID_Dataloop_create_struct(ints[0] /* count */,
					&ints[1] /* blklens */,
					aints /* disps */,
					types /* oldtype array */,
					dlp_p, dlsz_p, dldepth_p, flags);
	    break;
	case MPI_COMBINER_SUBARRAY:
	    /* TODO: NEED MPID_Dataloop_create_subarray() */
	    break;
	case MPI_COMBINER_DARRAY:
	    /* TODO: NEED MPID_Dataloop_create_darray() */
	    break;
	case MPI_COMBINER_DUP:
	    /* TODO: create MPID_Dataloop_create_dup */
	    /* IGNORE? */
	case MPI_COMBINER_NAMED:
	    /* TODO: create MPID_Dataloop_create_named ? */
	    /* TODO: NOT QUITE; NEED TO DEAL WITH BUILTINS ... */
	    MPID_Datatype_get_ptr(types[0], old_dtp);
	    MPID_Datatype_create_dataloop(old_dtp,
					  dlp_p, dlsz_p, dldepth_p, flags);
	    break;
	case MPI_COMBINER_RESIZED:
	    /* TODO: create MPID_Dataloop_create_resized */
	    /* IGNORE? */
	    break;
	case MPI_COMBINER_F90_REAL:
	case MPI_COMBINER_F90_COMPLEX:
	case MPI_COMBINER_F90_INTEGER:
	    /* TODO: WHAT DO I DO HERE? */
	default:
	    MPIU_Assert(0);
    }

    return;
}
