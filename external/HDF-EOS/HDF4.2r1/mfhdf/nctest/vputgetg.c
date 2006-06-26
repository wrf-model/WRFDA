/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: vputgetg.c,v 1.6 1997/11/05 19:41:06 koziol Exp $
 *********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include "netcdf.h"
#include "testcdf.h"		/* defines in-memory test cdf structure */
#include "add.h"		/* functions to update in-memory netcdf */
#include "val.h"
#include "error.h"
#include "tests.h"
#include "alloc.h"
#include "emalloc.h"
#ifdef HDF
#include "hdf.h"
#endif

#define max(A, B)	((A) > (B) ? (A) : (B))

/* 
 * For every variable in open netcdf, puts and gets three hypercubes 
 * of data of the appropriate type, comparing values from get to 
 * values put to check that both ncvarputg and ncvargetg worked.  The 
 * three hypercubes are
 *    - a large hypercube from (0, 0, ...) to the far corner (diagonally 
 *      opposite (0, 0, ...),
 *    - a size 1 hypercube from the far corner with edge lengths of 1 
 *      in every direction, and
 *    - a hypercube starting about 1/3 of the way along the diagonal
 *      from (0,0,...) extending 1/3 of the way in every direction 
 *      toward the far corner.
 */

int
test_varputgetg(cdfid)
     int cdfid;			/* handle of netcdf open and in data mode */
{
    int nerrs = 0;
    static char pname[] = "test_varputgetg";
    int id, ie, iv;		/* loop indices */
    int ne = 3;			/* number of test hypercubes for each var */
    struct cdfhc {		/* a hypercube with generic values */
	long cor[MAX_VAR_DIMS];	/* netcdf coordinates for lower corner */
	long edg[MAX_VAR_DIMS];	/* netcdf edge lengths to upper corner */
	void *vals;		/* pointer to block of values */
    } hc[3], tmp;		/* test hypercubes */
    long nel[3];		/* number of elements in hypercube */
    long strides[MAX_VAR_DIMS];	/* external strides */
    long basis[MAX_VAR_DIMS];	/* internal array, element-access 
				   basis vector */

    for (iv = 0; iv < test.nvars; iv++)	{ /* for each var in netcdf */
	long	tmpbasis;

	for (ie = 0; ie < ne; ie++)
	  nel[ie] = 1;		/* to compute space for hypercube values */

	/*
	 * The following macro returns the size of a dimension for a
	 * variable with a maximum  dimension size of 5 for the record
	 * dimension.
	 */
#	define EXTNPTS(varid, idim)	\
	    (test.dims[test.vars[varid].dims[id]].size == NC_UNLIMITED \
		? 5 \
		: test.dims[test.vars[varid].dims[id]].size)
#	define STRIDE(idim)		(idim + 2)
#	define INTNPTS(extnpts, idim)	(1 + (extnpts - 1) / STRIDE(idim))


	for (id = 0; id < test.vars[iv].ndims; id++) { /* set cubes */
	    strides[id]	= STRIDE(id);

	    /* start at "lower-left" corner, do whole variable */
	    hc[0].cor[id] = 0;
	    hc[0].edg[id] = INTNPTS(EXTNPTS(iv, id), id);
	    nel[0] *= hc[0].edg[id];

	    /* start at "upper-right" corner, do one point */
	    hc[1].cor[id] = EXTNPTS(iv, id) - 1;
	    hc[1].edg[id] = 1;
	    nel[1] *= hc[1].edg[id];

	    /* start about 1/3 way along diagonal, do 1/3 in each direction */
	    hc[2].cor[id] = EXTNPTS(iv, id)/3;
	    hc[2].edg[id] = INTNPTS(max(EXTNPTS(iv, id)/3, 1), id);
	    nel[2] *= hc[2].edg[id];
	}
	for (ie = 0; ie < ne; ie++) { /* for each of ne points */
	    int nelms = nel[ie]*nctypelen(test.vars[iv].type) + 8;
	    /* allocate space for the cube of values */
	    hc[ie].vals = emalloc(nelms);
	    tmp.vals = emalloc(nelms);

	    /* fill allocated space with different values of right type */
	    val_fill(test.vars[iv].type, nel[ie], hc[ie].vals);

	    /*
	     * Set internal-array element-access basis vector to be negative 
	     * of natural storage so as to access the elements of the array
	     * backwards.
	     */
	    tmpbasis	= nctypelen(test.vars[iv].type);
	    for (id = test.vars[iv].ndims-1; id >= 0; --id) {
		basis[id]	 = -tmpbasis;
		tmpbasis	*= hc[ie].edg[id];
	    }

	    if(ncvarputg (cdfid, iv, hc[ie].cor, hc[ie].edg, 
			  strides, basis, 
			  (char*)hc[ie].vals+(nel[ie]-1)*
			      nctypelen(test.vars[iv].type))
	       == -1) {
		error("%s: ncvarputg failed for point %d, variable %s",
		      pname, ie, test.vars[iv].name);
		nerrs++;
		errvar(&test, &test.vars[iv]);
		(void)fprintf(stderr,"  corner = (");
		for (id = 0 ; id < test.vars[iv].ndims; id++)
		  (void)fprintf(stderr,"%d%s",(int)hc[ie].cor[id],
				(id < test.vars[iv].ndims-1) ? ", " : "");
		(void)fprintf(stderr,")\n");
		(void)fprintf(stderr,"  edge = (");
		for (id = 0 ; id < test.vars[iv].ndims; id++)
		  (void)fprintf(stderr,"%d%s",(int)hc[ie].edg[id],
				(id < test.vars[iv].ndims-1) ? ", " : "");
		(void)fprintf(stderr,")\n");
		(void)fprintf(stderr,"  external strides = (");
		for (id = 0 ; id < test.vars[iv].ndims; id++)
		  (void)fprintf(stderr,"%d%s",(int)strides[id],
				(id < test.vars[iv].ndims-1) ? ", " : "");
		(void)fprintf(stderr,")\n");
		(void)fprintf(stderr,"  internal basis vector = (");
		for (id = 0 ; id < test.vars[iv].ndims; id++)
		  (void)fprintf(stderr,"%d%s",(int)basis[id],
				(id < test.vars[iv].ndims-1) ? ", " : "");
		(void)fprintf(stderr,")\n");
	    } else {
		long	dsize[MAX_VAR_DIMS];

		for (id = 0; id < test.vars[iv].ndims; id++)
		    dsize[id]	= EXTNPTS(iv, id);
		add_data(&test, iv, hc[ie].cor, dsize);
						    /* keep test in sync */
		if(ncvargetg (cdfid, iv, hc[ie].cor, hc[ie].edg, 
			      strides, basis,
			      (char*)tmp.vals+(nel[ie]-1)*
				  nctypelen(test.vars[iv].type))
		   == -1) {
		    error("%s: ncvargetg failed for point %d, variable %s",
			  pname, ie, test.vars[iv].name);
		    nerrs++;
		}
		else {
		    if (val_cmp(test.vars[iv].type, nel[ie],
				hc[ie].vals, tmp.vals) != 0) {
			error("%s: bad values returned from ncvargetg",
			      pname);
			nerrs++;
			errvar(&test, &test.vars[iv]); /* describe var */
		    }
		}
	    }

	    Free ((char *) hc[ie].vals);
	    Free ((char *) tmp.vals);
	}
    }
    return nerrs;
}
