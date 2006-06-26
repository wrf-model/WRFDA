/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: vardef.c,v 1.14 2000/08/29 13:57:05 koziol Exp $
 *********************************************************************/

#include <stdio.h>
#include <string.h>
#include <math.h>
#include "netcdf.h"
#include "testcdf.h"		/* defines in-memory test cdf structure */
#include "add.h"		/* functions to update in-memory netcdf */
#include "error.h"
#include "tests.h"
#include "alloc.h"
#include "emalloc.h"
#ifdef HDF
#include "hdf.h"
#endif

#define LEN_OF(array) ((sizeof array) / (sizeof array[0]))
#ifdef HDF
#define  EPS64          ((float64)1.0E-14)
#define  EPS32          ((float32)1.0E-7)
#endif

/*
 * Test ncvardef
 *    check that proper define worked with ncvarinq
 *    check that returned id is one more than previous id
 *    try redefining an existing variable, check error
 *    try adding scalar variable (no dimensions)
 *    try with bad datatype, check error
 *    try with bad number of dimensions, check error
 *    try with bad dimension ids, check error
 *    try in data mode, check error
 */
void
test_ncvardef(path)
     char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    int cdfid;			/* netcdf id */
    static char pname[] = "test_ncvardef";
    int id, iv;
    static struct cdfvar va[] = { /* variables of all shapes and sizes */
	{"bytev", NC_BYTE, 6, ___, 0},
	{"charv", NC_CHAR, 5, ___, 0},
	{"shortv", NC_SHORT, 4, ___, 0},
	{"longv", NC_LONG, 3, ___, 0},
	{"floatv", NC_FLOAT, 2, ___, 0},
	{"doublev", NC_DOUBLE, 1, ___, 0},
	{"scalarv", NC_DOUBLE, 0, ___, 0}
    };
    int nv = LEN_OF(va);	/* number of variables to define */
    int va_id[LEN_OF(va)];	/* variable ids */
    static struct cdfvar tmp =	/* variable for testing bad types, etc. */
	{"tmpv", NC_DOUBLE, 1, ___, 0};
    /* if d5 >= 91 in following, problem on machines with 16-bit ints ??? */
    static struct cdfdim di[] = { /* a bunch of dimensions */
	{"d0", 2}, {"d1",3}, {"d2",5}, {"d3", 6}, {"d4", 4}, {"d5", 31}};
    int nd = LEN_OF(di);	/* number of dimensions */
    int di_id[LEN_OF(di)];	/* dimension ids */
    
    (void) fprintf(stderr, "*** Testing %s ...\t", &pname[5]);

    if ((cdfid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return;
    }
    /* opened, defining a variable should fail in data mode */
    if (ncvardef(cdfid, va[0].name, va[0].type, va[0].ndims, va[0].dims)
	!= -1) {
	error("%s: ncvardef should have failed in data mode", pname);
	ncclose(cdfid); return;
    }
    /* enter define mode */
    if (ncredef(cdfid) == -1) {
	error("%s: cdredef failed", pname);
	ncclose(cdfid); return;
    }

    /* Add nd more dimensions */
    for (id = 0; id < nd; id++) {
	if ((di_id[id] = ncdimdef(cdfid, di[id].name, di[id].size)) == -1) {
	    error("%s: ncdimdef failed for %s, size %d",
		  pname, di[id].name, di[id].size);
	    ncclose(cdfid); return;
	}
	add_dim(&test, &di[id]);	/* keep in-memory netcdf in sync */
    }

    tmp.dims = (int *) emalloc(sizeof(int) * MAX_VAR_DIMS);
    tmp.name = (char *) emalloc(MAX_NC_NAME);

    /* in define mode, add variables of each type with various shapes */
    for (iv = 0; iv < nv; iv++) {
	/* set shape to use subset of dimensions previously defined */
	va[iv].dims = (int *) emalloc(sizeof(int) * va[iv].ndims);
	for (id = 0; id < va[iv].ndims; id++)
	  va[iv].dims[id] = di_id[id];
	if ((va_id[iv] = ncvardef(cdfid, va[iv].name, va[iv].type,
				  va[iv].ndims, va[iv].dims)) == -1) {
	    error("%s: ncvardef failed", pname);
	    errvar(&test,&va[iv]); /* prints details about variable */
	    ncclose(cdfid); return;
	}
	add_var(&test, &va[iv]); /* keep in-memory netcdf in sync */
	/* check that var id returned is one more than previous var id */
	if (va_id[iv] != test.nvars - 1) {
	    error("%s: ncvardef returned %d for var id, expected %d",
		  pname, va_id[iv], test.nvars-1);
	    ncclose(cdfid); return;
	}
	/* use ncvarinq to get values just set and compare values */
	if (ncvarinq(cdfid, va_id[iv], tmp.name, &tmp.type,
		      &tmp.ndims, tmp.dims, &tmp.natts) == -1) {
	    error("%s: ncvarinq failed", pname);
	    errvar(&test,&va[iv]); /* prints details about variable */
	    ncclose(cdfid); return;
	}
	if (strcmp(tmp.name, va[iv].name) != 0 ||
	    tmp.type != va[iv].type ||
	    tmp.ndims != va[iv].ndims ||
	    tmp.natts != va[iv].natts) {
	    error("%s: ncvardef and ncvarinq don't agree for %s",
		  pname, va[iv].name);
	    nerrs++;
	    errvar(&test,&va[iv]);
	    errvar(&test,&tmp);
	}
	for (id = 0; id < va[iv].ndims; id++) {
	    if (tmp.dims[id] != va[iv].dims[id]) {
	    error("%s: ncvardef and ncvarinq don't agree on shape of %s",
		  pname, va[iv].name);
	    nerrs++;
	    errvar(&test,&va[iv]);
	    errvar(&test,&tmp);
	    }
	}
    }
    /* try adding same variable again, this should fail */
    if (ncvardef(cdfid, va[0].name, va[0].type,
		  va[0].ndims, va[0].dims) != -1) {
	error("%s: ncvardef should not allow redefinition", pname);
	ncclose(cdfid); return;
    }
    /* try bad type, should fail */
    if (ncvardef(cdfid, "badtype", BAD_TYPE, va[0].ndims, va[0].dims) != -1) {
	error("%s: ncvardef should have failed on bad type", pname);
	ncclose(cdfid); return;
    }
    /* try bad ndims, should fail */
    if (ncvardef(cdfid, "badndims", va[0].type, -1, va[0].dims) != -1) {
	error("%s: ncvardef should have failed on bad ndims", pname);
	ncclose(cdfid); return;
    }
    /* try bad ids in dims vector, should fail */
    id = va[0].dims[0];
    va[0].dims[va[0].ndims-1] = -1;
    if (ncvardef(cdfid, "baddims", va[0].type, va[0].ndims, va[0].dims)
	!= -1) {
	error("%s: ncvardef should have failed on negative dim id in dims",
	      pname);
	ncclose(cdfid); return;
    }
    if (ncendef (cdfid) == -1) {
	error("%s: ncendef failed", pname);
	ncclose(cdfid); return;
    }

    /* try reading a value of each type, should get appropriate fill value */
    for (iv = 0; iv < nv; iv++) {
	static long where[] = {0,0,0,0,0,0};

	switch(va[iv].type) {
	  case NC_BYTE:
	    {
		char val, fillval = FILL_BYTE;
		if (ncvarget1(cdfid, va_id[iv], where, (void *) &val) != -1) {
		    if (val != fillval) {
			error("%s: unwritten byte not FILL_BYTE", pname);
			nerrs++;
		    }
		} else {
		    error("%s: ncvarget1 failure for byte", pname);
		    nerrs++;
		}
	    }
	    break;
	  case NC_CHAR:
	    {
		char val, fillval = FILL_CHAR;
		if (ncvarget1(cdfid, va_id[iv], where, (void *) &val) != -1) {
		    if (val != fillval) {
			error("%s: unwritten char not FILL_CHAR", pname);
			nerrs++;
		    }
		} else {
		    error("%s: ncvarget1 failure for char", pname);
		    nerrs++;
		}
	    }
	    break;
	  case NC_SHORT:
	    {
		short val, fillval = FILL_SHORT;
		if (ncvarget1(cdfid, va_id[iv], where, (void *) &val) != -1) {
		    if (val != fillval) {
			error("%s: unwritten short not FILL_SHORT", pname);
			nerrs++;
		    }
		} else {
		    error("%s: ncvarget1 failure for short", pname);
		    nerrs++;
		}
	    }
	    break;
	  case NC_LONG:
	    {
		nclong val, fillval;
                val = 0;
                fillval = 0;
                fillval = FILL_LONG;
		if (ncvarget1(cdfid, va_id[iv], where, (void *) &val) != -1) {
		    if ((nclong) val != (nclong) fillval) {
                        nclong a, b;

printf("\n\n Was expecting %d instead got a %d\n", (int)fillval, (int)val);
                        a = (nclong) val;
                        b = (nclong) fillval;
printf("\n\n After cast %d %d\n", (int)a, (int)b);

			error("%s: unwritten long not FILL_LONG", pname);
			nerrs++;
		    }
		} else {
		    error("%s: ncvarget1 failure for long", pname);
		    nerrs++;
		}
	    }
	    break;
	  case NC_FLOAT:
	    {
		float val, fillval = FILL_FLOAT;
		if (ncvarget1(cdfid, va_id[iv], where, (void *) &val) != -1) {
#ifdef HDF
            if (fabs((double)(val - fillval)) > fabs((double)(fillval*EPS32)))
#else /*!HDF */
		    if (val != fillval)
#endif
            {
                error("%s: unwritten float not FILL_FLOAT", pname);
                nerrs++;
                }
        } else {
            error("%s: ncvarget1 failure for float", pname);
            nerrs++;
        }
	    }
	    break;
	  case NC_DOUBLE:
	    {
		double val, fillval = FILL_DOUBLE;
		if (ncvarget1(cdfid, va_id[iv], where, (void *) &val) != -1) {
#ifdef HDF
           if (fabs((double)(val - fillval)) > fabs((double)(fillval*EPS64)))
#else  /* !HDF */
		    if (val != fillval)
#endif /* !HDF */
            {
            error("%s: unwritten double not FILL_DOUBLE", pname);
            nerrs++;
            }
        } else {
            error("%s: ncvarget1 failure for double", pname);
            nerrs++;
        }
	    }
	    break;
        default:
            break;
	}
    }

    if (ncclose (cdfid) == -1) {
	error("%s: ncclose failed", pname);
	return;
    }
    Free ((char *) tmp.dims);
    Free (tmp.name);
    for (iv = 0; iv < nv; iv++)
      if (va[iv].dims)
	Free((char *) va[iv].dims);
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");
}
