/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: cdftests.c,v 1.9 1998/02/02 21:52:53 smitchel Exp $
 *********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "netcdf.h"
#include "testcdf.h"		/* defines in-memory test netcdf structure */
#include "add.h"		/* functions to update in-memory netcdf */
#include "error.h"
#include "tests.h"
#include "alloc.h"
#include "emalloc.h"
#ifdef HDF
#include "hdf.h"
#endif

#define LEN_OF(array) ((sizeof array) / (sizeof array[0]))


/*
 * Test nccreate
 *    create a netcdf with no data, close it, test that it can be opened
 *    try again with NC_CLOBBER mode, check that no errors occurred
 *    try again with NC_NOCLOBBER mode, check error return
 * On exit, netcdf files are closed.
 * Uses: nccreate, ncendef, ncclose, ncopen.
 */
void
test_nccreate(path)
     char *path;		/* name of cdf file to create */
{
    int nerrs = 0;
    static char pname[] = "test_nccreate";
    int cdfid;

    (void) fprintf(stderr, "*** Testing %s ...\t", &pname[5]);

    if ((cdfid = nccreate(path, NC_CLOBBER)) == -1) {
	error("%s: nccreate failed to NC_CLOBBER", pname);
	return;
    }
    /* created OK */
    if (ncendef (cdfid) == -1) {
	error("%s: ncendef failed", pname);
	nerrs++;
    }
    if (ncclose (cdfid) == -1) {
	error("%s: ncclose failed", pname);
	nerrs++;
    }
    if ((cdfid = ncopen(path, NC_NOWRITE)) == -1) {
	error("%s: ncopen of newly created netcdf failed", pname);
	return;
    }
    /* opened OK */
    if (ncclose (cdfid) == -1) {
	error("%s: second ncclose failed", pname);
	nerrs++;
    }
    /* this call should fail, since we're using NC_NOCLOBBER mode */
    if ((cdfid = nccreate(path, NC_NOCLOBBER)) != -1) {
	error("%s: nccreate failed to honor NC_NOCLOBBER mode", pname);
	nerrs++;
    }

    /* Initialize in-memory netcdf to empty */
    add_reset(&test);
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");
}


/* 
 * Test ncopen
 *    try to open a non-existent netCDF, check error return
 *    open a netCDF with NC_WRITE mode, write something, close it
 *    open a netCDF with NC_NOWRITE mode, write something and check error
 *    try to open a netcdf twice, check whether returned cdf ids different
 * On exit, netcdf files are closed.
 * Uses: ncopen, ncredef, ncattput, ncendef, ncclose.
 */
void
test_ncopen(path)
     char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncopen";
    int cdfid0, cdfid1;
    static char title_val[] = "test netcdf";
    static char xpath[] = "tooth-fairy.cdf"; /* must not exist */
    static struct cdfatt title = /* attribute */
      {NC_GLOBAL, "title", NC_CHAR, LEN_OF (title_val), (void *) title_val};

    (void) fprintf(stderr, "*** Testing %s ...\t\t", &pname[5]);

    if((cdfid0 = ncopen(xpath, NC_NOWRITE)) != -1) {
	error("%s: ncopen should fail opening nonexistent file",
	      pname);
	return;
    }
    if((cdfid0 = ncopen(xpath, NC_WRITE)) != -1) {
	error("%s: ncopen should fail writing nonexistent file",
	      pname);
	return;
    }
    if ((cdfid0 = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed with NC_WRITE mode", pname);
	return;
    }
    /* opened */
    if (ncredef(cdfid0) == -1) {
	error("%s: cdredef failed", pname);
	ncclose(cdfid0); return;
    }
    /* in define mode */
    if (ncattput(cdfid0, NC_GLOBAL, "title", NC_CHAR, title.len, title.val)
	== -1) {
	error("%s: ncattput failed", pname);
	ncclose(cdfid0); return;
    }
    add_att(&test, NC_GLOBAL, &title); /* keep in-memory netcdf updated */
    if (ncendef (cdfid0) == -1) {
	error("%s: ncendef failed after ncattput", pname);
	ncclose(cdfid0); return;
    }
    if (ncclose (cdfid0) == -1) {
	error("%s: ncclose failed in NC_WRITE mode", pname);
	return;
    }

    if ((cdfid0 = ncopen(path, NC_NOWRITE)) == -1) {
	error("%s: ncopen failed with NC_NOWRITE mode", pname);
	return;
    }
    /* opened */
    /* this should fail, since in NC_NOWRITE mode */
    if (ncredef(cdfid0) != -1) {
	error("%s: cdredef should fail after NC_NOWRITE open", pname);
	ncclose(cdfid0); return;
    }

#if !(defined(vms) || defined(macintosh) || defined (SYMANTEC_C))
    if ((cdfid1 = ncopen(path, NC_WRITE)) == -1) {
	error("%s: second ncopen failed", pname);
	nerrs++;
    }
    else
    {
	    /* second open OK */
	    if (cdfid0 == cdfid1) {
		error("%s: ncopen should return new cdfid on second open",
		      pname);
		nerrs++;
	    }
	    if (ncclose (cdfid1) == -1) {
		error("%s: ncclose failed to close after second open", pname);
		nerrs++;
	    }
    }
#else /* Macintosh or VMS */
#if defined(macintosh) || defined (SYMANTEC_C)
	fprintf(stderr,"This version of the library Doesn't support shared access to files on Macintosh\n") ;
#else /* !macintosh */
	fprintf(stderr,"Doesn't support shared access on vms\n") ;
#endif /* !macintosh */
#endif /* Macintosh or VMS */

    if (ncclose (cdfid0) == -1) {
	error("%s: ncclose failed in NC_NOWRITE mode", pname);
	nerrs++;
    }
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");
}


/*
 * Test ncredef
 *    open a netCDF, enter define mode, add dimension, variable, attribute
 *    try ncredef from within define mode, check error
 *    leave define mode and close, releasing netcdf handle
 *    try ncredef with old handle, check error
 * On exit netcdf files are closed.
 * Uses: ncopen, ncredef, ncdimdef, ncvardef, ncattput, ncclose 
 */
void
test_ncredef(path)
     char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncredef";
    int cdfid;			/* netcdf id */
    int ii_dim;			/* dimension id */
    static struct cdfdim ii =	/* dimension */
      {"ii", 4};
    int aa_id;			/* variable id */
    static struct cdfvar aa =	/* variable */
      {"aa", NC_LONG, 1, ___, 0};
    static char units_val[] = "furlongs";
    static struct cdfatt aa_units = /* attribute */
      {___, "units", NC_CHAR, LEN_OF(units_val), (void *)units_val};

    (void) fprintf(stderr, "*** Testing %s ...\t\t", &pname[5]);

    if ((cdfid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return;
    }
    /* opened OK, enter define mode */
    if (ncredef(cdfid) == -1) {
	error("%s: cdredef failed", pname);
	ncclose(cdfid); return;
    }
    /* in define mode OK, add a dimension */
    if ((ii_dim = ncdimdef(cdfid, ii.name, ii.size)) == -1) {
	error("%s: ncdimdef failed", pname);
	ncclose(cdfid); return;
    }
    add_dim(&test, &ii);	/* keep in-memory netcdf in sync */

    /* dimension added OK, add a variable */
    aa.dims = (int *)emalloc(sizeof(int) * aa.ndims);
    aa.dims[0] = ii_dim;
    if ((aa_id = ncvardef(cdfid, aa.name, aa.type,
			   aa.ndims, aa.dims)) == -1) {
	error("%s: ncvardef failed", pname);
	ncclose(cdfid); return;
    }
    add_var(&test, &aa);	/* keep in-memory netcdf in sync */

    /* variable added OK, add a variable attribute */
    aa_units.var = aa_id;
    if (ncattput(cdfid, aa_units.var, aa_units.name,
		  aa_units.type, aa_units.len, (void *) aa_units.val) == -1) {
	error("%s: ncattput failed", pname);
	ncclose(cdfid); return;
    }
    add_att(&test, aa_id, &aa_units); /* keep in-memory netcdf in sync */

    if (ncredef(cdfid) != -1) {
	error("%s: cdredef in define mode should have failed", pname);
	ncclose(cdfid); return;
    }
    if (ncendef (cdfid) == -1) {
	error("%s: ncendef failed", pname);
	ncclose(cdfid); return;
    }
    if (ncclose (cdfid) == -1) {
	error("%s: ncclose failed", pname);
	return;
    }
    if (ncredef(cdfid) != -1) {
	error("%s: ncredef failed to report bad cdf handle", pname);
	nerrs++;
    }
    Free ((char *)aa.dims);
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");
}


/* 
 * Test ncendef
 *    check return from proper cdfendif after define mode
 *    try ncendef when in data mode, check error
 *    try ncendef with bad handle, check error
 *  On exit netcdf files are closed.
 * Uses: ncopen, ncredef, ncdimdef, ncvardef, ncattput, ncendef, ncclose
 */
void
test_ncendef(path)
     char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncendef";
    int cdfid;			/* netcdf id */
    int jj_dim, kk_dim;		/* dimension ids */
    int bb_id;			/* variable id */
    static struct cdfdim kk =	/* dimension */
      {"kk", 3};
    static struct cdfdim jj =	/* dimension */
      {"jj", 3};
    static struct cdfvar bb =	/* variable */
      {"bb", NC_LONG, 2, ___, 0};
    static float bb_rangev[2] = {0., 100.}; /* attribute vector */
    static struct cdfatt bb_range = /* attribute */
      {___, "valid_range", NC_FLOAT, LEN_OF(bb_rangev), (void *)bb_rangev};

    (void) fprintf(stderr, "*** Testing %s ...\t\t", &pname[5]);

    if ((cdfid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return;
    }
    /* opened */
    if (ncredef(cdfid) == -1) {
	error("%s: ncredef failed", pname);
	ncclose(cdfid); return;
    }
    /* in define mode, add dimensions */
    if ((jj_dim = ncdimdef(cdfid, jj.name, jj.size)) == -1 ||
	(kk_dim = ncdimdef(cdfid, kk.name, kk.size)) == -1) {
	error("%s: ncdimdef failed", pname);
	ncclose(cdfid); return;
    }
    add_dim(&test, &jj);	/* keep in-memory netcdf in sync */
    add_dim(&test, &kk);	/* keep in-memory netcdf in sync */
    
    /* dimensions added OK, add a variable */
    bb.dims = (int *) emalloc(sizeof(int) * bb.ndims);
    bb.dims[0] = kk_dim;
    bb.dims[1] = jj_dim;
    if ((bb_id = ncvardef(cdfid, bb.name, bb.type,
			   bb.ndims, bb.dims)) == -1) {
	error("%s: ncvardef failed", pname);
	ncclose(cdfid); return;
    }
    add_var(&test, &bb);	/* keep in-memory netcdf in sync */
    
    /* variable added OK, add a variable attribute */
    if (ncattput(cdfid, bb_id, bb_range.name, bb_range.type, bb_range.len,
		  (void *) bb_range.val) == -1) {
	error("%s: ncattput failed", pname);
	ncclose(cdfid); return;
    }
    add_att(&test, bb_id, &bb_range); /* keep in-memory netcdf in sync */
    
    if (ncendef (cdfid) == -1) {
	error("%s: ncendef failed", pname);
	ncclose(cdfid); return;
    }
    /* in data mode */
    if (ncendef (cdfid) != -1) { /* should fail in data mode */
	error("%s: ncendef in data mode should have failed", pname);
	ncclose(cdfid); return;
    }
    if (ncclose (cdfid) == -1) {
	error("%s: ncclose failed", pname);
	return;
    }
    /* should fail on a bad handle */
    if (ncendef (cdfid) != -1) {
	error("ncendef failed to report bad netcdf handle");
	nerrs++;
    }
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");
}


/* 
 * Test ncclose
 *    try on open netCDF
 *    try in define mode and data mode
 *    try with bad handle, check error
 *  On exit netcdf files are closed.
 */
void
test_ncclose(path)
     char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncclose";
    int cdfid;			/* netcdf id */

    (void) fprintf(stderr, "*** Testing %s ...\t\t", &pname[5]);

    if ((cdfid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return;
    }
    /* opened */
    if (ncredef(cdfid) == -1) {
	error("%s: ncredef failed", pname);
	ncclose(cdfid); return;
    }
    /* in define mode */
    if (ncclose (cdfid) == -1) {
	error("%s: ncclose in define mode failed", pname);
	nerrs++;
    }

    if ((cdfid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return;
    }
    /* in data mode */
    if (ncclose (cdfid) == -1) {
	error("%s: ncclose failed", pname);
	nerrs++;
    }
    if (ncclose (cdfid) != -1) { /* should fail, since cdfid is a bad handle */
	error("%s: ncclose failed to report bad cdf handle ", pname);
	nerrs++;
    }
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");
}


/* 
 * Test ncinquire
 *    try in data mode, check returned values
 *    try in define mode, after adding an unlimited dimension, variable
 *    try with bad handle, check error
 *  On exit netcdf files are closed.
 */
void
test_ncinquire(path)
     char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncinquire";
    int cdfid;			/* netcdf id */
    int ndims;			/* number of dimensions */
    int nvars;			/* number of variables */
    int ngatts;			/* number of global attributes */
    int xdimid;			/* id of unlimited dimension */
    int rec_dim;		/* dimension id */
    static struct cdfdim rec =	/* dimension */
      {"rec", NC_UNLIMITED};
    static struct cdfdim dims[] = { /* dimensions */
	{"i1", 5},{"i2", 3},{"i3", 7}
    };
    int id, nd = LEN_OF(dims);	/* number of dimensions */
    int dimids[LEN_OF(dims)];
    int cc_id;			/* variable id */
    static struct cdfvar cc[] =	{ /* record variables of various sizes */
	{"cc", NC_LONG, 1, ___, 0},
	{"cd", NC_SHORT, 2, ___, 0},
	{"ce", NC_FLOAT, 3, ___, 0}
    };
    int iv;
    int nv = LEN_OF(cc);	/* number of record variables */
    static char units_val[] = "moles";
    static struct cdfatt cc_units = /* attribute */
      {___, "units", NC_CHAR, LEN_OF(units_val), (void *)units_val};

    (void) fprintf(stderr, "*** Testing %s ...\t", &pname[5]);

    if ((cdfid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return;
    }
    /* opened, in data mode */
    if (ncinquire(cdfid, &ndims, &nvars, &ngatts, &xdimid) == -1) {
	error("%s: ncinquire in data mode failed", pname);
	ncclose(cdfid); return;
    }
    /* compare returned with expected values */
    if (ndims != test.ndims) {
	error("%s: ndims returned as %d, expected %d",
	    pname, ndims, test.ndims);
	nerrs++;
    }
    if (nvars != test.nvars) {
	error("%s: nvars returned as %d, expected %d",
	    pname, nvars, test.nvars);
	nerrs++;
    }
    if (ngatts != test.ngatts) {
	error("%s: ngatts returned as %d, expected %d",
	    pname, ngatts, test.ngatts);
	nerrs++;
    }
    if (xdimid != test.xdimid) {
	error("%s: xdimid returned as %d, expected %d",
	    pname, xdimid, test.xdimid);
	nerrs++;
    }

    if (ncredef(cdfid) == -1) {
	error("%s: ncredef failed", pname);
	ncclose(cdfid); return;
    }
    /* add dimensions */
    for (id = 0; id < nd; id++) {
	if ((dimids[id] = ncdimdef(cdfid, dims[id].name, dims[id].size))
	    == -1) {
	    error("%s: ncdimdef failed on normal dimension", pname);
	    ncclose(cdfid); return;
	}
	add_dim(&test, &dims[id]);
    }

    /* add an unlimited dimension */
    if ((rec_dim = ncdimdef(cdfid, rec.name, rec.size)) == -1) {
	error("%s: ncdimdef failed on NC_UNLIMITED dimension", pname);
	ncclose(cdfid); return;
    }
    add_dim(&test, &rec);

    /* add some record variables */
    for (iv = 0; iv < nv; iv++) {
	cc[iv].dims = (int *) emalloc(sizeof(int) * cc[iv].ndims);
	cc[iv].dims[0] = rec_dim; /* first dimension unlimited */
	for (id = 1; id < cc[iv].ndims; id++)
	  cc[iv].dims[id] = dimids[id];
	if ((cc_id = ncvardef(cdfid, cc[iv].name, cc[iv].type,
			       cc[iv].ndims, cc[iv].dims)) == -1) {
	    error("%s: ncvardef failed", pname);
	    ncclose(cdfid); return;
	}
	add_var(&test, &cc[iv]);

	/* add a variable attribute */
	if (ncattput(cdfid, cc_id, cc_units.name, cc_units.type,
		      cc_units.len, (void *) cc_units.val) == -1) {
	    error("%s: ncattput failed", pname);
	    ncclose(cdfid); return;
	}
	add_att(&test, cc_id, &cc_units);
    }
    /* try calling from define mode, compare returned values to expected */
    if (ncinquire(cdfid, &ndims, &nvars, &ngatts, &xdimid) == -1) {
	error("%s: ncinquire in define mode failed", pname);
	ncclose(cdfid); return;
    }
    /* compare returned with expected values */
    if (ndims != test.ndims) {
	error("%s: ndims returned as %d, expected %d",
	    pname, ndims, test.ndims);
	nerrs++;
    }
    if (nvars != test.nvars) {
	error("%s: nvars returned as %d, expected %d",
	    pname, nvars, test.nvars);
	nerrs++;
    }
    if (ngatts != test.ngatts) {
	error("%s: ngatts returned as %d, expected %d",
	    pname, ngatts, test.ngatts);
	nerrs++;
    }
    if (xdimid != test.xdimid) {
	error("%s: xdimid returned as %d, expected %d",
	    pname, xdimid, test.xdimid);
	nerrs++;
    }

    if (ncendef (cdfid) == -1) {
	error("%s: ncendef failed", pname);
	ncclose(cdfid); return;
    }

    if (ncclose (cdfid) == -1) {
	error("%s: ncclose failed", pname);
	return;
    }
    /* should fail, since bad handle */
    if (ncinquire (cdfid, &ndims, &nvars, &ngatts, &xdimid) != -1) {
	error("%s: ncinquire failed to report bad netcdf handle ", pname);
	nerrs++;
    }
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");
}


/*
 * Test ncsync
 *    try in define mode, check error
 *    try writing with one handle, reading with another on same netCDF
 *    try with bad handle, check error
 *  On exit netcdf files are closed.
 */
void
test_ncsync(path)
     char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncsync";
    int cdfid0, cdfid1;		/* netcdf ids */
    int ll_dim;			/* dimension id */
    static struct cdfdim ll =	/* dimension */
      {"ll", 3};
    int dd_id;			/* variable id */
    static struct cdfvar dd =	/* variable */
      {"dd", NC_SHORT, 1, ___, 0};
    static short dd_fill_valv[] = {-999};
    static struct cdfatt dd_fill_val = /* attribute */
      {___, "fill_value", NC_SHORT, LEN_OF(dd_fill_valv), (void *) dd_fill_valv};

    (void) fprintf(stderr, "*** Testing %s ...\t\t", &pname[5]);

    if ((cdfid0 = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen in NC_WRITE mode failed", pname);
	return;
    }

    /* opened */
    if (ncredef(cdfid0) == -1) {
	error("%s: ncredef failed", pname);
	ncclose(cdfid0); return;
    }
    /* in define mode, add a dimension, variable, and attribute */
    if ((ll_dim = ncdimdef(cdfid0, ll.name, ll.size)) == -1) {
	error("%s: ncdimdef failed", pname);
	ncclose(cdfid0);
	return;
    }
    add_dim(&test, &ll);

    dd.dims = (int *) emalloc(sizeof(int) * dd.ndims);
    dd.dims[0] = ll_dim;
    if ((dd_id=ncvardef(cdfid0, dd.name, dd.type, dd.ndims, dd.dims)) == -1) {
	error("%s: ncvardef failed", pname);
	ncclose(cdfid0);
	return;
    }
    add_var(&test, &dd);

    if (ncattput(cdfid0, dd_id, dd_fill_val.name, dd_fill_val.type,
		  dd_fill_val.len, (void *) dd_fill_val.val) == -1) {
	error("%s: ncattput failed", pname);
	ncclose(cdfid0);
	return;
    }
    add_att(&test, dd_id, &dd_fill_val);

    if (ncsync (cdfid0) != -1) {
	error("%s: ncsync in define mode should fail", pname);
	nerrs++;
    }

    if (ncendef (cdfid0) == -1) {
	error("%s: ncendef failed", pname);
	ncclose(cdfid0); return;
    }
    /* in data mode */
    if (ncsync (cdfid0) == -1) {
	error("%s: ncsync in data mode failed", pname);
	nerrs++;
    }

    /* put some data into a variable */
    {
	static long dd_start[] = {0};
	static long dd_edges[] = {2};
	static short dd_vals[] = {1, 2};
	short got_vals[2];

	if (ncvarput(cdfid0,dd_id,dd_start,dd_edges,(void *)dd_vals) == -1) {
	    error("%s: ncvarput failed", pname);
	    ncclose(cdfid0);
	    return;
	}
	add_data(&test,dd_id,dd_start,dd_edges); /* keep test in sync */
	if (ncsync (cdfid0) == -1) {
	    error("%s: ncsync after putting data failed", pname);
	    nerrs++;
	}

	if ((cdfid1 = ncopen(path, NC_NOWRITE)) == -1) {
#ifndef vms
	    error("%s: second ncopen failed", pname);
	    nerrs++;
#else
	    fprintf(stderr,"Doesn't support shared access on vms\n") ;
#endif
	} else {
		if (cdfid0 == cdfid1) {
		    error("%s: second ncopen should return distinct handle",
			  pname);
		    nerrs++;
		}	/* read data just put after a sync, should succeed */
		if (ncvarget(cdfid1,dd_id,dd_start,dd_edges,(void *)got_vals)
		    == -1) {
		    error("%s: ncvarget failed", pname);
		    nerrs++;
		}
		if (dd_vals[0] != got_vals[0] || dd_vals[1] != got_vals[1]) {
		    error("%s: ncvarget succeeded but data values wrong",
			  pname);
		}
   		if (ncclose (cdfid1) == -1) {
		    error("%s: ncclose failed", pname);
		    nerrs++;
		}
	    }
    }
    if (ncclose (cdfid0) == -1) {
	error("%s: ncclose failed", pname);
	nerrs++;
    }
    if (ncsync (cdfid0) != -1) { /* should fail, since cdfid0 is bad handle */
	error("%s: ncsync failed to report bad cdf handle ", pname);
	nerrs++;
    }
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");
}


/*
 * Test ncabort
 *    try in define mode, check that file was deleted
 *    try after writing variable
 *    try with bad handle, check error
 *  On exit netcdf files are closed.
 */
void
test_ncabort(path)
     char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncabort";
    static char fpath[] = "ufo.cdf";
    static short attv[] = {3};
    static struct cdfatt att = /* attribute */
      {___, "temp", NC_SHORT, LEN_OF(attv), (void *) attv};
    int cdfid;			/* netcdf id */

    (void) fprintf(stderr, "*** Testing %s ...\t\t", &pname[5]);

    if ((cdfid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen failed", pname);
	return;
    }
    /* opened */
    if (ncredef(cdfid) == -1) {
	error("%s: ncredef failed", pname);
	ncclose(cdfid); return;
    }
    /* in define mode, add a new global attribute */
    if (ncattput(cdfid, NC_GLOBAL, att.name, att.type, att.len, att.val) == -1) {
	error("%s: ncattput failed", pname);
	ncclose(cdfid); return;
    }

    /* abort in define mode, should restore to state before define mode */
    if (ncabort(cdfid) == -1) {
	error("%s: ncabort in define mode failed", pname);
	ncclose(cdfid); return;
    }
    if ((cdfid = ncopen(path, NC_WRITE)) == -1) {
	error("%s: ncopen after ncabort failed", pname);
	return;
    }
    /* check that new global attribute was not added */
    if (ncattinq(cdfid, NC_GLOBAL, att.name, &att.type, &att.len) != -1) {
	error("%s: ncabort should have restored state before ncredef", pname);
	ncclose(cdfid); return;
    }
    /* in data mode not being created, should just close */
    if (ncabort(cdfid) == -1) {
	error("%s: ncabort in define mode failed", pname);
	return;
    }
    if ((cdfid = nccreate(fpath, NC_CLOBBER)) == -1) {
	error("%s: nccreate failed to NC_CLOBBER", pname);
	return;
    }
    /* in define mode being created, should delete */
    if (ncabort(cdfid) == -1) {
	error("%s: ncabort after nccreate failed", pname);
	return;
    }
    /* check with ncopen that file doesn't exist */
    if (ncopen(fpath, NC_NOWRITE) != -1) {
	error("%s: ncabort deleted file, but ncopen found it", pname);
	return;
    }
    if (ncabort(cdfid) != -1) {	/* should fail, cdfid is bad handle */
	error("%s: ncclose failed to report bad cdf handle ", pname);
	nerrs++;
    }
    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");
}
