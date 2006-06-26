/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: ncgen.h,v 1.4 1996/06/28 23:33:28 acheng Exp $
 *********************************************************************/

#include "netcdf.h"
#include "generic.h"		/* defines union generic */
#ifdef VMS
#include <stdlib.h>
#endif
#define MAX_NC_ATTSIZE    2000	/* max size in bytes of an attribute */
#define MAX_NC_ATTS        500	/* max number of attributes in a netCDF */
#define MAXTRST		   500	/* maximum size of a single string value */
#define FORT_MAX_LINES	20	/* max lines in FORTRAN statement */
#define	FORT_MAX_STMNT	66*FORT_MAX_LINES /* max chars in FORTRAN statement */
#define C_MAX_STMNT	FORT_MAX_STMNT /* until we fix to break up C lines */
#define STREQ(a, b)     (*(a) == *(b) && strcmp((a), (b)) == 0)

extern struct dims {			/* dimensions */
    long size;
    char *name;
} dims[MAX_NC_DIMS];		/* table of netcdf dimensions */

extern int ncid;		/* handle for netCDF */
extern int ndims;		/* number of dimensions declared for netcdf */
extern int nvars;		/* number of variables declared for netcdf */
extern int natts;		/* number of attributes */
extern int nvdims;		/* number of dimensions for variables */
extern int dimnum;		/* dimension number index for variables */
extern int varnum;		/* variable number index for attributes */
extern int valnum;		/* value number index for attributes */
extern int rec_dim;		/* number of the unlimited dimension, if any */
extern long var_len;		/* variable length (product of dimensions) */
extern int var_size;		/* size of each element of variable */
extern long netcdf_record_number; /* current record number for variables */

extern struct vars {		/* variables */
    char *name;
    nc_type type;
    int ndims;
    int dims[MAX_VAR_DIMS];	/* should be a malloc'ed list, not an array */
    union generic fill_value;	/* set to value of _FillValue attribute */
    int has_data;		/* 1 if data specified, 0 otherwise */
} vars[MAX_NC_VARS];		/* should be a malloc'ed list, not an array */

extern struct atts {
    int var;			/* number of variable for this attribute */
    char *name;
    nc_type type;
    int len;
    void *val;
} atts[MAX_NC_ATTS];		/* should be a malloc'ed list, not an array */
