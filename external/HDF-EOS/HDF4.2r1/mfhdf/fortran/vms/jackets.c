/*
 *	Copyright 1990, University Corporation for Atmospheric Research
 *      See netcdf/README file for copying and redistribution conditions.
 */
/* $Header: /afs/ncsa/projects/hdf/cvs/hdf4/mfhdf/fortran/vms/jackets.c,v 1.3 1998/06/25 17:06:14 epourmal Exp $ */
/*
 * OVERVIEW
 *
 * This file contains jacket routines written in C for interfacing Fortran
 * netCDF function calls to the actual C binding for the NetCDF.  This code
 * is written explicitly for VMS.  In general, these functions handle
 * character-string parameter conventions, convert between
 * column-major-order arrays and row-major-order arrays, and map between
 * array indices beginning at one and array indices beginning at zero.
 *
 */

/* LINTLIBRARY */
#ifdef HDF
#include        "local_nc.h"
#else
#include	"netcdf.h"
#endif
#include	<ctype.h>
#include        <string.h>
#include	<stdio.h>
#include        <stdlib.h>
#include        <descrip.h>

/*
extern char *
malloc ();
*/

/*
 * global integer used for suppressing error messages and determining
 * the fatality of errors.
 */
extern int ncopts;		/* default is (NC_FATAL | NC_VERBOSE) */

/* global integer that contains a netCDF-specific error code */
extern int ncerr;

/* blank fill C string to make FORTRAN string */
static void
fcdcpy (fstring, fslen, sstring)
    char *fstring;		/* output string to be blank-filled */
    int fslen;			/* length of output string */
    char *sstring;		/* input string, null-terminated */
{
    int i, len = strlen(sstring);

    for (i = 0; i < len; i++)
	*(fstring + i) = *(sstring + i);
    for (i = len; i < fslen; i++)
	*(fstring + i) = ' ';
}


static void
reverse (array, length)
    int array[];		/* array to be reversed */
    int length;			/* length of array */
{
    int temp, i, j;

    for (i = 0, j = length - 1; i < j; i++, j--) {
	temp = array[i];
	array[i] = array[j];
	array[j] = temp;
    }
}


static void
revlongs (array, length)
    long array[];		/* array to be reversed */
    int length;			/* length of array */
{
    int i, j;
    long temp;

    for (i = 0, j = length - 1; i < j; i++, j--) {
	temp = array[i];
	array[i] = array[j];
	array[j] = temp;
    }
}


/* error handling function */
static void
handle_err (pname, rcode)
    char *pname;		/* procedure name */
    int rcode;			/* error return */
{
    extern void NCadvise();
    extern const char *cdf_routine_name; /* routine name in error messages */

    cdf_routine_name = pname;
    (void) NCadvise(rcode, "string won't fit in CHARACTER variable provided");
}

/* copy function used to copy strings with embedded blanks */
static void
fstrncpy (target, source, maxlen)
    char *target;		/* space to be copied into */
    char *source;		/* string to be copied */
    int maxlen;			/* maximum length of *source */
{
    while (maxlen-- && *source != '\0')
	*target++ = *source++;
    *target = '\0';
}

/* copy function used to copy strings terminated with blanks */
static void
nstrncpy (target, source, maxlen)
    char *target;		/* space to be copied into */
    char *source;		/* string to be copied */
    int maxlen;			/* maximum length of *source */
{
    while (maxlen-- && *source != ' ')
	*target++ = *source++;
    *target = '\0';
}


/*
 * Compute product of dimensions.
 */
static long
dimprod (dims, ndims)
     long *dims;			/* list of dimensions */
     int ndims;			/* number of dimensions in list */
{
    long *ip;
    long prod = 1;

    for (ip = dims; ip < &dims[ndims]; ip++)
      prod *= *ip;
    return prod;
}


#ifdef FORTRAN_HAS_NO_BYTE
/*
 * Convert multi-dimensional array of bytes stored in ints to packed array of
 * bytes, in malloc'ed space.  Returns pointer to bytes or NULL if malloc
 * failed.
 */
static char *
itob(ints, dims, ndims)
     int *ints;			/* multi-dimensional array of integers */
     long *dims;			/* list of dimensions */
     int ndims;			/* number of dimensions in list */
{
    long iocount = dimprod (dims, ndims);	/* product of dimensions */
    char *bytes = (char *) malloc (iocount * sizeof (char));
    int *ip;
    char *bp = bytes;

    if (bytes != NULL)
      for (ip = ints; iocount > 0; iocount--)
	*bp++ = (char) *ip++;
    return bytes;
}
#endif /* FORTRAN_HAS_NO_BYTE */

#ifdef FORTRAN_HAS_NO_SHORT
/*
 * Convert multi-dimensional array of shorts stored in ints to packed array of
 * shorts, in malloc'ed space.  Returns pointer to shorts or NULL if malloc
 * failed.
 */
static short *
itos(ints, dims, ndims)
     int *ints;		/* multi-dimensional array of ints */
     long *dims;			/* list of dimensions */
     int ndims;			/* number of dimensions in list */
{
    long iocount = dimprod (dims, ndims);	/* product of dimensions */
    short *shorts = (short *) malloc (iocount * sizeof (short));
    int *ip;
    short *sp = shorts;

    if (shorts != NULL)
      for (ip = ints; iocount > 0; iocount--)
	*sp++ = (short) *ip++;
    return shorts;
}
#endif /* FORTRAN_HAS_NO_SHORT */

/* ------------ VMS FORTRAN jackets for netCDF Functions ------------ */

/* used to set the C global variable ncopts from Fortran */
void ncpopt(val)
    int		*val;	
{
    ncopts = *val;
}


/* used to get the C global variable ncopts from Fortran */
void ncgopt(val)
    int		*val;	
{
    *val = ncopts;
}

/*
 * creates a new netCDF file, returning a netCDF ID.  New netCDF
 * file is placed in define mode.
 */
int
nccre(pathnamed, clobmode, rcode)
    struct dsc$descriptor_s * pathnamed;	
    int		*clobmode;	
    int		*rcode;	
{
    char	*pathname	= pathnamed->dsc$a_pointer;
    int		pathnamelen	= pathnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];
    int cdfid;

    (void) nstrncpy (name, pathname, pathnamelen);
    if ((cdfid = nccreate (name, *clobmode)) != -1) {
	*rcode = 0;
	return (cdfid);
    }
    *rcode = ncerr;
    return (-1);
}


/* opens an existing netCDF file for access */
int
ncopn(pathnamed, rwmode, rcode)
    struct dsc$descriptor_s * pathnamed;	
    int		*rwmode;	
    int		*rcode;	
{
    char	*pathname	= pathnamed->dsc$a_pointer;
    int		pathnamelen	= pathnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];
    int cdfid;

    (void) nstrncpy (name, pathname, pathnamelen);
    if ((cdfid = ncopen (name, *rwmode)) != -1) {
	*rcode = 0;
	return (cdfid);
    }
    *rcode = ncerr;
    return (-1);
}


/* adds a new dimension to an open netCDF file in define mode */
int
ncddef(cdfid, dimnamed, dimlen, rcode)
    int		*cdfid;	
    struct dsc$descriptor_s * dimnamed;	
    long		*dimlen;	
    int		*rcode;	
{
    char	*dimname	= dimnamed->dsc$a_pointer;
    int		dimnamelen	= dimnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];
    int dimid;

    (void) nstrncpy (name, dimname, dimnamelen);
    if ((dimid = ncdimdef (*cdfid, name, *dimlen)) != -1) {
	*rcode = 0;
	return (dimid + 1);
    }
    *rcode = ncerr;
    return (-1);
}


/*
 * returns the ID of a netCDF dimension, given the name of the
 * dimension
 */
int
ncdid(cdfid, dimnamed, rcode)
    int		*cdfid;	
    struct dsc$descriptor_s * dimnamed;	
    int		*rcode;	
{
    char	*dimname	= dimnamed->dsc$a_pointer;
    int		dimnamelen	= dimnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];
    int dimid;

    (void) nstrncpy (name, dimname, dimnamelen);
    if ((dimid = ncdimid (*cdfid, name)) != -1) {
	*rcode = 0;
	return (dimid + 1);
    }
    *rcode = ncerr;
    return (-1);
}


/* adds a new variable to an open netCDF file in define mode */
int
ncvdef(cdfid, varnamed, datatype, ndims, dimarray, rcode)
    int		*cdfid;	
    struct dsc$descriptor_s * varnamed;	
    int		*datatype;	
    int		*ndims;	
    int		*dimarray;	
    int		*rcode;	
{
    char	*varname	= varnamed->dsc$a_pointer;
    int		varnamelen	= varnamed->dsc$w_length;
    int varid, i, dimid[MAX_VAR_DIMS];
    char name[MAX_NC_NAME + 1];

    (void) nstrncpy (name, varname, varnamelen);
    for (i = 0; i < *ndims; i++)
	dimid[i] = dimarray[i] - 1;
    reverse (dimid, *ndims);
    if ((varid = ncvardef (*cdfid, name, (nc_type) *datatype, *ndims,
			   dimid)) != -1) {
	*rcode = 0;
	return (varid + 1);
    }
    *rcode = ncerr;
    return (-1);
}


/* returns the ID of a netCDF variable given its name */
int
ncvid(cdfid, varnamed, rcode)
    int		*cdfid;	
    struct dsc$descriptor_s * varnamed;	
    int		*rcode;	
{
    char	*varname	= varnamed->dsc$a_pointer;
    int		varnamelen	= varnamed->dsc$w_length;
    int varid;
    char name[MAX_NC_NAME + 1];

    (void) nstrncpy (name, varname, varnamelen);
    if ((varid = ncvarid (*cdfid, name)) != -1) {
	*rcode = 0;
	return (varid + 1);
    }
    *rcode = ncerr;
    return (-1);
}


/* returns number of bytes per netCDF data type */
int
nctlen(datatype, rcode)
    int		*datatype;	
    int		*rcode;	
{
    int itype;

    if ((itype = nctypelen ((nc_type) *datatype)) != -1) {
	*rcode = 0;
	return (itype);
    }
    *rcode = ncerr;
    return (-1);
}

/* closes an open netCDF file */
void
ncclos(cdfid, rcode)
    int		*cdfid;	
    int		*rcode;	
{
    *rcode = 0;
    if (ncclose (*cdfid) == -1)
	*rcode = ncerr;
}

/* puts an open netCDF into define mode */
void
ncredf(cdfid, rcode)
    int		*cdfid;	
    int		*rcode;	
{
    *rcode = 0;
    if (ncredef (*cdfid) == -1)
	*rcode = ncerr;
}

/* takes an open netCDF out of define mode */
void
ncendf(cdfid, rcode)
    int		*cdfid;	
    int		*rcode;	
{
    *rcode = 0;
    if (ncendef (*cdfid) == -1)
	*rcode = ncerr;
}

/* returns information about an open netCDF file given its netCDF ID */
void
ncinq(cdfid, ndims, nvars, natts, recdim, rcode)
    int		*cdfid;	
    int		*ndims;	
    int		*nvars;	
    int		*natts;	
    int		*recdim;	
    int		*rcode;	
{
    *rcode = 0;
    if (ncinquire (*cdfid, ndims, nvars, natts, recdim) == -1) {
	*rcode = ncerr;
	return;
    }
    if (*recdim != -1)
	(*recdim)++;
}

/*
 * makes sure that the disk copy of a netCDF file open for writing
 * is current
 */
void
ncsnc(cdfid, rcode)
    int		*cdfid;	
    int		*rcode;	
{
    *rcode = 0;
    if (ncsync (*cdfid) == -1)
	*rcode = ncerr;
}

/*
 * restores the netCDF to a known consistent state in case anything
 * goes wrong during the definition of new dimensions, variables
 * or attributes
 */
void
ncabor(cdfid, rcode)
    int		*cdfid;	
    int		*rcode;	
{
    *rcode = 0;
    if (ncabort (*cdfid) == -1)
	*rcode = ncerr;
}

/* returns the name and size of a dimension, given its ID */
void
ncdinq(cdfid, dimid, dimnamed, size, rcode)
    int		*cdfid;	
    int		*dimid;	
    struct dsc$descriptor_s * dimnamed;	
    long		*size;	
    int		*rcode;	
{
    char	*dimname	= dimnamed->dsc$a_pointer;
    int		dimnamelen	= dimnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];

    *rcode = 0;
    if (ncdiminq (*cdfid, *dimid - 1, name, size) == -1) {
	*rcode = ncerr;
	return;
    }
    if (strlen (name) > dimnamelen) {
	*rcode = NC_ESTS;
	handle_err ("NCDINQ", *rcode);
	return;
    }
    /* blank fill the input character string */
    fcdcpy (dimname, dimnamelen, name);
}

/* renames an existing dimension in a netCDF open for writing */
void
ncdren(cdfid, dimid, dimnamed, rcode)
    int		*cdfid;	
    int		*dimid;	
    struct dsc$descriptor_s * dimnamed;	
    int		*rcode;	
{
    char	*dimname	= dimnamed->dsc$a_pointer;
    int		dimnamelen	= dimnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];

    (void) nstrncpy (name, dimname, dimnamelen);
    *rcode = 0;
    if (ncdimrename (*cdfid, *dimid - 1, name) == -1)
	*rcode = ncerr;
}

/* returns information about a netCDF variable, given its ID */
void
ncvinq(cdfid, varid, varnamed, datatype, ndims, dimarray, natts, rcode)
    int		*cdfid;	
    int		*varid;	
    struct dsc$descriptor_s * varnamed;	
    int		*datatype;	
    int		*ndims;	
    int		*dimarray;	
    int		*natts;	
    int		*rcode;	
{
    char	*varname	= varnamed->dsc$a_pointer;
    int		varnamelen	= varnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];
    int dimid[MAX_VAR_DIMS], i;

    *rcode = 0;
    if (ncvarinq (*cdfid, *varid - 1, name, (nc_type *) datatype, ndims, dimid,
		  natts) == -1) {
	*rcode = ncerr;
	return;
    }
    for (i = 0; i < *ndims; i++)
	dimarray[i] = dimid[i] + 1;
    reverse (dimarray, *ndims);
    if (strlen (name) > varnamelen) {
	*rcode = NC_ESTS;
	handle_err ("NCVINQ", *rcode);
	return;
    }
    fcdcpy (varname, varnamelen, name);
}

/* puts a single numeric data value into a variable of an open netCDF */
void
ncvpt1(cdfid, varid, indices, value, rcode)
    int		*cdfid;	
    int		*varid;	
    long		*indices;	
    void	*value;	
    int		*rcode;	
{
    int datatype, ndims, natts, i;
    long nindices[MAX_VAR_DIMS];
    int dimid[MAX_VAR_DIMS];

    if (ncvarinq (*cdfid, *varid - 1, (char *) 0,
		  (nc_type *) & datatype, &ndims, dimid, &natts) == -1) {
	*rcode = ncerr;
	return;
    }
    for (i = 0; i < ndims; i++)
	nindices[i] = indices[i] - 1;
    revlongs (nindices, ndims);
    *rcode = 0;
#ifdef FORTRAN_HAS_NO_BYTE
    if ((nc_type) datatype == NC_BYTE) {	/* pack ints into bytes */
	char           bytes = *(int *) value;
	if (ncvarput1(*cdfid, *varid - 1, nindices,
		      (ncvoid *) &bytes) == -1) {
	    *rcode = ncerr;
	}
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_BYTE */
#ifdef FORTRAN_HAS_NO_SHORT
    if ((nc_type) datatype == NC_SHORT) {	/* pack ints into shorts */
	short          shorts = *(int *)value;
	if (ncvarput1(*cdfid, *varid - 1, nindices, (ncvoid *) &shorts) == -1) {
	    *rcode = ncerr;
	}
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_SHORT */
    if (ncvarput1 (*cdfid, *varid - 1, nindices, value) == -1) {
	*rcode = ncerr;
    }
}

/* puts a single character into an open netCDF file */
void
ncvp1c(cdfid, varid, indices, chvald, rcode)
    int		*cdfid;	
    int		*varid;	
    long		*indices;	
    struct dsc$descriptor_s * chvald;	
    int		*rcode;	
{
    char	*chval	= chvald->dsc$a_pointer;
    int		chvallen	= chvald->dsc$w_length;
    int datatype, ndims, natts, i;
    long nindices[MAX_VAR_DIMS];
    int dimid[MAX_VAR_DIMS];

    if (ncvarinq (*cdfid, *varid - 1, (char *) 0,
		  (nc_type *) & datatype, &ndims, dimid, &natts) == -1) {
	*rcode = ncerr;
	return;
    }
    for (i = 0; i < ndims; i++)
	nindices[i] = indices[i] - 1;
    revlongs (nindices, ndims);
    *rcode = 0;
    if (ncvarput1 (*cdfid, *varid - 1, nindices, (ncvoid *) chval) == -1) {
	*rcode = ncerr;
    }
}

/*
 * writes a hypercube of numeric values into a netCDF variable of an open
 * netCDF file
 */
void
ncvpt(cdfid, varid, start, count, value, rcode)
    int		*cdfid;	
    int		*varid;	
    long		*start;	
    long		*count;	
    void	*value;	
    int		*rcode;	
{
    long ncount[MAX_VAR_DIMS], nstart[MAX_VAR_DIMS], i;
    int ndims, datatype, dimarray[MAX_VAR_DIMS], natts;

    if (ncvarinq (*cdfid, *varid - 1, (char *) 0, (nc_type *) & datatype,
		  &ndims, dimarray, &natts) == -1) {
	*rcode = ncerr;
	return;
    }
    for (i = 0; i < ndims; i++) {
	ncount[i] = count[i];
	nstart[i] = start[i] - 1;
    }
    revlongs (ncount, ndims);
    revlongs (nstart, ndims);

    *rcode = 0;
#ifdef FORTRAN_HAS_NO_BYTE
    if ((nc_type) datatype == NC_BYTE) {	/* pack ints into bytes */
	char *bytes = itob (value, count, ndims);
	if (bytes == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	    }
	if (ncvarput (*cdfid, *varid - 1, nstart, ncount,
	              (ncvoid *) bytes) == -1) {
	    *rcode = ncerr;
	}
	free (bytes);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_BYTE */
#ifdef FORTRAN_HAS_NO_SHORT
    if ((nc_type) datatype == NC_SHORT) { /* pack ints into shorts */
	short *shorts = itos (value, count, ndims);
	if (shorts == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	    }
	if (ncvarput (*cdfid, *varid - 1, nstart, ncount, (ncvoid *) shorts) == -1) {
	    *rcode = ncerr;
	}
	free (shorts);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_SHORT */
    if (ncvarput (*cdfid, *varid - 1, nstart, ncount, value) == -1) {
	*rcode = ncerr;
    }
}

/* writes a hypercube of character values into an open netCDF file */
void
ncvptc(cdfid, varid, start, count, stringd, lenstr, rcode)
    int		*cdfid;	
    int		*varid;	
    long		*start;	
    long		*count;	
    struct dsc$descriptor_s * stringd;	
    int		*lenstr;	
    int		*rcode;	
{
    char	*string	= stringd->dsc$a_pointer;
    int		stringlen	= stringd->dsc$w_length;
    long ncount[MAX_VAR_DIMS], nstart[MAX_VAR_DIMS], i;
    int ndims, datatype, dimarray[MAX_VAR_DIMS], natts;

    if (ncvarinq (*cdfid, *varid - 1, (char *) 0,
		  (nc_type *) & datatype, &ndims, dimarray, &natts) == -1) {
	*rcode = ncerr;
	return;
    }
    if (dimprod(count,ndims) > *lenstr) {
	*rcode = NC_ESTS;
	handle_err ("NCVPTC", *rcode);
	return;
    }
    for (i = 0; i < ndims; i++) {
	ncount[i] = count[i];
	nstart[i] = start[i] - 1;
    }
    revlongs (ncount, ndims);
    revlongs (nstart, ndims);
    *rcode = 0;
    if (ncvarput (*cdfid, *varid - 1, nstart, ncount, (ncvoid *) string) == -1) {
	*rcode = ncerr;
    }
}

/* gets a single numeric value from a variable of an open netCDF file */
void
ncvgt1(cdfid, varid, indices, value, rcode)
    int		*cdfid;	
    int		*varid;	
    long		*indices;	
    void	*value;	
    int		*rcode;	
{
    long nindices[MAX_VAR_DIMS], i;
    int datatype, ndims, dimarray[MAX_VAR_DIMS], natts;

    if (ncvarinq (*cdfid, *varid - 1, (char *) 0, (nc_type *) & datatype,
		  &ndims, dimarray, &natts) == -1) {
	*rcode = ncerr;
	return;
    }
    for (i = 0; i < ndims; i++) {
	nindices[i] = indices[i] - 1;
    }
    revlongs (nindices, ndims);
    *rcode = 0;
#ifdef FORTRAN_HAS_NO_BYTE
    if ((nc_type) datatype == NC_BYTE) {
	char           bytes;
	int            *ip = (int *) value;
	char           *bp = &bytes;

	if (ncvarget1(*cdfid, *varid - 1, nindices, (ncvoid *) &bytes) == -1) {
	    *rcode = ncerr;
	    return;
	}
	*ip = *bp;
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_BYTE */
#ifdef FORTRAN_HAS_NO_SHORT
    if ((nc_type) datatype == NC_SHORT) {
	short          shorts;
	int            *ip = (int *) value;
	short          *sp = &shorts;

	if (ncvarget1(*cdfid, *varid - 1, nindices, (ncvoid *) &shorts) == -1) {
	    *rcode = ncerr;
	    return;
	}
	*ip = *sp;
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_SHORT */
    if (ncvarget1 (*cdfid, *varid - 1, nindices, value) == -1) {
	*rcode = ncerr;
    }
}

/*
 * gets a single character data value from a variable of an open
 * netCDF file
 */
void
ncvg1c(cdfid, varid, indices, chvald, rcode)
    int		*cdfid;	
    int		*varid;	
    long		*indices;	
    struct dsc$descriptor_s * chvald;	
    int		*rcode;	
{
    char	*chval	= chvald->dsc$a_pointer;
    int		chvallen	= chvald->dsc$w_length;
    long nindices[MAX_VAR_DIMS];
    int i, datatype, ndims, dimarray[MAX_VAR_DIMS], natts;

    if (ncvarinq (*cdfid, *varid - 1, (char *) 0,
		  (nc_type *) & datatype, &ndims, dimarray, &natts) == -1) {
	*rcode = ncerr;
	return;
    }

    for (i = 0; i < ndims; i++) {
	nindices[i] = indices[i] - 1;
    }
    revlongs (nindices, ndims);
    *rcode = 0;
    if (ncvarget1 (*cdfid, *varid - 1, nindices, (ncvoid *) chval) == -1) {
	*rcode = ncerr;
    }
}

/*
 * reads a hypercube of numeric values from a netCDF variable of an open
 * netCDF file
 */
void
ncvgt(cdfid, varid, start, count, value, rcode)
    int		*cdfid;	
    int		*varid;	
    long		*start;	
    long		*count;	
    void	*value;	
    int		*rcode;	
{
    long ncount[MAX_VAR_DIMS], nstart[MAX_VAR_DIMS];
    int i, ndims, datatype, dimarray[MAX_VAR_DIMS], natts;

    if (ncvarinq (*cdfid, *varid - 1, (char *) 0, (nc_type *) & datatype,
		  &ndims, dimarray, &natts) == -1) {
	*rcode = ncerr;
	return;
    }
    for (i = 0; i < ndims; i++) {
	ncount[i] = count[i];
	nstart[i] = start[i] - 1;
    }
    revlongs (ncount, ndims);
    revlongs (nstart, ndims);

    *rcode = 0;
#ifdef FORTRAN_HAS_NO_BYTE
    if ((nc_type) datatype == NC_BYTE) {
	long iocount = dimprod (count, ndims);	/* product of dimensions */
	char *bytes = (char *) malloc (iocount * sizeof (char));
	int *ip;
	char *bp = bytes;

	if (bytes == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncvarget (*cdfid, *varid - 1, nstart, ncount, (ncvoid *) bytes) == -1) {
	    *rcode = ncerr;
	    free (bytes);
	    return;
	}
	for (ip = (int *) value; iocount > 0; iocount--)
	  *ip++ = *bp++;
	free (bytes);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_BYTE */
#ifdef FORTRAN_HAS_NO_SHORT
    if ((nc_type) datatype == NC_SHORT) {
	long iocount = dimprod (count, ndims);	/* product of dimensions */
	short *shorts = (short *) malloc (iocount * sizeof (short));
	int *ip;
	short *sp = shorts;

	if (shorts == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncvarget (*cdfid, *varid - 1, nstart, ncount, (ncvoid *) shorts) == -1) {
	    *rcode = ncerr;
	    free (shorts);
	    return;
	}
	for (ip = (int *) value; iocount > 0; iocount--)
	    *ip++ = *sp++;
	free (shorts);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_SHORT */
    if (ncvarget (*cdfid, *varid - 1, nstart, ncount, value) == -1) {
	*rcode = ncerr;
    }
}

/* reads a hypercube of character values from a netCDF variable */
void
ncvgtc(cdfid, varid, start, count, stringd, lenstr, rcode)
    int		*cdfid;	
    int		*varid;	
    long		*start;	
    long		*count;	
    struct dsc$descriptor_s * stringd;	
    int		*lenstr;	
    int		*rcode;	
{
    char	*string	= stringd->dsc$a_pointer;
    int		stringlen	= stringd->dsc$w_length;
    long ncount[MAX_VAR_DIMS], nstart[MAX_VAR_DIMS];
    int i, ndims, datatype, dimarray[MAX_VAR_DIMS], natts;
    int prod = 1;

    if (ncvarinq (*cdfid, *varid - 1, (char *) 0,
		  (nc_type *) & datatype, &ndims, dimarray, &natts) == -1) {
	*rcode = ncerr;
	return;
    }
    for (i = 0; i < ndims; i++) {
	ncount[i] = count[i];
	nstart[i] = start[i] - 1;
	prod *= count[i];
    }
    if (prod > *lenstr) {
	*rcode = NC_ESTS;
	handle_err ("NCVGTC", *rcode);
	return;
    }
    revlongs (ncount, ndims);
    revlongs (nstart, ndims);
    *rcode = 0;
    if (ncvarget (*cdfid, *varid - 1, nstart, ncount, (ncvoid *) string) == -1) {
	*rcode = ncerr;
	return;
    }

    for (i = prod; i < *lenstr; i++)
	string[i] = ' ';
}

/* changes the name of a netCDF variable in an open netCDF file */
void
ncvren(cdfid, varid, varnamed, rcode)
    int		*cdfid;	
    int		*varid;	
    struct dsc$descriptor_s * varnamed;	
    int		*rcode;	
{
    char	*varname	= varnamed->dsc$a_pointer;
    int		varnamelen	= varnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];

    (void) nstrncpy (name, varname, varnamelen);
    *rcode = 0;
    if (ncvarrename (*cdfid, *varid - 1, name) == -1) {
	*rcode = ncerr;
    }
}

/*
 * adds or changes a numeric variable or global attribute of an open
 * netCDF file
 */
void
ncapt(cdfid, varid, attnamed, datatype, attlen, value, rcode)
    int		*cdfid;	
    int		*varid;	
    struct dsc$descriptor_s * attnamed;	
    int		*datatype;	
    int		*attlen;	
    void	*value;	
    int		*rcode;	
{
    char	*attname	= attnamed->dsc$a_pointer;
    int		attnamelen	= attnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];

    (void) nstrncpy (name, attname, attnamelen);

    *rcode = 0;
#ifdef FORTRAN_HAS_NO_BYTE
    if ((nc_type) *datatype == NC_BYTE) {	/* pack ints into bytes */
	char *bytes = itob (value, attlen, 1);

	if (bytes == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncattput (*cdfid, *varid - 1, name, (nc_type) *datatype, *attlen,
		      (ncvoid *) bytes) == -1) {
	    *rcode = ncerr;
	}
	free (bytes);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_BYTE */
#ifdef FORTRAN_HAS_NO_SHORT
    if ((nc_type) *datatype == NC_SHORT) {	/* pack ints into shorts */
	short *shorts = itos (value, attlen, 1);

	if (shorts == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncattput (*cdfid, *varid - 1, name, (nc_type) *datatype, *attlen,
		      (ncvoid *) shorts) == -1) {
	    *rcode = ncerr;
	}
	free (shorts);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_SHORT */
    if (ncattput (*cdfid, *varid - 1, name, (nc_type) *datatype, *attlen,
		  value) == -1) {
	*rcode = ncerr;
    }
}

/*
 * adds or changes a character variable or global attribute
 * of an open netCDF file
 */
void
ncaptc(cdfid, varid, attnamed, datatype, lenstr, stringd, rcode)
    int		*cdfid;	
    int		*varid;	
    struct dsc$descriptor_s * attnamed;	
    int		*datatype;	
    int		*lenstr;	
    struct dsc$descriptor_s * stringd;	
    int		*rcode;	
{
    char	*attname	= attnamed->dsc$a_pointer;
    int		attnamelen	= attnamed->dsc$w_length;
    char	*string	= stringd->dsc$a_pointer;
    int		stringlen	= stringd->dsc$w_length;
    char name[MAX_NC_NAME + 1];
    char *value;

    (void) nstrncpy (name, attname, attnamelen);
    if (((value = malloc ((unsigned) *lenstr + 1)) == NULL) || (*lenstr == 0)) {
	*rcode = NC_ESTS;
	handle_err ("NCAPTC", *rcode);
	return;
    }
    (void) fstrncpy (value, string, *lenstr);
    *rcode = 0;
    if (ncattput (*cdfid, *varid - 1, name, (nc_type) *datatype, *lenstr,
		  (ncvoid *) value) == -1) {
	*rcode = ncerr;
    }
    free (value);
}

/*
 * returns information about a netCDF attribute given its variable
 * ID and name
 */
void
ncainq(cdfid, varid, attnamed, datatype, attlen, rcode)
    int		*cdfid;	
    int		*varid;	
    struct dsc$descriptor_s * attnamed;	
    int		*datatype;	
    int		*attlen;	
    int		*rcode;	
{
    char	*attname	= attnamed->dsc$a_pointer;
    int		attnamelen	= attnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];

    (void) nstrncpy (name, attname, attnamelen);
    *rcode = 0;
    if (ncattinq (*cdfid, *varid - 1, name, (nc_type *) datatype, attlen) == -1) {
	*rcode = ncerr;
    }
}

/*
 * gets the value of a netCDF attribute given its variable ID
 * and name
 */
void
ncagt(cdfid, varid, attnamed, value, rcode)
    int		*cdfid;	
    int		*varid;	
    struct dsc$descriptor_s * attnamed;	
    void	*value;	
    int		*rcode;	
{
    char	*attname	= attnamed->dsc$a_pointer;
    int		attnamelen	= attnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];
#if defined(FORTRAN_HAS_NO_BYTE) || defined(FORTRAN_HAS_NO_SHORT)
    int datatype;
    int attlen;
#endif

    (void) nstrncpy (name, attname, attnamelen);
    *rcode = 0;
#if defined(FORTRAN_HAS_NO_BYTE) || defined(FORTRAN_HAS_NO_SHORT)
    if (ncattinq (*cdfid, *varid - 1, name, (nc_type *) &datatype, &attlen) == -1) {
	*rcode = ncerr;
	return;
    }
#endif
#ifdef FORTRAN_HAS_NO_BYTE
    if ((nc_type) datatype == NC_BYTE) {
	char *bytes = (char *) malloc (attlen);
	int *ip;
	char *bp = bytes;

	if (bytes == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncattget (*cdfid, *varid - 1, name, (ncvoid *) bytes) == -1) {
	    *rcode = ncerr;
	    free (bytes);
	    return;
	}
	for (ip = (int *) value; attlen > 0; attlen--)
	    *ip++ = *bp++;
	free (bytes);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_BYTE */
#ifdef FORTRAN_HAS_NO_SHORT
    if ((nc_type) datatype == NC_SHORT) {
	short *shorts = (short *) malloc (attlen * sizeof (short));
	int *ip;
	short *sp = shorts;

	if (shorts == NULL) {
	    *rcode = NC_SYSERR;
	    return;
	}
	if (ncattget (*cdfid, *varid - 1, name, (ncvoid *) shorts) == -1) {
	    *rcode = ncerr;
	    free (shorts);
	    return;
	}
	for (ip = (int *) value; attlen > 0; attlen--)
	    *ip++ = *sp++;
	free (shorts);
	return;
    }				/* else */
#endif				/* FORTRAN_HAS_NO_SHORT */

    if (ncattget (*cdfid, *varid - 1, name, value) == -1) {
	*rcode = ncerr;
    }
}

/*
 * gets the value of a netCDF character attribute given its variable
 * ID and name
 */
void
ncagtc(cdfid, varid, attnamed, stringd, lenstr, rcode)
    int		*cdfid;	
    int		*varid;	
    struct dsc$descriptor_s * attnamed;	
    struct dsc$descriptor_s * stringd;	
    int		*lenstr;	
    int		*rcode;	
{
    char	*attname	= attnamed->dsc$a_pointer;
    int		attnamelen	= attnamed->dsc$w_length;
    char	*string	= stringd->dsc$a_pointer;
    int		stringlen	= stringd->dsc$w_length;
    char name[MAX_NC_NAME + 1];
    int datatype;
    int attlen;
    int i;

    (void) nstrncpy (name, attname, attnamelen);
    *rcode = 0;
    if (ncattinq (*cdfid, *varid - 1, name, (nc_type *) &datatype, &attlen) == -1) {
	*rcode = ncerr;
	return;
    }
    if (attlen > *lenstr) {
	*rcode = NC_ESTS;
	handle_err ("NCAGTC", *rcode);
	return;
    }
    if (ncattget (*cdfid, *varid - 1, name, (ncvoid *) string) == -1) {
	*rcode = ncerr;
	return;
    }

    for (i = attlen; i < *lenstr; i++)
	string[i] = ' ';
}

/* copies an attribute from one open netCDF file to another */
void
ncacpy(incdfid, invarid, attnamed, outcdfid, outvarid, rcode)
    int		*incdfid;	
    int		*invarid;	
    struct dsc$descriptor_s * attnamed;	
    int		*outcdfid;	
    int		*outvarid;	
    int		*rcode;	
{
    char	*attname	= attnamed->dsc$a_pointer;
    int		attnamelen	= attnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];

    (void) nstrncpy (name, attname, attnamelen);
    *rcode = 0;
    if (ncattcopy (*incdfid, *invarid - 1, name,
		   *outcdfid, *outvarid - 1) == -1) {
	*rcode = ncerr;
    }
}

/*
 * gets the name of an attribute given its variable ID and number
 * as an attribute of that variable
 */
void
ncanam(cdfid, varid, attnum, attnamed, rcode)
    int		*cdfid;	
    int		*varid;	
    int		*attnum;	
    struct dsc$descriptor_s * attnamed;	
    int		*rcode;	
{
    char	*attname	= attnamed->dsc$a_pointer;
    int		attnamelen	= attnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];

    *rcode = 0;
    if (ncattname (*cdfid, *varid - 1, *attnum - 1, name) == -1) {
	*rcode = ncerr;
	return;
    }
    if (strlen (name) > attnamelen) {
	*rcode = NC_ESTS;
	handle_err ("NCANAM", *rcode);
	return;
    }
    fcdcpy (attname, attnamelen, name);
}


/* renames an attribute in an open netCDF file */
void
ncaren(cdfid, varid, attnamed, newnamed, rcode)
    int		*cdfid;	
    int		*varid;	
    struct dsc$descriptor_s * attnamed;	
    struct dsc$descriptor_s * newnamed;	
    int		*rcode;	
{
    char	*attname	= attnamed->dsc$a_pointer;
    int		attnamelen	= attnamed->dsc$w_length;
    char	*newname	= newnamed->dsc$a_pointer;
    int		newnamelen	= newnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1], nname[MAX_NC_NAME + 1];

    (void) nstrncpy (name, attname, attnamelen);
    (void) nstrncpy (nname, newname, newnamelen);
    *rcode = 0;
    if (ncattrename (*cdfid, *varid - 1, name, nname) == -1) {
	*rcode = ncerr;
    }
}

/*
 * deletes an attribute from an open netCDF file given the attribute
 * name
 */
void
ncadel(cdfid, varid, attnamed, rcode)
    int		*cdfid;	
    int		*varid;	
    struct dsc$descriptor_s * attnamed;	
    int		*rcode;	
{
    char	*attname	= attnamed->dsc$a_pointer;
    int		attnamelen	= attnamed->dsc$w_length;
    char name[MAX_NC_NAME + 1];

    (void) nstrncpy (name, attname, attnamelen);
    *rcode = 0;
    if (ncattdel (*cdfid, *varid - 1, name) == -1) {
	*rcode = ncerr;
    }
}


/*
 * sets the fill mode of a netCDF file open for writing
 */
int
ncsfil(cdfid, fillmode, rcode)
    int		*cdfid;	
    int		*fillmode;	
    int		*rcode;	
{
    int retval;

    if ((retval = ncsetfill (*cdfid, *fillmode)) != -1) {
	*rcode = 0;
	return retval;
    }
    *rcode = ncerr;
    return (-1);
}

