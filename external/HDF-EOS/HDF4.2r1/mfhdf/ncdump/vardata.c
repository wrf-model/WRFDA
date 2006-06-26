/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: vardata.c,v 1.9 2000/05/08 20:40:18 wendling Exp $
 *********************************************************************/

#include <stdio.h>
#include <ctype.h>
#include <stdlib.h>
#include <string.h>
#include <netcdf.h>
#include "ncdump.h"
#include "dumplib.h"
#include "vardata.h"

/*
 * Function from ncdump.c. "Fixes" variable names to remove spaces and other
 * "illegal" characters.
 */
extern char *fixstr(char *str);

static void annotate
    PROTO((struct ncvar *vp,struct fspec *fsp,long cor[], long iel));

#define	STREQ(a, b)	(*(a) == *(b) && strcmp((a), (b)) == 0)

/*
 * Print a row of variable values.  Makes sure output lines aren't too long
 * by judiciously inserting newlines. 
 */
static void
pr_vals(vp, len, fmt, more, lastrow, vals)
     struct ncvar *vp;		/* variable */
     long len;			/* number of values to print */
     char *fmt;			/*
				 * printf format used for each value.  If
				 * nc_type is NC_CHAR and this is NULL,
				 * character arrays will be printed as strings
				 * enclosed in quotes.
				 */
     bool more;			/*
				 * true if more data will follow, so add
				 * trailing comma
				 */
     bool lastrow;		/*
				 * true if this is the last row for this
				 * variable, so terminate with ";" instead of
				 * ","
				 */
     void *vals;		/* pointer to block of values */
{
    long iel;
    union {
	char *cp;
	short *sp;
	nclong *lp;
	float *fp;
	double *dp;
    } gp;
    char *sp;
    unsigned char uc;
    float fill_float;
    double fill_double;
    char sout[100];		/* temporary string for each encoded output */

    fill_float = FILL_FLOAT;	/* static initialization hits ultrix cc bug */
    fill_double = FILL_DOUBLE;
    switch (vp->type) {
      case NC_BYTE:
	gp.cp = (char *) vals;
	for (iel = 0; iel < len-1; iel++) {
	    (void) sprintf(sout, fmt, *gp.cp++);
	    (void) strcat(sout, ", ");
	    lput(sout);
	}
	(void) sprintf(sout, fmt, *gp.cp++);
	lput(sout);
	break;
      case NC_CHAR:
	gp.cp = (char *) vals;
	if (fmt == 0 || STREQ(fmt,"%s") || STREQ(fmt,"")) { /* as string */
	    Printf("\"");
	    /* adjust len so trailing nulls don't get printed */
	    sp = gp.cp + len;
	    while (len > 0 && *--sp == '\0')
	      len--;
	    for (iel = 0; iel < len; iel++)
	      switch (uc = *gp.cp++ & 0377) {
		case '\b':
		  Printf("\\b");
		  break;
		case '\f':
		  Printf("\\f");
		  break;
		case '\n':	/* generate linebreaks after new-lines */
		  Printf("\\n\",\n    \"");
		  break;
		case '\r':
		  Printf("\\r");
		  break;
		case '\t':
		  Printf("\\t");
		  break;
		case '\v':
		  Printf("\\v");
		  break;
		case '\\':
		  Printf("\\\\");
		  break;
		case '\'':
		  Printf("\\\'");
		  break;
		case '\"':
		  Printf("\\\"");
		  break;
		default:
		  if (isprint(uc))
		    Printf("%c",uc);
		  else
		    Printf("\\%.3o",uc);
		  break;
	      }
	    Printf("\"");
	} else {		/* use format from C_format attribute */
	    for (iel = 0; iel < len-1; iel++) {
		(void) sprintf(sout, fmt, *gp.cp++);
		(void) strcat(sout, ", ");
		lput(sout);
	    }
	    (void) sprintf(sout, fmt, *gp.cp++);
	    lput(sout);
	}
	break;
      case NC_SHORT:
	gp.sp = (short *) vals;
	for (iel = 0; iel < len-1; iel++) {
	    (void) sprintf(sout, fmt, *gp.sp++);
	    (void) strcat(sout, ", ");
	    lput(sout);
	}
	(void) sprintf(sout, fmt, *gp.sp++);
	lput(sout);
	break;
      case NC_LONG:
	gp.lp = (nclong *) vals;
	for (iel = 0; iel < len-1; iel++) {
	    (void) sprintf(sout, fmt, *gp.lp++);
	    (void) strcat(sout, ", ");
	    lput(sout);
	}
	(void) sprintf(sout, fmt, *gp.lp++);
	lput(sout);
	break;
      case NC_FLOAT:
	gp.fp = (float *) vals;
	for (iel = 0; iel < len-1; iel++) {
	    if (*gp.fp >= fill_float)
	      (void) sprintf(sout, "FloatInf, ");
	    else {
		(void) sprintf(sout, fmt, *gp.fp);
		(void) strcat(sout, ", ");
	    }
	    lput(sout);
	    gp.fp++;
	}
	if (*gp.fp >= fill_float)
	  (void) sprintf(sout, "FloatInf");
	else
	  (void) sprintf(sout, fmt, *gp.fp);
	lput(sout);
	gp.fp++;
	break;
      case NC_DOUBLE:
	gp.dp = (double *) vals;
	for (iel = 0; iel < len-1; iel++) {
	    if (*gp.dp >= fill_double)
	      (void) sprintf(sout, "DoubleInf, ");
	    else {
		(void) sprintf(sout, fmt, *gp.dp);
		(void) strcat(sout, ", ");
	    }
	    lput(sout);
	    gp.dp++;
	}
	if (*gp.dp >= fill_double)
	  (void) sprintf(sout, "DoubleInf");
	else
	  (void) sprintf(sout, fmt, *gp.dp);
	lput(sout);
	gp.dp++;
	break;
      default:
	error("pr_vals: bad type");
    }
    if (more) {
	lput(", ");
    } else {
	if(lastrow) {
	    lput(" ;");
	    lput("\n");
	} else {
	    lput(",\n");
	    lput("  ");
	}
    }
}


/*
 * print last delimiter in each line before annotation (, or ;)
 */
static void
lastdelim (more, lastrow)
     bool more;
     bool lastrow;
{
    if (more) {
	Printf(", ");
    } else {
	if(lastrow) {
	    Printf(";");
	} else {
	    Printf(",");
	}
    }
}


/*
 * Annotates a value in data section with var name and indices in comment
 */
static void
annotate(vp, fsp, cor, iel)
     struct ncvar *vp;		/* variable */
     struct fspec* fsp;		/* formatting specs */
     long cor[];		/* corner coordinates */
     long iel;			/* which element in current row */
{
    int vrank = vp->ndims;
    int id;
    
    /* print indices according to data_lang */
    (void) printf("  // %s(", vp->name);
    switch (fsp->data_lang) {
      case LANG_C:
	/* C variable indices */
	for (id = 0; id < vrank-1; id++)
	  Printf("%d,", (int)cor[id]);
	Printf("%d", (int)(cor[id] + iel));
	break;
      case LANG_F:
	/* Fortran variable indices */
	Printf("%d", (int)(cor[vrank-1] + iel + 1));
	for (id = vrank-2; id >=0 ; id--) {
	    Printf(",%d", (int)(1 + cor[id]));
	}
	break;
    }
    Printf(")\n    ");
}


/*
 * Print a number of commented variable values, where the comments for each
 * value identify the variable, and each dimension index.
 */
static void
pr_cvals(vp, len, fmt, more, lastrow, vals, fsp, cor)
     struct ncvar *vp;		/* variable */
     long len;			/* number of values to print */
     char *fmt;			/*
				 * printf format used for each value.  If
				 * nc_type is NC_CHAR and this is NULL,
				 * character arrays will be printed as strings
				 * enclosed in quotes.
				 */
     bool more;			/*
				 * true if more data for this row will follow,
				 * so add trailing comma
				 */
     bool lastrow;		/*
				 * true if this is the last row for this
				 * variable, so terminate with ";" instead of
				 * ","
				 */
     void *vals;		/* pointer to block of values */
     struct fspec* fsp;		/* formatting specs */
     long cor[];		/* corner coordinates */
{
    long iel;
    union {
	char *cp;
	short *sp;
	nclong *lp;
	float *fp;
	double *dp;
    } gp;
    char *sp;
    unsigned char uc;
    float fill_float;
    double fill_double;

    fill_float = FILL_FLOAT;	/* static initialization hits ultrix cc bug */
    fill_double = FILL_DOUBLE;
    switch (vp->type) {
      case NC_BYTE:
	gp.cp = (char *) vals;
	for (iel = 0; iel < len-1; iel++) {
	    Printf(fmt, *gp.cp++);
	    Printf(", ");
	    annotate (vp, fsp, cor, iel);
	}
	Printf(fmt, *gp.cp++);
	lastdelim (more, lastrow);
	annotate (vp, fsp, cor, iel);
	break;
      case NC_CHAR:
	gp.cp = (char *) vals;
	if (fmt == 0 || STREQ(fmt,"%s") || STREQ(fmt,"")) { /* as string */
	    Printf("\"");
	    /* adjust len so trailing nulls don't get printed */
	    sp = gp.cp + len;
	    while (len > 0 && *--sp == '\0')
	      len--;
	    for (iel = 0; iel < len; iel++)
	      switch (uc = *gp.cp++ & 0377) {
		case '\b':
		  Printf("\\b");
		  break;
		case '\f':
		  Printf("\\f");
		  break;
		case '\n':	/* generate linebreaks after new-lines */
		  Printf("\\n\",\n    \"");
		  break;
		case '\r':
		  Printf("\\r");
		  break;
		case '\t':
		  Printf("\\t");
		  break;
		case '\v':
		  Printf("\\v");
		  break;
		case '\\':
		  Printf("\\\\");
		  break;
		case '\'':
		  Printf("\\\'");
		  break;
		case '\"':
		  Printf("\\\"");
		  break;
		default:
		  if (isprint(uc))
		    Printf("%c",uc);
		  else
		    Printf("\\%.3o",uc);
		  break;
	      }
	    Printf("\"");
	    annotate (vp, fsp, cor, 0);
	} else {		/* use format from C_format attribute */
	    for (iel = 0; iel < len-1; iel++) {
		Printf(fmt, *gp.cp++);
		Printf(", ");
		annotate (vp, fsp, cor, iel);
	    }
	    Printf(fmt, *gp.cp++);
	    lastdelim (more, lastrow);
	    annotate (vp, fsp, cor, iel);
	}
	break;
      case NC_SHORT:
	gp.sp = (short *) vals;
	for (iel = 0; iel < len-1; iel++) {
	    Printf(fmt, *gp.sp++);
	    Printf(", ");
	    annotate (vp, fsp, cor, iel);
	}
	Printf(fmt, *gp.sp++);
	lastdelim (more, lastrow);
	annotate (vp, fsp, cor, iel);
	break;
      case NC_LONG:
	gp.lp = (nclong *) vals;
	for (iel = 0; iel < len-1; iel++) {
	    Printf(fmt, *gp.lp++);
	    Printf(", ");
	    annotate (vp, fsp, cor, iel);
	}
	Printf(fmt, *gp.lp++);
	lastdelim (more, lastrow);
	annotate (vp, fsp, cor, iel);
	break;
      case NC_FLOAT:
	gp.fp = (float *) vals;
	for (iel = 0; iel < len-1; iel++) {
	    if (*gp.fp >= fill_float)
	      Printf("FloatInf");
	    else
	      Printf(fmt, *gp.fp);
	    Printf(",");
	    annotate (vp, fsp, cor, iel);
	    gp.fp++;
	}
	if (*gp.fp >= fill_float)
	  Printf("FloatInf");
	else
	  Printf(fmt, *gp.fp);
	lastdelim (more, lastrow);
	annotate (vp, fsp, cor, iel);
	gp.fp++;
	break;
      case NC_DOUBLE:
	gp.dp = (double *) vals;
	for (iel = 0; iel < len-1; iel++) {
	    if (*gp.dp >= fill_double)
	      Printf("DoubleInf");
	    else {
		Printf(fmt, *gp.dp);
	    }
	    Printf(",");
	    annotate (vp, fsp, cor, iel);
	    gp.dp++;
	}
	if (*gp.dp >= fill_double)
	  Printf("DoubleInf");
	else
	  Printf(fmt, *gp.dp);
	lastdelim (more, lastrow);
	annotate (vp, fsp, cor, iel);
	gp.dp++;
	break;
      default:
	error("pr_vals: bad type");
    }
}


/*
 * Updates a vector of ints, odometer style.  Returns 0 if odometer
 * overflowed, else 1.
 */
static int
upcorner(dims,ndims,odom,add)
     long *dims;		/* The "odometer" limits for each dimension  */
     int ndims;			/* Number of dimensions */
     long* odom;		/* The "odometer" vector to be updated */
     long* add;			/* A vector to "add" to odom on each update */
{
    int id;
    int ret = 1;

    for (id = ndims-1; id > 0; id--) {
	odom[id] += add[id];
	if(odom[id] >= dims[id]) {
	    odom[id-1]++;
	    odom[id] -= dims[id];
	}
    }
    odom[0] += add[0];
    if (odom[0] >= dims[0])
      ret = 0;
    return ret;
}


int
vardata(vp, vdims, ncid, varid, fsp)
     struct ncvar *vp;		/* variable */
     long vdims[];		/* variable dimension sizes */
     int ncid;			/* netcdf id */
     int varid;			/* variable id */
     struct fspec* fsp;		/* formatting specs */
{
    long cor[MAX_VAR_DIMS];	/* corner coordinates */
    long edg[MAX_VAR_DIMS];	/* edges of hypercube */
    long add[MAX_VAR_DIMS];	/* "odometer" increment to next "row"  */
#define VALBUFSIZ 8192
    double vals[VALBUFSIZ/sizeof(double)] ; /* aligned buffer */
    int gulp = VALBUFSIZ/nctypelen(vp->type);

    int id;
    int ir;
    long nels;
    long ncols;
    long nrows;
    int vrank = vp->ndims;
    char *fixed_var;

    /* printf format used to print each value */
    const char *fmt = get_fmt(ncid, varid, vp->type);

    nels = 1;
    for (id = 0; id < vrank; id++) {
	cor[id] = 0;
	edg[id] = 1;
	nels *= vdims[id];	/* total number of values for variable */
    }

    fixed_var = fixstr(vp->name);

    if (vrank <= 1) {
	Printf("\n %s = ", fixed_var);
	set_indent (strlen(fixed_var) + 4);
    } else {
	Printf("\n %s =\n  ", fixed_var);
	set_indent (2);
    }

    if (vrank < 1) {
	ncols = 1;
    } else {
	ncols = vdims[vrank-1];	/* size of "row" along last dimension */
	edg[vrank-1] = vdims[vrank-1];
	for (id = 0; id < vrank; id++)
	  add[id] = 0;
	if (vrank > 1)
	  add[vrank-2] = 1;
    }
    nrows = nels/ncols;		/* number of "rows" */
    
    for (ir = 0; ir < nrows; ir++) {
	/*
	 * rather than just printing a whole row at once (which might exceed
	 * the capacity of MSDOS platforms, for example), we break each row
	 * into smaller chunks, if necessary.
	 */
	long corsav=0;
	long left = ncols;
	bool lastrow;

	if (vrank > 0) {
	    corsav = cor[vrank-1];
	    if (fsp->brief_data_cmnts != false
		&& vrank > 1
		&& left > 0) {	/* print brief comment with indices range */
		Printf("// %s(",fixed_var);
		switch (fsp->data_lang) {
		  case LANG_C:
		    /* print brief comment with C variable indices */
		    for (id = 0; id < vrank-1; id++)
		      Printf("%d,", (int)cor[id]);
		    if (vdims[vrank-1] == 1)
		      Printf("0");
		    else
		      Printf(" 0-%d", (int)vdims[vrank-1]-1);
		    break;
		  case LANG_F:
		    /* print brief comment with Fortran variable indices */
		    if (vdims[vrank-1] == 1)
		      Printf("1");
		    else
		      Printf("1-%d ",(int)vdims[vrank-1]);
		    for (id = vrank-2; id >=0 ; id--) {
			Printf(",%d", (int)(1 + cor[id]));
		    }
		    break;
		}
		Printf(")\n    ");
		set_indent(4);
	    }
	}
	lastrow = (ir == nrows-1) ? true:false;
	while (left > 0) {
	    long toget = left < gulp ? left : gulp;
	    if (vrank > 0)
	      edg[vrank-1] = toget;
	    (void) ncvarget (ncid, varid, cor, edg, (void *) vals);
	    if (fsp->full_data_cmnts)
	      pr_cvals(vp, toget, fmt, left > toget, lastrow, (void *) vals,
		       fsp, cor);
	    else
	      pr_vals(vp, toget, fmt, left > toget, lastrow, (void *) vals);
	    left -= toget;
	    if (vrank > 0)
	      cor[vrank-1] += toget;
	}
	if (vrank > 0)
	  cor[vrank-1] = corsav;
	if (ir < nrows-1)
	  if (!upcorner(vdims,vp->ndims,cor,add))
	    error("vardata: odometer overflowed!");
	set_indent(2);
    }

    free(fixed_var);
    return 0;
}
