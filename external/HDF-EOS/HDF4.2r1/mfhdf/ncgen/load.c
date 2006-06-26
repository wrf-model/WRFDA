/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: load.c,v 1.11 2000/08/29 13:57:00 koziol Exp $
 *********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "ncgen.h"
#include "genlib.h"

extern int netcdf_flag;
extern int c_flag;
extern int fortran_flag;

void
load_netcdf(rec_start)	/* write out record from in-memory structure */
     void *rec_start;
{
    int idim;
    int istat=0;
    long coords[MAX_VAR_DIMS];
    long edges[MAX_VAR_DIMS];
    char *charvalp=NULL;
    short *shortvalp=NULL;
    nclong *longvalp=NULL;
    float *floatvalp=NULL;
    double *doublevalp=NULL;

    /* load values into variable */

    switch (vars[varnum].type) {
      case NC_CHAR:
      case NC_BYTE:
	charvalp = (char *) rec_start;
	break;
      case NC_SHORT:
	shortvalp = (short *) rec_start;
	break;
      case NC_LONG:
	longvalp = (nclong *) rec_start;
	break;
      case NC_FLOAT:
	floatvalp = (float *) rec_start;
	break;
      case NC_DOUBLE:
	doublevalp = (double *) rec_start;
	break;
      default:
        break;
    }
    /* initialize coords to upper left corner (0,0,0,...) */
    if (vars[varnum].dims[0] == rec_dim) {
	coords[0] = netcdf_record_number;
	edges[0] = 1;
    }
    else {
	coords[0] = 0;
	edges[0] = dims[vars[varnum].dims[0]].size;
    }

    for (idim = 1; idim < vars[varnum].ndims; idim++) {
	coords[idim] = 0;
	edges[idim] = dims[vars[varnum].dims[idim]].size;
    }
    
    switch (vars[varnum].type) {
      case NC_CHAR:
      case NC_BYTE:
	istat = ncvarput (ncid,varnum,coords,edges,(void *)charvalp);
	break;
      case NC_SHORT:
	istat = ncvarput (ncid,varnum,coords,edges,(void *)shortvalp);
	break;
      case NC_LONG:
	istat = ncvarput (ncid,varnum,coords,edges,(void *)longvalp);
	break;
      case NC_FLOAT:
	istat = ncvarput (ncid,varnum,coords,edges,(void *)floatvalp);
	break;
      case NC_DOUBLE:
	istat = ncvarput (ncid,varnum,coords,edges,(void *)doublevalp);
	break;
      default:
        break;
    }
    if (istat == -1)
      derror("error putting value for variable %s",vars[varnum].name);
}


#define fpr    (void) fprintf

/*
 * Remove trailing zeros (after decimal point) but not trailing decimal
 * point from ss, a string representation of a floating-point number that
 * might include an exponent part.
 */
    static void
    tztrim(ss)
char *ss;			/* returned string representing dd */
{
    char *cp, *ep;
    
    cp = ss;
    if (*cp == '-')
        cp++;
    while(isdigit((int)*cp) || *cp == '.')
        cp++;
    if (*--cp == '.')
        return;
    ep = cp+1;
    while (*cp == '0')
        cp--;
    cp++;
    if (cp == ep)
        return;
    while (*ep)
        *cp++ = *ep++;
    *cp = '\0';
    return;
}


/* generate C to put netCDF record from in-memory data */
static void
gen_load_c(rec_start)
     void *rec_start;
{
    int idim, ival;
    char *val_string;
    char *charvalp=NULL;
    short *shortvalp=NULL;
    nclong *longvalp=NULL;
    float *floatvalp=NULL;
    double *doublevalp=NULL;
    char stmnt[C_MAX_STMNT];
    int stmnt_len;
    char s2[MAX_NC_NAME + 2];

    /* initialize coords to upper left corner (rec_num,0,0,...) */
    cline("");
    sprintf(stmnt, "   {\t\t\t/* store %s */", vars[varnum].name);
    cline(stmnt);

    if (vars[varnum].ndims > 0) {
	sprintf(stmnt, "    static long %s_start[] = {", vars[varnum].name);
	if (vars[varnum].dims[0] == rec_dim)
	  sprintf(s2, "%ld%s",
		  netcdf_record_number,
		  0 < vars[varnum].ndims-1 ? ", " : "};");
	else
	  sprintf(s2, "%d%s",
		  0,
		  0 < vars[varnum].ndims-1 ? ", " : "};");
	strcat(stmnt, s2);
	for (idim = 1; idim < vars[varnum].ndims; idim++) {
	    sprintf(s2, "%d%s",
		    0,
		    idim < vars[varnum].ndims-1 ? ", " : "};");
	    strcat(stmnt, s2);
	}
	cline(stmnt);
	
	/* initialize edge lengths from upper left corner */
	sprintf(stmnt, "    static long %s_edges[] = {", vars[varnum].name);
	if (vars[varnum].dims[0] == rec_dim)
	  sprintf(s2, "%d%s",
		  1,
		  0 < vars[varnum].ndims-1 ? ", " : "};");
	else
	  sprintf(s2, "%ld%s",
		  dims[vars[varnum].dims[0]].size,
		  0 < vars[varnum].ndims-1 ? ", " : "};");
	strcat(stmnt, s2);
	for (idim = 1; idim < vars[varnum].ndims; idim++) {
	    sprintf(s2, "%ld%s",
		    dims[vars[varnum].dims[idim]].size,
		    idim < vars[varnum].ndims-1 ? ", " : "};");
	    strcat(stmnt, s2);
	}
	cline(stmnt);
	
	/* load variable with data values using static initialization */
	sprintf(stmnt, "    static %s %s[] = {",
		ncctype(vars[varnum].type),
		vars[varnum].name);
	
	stmnt_len = strlen(stmnt);
	switch (vars[varnum].type) {
	  case NC_CHAR:
	    val_string = cstrstr((char *) rec_start, var_len);
	    sprintf(s2, "%s", val_string);
	    strcat(stmnt, s2);
	    free(val_string);
	    break;
	  default:
	    switch (vars[varnum].type) {
	      case NC_BYTE:
		charvalp = (char *) rec_start;
		break;
	      case NC_SHORT:
		shortvalp = (short *) rec_start;
		break;
	      case NC_LONG:
		longvalp = (nclong *) rec_start;
		break;
	      case NC_FLOAT:
		floatvalp = (float *) rec_start;
		break;
	      case NC_DOUBLE:
		doublevalp = (double *) rec_start;
		break;
          default:
            break;
	    }
            for (ival = 0; ival < var_len-1; ival++) {
		switch (vars[varnum].type) {
		  case NC_BYTE:
			sprintf(s2, "%d, ", *charvalp++);
		    break;
		  case NC_SHORT:
			sprintf(s2, "%d, ", *shortvalp++);
		    break;
		  case NC_LONG:
			sprintf(s2, "%d, ", (int)*longvalp++);
		    break;
		  case NC_FLOAT:
			sprintf(s2, "%.8g, ", *floatvalp++);
		    break;
		  case NC_DOUBLE:
                        sprintf(s2, "%#.16g", *doublevalp++);
                        tztrim(s2);
			strcat(s2, ", ");
		    break;
          default:
            break;
		}
		stmnt_len += strlen(s2);
		if (stmnt_len < C_MAX_STMNT)
		  strcat(stmnt, s2);
		else {
		    cline(stmnt);
		    strcpy(stmnt,s2);
		    stmnt_len = strlen(stmnt);
		}
	    }
	    for (;ival < var_len; ival++) {
		switch (vars[varnum].type) {
		  case NC_BYTE:
			sprintf(s2, "%d", *charvalp);
		    break;
		  case NC_SHORT:
			sprintf(s2, "%d", *shortvalp);
		    break;
		  case NC_LONG:
			sprintf(s2, "%d", (int)*longvalp);
		    break;
		  case NC_FLOAT:
			sprintf(s2, "%.8g", *floatvalp);
		    break;
		  case NC_DOUBLE:
 			sprintf(s2, "%#.16g", *doublevalp++);
			tztrim(s2);
                        break;
          default:
            break;
		}
		stmnt_len += strlen(s2);
		if (stmnt_len < C_MAX_STMNT)
		  strcat(stmnt, s2);
		else {
		    cline(stmnt);
		    strcpy(stmnt,s2);
		    stmnt_len = strlen(stmnt);
		}
	    }
	    break;
	}
	strcat(stmnt,"};");
	cline(stmnt);
	sprintf(stmnt,
		"    ncvarput(ncid, %s_id, %s_start, %s_edges, (void *)%s);",
		vars[varnum].name,
		vars[varnum].name,
		vars[varnum].name,
		vars[varnum].name);
	cline(stmnt);
	cline("   }");
    } else {			/* scalar variables */
	/* load variable with data values using static initialization */
	sprintf(stmnt, "    static %s %s = {",
		ncctype(vars[varnum].type),
		vars[varnum].name);
	
	switch (vars[varnum].type) {
	  case NC_CHAR:
	    val_string = cstrstr((char *) rec_start, var_len);
	    sprintf(s2, "%s", val_string);
	    strcat(stmnt, s2);
	    free(val_string);
	    break;
	  case NC_BYTE:
	    charvalp = (char *) rec_start;
	    sprintf(s2, "%d", *charvalp);
	    strcat(stmnt, s2);
	    break;
	  case NC_SHORT:
	    shortvalp = (short *) rec_start;
	    sprintf(s2, "%d", *shortvalp);
	    strcat(stmnt, s2);
	    break;
	  case NC_LONG:
	    longvalp = (nclong *) rec_start;
	    sprintf(s2, "%d", (int)*longvalp);
	    strcat(stmnt, s2);
	    break;
	  case NC_FLOAT:
	    floatvalp = (float *) rec_start;
	    sprintf(s2, "%.8g", *floatvalp);
	    strcat(stmnt, s2);
	    break;
	  case NC_DOUBLE:
	    doublevalp = (double *) rec_start;
 	    sprintf(s2, "%#.16g", *doublevalp++);
 	    tztrim(s2);
	    strcat(stmnt, s2);
	    break;
      default:
        break;
	}
	strcat(stmnt,"};");
	cline(stmnt);
	sprintf(stmnt,
		"    ncvarput1(ncid, %s_id, (long *)0, (void *)&%s);",
		vars[varnum].name,
		vars[varnum].name);
	cline(stmnt);
	cline("   }");
    }
}

/*
 * Add to a partial Fortran statement, checking if it's too long.  If it is too
 * long, output the first part of it as a single statement with continuation
 * characters and start a new (probably invalid) statement with the remainder.
 * This will cause a Fortran compiler error, but at least all the information
 * will be available.
 */
static void
fstrcat(s, t, slenp)
     char *s;			/* source string of stement being built */
     char *t;			/* string to be appended to source */
     long *slenp;		/* pointer to length of source string */
{
    *slenp += strlen(t);
    if (*slenp >= FORT_MAX_STMNT) {
	derror("FORTRAN statement too long: %s",s);
	fline(s);
	strcpy(s, t);
	*slenp = strlen(s);
    } else {
	strcat(s, t);
    }
}


static void
gen_load_fortran(rec_start)  /* make Fortran to put record */
     void *rec_start;
{
    int idim, ival;
    char *val_string;
    char *charvalp;
    short *shortvalp;
    nclong *longvalp;
    float *floatvalp;
    double *doublevalp;
    char stmnt[FORT_MAX_STMNT];
    long stmnt_len;
    char s2[MAX_NC_NAME + 2];

    /* initialize coords to upper left corner (1,1,...,rec_num) */
    sprintf(stmnt, "* store %s", vars[varnum].name);
    fline(stmnt);

    if (vars[varnum].ndims > 0) {
	for (idim = 1; idim < vars[varnum].ndims; idim++) {
	    sprintf(stmnt, "corner(%d) = 1", idim);
	    fline(stmnt);
	}
	if (vars[varnum].dims[0] == rec_dim) {
	    sprintf(stmnt, "corner(%d) = %d", idim, (int)(netcdf_record_number+1));
	    fline(stmnt);
	} else {
	    sprintf(stmnt, "corner(%d) = 1", idim);
	    fline(stmnt);
	}
	for (idim = vars[varnum].ndims-1; idim > 0; idim--) {
	    sprintf(stmnt, "edges(%d) = %d", vars[varnum].ndims - idim,
		    (int)dims[vars[varnum].dims[idim]].size);
	    fline(stmnt);
	}
	if (vars[varnum].dims[0] == rec_dim) {
	    sprintf(stmnt, "edges(%d) = 1", vars[varnum].ndims - idim);
	    fline(stmnt);
	} else {
	    sprintf(stmnt, "edges(%d) = %d", vars[varnum].ndims - idim,
		    (int)dims[vars[varnum].dims[0]].size);
	    fline(stmnt);
	}
    } else {			/* scalar variables */
	fline("corner(1) = 1");
	fline("edges(1) = 1");
    }

    /* load variable with data values  */
    if (vars[varnum].type != NC_CHAR) {
	sprintf(stmnt, "data %s /",vars[varnum].name);
	stmnt_len = strlen(stmnt);
	switch (vars[varnum].type) {
	  case NC_BYTE:
	    charvalp = (char *) rec_start;
	    for (ival = 0; ival < var_len-1; ival++) {
		val_string = fstring(NC_BYTE,(void *)charvalp++,0);
		sprintf(s2, "%s, ", val_string);
		fstrcat(stmnt, s2, &stmnt_len);
		free(val_string);
	    }
	    val_string = fstring(NC_BYTE,(void *)charvalp++,0);
	    fstrcat(stmnt, val_string, &stmnt_len);
	    free(val_string);
	    break;
	  case NC_SHORT:
	    shortvalp = (short *) rec_start;
	    for (ival = 0; ival < var_len-1; ival++) {
		sprintf(s2, "%d, ", *shortvalp++);
		fstrcat(stmnt, s2, &stmnt_len);
	    }
	    sprintf(s2, "%d", *shortvalp);
	    fstrcat(stmnt, s2, &stmnt_len);
	    break;
	  case NC_LONG:
	    longvalp = (nclong *) rec_start;
	    for (ival = 0; ival < var_len-1; ival++) {
		sprintf(s2, "%d, ", (int)*longvalp++);
		fstrcat(stmnt, s2, &stmnt_len);
	    }
	    sprintf(s2, "%d", (int)*longvalp);
	    fstrcat(stmnt, s2, &stmnt_len);
	    break;
	  case NC_FLOAT:
	    floatvalp = (float *) rec_start;
	    for (ival = 0; ival < var_len-1; ival++) {
		sprintf(s2, "%.8g, ", *floatvalp++);
		fstrcat(stmnt, s2, &stmnt_len);
	    }
	    sprintf(s2, "%.8g", *floatvalp);
	    fstrcat(stmnt, s2, &stmnt_len);
	    break;
	  case NC_DOUBLE:
	    doublevalp = (double *) rec_start;
	    for (ival = 0; ival < var_len-1; ival++) {
 		sprintf(s2, "%#.16g", *doublevalp++);
 		tztrim(s2);
		fstrcat(s2, ", ", &stmnt_len);
		fstrcat(stmnt, s2, &stmnt_len);
	    }
 	    sprintf(s2, "%#.16g", *doublevalp++);
 	    tztrim(s2);
	    fstrcat(stmnt, s2, &stmnt_len);
	    break;
      default:
        break;
	}
	fstrcat(stmnt, "/", &stmnt_len);
	fline(stmnt);
	
	sprintf(stmnt, "call ncvpt(ncid, %sid, corner, edges, %s, iret)",
		vars[varnum].name, vars[varnum].name);
    } else {			/* for strings, call ncvptc() */
	int dimprod = 1;

	val_string = fstrstr((char *) rec_start, var_len);
	sprintf(stmnt, "%s = %s",vars[varnum].name, val_string);
	free(val_string);
	stmnt_len = strlen(stmnt);
	fstrcat(stmnt, " // char(0)", &stmnt_len);
	fline(stmnt);
	for (idim = vars[varnum].ndims-1; idim > 0; idim--)
	  dimprod *= dims[vars[varnum].dims[idim]].size;
	if (vars[varnum].dims[0] != rec_dim)
	  dimprod *= dims[vars[varnum].dims[0]].size;
	
	sprintf(stmnt, "call ncvptc(ncid, %sid, corner, edges, %s, %d, iret)",
		vars[varnum].name, vars[varnum].name, dimprod);
    }
    fline(stmnt);
}


/* invoke netcdf calls (or generate C or Fortran code) to load netcdf variable
 * from in-memory data.  Assumes following global variables set from yacc
 * parser:
 * int varnum        - number of variable to be loaded.
 * struct vars[varnum] - structure containing info on variable, specifically
 *                     name, type, ndims, dims, fill_value, has_data
 * int rec_dim       - id of record dimension, or -1 if none
 * struct dims[]     - structure containing name and size of dimensions.
 * int netcdf_record_number - number of current record for this variable.
 */
void
put_variable(rec_start)
     void *rec_start;		/* points to data to be loaded  */
{
    if (netcdf_flag)
      load_netcdf(rec_start);	/* put variable values (one record's worth) */
    if (c_flag)		/* create C code to put values */
      gen_load_c(rec_start);
    if (fortran_flag)		/* create Fortran code to put values */
      gen_load_fortran(rec_start);
}

