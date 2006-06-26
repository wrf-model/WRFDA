/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: generate.c,v 1.10 1998/12/08 21:38:02 koziol Exp $
 *********************************************************************/

#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include "ncgen.h"
#include "genlib.h"

extern char *netcdf_name; /* output netCDF filename, if on command line. */

static const char *ncftype(nc_type);
static const char *ftypename(nc_type);
static char *cstring(nc_type, void *, int);

extern int netcdf_flag;
extern int c_flag;
extern int fortran_flag;

/* create netCDF from in-memory structure */
static void
gen_netcdf(filename)
     char *filename;		/* name for output netcdf file */
{
    int idim, ivar, iatt;
    int istat;
    int dimids[MAX_NC_DIMS];
    int varids[MAX_NC_VARS];

    ncid = nccreate (filename, NC_CLOBBER);

    /* define dimensions from info in dims array */
    for (idim = 0; idim < ndims; idim++) {
	dimids[idim] = ncdimdef(ncid, dims[idim].name, dims[idim].size);
	if (dimids[idim] == -1)
	  derror("error defining dimension %s of size %ld",
		 dims[idim].name,dims[idim].size);
    }

    /* define variables from info in vars array */
    for (ivar = 0; ivar < nvars; ivar++) {
	varids[ivar] = ncvardef (ncid,
				  vars[ivar].name,
				  vars[ivar].type,
				  vars[ivar].ndims,
				  vars[ivar].dims);
	if (varids[ivar] == -1)
	  derror("error defining variable %s", vars[ivar].name);
    }

    /* define attributes from info in atts array */
    for (iatt = 0; iatt < natts; iatt++) {
	istat = ncattput(ncid,
			  (atts[iatt].var == -1) ? NC_GLOBAL : atts[iatt].var,
			  atts[iatt].name,
			  atts[iatt].type,
			  atts[iatt].len,
			  atts[iatt].val);
	if (istat == -1)
	  derror("error defining attribute %s for variable %s",
		 atts[iatt].name,vars[atts[ivar].var].name);
    }

    istat = ncendef (ncid);
    if (istat == -1) {
	derror("error returned from ncendef, exiting ...");
	exit(2);
    }
}

/*
 * Output a C statement.
 */
void
cline(stmnt)
     const char *stmnt;
{
    FILE *cout = stdout;
    
    fputs(stmnt, cout);
    fputs("\n", cout);
}


/* generate C code for creating netCDF from in-memory structure */
static void
gen_c(filename)
     char *filename;
{
    int idim, ivar, iatt, jatt, itype, maxdims;
    int scalar_atts, vector_atts;
    char *val_string;
    char stmnt[C_MAX_STMNT];
    char s2[MAX_NC_NAME + 2];

    static const char *ctypes[] = {"char","short","nclong","float","double"};
    int ntypes = (sizeof ctypes) / (sizeof ctypes[0]);

    /* wrap in main program */
    cline("#include \"netcdf.h\"");
    cline("");
    cline("int");
    sprintf(stmnt, "main() {\t\t\t/* create %s */", filename);
    cline(stmnt);

    /* create necessary declarations */
    cline("");
    cline("   int  ncid;\t\t\t/* netCDF id */");

    if (ndims > 0) {
	cline("");
	cline("   /* dimension ids */");
	strcpy(stmnt, "   int  ");
	for (idim = 0; idim < ndims; idim++) {
	    sprintf(s2,
		    "%s_dim%s",
		    dims[idim].name,
		    idim == ndims-1 ? ";" : ", ");
	    if (strlen(stmnt) + strlen(s2) >= C_MAX_STMNT) {
		if (idim < ndims-1) {
		    stmnt[strlen(stmnt)-2] = '\0'; /* truncate trailing ", " */
		    strcat(stmnt, ";");
		    cline(stmnt);
		    strcpy(stmnt, "   int  ");
		}
	    }
	    strcat(stmnt, s2);
	}
	cline(stmnt);
    }

    maxdims = 0;	/* most dimensions of any variable */
    for (ivar = 0; ivar < nvars; ivar++)
      if (vars[ivar].ndims > maxdims)
	maxdims = vars[ivar].ndims;

    if (nvars > 0) {
	cline("");
	cline("   /* variable ids */");
	strcpy(stmnt, "   int  ");
	for (ivar = 0; ivar < nvars; ivar++) {
	    sprintf(s2, "%s_id%s",
		    vars[ivar].name,
		    ivar == nvars-1 ? ";" : ", ");
	    if (strlen(stmnt) + strlen(s2) >= C_MAX_STMNT) {
		if (ivar < nvars-1) {
		    stmnt[strlen(stmnt)-2] = '\0'; /* truncate trailing ", " */
		    strcat(stmnt, ";");
		    cline(stmnt);
		    strcpy(stmnt, "   int  ");
		}
	    }
	    strcat(stmnt, s2);
	}
	cline(stmnt);

	if (maxdims > 0) {	/* we have dimensioned variables */
	    cline("");
	    cline("   /* variable shapes */");
	    sprintf(stmnt, "   int dims[%d];", maxdims);
	    cline(stmnt);
	}
    }

    /* determine if any containers for scalar attributes needed */
    scalar_atts = 0;
    for (iatt = 0; iatt < natts; iatt++) {
	if (atts[iatt].len == 1) {
	    scalar_atts = 1;
	    break;
	}
    }
    if (scalar_atts) {
	cline("");
	cline("   /* containers for scalar attributes */");
	for (itype = 0; itype < ntypes; itype++) {
	    for (iatt = 0; iatt < natts; iatt++) {
		char type_name[12]; /* big enough for longest c typename */
		(void) strcpy(type_name,ncctype(atts[iatt].type));
		if (atts[iatt].len == 1 &&
		    strcmp(type_name,ctypes[itype]) == 0) {
		    sprintf(stmnt, "   %s  %s_val;", type_name, type_name);
		    cline(stmnt);
		    break;
		}
	    }
	}
    }

    /* determine if we need any attribute vectors */
    vector_atts = 0;
    for (iatt = 0; iatt < natts; iatt++) {
	if (atts[iatt].len > 1 && atts[iatt].type != NC_CHAR) {
	    vector_atts = 1;
	    break;
	}
    }
    if (vector_atts) {
	cline("");
	cline("   /* attribute vectors */");
	for (iatt = 0; iatt < natts; iatt++) {
	    if (atts[iatt].len > 1 && atts[iatt].type != NC_CHAR) {
		sprintf(stmnt,
		    "   %s  %s_%s[%d];",
		    ncctype(atts[iatt].type),
		    atts[iatt].var == -1 ? "cdf" : vars[atts[iatt].var].name,
		    atts[iatt].name,
		    atts[iatt].len);
		cline(stmnt);
	    }
	}
    }

    /* create netCDF file, uses NC_CLOBBER mode */
    cline("");
    cline("   /* enter define mode */");
    sprintf(stmnt,
	    "   ncid = nccreate(\"%s\", NC_CLOBBER);",
	    filename);
    cline(stmnt);
    
    /* define dimensions from info in dims array */
    if (ndims > 0) {
	cline("");
	cline("   /* define dimensions */");
    }
    for (idim = 0; idim < ndims; idim++) {
	if (dims[idim].size == NC_UNLIMITED)
	  sprintf(stmnt,
		  "   %s_dim = ncdimdef(ncid, \"%s\", NC_UNLIMITED);",
		  dims[idim].name,
		  dims[idim].name);
	else
	  sprintf(stmnt,
		  "   %s_dim = ncdimdef(ncid, \"%s\", %dL);",
		  dims[idim].name,dims[idim].name,
		  (int)dims[idim].size);
	cline(stmnt);
    }

    /* define variables from info in vars array */
    if (nvars > 0) {
	cline("");
	cline("   /* define variables */");
	for (ivar = 0; ivar < nvars; ivar++) {
	    cline("");
	    for (idim = 0; idim < vars[ivar].ndims; idim++) {
		sprintf(stmnt,
			"   dims[%d] = %s_dim;",
			idim,
			dims[vars[ivar].dims[idim]].name);
		cline(stmnt);
	    }
	    if (vars[ivar].ndims > 0) {	/* a dimensioned variable */
		sprintf(stmnt,
			"   %s_id = ncvardef (ncid, \"%s\", %s, %d, dims);",
			vars[ivar].name,
			vars[ivar].name,
			nctype(vars[ivar].type),
			vars[ivar].ndims);
	    } else {		/* a scalar */
		sprintf(stmnt,
			"   %s_id = ncvardef (ncid, \"%s\", %s, %d, 0);",
			vars[ivar].name,
			vars[ivar].name,
			nctype(vars[ivar].type),
			vars[ivar].ndims);
	    }
	    cline(stmnt);
	}
    }
    
    /* define attributes from info in atts array */
    if (natts > 0) {
	cline("");
	cline("   /* assign attributes */");
	for (iatt = 0; iatt < natts; iatt++) {
	    if (atts[iatt].type == NC_CHAR && atts[iatt].len > 1) { /* string */
		val_string = cstrstr((char *) atts[iatt].val,
				     (long)atts[iatt].len);
		sprintf(stmnt,
			"   ncattput (ncid, %s%s, \"%s\", NC_CHAR, %d, (void *)%s);",
			atts[iatt].var == -1 ? "NC_GLOBAL" : vars[atts[iatt].var].name,
			atts[iatt].var == -1 ? "" : "_id",
			atts[iatt].name,
			atts[iatt].len,
			val_string);
		cline(stmnt);
		free (val_string);
	    }
	    else if (atts[iatt].len <= 1) {	/* scalar attribute */
		val_string = cstring(atts[iatt].type, atts[iatt].val, 0);
		sprintf(stmnt, "   %s_val = %s;",
			ncctype(atts[iatt].type),
			val_string);
		cline(stmnt);
		sprintf(stmnt,
			"   ncattput (ncid, %s%s, \"%s\", %s, %d,(void *) &%s_val);",
			atts[iatt].var == -1 ? "NC_GLOBAL" : vars[atts[iatt].var].name,
			atts[iatt].var == -1 ? "" : "_id",
			atts[iatt].name,
			nctype(atts[iatt].type),
			atts[iatt].len,
			ncctype(atts[iatt].type));
		cline(stmnt);
		free (val_string);
	    }
	    else {			/* vector attribute */
		for (jatt = 0; jatt < atts[iatt].len ; jatt++) {
		    val_string = cstring(atts[iatt].type,atts[iatt].val,jatt);
		    sprintf(stmnt, "   %s_%s[%d] = %s;",
			    atts[iatt].var == -1 ? "cdf" : vars[atts[iatt].var].name,
			    atts[iatt].name,
			    jatt, 
			    val_string);
		    cline(stmnt);
		    free (val_string);
		}
		
		sprintf(stmnt,
			"   ncattput (ncid, %s%s, \"%s\", %s, %d, (void *) %s_%s);",
			atts[iatt].var == -1 ? "NC_GLOBAL" : vars[atts[iatt].var].name,
			atts[iatt].var == -1 ? "" : "_id",
			atts[iatt].name,
			nctype(atts[iatt].type),
			atts[iatt].len,
			atts[iatt].var == -1 ? "cdf" : vars[atts[iatt].var].name,
			atts[iatt].name);
		cline(stmnt);
	    }
	}
    }
    cline("");
    cline("   /* leave define mode */");
    cline("   ncendef (ncid);");
}

/*
 * From a long line FORTRAN statment, generates the necessary FORTRAN
 * lines with continuation characters in column 6.  If stmnt starts with "*",
 * it is treated as a one-line comment.  Statement labels are *not* handled,
 * but since we don't generate any labels, we don't care.
 */
void
fline(stmnt)
     const char *stmnt;
{
    FILE *fout = stdout;
    int len = strlen(stmnt);
    int line = 0;
    static char cont[] = {	/* continuation characters */
	' ', '1', '2', '3', '4', '5', '6', '7', '8', '9',
	'+', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i'};
    
    if(stmnt[0] == '*') {
	fputs(stmnt, fout);
	fputs("\n", fout);
	return;
    }

    while (len > 0) {
	if (line >= FORT_MAX_LINES)
	  derror("FORTRAN statement too long: %s",stmnt);
	(void) fprintf(fout, "     %c", cont[line++]);
	(void) fprintf(fout, "%.66s\n", stmnt);
	len -= 66;
	if (len > 0)
	  stmnt += 66;
    }
}


/* generate FORTRAN code for creating netCDF from in-memory structure */
static void
gen_fortran(filename)
     char *filename;
{
    int idim, ivar, iatt, jatt, itype, maxdims;
    int vector_atts;
    char *val_string;
    char stmnt[FORT_MAX_STMNT];
    char s2[MAX_NC_NAME + 2];
    /* Need how many netCDF types there are, because we create an array
     * for each type of attribute. */
    int ntypes = 6;		/* number of netCDF types, NC_BYTE, ... */
    nc_type types[6];		/* at least ntypes */
    const char *ftypes[NC_DOUBLE + 1];
    int max_atts[NC_DOUBLE + 1];

    types[0] = NC_BYTE;
    types[1] = NC_CHAR;
    types[2] = NC_SHORT;
    types[3] = NC_LONG;
    types[4] = NC_FLOAT;
    types[5] = NC_DOUBLE;
    ftypes[(int) NC_BYTE] = "byte";
    ftypes[(int) NC_CHAR] = "char";
    ftypes[(int) NC_SHORT] = "short";
    ftypes[(int) NC_LONG] = "nclong";
    ftypes[(int) NC_FLOAT] = "float";
    ftypes[(int) NC_DOUBLE] = "double";

    /* wrap in main program */
#ifdef MSDOS
    printf("$include: \"msoft.int\"\n");
#endif
    sprintf(stmnt, "program fgennc");
    fline(stmnt);
#ifdef MSDOS
    printf("$include: \"netcdf.inc\"\n");
#else
    fline("include 'netcdf.inc'");
#endif

    /* create necessary declarations */
    fline("integer  iret");
    fline("* netCDF id");
    fline("integer  ncid");

    if (ndims > 0) {
	fline("* dimension ids");
	strcpy(stmnt, "integer  ");
	for (idim = 0; idim < ndims; idim++) {
	    sprintf(s2, "%sdim%s", dims[idim].name,
		    idim == ndims-1 ? "" : ", ");
	    if (strlen(stmnt) + strlen(s2) >= FORT_MAX_STMNT) {
		if (idim < ndims-1) {
		    stmnt[strlen(stmnt)-2] = '\0'; /* truncate trailing ", " */
		    fline(stmnt);
		    strcpy(stmnt,"integer  ");
		}
	    }
	    strcat(stmnt, s2);
	}
	fline(stmnt);
    }

    maxdims = 0;		/* most dimensions of any variable */
    for (ivar = 0; ivar < nvars; ivar++)
      if (vars[ivar].ndims > maxdims)
	maxdims = vars[ivar].ndims;

    if (nvars > 0) {
	fline("* variable ids");
	strcpy(stmnt,"integer  ");
	for (ivar = 0; ivar < nvars; ivar++) {
	    sprintf(s2, "%sid%s", vars[ivar].name,
		    ivar == nvars-1 ? "" : ", ");
	    if (strlen(stmnt) + strlen(s2) >= FORT_MAX_STMNT) {
		if (ivar < nvars-1) {
		    stmnt[strlen(stmnt)-2] = '\0'; /* truncate trailing ", " */
		    fline(stmnt);
		    strcpy(stmnt,"integer  ");
		}
	    }
	    strcat(stmnt, s2);
	}
	fline(stmnt);

	if (maxdims > 0) {	/* we have dimensioned variables */
	    fline("* variable shapes");
	    sprintf(stmnt, "integer dims(%d)", maxdims);
	    fline(stmnt);
	}
    }

    fline("* corners and edge lengths");
#ifdef MSDOS
    sprintf(stmnt, "integer*4 corner(%d), edges(%d)", maxdims, maxdims);
#else
    sprintf(stmnt, "integer corner(%d), edges(%d)", maxdims, maxdims);
#endif
    fline(stmnt);

    /* declarations for variables to be initialized */
    if (nvars > 0) {		/* we have variables */
	fline("* data variables");
	for (ivar = 0; ivar < nvars; ivar++) {
	    if (vars[ivar].type != NC_CHAR) {
		if (vars[ivar].ndims == 0) { /* scalar */
		    sprintf(stmnt, "%s %s", ncftype(vars[ivar].type),
			    vars[ivar].name);
		    fline(stmnt);
		} else {
		    sprintf(stmnt, "%s %s(", ncftype(vars[ivar].type),
			    vars[ivar].name);
		    /* reverse dimensions for FORTRAN */
		    for (idim = vars[ivar].ndims-1; idim > 0; idim--) {
			sprintf(s2, "%d,", (int)dims[vars[ivar].dims[idim]].size);
			strcat(stmnt, s2);
		    }
		    if (vars[ivar].dims[0] == rec_dim)
		      sprintf(s2, "%d)", 1);
		    else
		      sprintf(s2, "%d)", (int)dims[vars[ivar].dims[0]].size);
		    strcat(stmnt, s2);
		    fline(stmnt);
		}
	    } else {		/* for strings, declare multi-char variable */
		int dimprod = 1;
		for (idim = vars[ivar].ndims-1; idim > 0; idim--)
		  dimprod *= dims[vars[ivar].dims[idim]].size;
		if (vars[ivar].ndims != 0) { /* not a scalar */
		    if (vars[ivar].dims[0] != rec_dim)
		      dimprod *= dims[vars[ivar].dims[0]].size;
		}
		sprintf(stmnt, "%s*%d %s", ncftype(vars[ivar].type),
			dimprod,
			vars[ivar].name);
		fline(stmnt);
	    }
	}
    }

    /* determine what attribute vectors needed */
    for (itype = 0; itype < ntypes; itype++)
      max_atts[(int)types[itype]] = 0;

    vector_atts = 0;
    for (iatt = 0; iatt < natts; iatt++) {
	if (atts[iatt].len > max_atts[(int) atts[iatt].type]) {
	    max_atts[(int)atts[iatt].type] = atts[iatt].len;
	    vector_atts = 1;
	}
    }
    if (vector_atts) {
	fline("* attribute vectors");
	for (itype = 0; itype < ntypes; itype++) {
	    if (types[itype] != NC_CHAR && max_atts[(int)types[itype]] > 0) {
		sprintf(stmnt, "%s  %sval(%d)", ncftype(types[itype]),
			ftypes[(int)types[itype]],
			max_atts[(int)types[itype]]);
		fline(stmnt);
	    }
	}
    }
    
    /* create netCDF file, uses NC_CLOBBER mode */
    fline("* enter define mode");
    sprintf(stmnt, "ncid = nccre (\'%s\', NCCLOB, iret)", filename);
    fline(stmnt);
    
    /* define dimensions from info in dims array */
    if (ndims > 0)
      fline("* define dimensions");
    for (idim = 0; idim < ndims; idim++) {
	if (dims[idim].size == NC_UNLIMITED)
	  sprintf(stmnt, "%sdim = ncddef(ncid, \'%s\', NCUNLIM, iret)",
		  dims[idim].name,dims[idim].name);
	else
	  sprintf(stmnt, "%sdim = ncddef(ncid, \'%s\', %d, iret)",
		  dims[idim].name,dims[idim].name,(int)dims[idim].size);
	fline(stmnt);
    }
	  
    /* define variables from info in vars array */
    if (nvars > 0) {
	fline("* define variables");
	for (ivar = 0; ivar < nvars; ivar++) {
	    for (idim = 0; idim < vars[ivar].ndims; idim++) {
		sprintf(stmnt, "dims(%d) = %sdim",
			vars[ivar].ndims - idim, /* reverse dimensions */
			dims[vars[ivar].dims[idim]].name);
		fline(stmnt);
	    }
	    if (vars[ivar].ndims > 0) {	/* a dimensioned variable */
		sprintf(stmnt, 
			"%sid = ncvdef (ncid, \'%s\', %s, %d, dims, iret)",
			vars[ivar].name,
			vars[ivar].name,
			ftypename(vars[ivar].type),
			vars[ivar].ndims);
	    } else {		/* a scalar */
		sprintf(stmnt, 
			"%sid = ncvdef (ncid, \'%s\', %s, %d, 0, iret)",
			vars[ivar].name,
			vars[ivar].name,
			ftypename(vars[ivar].type),
			vars[ivar].ndims);
	    }
	    fline(stmnt);
	}
    }

    /* define attributes from info in atts array */
    if (natts > 0) {
	fline("* assign attributes");
	for (iatt = 0; iatt < natts; iatt++) {
	    if (atts[iatt].type == NC_CHAR && atts[iatt].len > 1) { /* string */
		val_string = fstrstr((char *) atts[iatt].val,
				     (long)atts[iatt].len);
		sprintf(stmnt, 
			"call ncaptc(ncid, %s%s, \'%s\', NCCHAR, %d, %s, iret)",
			atts[iatt].var == -1 ? "NCGLOBAL" : vars[atts[iatt].var].name,
			atts[iatt].var == -1 ? "" : "id",
			atts[iatt].name,
			atts[iatt].len,
			val_string);
		fline(stmnt);
		free(val_string);
	    } else {
		for (jatt = 0; jatt < atts[iatt].len ; jatt++) {
		    val_string = fstring(atts[iatt].type,atts[iatt].val,jatt);
		    sprintf(stmnt, "%sval(%d) = %s",
			    ftypes[(int)atts[iatt].type],
			    jatt+1, 
			    val_string);
		    fline(stmnt);
		    free (val_string);
		}
	    
		sprintf(stmnt,
			"call ncapt(ncid, %s%s, \'%s\', %s, %d, %sval, iret)",
			atts[iatt].var == -1 ? "NCGLOBAL" : vars[atts[iatt].var].name,
			atts[iatt].var == -1 ? "" : "id",
			atts[iatt].name,
			ftypename(atts[iatt].type),
			atts[iatt].len,
			ftypes[(int)atts[iatt].type]);
		fline(stmnt);
	    }
	}
    }
    fline("* leave define mode");
    fline("call ncendf(ncid, iret)");
}


/* return C name for netCDF type, given type code */
const char *
nctype(type)
     nc_type type;			/* netCDF type code */
{
    switch (type) {
      case NC_BYTE:
	return "NC_BYTE";
      case NC_CHAR:
	return "NC_CHAR";
      case NC_SHORT:
	return "NC_SHORT";
      case NC_LONG:
	return "NC_LONG";
      case NC_FLOAT:
	return "NC_FLOAT";
      case NC_DOUBLE:
	return "NC_DOUBLE";
      default:
	derror("nctype: bad type code");
	return NULL;
    }
}


/* return FORTRAN name for netCDF type, given type code */
static const char *
ftypename(type)
     nc_type type;			/* netCDF type code */
{
    switch (type) {
      case NC_BYTE:
	return "NCBYTE";
      case NC_CHAR:
	return "NCCHAR";
      case NC_SHORT:
	return "NCSHORT";
      case NC_LONG:
	return "NCLONG";
      case NC_FLOAT:
	return "NCFLOAT";
      case NC_DOUBLE:
	return "NCDOUBLE";
      default:
	derror("ftypename: bad type code");
	return NULL;
    }
}


/* return C type name for netCDF type, given type code */

const char *
ncctype(type)
     nc_type type;			/* netCDF type code */
{
    switch (type) {
      case NC_BYTE:
	return "char";
      case NC_CHAR:
	return "char";
      case NC_SHORT:
	return "short";
      case NC_LONG:
	return "nclong";
      case NC_FLOAT:
	return "float";
      case NC_DOUBLE:
	return "double";
      default:
	derror("ncctype: bad type code");
	return NULL;
    }
}


/* return Fortran type name for netCDF type, given type code */

static const char *
ncftype(type)
     nc_type type;		/* netCDF type code */
{
    switch (type) {
#ifdef cray
#ifndef _CRAYMPP
      /* Traditional vector pipe cray */
      case NC_BYTE:
	return "integer";
      case NC_CHAR:
	return "character";
      case NC_SHORT:
	return "integer";
      case NC_LONG:
	return "integer";
      case NC_FLOAT:
	return "real";
      case NC_DOUBLE:
	return "real";		/* we don't support CRAY 128-bit doubles */
#else
      /* a T3D/E.  INTEGER and REAL are both 8 bytes. */
      case NC_BYTE:
	return "integer";
      case NC_CHAR:
	return "character";
      case NC_SHORT:
	return "integer";
      case NC_LONG:
	return "integer*4";
      case NC_FLOAT:
	return "real*4";
      case NC_DOUBLE:
	return "real";
#endif	/* (_CRAYMPP) */
#else
      case NC_BYTE:
	return "byte";		/* non-standard */
      case NC_CHAR:
	return "character";
      case NC_SHORT:
	return "integer*2";
      case NC_LONG:
#ifdef MSDOS
	return "integer*4";
#else
	return "integer";
#endif
      case NC_FLOAT:
	return "real";
      case NC_DOUBLE:
	return "double precision";
#endif	
      default:
	derror("ncctype: bad type code");
	return NULL;

    }
}


/*
 * Given a netcdf type, a pointer to a vector of values of that type,
 * and the index of the vector element desired, returns a pointer to a
 * malloced string representing the value in C.
 */

static char *
cstring(type,valp, num)
     nc_type type;			/* netCDF type code */
     void *valp;		/* pointer to vector of values */
     int num;			/* element of vector desired */
{
    static char *cp, *sp, ch;
    char *bytep;
    short *shortp;
    nclong *longp;
    float *floatp;
    double *doublep;

    switch (type) {
      case NC_CHAR:
	sp = cp = (char *) emalloc (7);
	*cp++ = '\'';
	ch = *((char *)valp + num);
	switch (ch) {
	  case '\b': *cp++ = '\\'; *cp++ = 'b'; break;
	  case '\f': *cp++ = '\\'; *cp++ = 'f'; break;
	  case '\n': *cp++ = '\\'; *cp++ = 'n'; break;
	  case '\r': *cp++ = '\\'; *cp++ = 'r'; break;
	  case '\t': *cp++ = '\\'; *cp++ = 't'; break;
	  case '\v': *cp++ = '\\'; *cp++ = 'v'; break;
	  case '\\': *cp++ = '\\'; *cp++ = '\\'; break;
/*, tj	  case '\?': *cp++ = '\\'; *cp++ = '?'; break; */
	  case '\'': *cp++ = '\\'; *cp++ = '\''; break;
	  default:
/*, tj	    if (ch < '\040' || ch > '\176') { */ /* assumes ASCII */
	    if (!isprint((unsigned char)ch)) {
		static char octs[] = "01234567";
		int rem = ((unsigned char)ch)%64;
		*cp++ = '\\';
		*cp++ = octs[((unsigned char)ch)/64]; /* to get, e.g. '\177' */
		*cp++ = octs[rem/8];
		*cp++ = octs[rem%8];
	    } else {
		*cp++ = ch;
	    }
	    break;
	}
	*cp++ = '\'';
	*cp = '\0';
	return sp;
	
      case NC_BYTE:
	cp = (char *) emalloc (7);
	bytep = (char *)valp;
	(void) sprintf(cp,"'\\%o'", * (bytep + num) & 0xff);
	return cp;

      case NC_SHORT:
	cp = (char *) emalloc (10);
	shortp = (short *)valp;
	(void) sprintf(cp,"%d",* (shortp + num));
	return cp;

      case NC_LONG:
	cp = (char *) emalloc (20);
	longp = (nclong *)valp;
	(void) sprintf(cp,"%d",(int)* (longp + num));
	return cp;

      case NC_FLOAT:
	cp = (char *) emalloc (20);
	floatp = (float *)valp;
	(void) sprintf(cp,"%.8g",* (floatp + num));
	return cp;

      case NC_DOUBLE:
	cp = (char *) emalloc (20);
	doublep = (double *)valp;
	(void) sprintf(cp,"%.16g",* (doublep + num));
	return cp;

      default:
	derror("cstring: bad type code");
	return 0;
    }
}


/*
 * Given a netcdf type, a pointer to a vector of values of that type,
 * and the index of the vector element desired, returns a pointer to a
 * malloced string representing the value in FORTRAN.
 */
char *
fstring(type,valp, num)
     nc_type type;			/* netCDF type code */
     void *valp;		/* pointer to vector of values */
     int num;			/* element of vector desired */
{
    static char *cp, *sp;
    char ch;
    short *shortp;
    nclong *longp;
    float *floatp;
    double *doublep;

    switch (type) {
      case NC_CHAR:
      case NC_BYTE:
	sp = cp = (char *) emalloc (10);
	ch = *((char *)valp + num);
	if (isprint((unsigned char)ch)) {
	    *cp++ = '\'';
	    *cp++ = ch;
	    *cp++ = '\'';
	    *cp = '\0';
	} else {
	    sprintf(cp,"%d",(unsigned char)ch); /* char(%d) ? */
	}
	return sp;

      case NC_SHORT:
	cp = (char *) emalloc (10);
	shortp = (short *)valp;
	(void) sprintf(cp,"%d",* (shortp + num));
	return cp;

      case NC_LONG:
	cp = (char *) emalloc (20);
	longp = (nclong *)valp;
	(void) sprintf(cp,"%d",(int)* (longp + num));
	return cp;

      case NC_FLOAT:
	cp = (char *) emalloc (20);
	floatp = (float *)valp;
	(void) sprintf(cp,"%.8g",* (floatp + num));
	return cp;

      case NC_DOUBLE:
	cp = (char *) emalloc (20);
	doublep = (double *)valp;
	(void) sprintf(cp,"%.16g",* (doublep + num));
	return cp;

      default:
	derror("fstring: bad type code");
	return 0;
    }
}


/*
 * Given a pointer to a counted string, returns a pointer to a malloced string
 * representing the string as a C constant.
 */
char *
cstrstr(valp, len)
     char *valp;		/* pointer to vector of characters*/
     long len;			/* number of characters in valp */
{
    static char *sp;
    char *cp;
    char *istr, *istr0;		/* for null-terminated copy */

    if(4*len+3 != (unsigned)(4*len+3)) {
	derror("too much character data!");
	exit(9);
    }
    istr0 = istr = (char *) emalloc((int)len + 1);
    strncpy(istr, (char *) valp, (int)len);
    istr[len] = '\0';

    sp = cp = (char *) emalloc(4*(int)len+3);

    *cp++ = '"';
    while (*istr != '\0') {
	switch (*istr) {
	  case '\b': *cp++ = '\\'; *cp++ = 'b'; break;
	  case '\f': *cp++ = '\\'; *cp++ = 'f'; break;
	  case '\n': *cp++ = '\\'; *cp++ = 'n'; break;
	  case '\r': *cp++ = '\\'; *cp++ = 'r'; break;
	  case '\t': *cp++ = '\\'; *cp++ = 't'; break;
	  case '\v': *cp++ = '\\'; *cp++ = 'v'; break;
	  case '\\': *cp++ = '\\'; *cp++ = '\\'; break;
/*, tj	  case '\?': *cp++ = '\\'; *cp++ = '?'; break; */
	  case '\'': *cp++ = '\\'; *cp++ = '\''; break;
	  default:
/*, tj	    if (*istr < '\040' || *istr > '\176') { */ /* assumes ASCII */
	    if (!isprint((unsigned char)*istr)) {
		static char octs[] = "01234567";
		int rem = ((unsigned char)*istr)%64;
		*cp++ = '\\';
		*cp++ = octs[((unsigned char)*istr)/64]; /* to get, e.g. '\177' */
		*cp++ = octs[rem/8];
		*cp++ = octs[rem%8];
	    } else {
		*cp++ = *istr;
	    }
	    break;
	}
	istr++;
    }
    *cp++ = '"';
    *cp = '\0';
    free(istr0);
    return sp;
}


/*
 * Given a pointer to a counted string (not necessarily null-terminated),
 * returns a pointer to a malloced string representing the string as a
 * FORTRAN string expression.  For example, the string "don't" would yield
 * the FORTRAN string "'don''t'", and the string "ab\ncd" would yield
 * "'ab'//char(10)//'cd'".
 */
char *
fstrstr(str, ilen)
     char *str;			/* pointer to vector of characters */
     long ilen;			/* number of characters in istr */
{
    static char *ostr;
    char *cp, tstr[12];
    int was_print = 0;		/* true if last character was printable */
    char *istr, *istr0;		/* for null-terminated copy */


    if(12*ilen != (unsigned)(12*ilen)) {
	derror("too much character data!");
	exit(9);
    }
    istr0 = istr = (char *) emalloc((int)ilen + 1);
    strncpy(istr, (char *) str, (int)ilen);
    istr[ilen] = '\0';
    
    ostr = cp = (char *) emalloc(12*(int)ilen);
    *ostr = '\0';
    if (*istr == '\0') {	/* empty string input, not legal in FORTRAN */
	strcat(ostr,"' '");
	free(istr0);
	return ostr;
    }
    if (isprint((unsigned char)*istr)) {	/* handle first character in input */
	*cp++ = '\'';
	if (*istr == '\'') {
	    *cp++ = '\'';
	    *cp++ = '\'';
	} else {
	    *cp++ = *istr;
	}
	*cp = '\0';
	was_print = 1;
    } else {
	sprintf(tstr, "char(%d)", (unsigned char)*istr);
	strcat(cp, tstr);
	cp += strlen(tstr);
	was_print = 0;
    }
    istr++;

    while (*istr != '\0') {	/* handle subsequent characters in input */
	if (isprint((unsigned char)*istr)) {
	    if (! was_print) {
		strcat(cp, "//'");
		cp += 3;
	    }
	    if (*istr == '\'') {
		*cp++ = '\'';
		*cp++ = '\'';
	    } else {
		*cp++ = *istr;
	    }
	    *cp = '\0';
	    was_print = 1;
	} else {
	    if (was_print) {
		*cp++ = '\'';
		*cp = '\0';
	    }
	    sprintf(tstr, "//char(%d)", (unsigned char)*istr);
	    strcat(cp, tstr);
	    cp += strlen(tstr);
	    was_print = 0;
	}
	istr++;
    }
    if (was_print)
      *cp++ = '\'';
    *cp = '\0';
    free(istr0);
    return ostr;
}


/* invoke netcdf calls (or generate C or Fortran code) to create netcdf
 * from in-memory structure. */
void
define_netcdf(netcdfname)
     char *netcdfname;
{
    char *filename;		/* output file name */
    
    if (netcdf_name) {		/* name given on command line */
	filename = netcdf_name;
    } else {			/* construct name from CDL name */
	filename = (char *) emalloc(strlen(netcdfname) + 5);
	(void) strcpy(filename,netcdfname);
	if (netcdf_flag == 1)
	  (void) strcat(filename,".nc"); /* new, favored extension */
	else if (netcdf_flag == -1)
	  (void) strcat(filename,".cdf"); /* old, deprecated extension */
    }
    if (netcdf_flag)
      gen_netcdf(filename);	/* create netcdf */
    if (c_flag)			/* create C code to create netcdf */
      gen_c(filename);
    if (fortran_flag)		/* create Fortran code to create netcdf */
      gen_fortran(filename);
}
