/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: ncdump.h,v 1.4 1996/03/26 22:34:10 georgev Exp $
 *********************************************************************/

#define  Printf  (void) printf

typedef enum {false=0, true=1} bool;

struct ncdim {			/* dimension */
    char name[MAX_NC_NAME];
    long size;
};

struct ncvar {			/* variable */
    char name[MAX_NC_NAME];
    nc_type type;
    int ndims;
    int dims[MAX_VAR_DIMS];
    int natts;
};

struct ncatt {			/* attribute */
    int var;
    char name[MAX_NC_NAME];
    nc_type type;
    int len;
    void *val;
};

typedef
enum {LANG_NONE, LANG_C, LANG_F} Nclang; 

struct fspec {			/* specification for how to format dump */
    char *name;			/*
				 * name specified with -n or derived from file
				 * name
				 */
    bool header_only;		/*
				 * if true, don't print any variable data
				 */
    bool coord_vals;		/*
				 * if true, print header and coordinate
				 * dimension values (values of variables that
				 * are also dimensions), but no other variable
				 * data
				 */
    bool brief_data_cmnts;	/*
				 * if true, put // comments in data section
				 * identifying variable and indices, useful for
				 * navigating through large multi-dimensional
				 * data lists.
				 */
    bool full_data_cmnts;	/*
				 * if true, put // comments in data section
				 * identifying every value, useful for
				 * navigating through large multi-dimensional
				 * data lists.
				 */
    Nclang data_lang;		/*
				 * Specifies index conventions used in data
				 * comments, either LANG_C (C, 0-based, column
				 * major) or LANG_F (Fortran, 1-based, row
				 * major)
				 */
    int nlvars;			/*
				 * Number of variables specified with -v option
				 * on command line
				 */
    char** lvars;		/*
				 * list of variable names specified with -v
				 * option on command line
				 */
};

#ifdef OLD_WAY
extern int getopt               PROTO((
                                       int argc,
                                       char **argv,
                                       char *opts
                                ));

#endif /* HP9000 */

