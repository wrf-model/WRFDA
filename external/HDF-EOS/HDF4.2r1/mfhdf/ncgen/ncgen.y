/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: ncgen.y,v 1.7 1997/11/05 19:40:51 koziol Exp $
 *********************************************************************/

/* yacc source for "ncgen", a netCDL parser and netCDF generator */

%{
#ifndef lint
static char SccsId[] = "$Id: ncgen.y,v 1.7 1997/11/05 19:40:51 koziol Exp $";
#endif
#include        <string.h>
#include	<stdlib.h>
#include        "ncgen.h"

typedef struct Symbol {		/* symbol table entry */
	char    	*name;
	struct Symbol   *next;
	unsigned	is_dim : 1;	/* appears as netCDF dimension */
	unsigned	is_var : 1;	/* appears as netCDF variable */
	unsigned	is_att : 1;	/* appears as netCDF attribute */
	int             dnum;	        /* handle as a dimension */
	int             vnum;	        /* handle as a variable */
	} *YYSTYPE1;

#define YYSTYPE YYSTYPE1
YYSTYPE install(), lookup();
YYSTYPE symlist;		/* symbol table: linked list */

void init_netcdf();		/* initializes netcdf counts (e.g. nvars) */
void define_netcdf();		/* generates all define mode stuff */
void load_netcdf();		/* generates variable puts */
void close_netcdf();		/* generates close */

void derror();			/* varargs message emitter */
void *emalloc(), *erealloc();	/* malloc that checks for memory exhausted */
void clearout();		/* initializes symbol table */
void nc_getfill();		/* to get fill value for various types */
void nc_putfill();		/* to get fill value for various types */
void nc_fill();		/* fills a generic array with a value */
int  put_variable();            /* invoke nc calls or generate code to put */
                                /* variable values            */
extern int derror_count;	/* counts errors in netcdf definition */
extern int lineno;		/* line number for error messages */

static int not_a_string;	/* whether last constant read was a string */
static char termstring[MAXTRST]; /* last terminal string read */
static double double_val;	/* last double value read */
static float float_val;		/* last float value read */
static nclong long_val;		/* last long value read */
static short short_val;		/* last short value read */
static char char_val;		/* last char value read */
static char byte_val;		/* last byte value read */

static nc_type type_code;	/* holds declared type for variables */
static nc_type atype_code;	/* holds derived type for attributes */
static char *netcdfname;	/* to construct netcdf file name */
static void *att_space;		/* pointer to block for attribute values */
static nc_type valtype;		/* type code for list of attribute values  */

static char *char_valp;		/* pointers used to accumulate data values */
static char *byte_valp;
static short *short_valp;
static nclong *long_valp;
static float *float_valp;
static double *double_valp;
static void *rec_cur;		/* pointer to where next data value goes */
static void *rec_start;		/* start of space for a record of data */
%}

/* DECLARATIONS */

%token
	NC_UNLIMITED_K /* keyword for unbounded record dimension */
	BYTE_K	    /* keyword for byte datatype */
	CHAR_K	    /* keyword for char datatype */
	SHORT_K	    /* keyword for short datatype */
	LONG_K	    /* keyword for long datatype */
	FLOAT_K	    /* keyword for float datatype */
	DOUBLE_K    /* keyword for double datatype */
	IDENT	    /* name for a dimension, variable, or attribute */
	TERMSTRING  /* terminal string */
	BYTE_CONST  /* byte constant */
	CHAR_CONST  /* char constant */
	SHORT_CONST /* short constant */
	LONG_CONST  /* long constant */
	FLOAT_CONST /* float constant */
	DOUBLE_CONST /* double constant */
	DIMENSIONS  /* keyword starting dimensions section, if any */
	VARIABLES   /* keyword starting variables section, if any */
	NETCDF      /* keyword declaring netcdf name */
	DATA        /* keyword starting data section, if any */

%start	ncdesc /* start symbol for grammar */

%%

/* RULES */

ncdesc:	NETCDF
		'{'
		   { init_netcdf(); }
                dimsection	/* dimension declarations */
                     {
                       if (ndims > MAX_NC_DIMS)
                         derror("Too many dimensions");
                   }
                vasection	/* variable and attribute declarations */
		   {
		       if (derror_count == 0)
			 define_netcdf(netcdfname);
		   }
		datasection     /* data, variables loaded as encountered */
                '}'
		   {
		       if (derror_count == 0)
			 close_netcdf();
		   }
		;
dimsection:     /* empty */
		| DIMENSIONS dimdecls
		;
dimdecls:       dimdecline ';'
		| dimdecls dimdecline ';'
		;
dimdecline:     dimdecl
                | dimdecline ',' dimdecl
                ;
dimdecl:        dimd '=' LONG_CONST
		   { if (long_val <= 0)
			 derror("negative dimension size");
		     dims[ndims].size = long_val;
		     ndims++;
		   }
                | dimd '=' NC_UNLIMITED_K
		   {  if (rec_dim != -1)
			 derror("only one NC_UNLIMITED dimension allowed");
		     rec_dim = ndims; /* the unlimited (record) dimension */
		     dims[ndims].size = NC_UNLIMITED;
		     ndims++;
		   }
                ;
dimd:           dim
		   { if ($1->is_dim == 1) {
		        derror( "duplicate dimension declaration for %s",
		                $1->name);
		     }
	             $1->is_dim = 1;
		     $1->dnum = ndims;
		     dims[ndims].name = (char *) emalloc(strlen($1->name)+1);
		     (void) strcpy(dims[ndims].name, $1->name);
		   }
                ;
dim:		IDENT
		;
vasection:      /* empty */
		| VARIABLES vadecls
		;
vadecls:        vadecl ';'
                | vadecls vadecl ';'
                ;
vadecl:         vardecl | attdecl
                ;
vardecl:        type varlist
                ;
type:             BYTE_K  { type_code = NC_BYTE; }
		| CHAR_K  { type_code = NC_CHAR; }
		| SHORT_K  { type_code = NC_SHORT; }
		| LONG_K  { type_code = NC_LONG; }
		| FLOAT_K  { type_code = NC_FLOAT; }
		| DOUBLE_K  { type_code = NC_DOUBLE; }
		;
varlist:        varspec
                | varlist ',' varspec
                ;
varspec:        var
		   {
		    if (nvars >= MAX_NC_VARS)
		       derror("too many variables");
		    nvdims = 0;
		    /* make sure variable not re-declared */
		    if ($1->is_var == 1) {
		       derror( "duplicate variable declaration for %s",
		               $1->name);
		    }
	            $1->is_var = 1;
		    $1->vnum = nvars;
		    vars[nvars].name = (char *) emalloc(strlen($1->name)+1);
		    (void) strcpy(vars[nvars].name, $1->name);
		    vars[nvars].type = type_code;
		    /* set default fill value.  You can override this with
		     * the variable attribute "_FillValue". */
		    nc_getfill(type_code, &vars[nvars].fill_value);
		    vars[nvars].has_data = 0; /* has no data (yet) */
		   }
		dimspec
		   {
		    vars[nvars].ndims = nvdims;
		    nvars++;
		   }
		;
var:            IDENT
                ;
dimspec:	/* empty */
		| '(' dimlist ')'
		;
dimlist:        vdim
                | dimlist ',' vdim
                ;
vdim:		dim
		   {
		    if (nvdims >= MAX_VAR_DIMS) {
		       derror("%s has too many dimensions",vars[nvars].name);
		    }
		    if ($1->is_dim == 1)
		       dimnum = $1->dnum;
		    else {
		       derror( "%s is not declared as a dimension",
			       $1->name);
	               dimnum = ndims;
		    }
		    if (rec_dim != -1 && dimnum == rec_dim && nvdims != 0) {
		       derror("unlimited dimension must be first");
		    }
		    vars[nvars].dims[nvdims] = dimnum;
                    nvdims++;
		   }
		;
attdecl:        att
		   {
		       valnum = 0;
		       valtype = NC_UNSPECIFIED;
		       /* get a large block for attributes, realloc later */
		       att_space = emalloc(MAX_NC_ATTSIZE);
		       /* make all kinds of pointers point to it */
		       char_valp = (char *) att_space;
		       byte_valp = (char *) att_space;
		       short_valp = (short *) att_space;
		       long_valp = (nclong *) att_space;
		       float_valp = (float *) att_space;
		       double_valp = (double *) att_space;
		   }
		'=' attvallist
		   {
		       if (natts >= MAX_NC_ATTS)
			 derror("too many attributes");
		       atts[natts].var = varnum ;
		       atts[natts].type = valtype;
		       atts[natts].len = valnum;
		       /* shrink space down to what was really needed */
		       att_space = erealloc(att_space, valnum*nctypelen(valtype));
		       atts[natts].val = att_space;
		       if (STREQ(atts[natts].name, _FillValue)) {
			   nc_putfill(atts[natts].type,
				       atts[natts].val,
				       &vars[atts[natts].var].fill_value);
		       }
		       natts++;
		   }
                ;
att:            avar ':' attr
                |    ':' attr
		   {
		    varnum = -1;  /* handle of "global" attribute */
		   }
                ;

avar:           var
		   { if ($1->is_var == 1)
		       varnum = $1->vnum;
		    else {
		      derror("%s not declared as a variable, fatal error",
			     $1->name);
		      YYABORT;
		      }
		   }
		;
attr:		IDENT
		   {
		       atts[natts].name = (char *) emalloc(strlen($1->name)+1);
		       (void) strcpy(atts[natts].name,$1->name);
		   }
		;
attvallist:     aconst
                | attvallist ',' aconst
                ;
aconst:		attconst
		   {
		    if (valtype == NC_UNSPECIFIED)
		      valtype = atype_code;
		    if (valtype != atype_code)
		      derror("values for attribute must be all of same type");
		   }
		;

attconst:      CHAR_CONST
                   {
		       atype_code = NC_CHAR;
		       *char_valp++ = char_val;
		       valnum++;
		   }
	       | TERMSTRING
		   {
		       atype_code = NC_CHAR;
		       {
			   /* don't null-terminate attribute strings */
			   int len = strlen(termstring);
			   valnum += len;
			   (void)strncpy(char_valp,termstring,len);
			   char_valp += len;
		       }
		   }
                | BYTE_CONST
                   {
		       atype_code = NC_BYTE;
		       *byte_valp++ = byte_val;
		       valnum++;
		   }
                | SHORT_CONST
                   {
		       atype_code = NC_SHORT;
		       *short_valp++ = short_val;
		       valnum++;
		   }
                | LONG_CONST
                   {
		       atype_code = NC_LONG;
		       *long_valp++ = long_val;
		       valnum++;
		   }
                | FLOAT_CONST
                   {
		       atype_code = NC_FLOAT;
		       *float_valp++ = float_val;
		       valnum++;
		   }
                | DOUBLE_CONST
                   {
		       atype_code = NC_DOUBLE;
		       *double_valp++ = double_val;
		       valnum++;
		   }
                ;

datasection:    /* empty */
		| DATA datadecls
		;

datadecls:      datadecl ';'
                | datadecls datadecl ';'
                ;
datadecl:       avar
		   {
		       valtype = vars[varnum].type; /* variable type */
		       valnum = 0;	/* values accumulated for variable */
		       vars[varnum].has_data = 1;
		       /* compute dimensions product (size of a "record") */
		       var_size = nctypelen(valtype);
		       if (vars[varnum].ndims == 0)
			   var_len = 1;
		       else if (vars[varnum].dims[0] == rec_dim) {
			   var_len = 1; /* one record for unlimited vars */
			   netcdf_record_number = 0;
		       }
		       else
			 var_len = dims[vars[varnum].dims[0]].size;
		       for(dimnum = 1; dimnum < vars[varnum].ndims; dimnum++)
			 var_len = var_len*dims[vars[varnum].dims[dimnum]].size;
		       /* allocate memory for a record of variable data */
		       if (var_len*var_size != (unsigned)(var_len*var_size)) {
			   derror("too much data for this machine");
			   exit(9);
		       }
		       rec_start = malloc ((unsigned)(var_len*var_size));
		       if (rec_start == 0) {
			   derror ("out of memory\n");
			   exit(3);
		       }
		       rec_cur = rec_start;
		       switch (valtype) {
			 case NC_CHAR:
			   char_valp = (char *) rec_start;
			   break;
			 case NC_BYTE:
			   byte_valp = (char *) rec_start;
			   break;
			 case NC_SHORT:
			   short_valp = (short *) rec_start;
			   break;
			 case NC_LONG:
			   long_valp = (nclong *) rec_start;
			   break;
			 case NC_FLOAT:
			   float_valp = (float *) rec_start;
			   break;
			 case NC_DOUBLE:
			   double_valp = (double *) rec_start;
			   break;
		       }
		 }
		'=' constlist
                   {
		       if (valnum > 0 && valnum < var_len) { /* leftovers */
			   nc_fill(valtype,
				    var_len - valnum,
				    rec_cur,
				    vars[varnum].fill_value);
			   /* put out record of var_len values */
			   if (derror_count == 0)
			     put_variable(rec_start);
		       }
		       free ((char *) rec_start);
		 }
                ;
constlist:      dconst
                | constlist ',' dconst
                ;
dconst:
                   {
		       if(valnum >= var_len) {
			   derror("too many values for this variable");
			   exit (4);
		       }
		       not_a_string = 1;
                   }
                const
		   {
		       if (not_a_string) {
			   switch (valtype) {
			     case NC_CHAR:
			       rec_cur = (void *) char_valp;
			       break;
			     case NC_BYTE:
			       rec_cur = (void *) byte_valp;
			       break;
			     case NC_SHORT:
			       rec_cur = (void *) short_valp;
			       break;
			     case NC_LONG:
			       rec_cur = (void *) long_valp;
			       break;
			     case NC_FLOAT:
			       rec_cur = (void *) float_valp;
			       break;
			     case NC_DOUBLE:
			       rec_cur = (void *) double_valp;
			       break;
			   }
		       }
		       if (valnum >= var_len) {
			   /* put out record of var_len elements */
			   if (derror_count == 0)
			     put_variable(rec_start);
			   /* if this variable is unbounded, reset for */
			   /* next record */
			   if (vars[varnum].dims[0] == rec_dim) {
			       valnum = 0;
			       netcdf_record_number++;
			       rec_cur = rec_start;
			       switch (valtype) {
				 case NC_CHAR:
				   char_valp = (char *) rec_start;
				   break;
				 case NC_BYTE:
				   byte_valp = (char *) rec_start;
				   break;
				 case NC_SHORT:
				   short_valp = (short *) rec_start;
				   break;
				 case NC_LONG:
				   long_valp = (nclong *) rec_start;
				   break;
				 case NC_FLOAT:
				   float_valp = (float *) rec_start;
				   break;
				 case NC_DOUBLE:
				   double_valp = (double *) rec_start;
				   break;
			       }
			   }
		       }
		 }
		;

const:         CHAR_CONST
                   {
		       atype_code = NC_CHAR;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = char_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = char_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = char_val;
			   break;
			 case NC_LONG:
			   *long_valp++ = char_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = char_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = char_val;
			   break;
		       }
		       valnum++;
		   }
	       | TERMSTRING
		   {
		       not_a_string = 0;
		       atype_code = NC_CHAR;
		       {
			   int len = strlen(termstring);

			   valnum += len;
			   if(valnum > var_len) {
			       derror("string won't fit in this variable");
			       exit (5);
			   }
			   switch (valtype) {
			     case NC_CHAR:
			       (void)strncpy(char_valp,termstring,len);
			       char_valp += len;
			       rec_cur = (void *) char_valp;
			       break;
			     case NC_BYTE:
			       (void)strncpy(byte_valp,termstring,len);
			       byte_valp += len;
			       rec_cur = (void *) byte_valp;
			       break;
			     case NC_SHORT:
			     case NC_LONG:
			     case NC_FLOAT:
			     case NC_DOUBLE:
			       derror("string value invalid for %s variable",
				      nctype(valtype));
			       break;
			   }
		       }
		   }
                | BYTE_CONST
                   {
		       atype_code = NC_BYTE;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = byte_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = byte_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = byte_val;
			   break;
			 case NC_LONG:
			   *long_valp++ = byte_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = byte_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = byte_val;
			   break;
		       }
		       valnum++;
		   }
                | SHORT_CONST
                   {
		       atype_code = NC_SHORT;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = short_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = short_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = short_val;
			   break;
			 case NC_LONG:
			   *long_valp++ = short_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = short_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = short_val;
			   break;
		       }
		       valnum++;
		   }
                | LONG_CONST
                   {
		       atype_code = NC_LONG;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = long_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = long_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = long_val;
			   break;
			 case NC_LONG:
			   *long_valp++ = long_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = long_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = long_val;
			   break;
		       }
		       valnum++;
		   }
                | FLOAT_CONST
                   {
		       atype_code = NC_FLOAT;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = float_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = float_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = float_val;
			   break;
			 case NC_LONG:
			   *long_valp++ = float_val;
			   break;
			 case NC_FLOAT:
			   *float_valp++ = float_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = float_val;
			   break;
		       }
		       valnum++;
		   }
                | DOUBLE_CONST
                   {
		       atype_code = NC_DOUBLE;
		       switch (valtype) {
			 case NC_CHAR:
			   *char_valp++ = double_val;
			   break;
			 case NC_BYTE:
			   *byte_valp++ = double_val;
			   break;
			 case NC_SHORT:
			   *short_valp++ = double_val;
			   break;
			 case NC_LONG:
			   *long_valp++ = double_val;
			   break;
			 case NC_FLOAT:
			   if (double_val == FILL_DOUBLE)
			     *float_valp++ = FILL_FLOAT;
			   else
			     *float_valp++ = double_val;
			   break;
			 case NC_DOUBLE:
			   *double_valp++ = double_val;
			   break;
		       }
		       valnum++;
		   }
                ;

/* END OF RULES */

%%

/* PROGRAMS */

/* get lexical input routine generated by lex  */
#include "ncgenyy.c"

void derror();

yyerror(s)	/* called for yacc syntax error */
     char *s;
{
	derror(s);
}

#ifndef yywrap
int
yywrap()			/* returns 1 on EOF if no more input */
{
    return  1;
}
#endif /* yywrap() */


/* Symbol table operations for ncgen tool */

YYSTYPE lookup(sname)       /* find sname in symbol table (linear search) */
char *sname;
{
    YYSTYPE sp;
    for (sp = symlist; sp != (YYSTYPE) 0; sp = sp -> next)
	if (STREQ(sp -> name, sname)) {
	    return sp;
	}
    return 0;			/* 0 ==> not found */
}

YYSTYPE install(sname)  /* install sname in symbol table */
char *sname;
{
    YYSTYPE sp;

    sp = (YYSTYPE) emalloc (sizeof (struct Symbol));
    sp -> name = (char *) emalloc (strlen (sname) + 1);/* +1 for '\0' */
    (void) strcpy (sp -> name, sname);
    sp -> next = symlist;	/* put at front of list */
    sp -> is_dim = 0;
    sp -> is_var = 0;
    sp -> is_att = 0;
    symlist = sp;
    return sp;
}

void
clearout()	/* reset symbol table to empty */
{
    YYSTYPE sp, tp;
    for (sp = symlist; sp != (YYSTYPE) 0;) {
	tp = sp -> next;
	free (sp -> name);
	free ((char *) sp);
	sp = tp;
    }
    symlist = 0;
}
