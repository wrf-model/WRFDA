/*********************************************************************
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: vardata.h,v 1.2 1996/03/26 22:34:11 georgev Exp $
 *********************************************************************/

extern char *progname;		/* for error messages */

#undef PROTO
#ifndef NO_HAVE_PROTOTYPES 
#   define	PROTO(x)	x
#else
#   define	PROTO(x)	()
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Output the data for a single variable, in CDL syntax. */
extern int	vardata		PROTO((
				       struct ncvar*, /* variable */
				       long [], /* variable dimension sizes */
				       int, /* netcdf id */
				       int, /* variable id */
				       struct fspec* /* formatting specs */
				       ));

#ifdef __cplusplus
}
#endif
