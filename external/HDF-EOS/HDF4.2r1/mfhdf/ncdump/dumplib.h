/*********************************************************************
 *   Copyright 1993, University Corporation for Atmospheric Research
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: dumplib.h,v 1.7 1997/11/05 19:40:37 koziol Exp $
 *********************************************************************/

extern char *progname;		/* for error messages */

#ifndef EXIT_FAILURE
#ifndef vms
#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1
#else
#define EXIT_SUCCESS 1
#define EXIT_FAILURE 0
#endif
#endif

#undef PROTO
#ifndef NO_HAVE_PROTOTYPES 
#   define	PROTO(x)	x
#else
#   define	PROTO(x)	()
#endif

#ifdef __cplusplus
extern "C" {
#endif

/* Print error message to stderr, don't exit */
#ifndef MIPSEL /* punt */
#ifndef NO_STDARG
extern void error(const char *fmt, ...);
#else
extern void error();
#endif
#endif

/*
extern void	error		PROTO((
				       char *fmt,
				       ...
				       ));
*/

/* set position in line before lput() calls */
extern void	set_indent	PROTO((
				       int indent
				       ));

/* set maximum line length */
extern void	set_max_len	PROTO((
				       int len
				       ));

/* splits lines to keep them short */
extern void	lput		PROTO((
				       const char *string
				       ));

/* In case different formats specified with -d option, set them here. */
extern void	set_formats	PROTO((
				       char *flt_fmt,
				       char *dbl_fmt
				       ));

/* Determine print format to use for each value for this variable. */
const char *		get_fmt		PROTO((
				       int ncid,
				       int varid,
				       nc_type type
				       ));

/* structure for list of variables specified with -v option */
struct vnode
{
    struct vnode* next;
    int id;
};
typedef struct vnode vnode;

/* Get new variable list */
extern vnode*	newvlist	PROTO((
				       void
				       ));

/* Add a variable id to variable list */
extern void	varadd		PROTO((
				       vnode* vlist,
				       int varid
				       ));

/* Test if a variable id is in variable list */
extern int	varmember	PROTO((
				       vnode* vlist,
				       int varid
				       ));

#ifdef __cplusplus
}
#endif
