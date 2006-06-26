/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: genlib.h,v 1.8 1997/11/05 19:40:45 koziol Exp $
 *********************************************************************/

extern char	*progname;	/* for error messages */
extern char	*cdlname;	/* for error messages */

#undef PROTO
#ifndef NO_HAVE_PROTOTYPES 
#   define	PROTO(x)	x
#else
#   define	PROTO(x)	()
#endif

#ifdef __cplusplus
extern "C" {
#endif

#ifndef MIPSEL /* punt */
#ifndef NO_STDARG
extern void derror(const char *fmt, ...);
#else
extern void derror();
#endif
#endif

/*
extern void	derror		PROTO((
				       char *fmt,
				       ...
				       ));
*/

extern void	*emalloc	PROTO((
				       int size
				       ));
extern void	*erealloc	PROTO((
				       void *ptr,
				       int size
				       ));
extern void     usage           PROTO((
                                       void
                                       ));

extern void     yyerror         PROTO((
                                       char *
                                       ));

extern int      yyparse         PROTO((
                                       void
                                       ));

extern void     put_variable    PROTO((
                                       void *
                                       ));
#ifdef OLD_WAY
extern int      getopt          PROTO((
                                       int  argc,
                                       char **argv,
                                       char *opts
                                       ))
#endif

/* generate.c */
void cline(const char *stmnt);
void fline(const char *stmnt);
const char *nctype(nc_type);
const char *ncctype(nc_type);
char *cstrstr(char *, long);
char *fstrstr(char *, long);
char *fstring(nc_type, void *, int);
void define_netcdf(char *netcdfname);

/* load.c */
void load_netcdf(void *rec_start);

/* getfill.c */
void nc_fill(nc_type , long , void *, union generic );
void nc_getfill(nc_type , union generic *);
void nc_putfill(nc_type , void *, union generic *);

/* init.c */
void init_netcdf(void);

/* close.c */
void close_netcdf(void);

#ifdef __cplusplus
}
#endif
