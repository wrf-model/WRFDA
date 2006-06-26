/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: genlib.c,v 1.3 1997/11/05 19:40:44 koziol Exp $
 *********************************************************************/

#include <stdio.h>
#include <stdlib.h>
#ifndef NO_STDARG
#include	<stdarg.h>
#else
/* try varargs instead */
#include	<varargs.h>
#endif /* !NO_STDARG */

#include "ncgen.h"
#include "genlib.h"

int	lineno = 1;
int	derror_count = 0;


/*
 * For logging error conditions.
 */
#ifndef NO_STDARG
void
derror(const char *fmt, ...)
#else
/*VARARGS1*/
void
derror(fmt, va_alist)
     const char *fmt ;		/* error-message printf-style format */
     va_dcl			/* variable number of error args, if any */
#endif /* !NO_STDARG */
{
    va_list args ;


    if (lineno == 1)
      (void) fprintf(stderr,"%s: %s: ", progname, cdlname);
    else  
      (void) fprintf(stderr,"%s: %s line %d: ", progname, cdlname, lineno);

#ifndef NO_STDARG
    va_start(args ,fmt) ;
#else
    va_start(args) ;
#endif /* !NO_STDARG */

    (void) vfprintf(stderr,fmt,args) ;
    va_end(args) ;

    (void) fputc('\n',stderr) ;
    (void) fflush(stderr);	/* to ensure log files are current */
    derror_count++;
}


void *
emalloc (size)			/* check return from malloc */
int size;
{
    void   *p;

    if (size < 0) {
        derror ("negative arg to emalloc: %d", size);
	return 0;
    }
    p = (void *) malloc ((unsigned) size);
    if (p == 0) {
	derror ("out of memory\n");
	exit(3);
    }
    return p;
}

void *
erealloc (ptr,size)		/* check return from realloc */
     void *ptr;
     int size;
{
    void *p;

    if (size < 0) {
        derror ("negative arg to realloc");
	return 0;
    }
    p = (void *) realloc ((char *) ptr, (unsigned) size);

    if (p == 0) {
 	derror ("out of memory");
	exit(3);
    }
    return p;
}
