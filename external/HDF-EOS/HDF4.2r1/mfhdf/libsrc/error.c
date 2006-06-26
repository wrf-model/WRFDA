/*
 *	Copyright 1993, University Corporation for Atmospheric Research
 *      See netcdf/COPYRIGHT file for copying and redistribution conditions.
 */
/* $Id: error.c,v 1.14 90/02/23 16:08:55 davis Exp */


/* 
 *   Utility Functions to implement consistant error logging
 * mechanisms for netcdf
 */

/*LINTLIBRARY*/

#ifdef NO_STDARG  /* The 4.0 release should be ANSI compliant */
#undef NO_STDARG
#endif

#include	"local_nc.h"
#include	<stdio.h>
#ifndef NO_STDARG
#include	<stdarg.h>
#else
/* try varargs instead */
#include	<varargs.h>
#endif /* !NO_STDARG */

#include <errno.h>
#if defined ERRNO_MISSING
extern int errno;
#endif

#ifdef VMS
#include <ssdef.h>
#endif

#ifndef NO_STRERROR
#include <string.h> /* contains prototype for ansi libc function strerror() */
#else
/* provide a strerror function for older unix systems */
char *
strerror(errnum)
int errnum ;
{
	extern int sys_nerr;
	extern const char * const sys_errlist[];

	if (errnum < 0 || errnum >= sys_nerr)
		return NULL ;

	return (char *)sys_errlist[errnum] ;
}
#endif /* NO_STRERROR */


#ifdef vms
/*
 * On the vms system, when a system error occurs which is not
 * mapped into the unix styled errno values, errno is set EVMSERR
 * and a VMS error code is set in vaxc$errno.
 * This routine prints the systems message associated with status return
 * from a system services call.
 */

#include <descrip.h>
#include <ssdef.h>

static int
vmserror1( int status )
{
	short msglen ;
	char msgbuf[256] ;
	$DESCRIPTOR(message, msgbuf) ;
	register ret ;

	ret = SYS$GETMSG(status, &msglen, &message, 15, 0) ;
	
	switch(ret) {
	case SS$_BUFFEROVF :
	case SS$_NORMAL :
		(void) fprintf(stderr, "%.*s\n", msglen, msgbuf) ;
		break ;
	default :
		break ;
	}
	return(ret) ;
}
#endif /* vms */


#ifdef USE_doprnt_FOR_vfprintf
/* 
 * kludge for ze ancienne regieme
 */
vfprintf(stream, fmt, va_alist)
     FILE *stream;
     char *fmt;
     va_dcl
{

  return _doprnt(fmt, va_alist, stream);

}
#endif


/*
 * Log SYSTEM errors
 * Use where you would want to call perror(3).
 * Calling sequence is
 *	nc_serror(format, arg1, arg2,...)
 * with zero or more args of types compatible with the associated format
 * specifiers.  For example:
 *         nc_serror("shutting down");
 *	   nc_serror("can't open %s", file_name);
 *         nc_serror("process %d in state %s",pid,state);
 */
void
#ifndef NO_STDARG
nc_serror(const char *fmt, ...)
#else
/*VARARGS1*/
nc_serror(fmt, va_alist)
     const char *fmt ;
     va_dcl
#endif /* !NO_STDARG */
{
    
    if( ncopts & NC_VERBOSE ) {
    	va_list args ;
        static const char unknown[] = "Unknown Error";
    	int errnum = errno;	/* save real errno in case we wipe it out */
    	const char * cp;

#ifndef NO_STDARG
        va_start(args, fmt) ;
#else
        va_start(args) ;
#endif /* !NO_STDARG */

        (void) fprintf(stderr,"%s: ", cdf_routine_name);
        (void) vfprintf(stderr,fmt,args) ;
        va_end(args) ;
        
        switch(errnum) {
	case 0 :
            ncerr = NC_NOERR ;
            (void) fputc('\n',stderr) ;
            break ;
#ifdef vms
	case EVMSERR :
            ncerr = NC_SYSERR ;
            (void) fputc(': ',stderr) ;
            (void) vmserror1(vaxc$errno) ;
            break ;
#endif /* vms */
            default :
		ncerr = NC_SYSERR ;
            (void) fprintf(stderr,": %s\n",
                           (cp = strerror(errnum)) == NULL ? unknown : cp ) ;
            break ;
        }

        (void) fflush(stderr);	/* to ensure log files are current */
        errno = 0 ; /* ??? */
    } /* NC_VERBOSE */
    
    if( ncopts & NC_FATAL ) {
	exit(ncopts) ;
    }
}

/*
 * Like nc_serror above, but doesn't check for system error.
 * Use for logging error conditions which are not system errors.
 * Calling sequence is
 *	NCadvise(ncerr, format, arg1, arg2,...)
 * with zero or more args of types compatible with the associated format
 * specifiers.  For example:
 *         NCadvise(NC_NOERR, "just advice");
 *         NCadvise(NC_EBADID, "%d is not a valid cdf id", cdfid);
 */
#ifndef NO_STDARG
void
NCadvise(int err, const char *fmt,...)
#else
/*VARARGS1*/
void
NCadvise(err, fmt, va_alist)
     int err ;
     const char *fmt ;
     va_dcl
#endif /* !NO_STDARG */
{
	va_list args ;

	ncerr = err ;

	if( ncopts & NC_VERBOSE )
	{
		(void) fprintf(stderr,"%s: ", cdf_routine_name);
#ifndef NO_STDARG
		va_start(args ,fmt) ;
#else
		va_start(args) ;
#endif /* !NO_STDARG */
		(void) vfprintf(stderr,fmt,args) ;
		va_end(args) ;
		(void) fputc('\n',stderr) ;
		(void) fflush(stderr);	/* to ensure log files are current */
	}

	if( (ncopts & NC_FATAL) && ncerr != NC_NOERR )
	{
		exit(ncopts) ;
	}
}
