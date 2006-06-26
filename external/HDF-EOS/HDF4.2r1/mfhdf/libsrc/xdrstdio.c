#if !defined(lint)
static char rcsid[] = "$Id: xdrstdio.c,v 1.2 1993/05/03 21:25:17 chouck Exp $";
#endif

/*
 * xdr_stdio.c, XDR implementation on standard i/o file.
 *
 * This set of routines implements a XDR on a stdio stream.
 * XDR_ENCODE serializes onto the stream, XDR_DECODE de-serializes
 * from the stream.
 *
 * Based on Sun sources from the portable distribution:
 *
 * @(#)xdr_stdio.c	1.1 87/11/04 3.9 RPCSRC
 * @(#)xdr_stdio.c 1.16 87/08/11 Copyr 1984 Sun Micro
 * Copyright (C) 1984, Sun Microsystems, Inc.
 *
 *  UCAR/Unidata modifications for netcdf:
 *  	_destroy closes the stream.
 *      current stream position cached in x_public
 *			 (assumes caddr_t can hold a long)
 *      last op cached in x_handy
 */

#include <stdio.h>
#include "local_nc.h" /* prototypes for NCadvis, nc_error */
		      /* also obtains <stdio.h>, <rpc/types.h>, &
		       * <rpc/xdr.h> */
#ifdef DOS_FS
#include    <fcntl.h>   /* O_BINARY */
#define USE_BFLAG
#endif

#ifdef USE_XDRNCSTDIO


#define XDRNC_LASTOP(xdrs) (*((enum xdr_op*)&((xdrs)->x_handy)))
#define XDRNC_POS(xdrs) (*((long*)&((xdrs)->x_public)))


static bool_t	xdrNCstdio_getlong();
static bool_t	xdrNCstdio_putlong();
static bool_t	xdrNCstdio_getbytes();
static bool_t	xdrNCstdio_putbytes();
static u_long	xdrNCstdio_getpos();
static bool_t	xdrNCstdio_setpos();
static long *	xdrNCstdio_inline();
static void	xdrNCstdio_destroy();

/*
 * Ops vector for stdio type XDR
 */
static struct xdr_ops	xdrNCstdio_ops = {
	xdrNCstdio_getlong,	/* deserialize a long int */
	xdrNCstdio_putlong,	/* serialize a long int */
	xdrNCstdio_getbytes,	/* deserialize counted bytes */
	xdrNCstdio_putbytes,	/* serialize counted bytes */
	xdrNCstdio_getpos,	/* get offset in the stream */
	xdrNCstdio_setpos,	/* set offset in the stream */
	xdrNCstdio_inline,	/* prime stream for inline macros */
	xdrNCstdio_destroy	/* destroy stream */
};

/*
 * Initialize a stdio xdr stream.
 * Sets the xdr stream handle xdrs for use on the stream file.
 * Operation flag is set to op.
 */
void
xdrNCstdio_create(xdrs, file, op)
	XDR *xdrs;
	FILE *file;
	enum xdr_op op;
{

	xdrs->x_op = op;
	xdrs->x_ops = &xdrNCstdio_ops;
	xdrs->x_private = (caddr_t)file;
	xdrs->x_handy = 0;
	xdrs->x_base = 0;
	XDRNC_LASTOP(xdrs) = XDR_FREE ;
	XDRNC_POS(xdrs) = ftell(file) ;
}

/*
 * Destroy a NCstdio xdr stream.
 * Cleans up the xdr stream handle xdrs previously set up by xdrNCstdio_create.
 * This function closes the stream.
 */
static void
xdrNCstdio_destroy(xdrs)
	XDR *xdrs;
{
	(void)fclose((FILE *)xdrs->x_private);
}

static bool_t
xdrNCstdio_getlong(xdrs, lp)
	XDR *xdrs;
	long *lp;
{
	if (fread((caddr_t)lp, sizeof(long), 1, (FILE *)xdrs->x_private) != 1)
	{
		XDRNC_POS(xdrs) = ftell((FILE *)xdrs->x_private);
		return (FALSE);
	}
#ifdef SWAP
	*lp = ntohl(*lp);
#endif
	XDRNC_POS(xdrs) += sizeof(long) ;
	return (TRUE);
}

static bool_t
xdrNCstdio_putlong(xdrs, lp)
	XDR *xdrs;
	long *lp;
{
#ifdef SWAP
	long mycopy = htonl(*lp);
	lp = &mycopy;
#endif
	if (fwrite((caddr_t)lp, sizeof(long), 1, (FILE *)xdrs->x_private) != 1)
	{
		XDRNC_POS(xdrs) = ftell((FILE *)xdrs->x_private);
		return (FALSE);
	}
	XDRNC_POS(xdrs) += sizeof(long) ;
	return (TRUE);
}

static bool_t
xdrNCstdio_getbytes(xdrs, addr, len)
	XDR *xdrs;
	caddr_t addr;
	u_int len;
{
	if ((len != 0) && (fread(addr, (int)len, 1, (FILE *)xdrs->x_private) != 1))
	{
		XDRNC_POS(xdrs) = ftell((FILE *)xdrs->x_private);
		return (FALSE);
	}
	XDRNC_POS(xdrs) += len ;
	return (TRUE);
}

static bool_t
xdrNCstdio_putbytes(xdrs, addr, len)
	XDR *xdrs;
	caddr_t addr;
	u_int len;
{
	if ((len != 0) && (fwrite(addr, (int)len, 1, (FILE *)xdrs->x_private) != 1))
	{
		XDRNC_POS(xdrs) = ftell((FILE *)xdrs->x_private);
		return (FALSE);
	}
	XDRNC_POS(xdrs) += len ;
	return (TRUE);
}

static u_long
xdrNCstdio_getpos(xdrs)
	XDR *xdrs;
{
	XDRNC_POS(xdrs) = ftell((FILE *)xdrs->x_private);
	return ((u_long)XDRNC_POS(xdrs));
}

static bool_t
xdrNCstdio_setpos(xdrs, pos) 
	XDR *xdrs;
	u_long pos;
{ 
	if(xdrs->x_op == XDRNC_LASTOP(xdrs) && pos == XDRNC_POS(xdrs))
		return TRUE ;
	/* else */
	XDRNC_LASTOP(xdrs) = xdrs->x_op ;
	if (fseek((FILE *)xdrs->x_private,(long)pos, 0) < 0)
	{
		XDRNC_POS(xdrs) = ftell((FILE *)xdrs->x_private);
		return (FALSE);
	}
	/* else */
	XDRNC_POS(xdrs) = pos ;
	return TRUE ;
}

/*ARGSUSED*/
static long *
xdrNCstdio_inline(xdrs, len)
	XDR *xdrs;
	u_int len;
{

	/*
	 * Must do some work to implement this: must insure
	 * enough data in the underlying stdio buffer,
	 * that the buffer is aligned so that we can indirect through a
	 * long *, and stuff this pointer in xdrs->x_buf.  Doing
	 * a fread or fwrite to a scratch buffer would defeat
	 * most of the gains to be had here and require storage
	 * management on this buffer, so we don't do this.
	 */
	return (NULL);
}
#else

/*
 * Destroy a stdio xdr stream.
 * Cleans up the xdr stream handle xdrs previously set up by xdrstdio_create.
 * This function closes the stream, while the default
 * library function, xdrstdio_destroy() which merely flushes it.
 */
static void
xdrNCstdio_destroy(xdrs)
	XDR *xdrs;
{
	(void)fclose((FILE *)xdrs->x_private);
}

static bool_t
xdrNCstdio_setpos(xdrs, pos) 
	XDR *xdrs;
	u_long pos;
{ 

	static XDR *last = NULL ;
	static enum xdr_op lastop = XDR_FREE ;

	if(xdrs == last && xdrs->x_op == lastop && pos == xdr_getpos(xdrs))
		return TRUE ;
	/* else */
	last = xdrs ;
	lastop = xdrs->x_op ;
	if (fseek((FILE *)xdrs->x_private,(long)pos, 0) < 0)
	{
		return (FALSE);
	}
	/* else */
	return TRUE ;
}

#endif /* USE_XDRNCSTDIO */


/********/

#include <unistd.h> /* access(), F_OK */
#ifndef F_OK
#define F_OK    0
#endif

#include "netcdf.h" /* NC_ */
#include "local_nc.h" /* prototypes for NCadvis, nc_error */

#ifndef USE_BFLAG 	/* Doesn't Understand the "b" (binary flag to fopen) */
#define CREAT_FOPENFLAGS "w+"
#define WRITE_FOPENFLAGS "r+"
#define NOWRITE_FOPENFLAGS "r"
#else
#define CREAT_FOPENFLAGS "w+b"
#define WRITE_FOPENFLAGS "r+b"
#define NOWRITE_FOPENFLAGS "rb"
#endif /* !USE_BFLAG */


/*
 * "sync" (flush) xdr stream.
 */
int
NCxdrfile_sync(xdrs)
	XDR *xdrs;
{
	/* assumes xdrstdio, violates layering */
	FILE *fp = (FILE *)xdrs->x_private ;
	if(fflush(fp) == 0)
		return 0 ; /* success */
	/* else, failure */
	return -1 ; 
}


int
NCxdrfile_create(xdrs, path, ncmode)
XDR *xdrs ;
const char *path ;
int ncmode ;
{
	char *fmode ;
	FILE *fp ;
	enum xdr_op op ;

	switch(ncmode & 0x0f) {
	case NC_NOCLOBBER :
		if( access(path, F_OK) != -1 )
		{
			NCadvise(NC_EEXIST, "\"%s\": File exists", path) ;
			return(-1) ;
		}
		/* fall into */
	case NC_CLOBBER :
		fmode = CREAT_FOPENFLAGS ;
		break ;
	case NC_WRITE :
		fmode = WRITE_FOPENFLAGS ;
		break ;
	case NC_NOWRITE :
		fmode = NOWRITE_FOPENFLAGS ;
		break ;
	default:
		NCadvise(NC_EINVAL, "Bad flag %0x", ncmode & 0x0f) ;
		return(-1) ;
	}

#ifdef DOS_FS
	/*
 	 * set default mode to binary to suppress the expansion of
	 * 0x0f into CRLF
	 */
	if(_fmode != O_BINARY)
		_fmode = O_BINARY ;
#endif
	fp = fopen(path, fmode) ;
	if( fp == NULL )
	{
		nc_serror("filename \"%s\"", path) ;
		return(-1) ;
	} /* else */

	if( ncmode & NC_CREAT )
	{
		op = XDR_ENCODE ;
	} else {
		op = XDR_DECODE ;
	}
	
#ifndef USE_XDRNCSTDIO
	xdrstdio_create(xdrs, fp, op) ;
	xdrs->x_ops->x_destroy = xdrNCstdio_destroy ;
	xdrs->x_ops->x_setpostn = xdrNCstdio_setpos ;
#else
	xdrNCstdio_create(xdrs, fp, op) ;
#endif

	return fileno(fp) ;
}
