/*
 *	Copyright 1993, University Corporation for Atmospheric Research
 *      See netcdf/COPYRIGHT file for copying and redistribution conditions.
 */
/*	$Id: file.c,v 1.14 1997/11/05 19:40:13 koziol Exp $ */

#ifdef DEBUG
#include <assert.h>
#endif /* DEBUG */

#include	<string.h>
#include	"local_nc.h"
#include	"alloc.h"
#ifdef vms
#   include <unixio.h>
#   include <unixlib.h>
#   include <file.h>
#endif

static int _ncdf = 0 ; /*  high water mark on open cdf's */
static NC *_cdfs[MAX_NC_OPEN] ;
#define HNDLE(id) (((id) >= 0 && (id) < _ncdf) ? _cdfs[(id)] : NULL)
#define STASH(id) (((id) >= 0 && (id) < _ncdf) ?  \
		HNDLE(_cdfs[(id)]->redefid) : NULL)

#ifdef NO_STDC_REMOVE
/* try unix 'unlink' */
#define remove(fpath) unlink((fpath))
#endif

#ifdef DOS_FS
#define SEP '\\' /* this separates path components on DOS */
#endif
#ifndef SEP
#define SEP '/' /* default, unix */
#endif

/*
 *  Check validity of cdf handle, return pointer to NC struct or
 * NULL on error.
 */
NC *
NC_check_id(cdfid)
int cdfid ;
{
	NC *handle ;
	
	handle = ( cdfid >= 0 && cdfid < _ncdf) ? _cdfs[cdfid] : NULL ;
	if(handle == NULL)
	{
		NCadvise(NC_EBADID, "%d is not a valid cdfid", cdfid) ;
		return(NULL) ;
	}
	return(handle) ;
}


/*
 *  Check to see if in define mode.
 * If 'iserr' arg is true, advise.
 */
bool_t
NC_indefine(cdfid, iserr) /* Should be a Macro ? */
int cdfid ;
bool_t iserr ;
{
	bool_t ret  ;
	ret = (cdfid >= 0 && cdfid < _ncdf) ?
		(bool_t)(_cdfs[cdfid]->flags & NC_INDEF) : FALSE ;
	if(!ret && iserr)
	{
		if(cdfid < 0 || cdfid >= _ncdf)
			NCadvise(NC_EBADID, "%d is not a valid cdfid", cdfid) ;
		else
			NCadvise(NC_ENOTINDEFINE, "%s Not in define mode",
				_cdfs[cdfid]->path) ;
	}
	return(ret) ;
}


/*
 *  Common code for ncopen and nccreate.
 */
static int
NC_open(path, mode )
const char	*path ;	/* file name */
int mode ;
{
	NC *handle ;
	int id ;


	/* find first available id */
	for(id = 0 ; id < _ncdf; id++)
		if( _cdfs[id] == NULL) break ;

	if(id == _ncdf && _ncdf >= MAX_NC_OPEN) /* will need a new one */
      {
          NCadvise(NC_ENFILE, "maximum number of open cdfs %d exceeded",
                   _ncdf) ;
          return(-1) ;
      }

	handle = NC_new_cdf(path, mode) ;
	if( handle == NULL)
      {
          if((mode & 0x0f) == NC_CLOBBER)
            {
                if( remove(path) != 0 )
                    nc_serror("couldn't remove filename \"%s\"", path) ;
            }
          return(-1) ;
      }

	(void) strncpy(handle->path, path, FILENAME_MAX) ;
	_cdfs[id] = handle ;
	if(id == _ncdf)
		_ncdf++ ;
	return(id) ;
}


int nccreate(path, cmode)
const char	*path ;	/* file name */
int 		cmode ;
{
	cdf_routine_name = "nccreate" ;

	if(cmode & NC_CREAT)
	{
		return(NC_open(path, cmode)) ;
	}
	NCadvise(NC_EINVAL, "Bad Flag") ;
	return(-1) ;
}


int ncopen(path,mode)
const char	*path ;	/* file name */
int 		mode ;
{
	cdf_routine_name = "ncopen" ;
	if(mode & NC_CREAT)
	{
		NCadvise(NC_EINVAL, "Bad Flag") ;
		return(-1) ;
	}
	return(NC_open(path, mode)) ;
}


int ncsync(id)
int id ;
{
	NC *handle ;

	cdf_routine_name = "ncsync" ;

	handle = NC_check_id(id) ; 
	if(handle == NULL)
		return(-1) ;

	if( handle->flags & NC_INDEF )
      {
          NCadvise(NC_EINDEFINE, "Unfinished definition") ;
          return(-1) ;
      }

	if(handle->flags & NC_RDWR)
      {
          handle->xdrs->x_op = XDR_ENCODE ;
          if(handle->flags & NC_HDIRTY)
            {
                if(!xdr_cdf(handle->xdrs, &handle) )
                    return(-1) ;
                handle->flags &= ~(NC_NDIRTY | NC_HDIRTY) ;
            }
          else if(handle->flags & NC_NDIRTY)
            {
                if(!xdr_numrecs(handle->xdrs, handle) )
                    return(-1) ;
#ifdef HDF
                if (handle->file_type != HDF_FILE)
#endif
                    handle->flags &= ~(NC_NDIRTY) ;
            }
      } 
    else /* read only */
      {
          /* assert(handle->xdrs->x_op == XDR_DECODE) ; */
          /* free the stuff in handle that xdr_cdf allocates */
          handle->xdrs->x_op = XDR_FREE ;
          (void) xdr_cdf(handle->xdrs, &handle) ;
          handle->xdrs->x_op = XDR_DECODE ;
 
          if(!xdr_cdf(handle->xdrs, &handle) )
            {
                nc_serror("xdr_cdf") ;
                NC_free_cdf(handle) ; /* ?? what should we do now? */
                return(-1) ;
            }
          if( NC_computeshapes(handle) == -1)
              return(-1) ;
      }

	(void) NCxdrfile_sync(handle->xdrs) ;

	return(0) ;
}


/*
 *  In data mode, same as ncclose ;
 * In define mode, restore previous definition ;
 * In create, remove the file ;
 */
int ncabort(cdfid)
int cdfid ;
{
	NC *handle ;
	char path[FILENAME_MAX + 1] ;
	unsigned flags ;
#ifdef HDF
    intn   file_type;
#endif

	cdf_routine_name = "ncabort" ;


	handle = NC_check_id(cdfid) ; 
	if(handle == NULL)
		return(-1) ;

	flags = handle->flags ; /* need to save past free_cdf */

	/* NC_CREAT implies NC_INDEF, in both cases need to remove handle->path */
	if(flags & (NC_INDEF | NC_CREAT))
      {
          (void)strncpy(path, handle->path, FILENAME_MAX) ; /* stash path */
          if(!(flags & NC_CREAT)) /* redef */
            {
                NC_free_cdf(STASH(cdfid)) ;

                _cdfs[handle->redefid] = NULL ;
                if(handle->redefid == _ncdf - 1)
                    _ncdf-- ;
                handle->redefid = -1 ;
            }
      }
	else if(handle->flags & NC_RDWR)
      {
          handle->xdrs->x_op = XDR_ENCODE ;
          if(handle->flags & NC_HDIRTY)
            {
                if(!xdr_cdf(handle->xdrs, &handle) )
                    return(-1) ;
            }
          else if(handle->flags & NC_NDIRTY)
            {
                if(!xdr_numrecs(handle->xdrs, handle) )
                    return(-1) ;
            }
      }


#ifdef HDF
    file_type = handle->file_type;
#endif

	NC_free_cdf(handle) ; /* calls fclose */


#ifdef HDF
    switch(file_type) 
      {

      case netCDF_FILE:
          if(flags & (NC_INDEF | NC_CREAT))
            {
                if( remove(path) != 0 )
                    nc_serror("couldn't remove filename \"%s\"", path) ;
            }
          break;
      case HDF_FILE:
          if(flags & NC_CREAT)
            {
                if( remove(path) != 0 )
                    nc_serror("couldn't remove filename \"%s\"", path) ;
            }
          break;
      }
#else
    if(flags & (NC_INDEF | NC_CREAT))
      {
          if( remove(path) != 0 )
              nc_serror("couldn't remove filename \"%s\"", path) ;
      }
#endif

	_cdfs[cdfid] = NULL ;

	if(cdfid == _ncdf - 1)
		_ncdf-- ;

	return(0) ;
}


/* 
 * Deprecated function ;
 */
int ncnobuf(cdfid)
int cdfid ;
{
	NC *handle ;

	cdf_routine_name = "ncnobuf" ;

	handle = NC_check_id(cdfid) ; 
	if(handle == NULL)
		return(-1) ;
/* NOOP */
	return(0);
}


/*
 * Given the path to a file "proto",
 * we replace the filename component with
 * a name like one would get from tmpnam(3S).
 * (Many implementations of tmpnam insist on giving us
 * a directory like /usr/tmp as well. Since we are making a copy which we
 * will eventually rename() back to proto, we want the return of NCtempname
 * and proto to dwell on the same filesystem.)
 */
static char *
NCtempname(proto)
const char *proto ;
{
/* NO_ACCESS defined if the OS lacks the access() function */
#ifndef NO_ACCESS
#	define TN_NACCES 1
#else 
#	define TN_NACCES 0
#endif /* !NO_ACCESS */
/* NO_GETPID defined if the OS lacks the getpid() function */
#ifndef NO_GETPID
#	define TN_NDIGITS 4
    unsigned int pid ; /* OS/2 DOS (MicroSoft Lib) allows "negative" int pids */
#else
#	define TN_NDIGITS 0
#endif /* !NO_GETPID */

	static char seed[] = {'a','a','a', '\0'} ;
#define TN_NSEED (sizeof(seed) -1)
	static char tnbuf[FILENAME_MAX +1] ;
	char *begin, *cp, *sp ;

	/* assert(TN_NSEED > 0) ; */
	strcpy(tnbuf,proto) ;

#ifdef SEP
	if ((begin = strrchr(tnbuf, SEP)) == NULL)
		begin = tnbuf ;
	else
	    begin++ ;

	if(&tnbuf[FILENAME_MAX] - begin <= TN_NSEED + TN_NACCES + TN_NDIGITS)
      {
          /* not big enough */
          tnbuf[0] = '\0' ;
          return tnbuf ;
      }
#else
	begin = tnbuf ;
#endif /* SEP */

	*begin = '\0' ;
	(void) strcat(begin, seed) ;

	cp = begin + TN_NSEED + TN_NACCES + TN_NDIGITS ;
#ifndef NO_GETPID
	*cp = '\0' ;
	pid = getpid() ;
	while(--cp >= begin + TN_NSEED + TN_NACCES)
      {
          *cp = (pid % 10) + '0' ;
          pid /= 10 ;
      }
#else
	*cp-- = '\0' ;
#endif /* !NO_GETPID */

	/* update seed for next call */
	sp = seed ;
	while(*sp == 'z')
		*sp++ = 'a' ;
	if(*sp != '\0')
		++*sp ;

#ifndef NO_ACCESS
	for(*cp = 'a' ; access(tnbuf, 0) == 0 ; )
      {
          if(++*cp > 'z')
            {
                /* ran out of tries */
                tnbuf[0] = '\0' ;
                return tnbuf ;
            }
      }
#endif /* !NO_ACCESS */

	return tnbuf ;
}


int ncredef(cdfid)
int cdfid ;
{
	NC *handle ;
	NC *new ;
	int id ;
	char *scratchfile ;

	cdf_routine_name = "ncredef" ;

	handle = NC_check_id(cdfid) ; 
	if(handle == NULL)
		return(-1) ;
	if( handle->flags & NC_INDEF) /* in define mode already */
      {
          NC *stash = STASH(cdfid) ;
          if(stash) NCadvise(NC_EINDEFINE, "%s: in define mode aleady",
                             stash->path) ;
          return(-1) ;
      }
	if(!(handle->flags & NC_RDWR))
      {
          NCadvise(NC_EPERM, "%s: NC_NOWRITE", handle->path) ;
          return(-1) ;
      }


#ifdef HDF
    if(handle->file_type == HDF_FILE) 
      {
          handle->flags |= NC_INDEF ;
          handle->redefid = TRUE;
          return(0);
      }
#endif

	/* find first available id */
	for(id = 0 ; id < _ncdf; id++)
		if( _cdfs[id] == NULL) break ;

	if(id == _ncdf && _ncdf >= MAX_NC_OPEN) /* will need a new one */
      {
          NCadvise(NC_ENFILE, "maximum number of open cdfs %d exceeded",
                   _ncdf) ;
          return(-1) ;
      }

	if( ncopts & NC_NOFILL )
      {
          /* fill last record */
          handle->xdrs->x_op = XDR_ENCODE ;
          if(handle->flags & NC_NDIRTY)
            {
                if(!xdr_numrecs(handle->xdrs, handle) )
                    return(-1) ;
                handle->flags &= ~(NC_NDIRTY) ;
            }
      }

	scratchfile = NCtempname(handle->path) ;

	new = NC_dup_cdf(scratchfile, NC_NOCLOBBER, handle) ;
	if(new == NULL)
      {
          return(-1) ;
      }

	handle->flags |= NC_INDEF ;
	(void) strncpy(new->path, scratchfile, FILENAME_MAX) ;

	/* put the old handle in the new id */
	_cdfs[id] = handle ;
	if(id == _ncdf)
		_ncdf++ ;

	/* put the new handle in old id */
	_cdfs[cdfid] = new ;

	new->redefid = id ;

	return(0) ;
}


/*
 * Compute offsets and put into the header
 */
static void
NC_begins(handle)
NC *handle ;
{
	int ii ;
	u_long index = 0 ;
	NC_var **vpp ;
	NC_var *last = NULL ;

	if(handle->vars == NULL) 
		return ;

	index = NC_xlen_cdf(handle) ;

	/* loop thru vars, first pass is for the 'non-record' vars */
	vpp = (NC_var **)handle->vars->values ;
	for(ii = 0 ; ii < handle->vars->count ; ii++, vpp++)
      {
          if( IS_RECVAR(*vpp) )
            {
                continue ;	/* skip record variables on this pass */
            }

          (*vpp)->begin = index ;
          index += (*vpp)->len ;
#ifdef EDEBUG
          NCadvise(NC_NOERR, "%s pass 1 begin %d, length %d",
                   (*vpp)->name->values, (*vpp)->begin, (*vpp)->len) ;
#endif /* EDEBUG */
      }

	handle->begin_rec = index ;
	handle->recsize = 0 ;

	/* loop thru vars, second pass is for the 'non-record' vars */
	vpp = (NC_var **)handle->vars->values ;
	for(ii = 0 ; ii < handle->vars->count ; ii++, vpp++)
      {
          if( !IS_RECVAR(*vpp) )
            {
                continue ;	/* skip non-record variables on this pass */
            }

          (*vpp)->begin = index ;
#ifdef EDEBUG
          NCadvise(NC_NOERR, "%s pass 2 begin %d, len %d, *dsizes %d",
                   (*vpp)->name->values, (*vpp)->begin, index, (*vpp)->len, *(*vpp)->dsizes ) ;
#endif /* EDEBUG */
          index += (*vpp)->len ;
          handle->recsize += (*vpp)->len ;
          last = (*vpp) ;
      }
	/*
	 * for special case of exactly one record variable, pack values
	 */
	if(last != NULL && handle->recsize == last->len)
		handle->recsize = *last->dsizes ;
	handle->numrecs = 0 ;
}


/*
 * Copy nbytes bytes from source to target.
 * Streams target and source should be positioned before the call.
 * opaque I/O, no XDR conversion performed (or needed).
 * The Macros XDR_GETBYTES and XDR_PUTBYTES may not be
 * supported on your xdr implementation. If not, calls
 * to xdr_opaque may be used.
 */
bool_t
NC_dcpy( target, source, nbytes)
XDR *target ;
XDR *source ;
long nbytes ;
{
/* you may wish to tune this: big on a cray, small on a PC? */
#define NC_DCP_BUFSIZE 8192
	char buf[NC_DCP_BUFSIZE] ;

	while(nbytes > sizeof(buf))
	{
		if(!XDR_GETBYTES(source, buf, sizeof(buf)))
			goto err ;
		if(!XDR_PUTBYTES(target, buf, sizeof(buf)))
			goto err ;
		nbytes -= sizeof(buf) ;
	}
	/* we know nbytes <= sizeof(buf) at this point */
	if(!XDR_GETBYTES(source, buf, nbytes))
		goto err ;
	if(!XDR_PUTBYTES(target, buf, nbytes))
		goto err ;
	return(TRUE) ;
err:
	NCadvise(NC_EXDR, "NC_dcpy") ;
	return(FALSE) ;
}


/*
 * XDR the data of varid in old, target is the new xdr strm
 */
static bool_t
NC_vcpy( target, old, varid)
XDR *target ;
NC *old ;
int varid ;
{
	NC_var **vpp ;
	vpp = (NC_var **)old->vars->values ;
	vpp += varid ;

	if( !xdr_setpos(old->xdrs, (*vpp)->begin) )
      {
          NCadvise(NC_EXDR, "NC_vcpy: xdr_setpos") ;
          return(FALSE) ;
      }
		
	return(NC_dcpy(target, old->xdrs, (*vpp)->len)) ;
}


/*
 * XDR the data of (varid, recnum) in old, target is the new xdr strm
 */
static bool_t
NC_reccpy( target, old, varid, recnum)
XDR *target ;
NC *old ;
int varid ;
int recnum ;
{
	NC_var **vpp ;
	vpp = (NC_var **)old->vars->values ;
	vpp += varid ;

	if( !xdr_setpos(old->xdrs,
                    (*vpp)->begin + old->recsize*recnum )){
		NCadvise(NC_EXDR, "NC_reccpy: xdr_setpos") ;
		return(FALSE) ;
	}
	
	return(NC_dcpy(target, old->xdrs, (*vpp)->len)) ;
}


/*
 *  Common code for ncendef, ncclose(endef)
 */
static
int NC_endef( cdfid, handle )
int cdfid ;
NC *handle ;
{
	XDR *xdrs ;
	int ii ;
	int jj = 0 ;
	NC_var **vpp ;
	NC *stash = STASH(cdfid) ; /* faster rvalue */

#ifdef HDF
    if(handle->file_type != HDF_FILE)
#endif	
        NC_begins(handle) ;

	xdrs = handle->xdrs ;
	xdrs->x_op = XDR_ENCODE ;

	if(!xdr_cdf(xdrs, &handle) )
      {
          nc_serror("xdr_cdf") ;
          return(-1) ;
      }

#ifdef HDF
    /* Get rid of the temporary buffer allocated for I/O */
    SDPfreebuf();

    if(handle->file_type == HDF_FILE) 
      {
          handle->flags &= ~(NC_CREAT | NC_INDEF | NC_NDIRTY | NC_HDIRTY) ;
          return(0) ;
      }
#endif	

    if(handle->vars == NULL) 
		goto done ;
	
	/* loop thru vars, first pass is for the 'non-record' vars */
	vpp = (NC_var **)handle->vars->values ;
	for(ii = 0 ; ii < handle->vars->count ; ii++, vpp++)
      {
          if( IS_RECVAR(*vpp) )
            {
                continue ;	/* skip record variables on this pass */
            }

#ifdef DEBUG
          assert( (*vpp)->begin == xdr_getpos(xdrs) ) ;
#endif /* DEBUG */

          if( !(handle->flags & NC_CREAT) && stash->vars != NULL
              && ii < stash->vars->count)
            {
                /* copy data */
                if( !NC_vcpy(xdrs, stash, ii) )
                    return(-1) ;
                continue ;
            } /* else */

          if( !(handle->flags & NC_NOFILL) )
              if( !xdr_NC_fill(xdrs, *vpp) )
                  return(-1) ;
      }

	if(!(handle->flags & NC_CREAT)) /* after redefinition */
      {
          for(jj = 0 ; jj < stash->numrecs ; jj++)
            {
                vpp = (NC_var **)handle->vars->values ;
                for(ii = 0 ; ii < handle->vars->count ; ii++, vpp++)
                  {
                      if( !IS_RECVAR(*vpp) )
                        {
                            continue ;	/* skip non-record variables on this pass */
                        }
                      if( stash->vars != NULL && ii < stash->vars->count)
                        {
                            /* copy data */
                            if( !NC_reccpy(xdrs, stash, ii, jj) )
                                return(-1) ;
                            continue ;
                        } /* else */
                      if( !(handle->flags & NC_NOFILL) )
                          if( !xdr_NC_fill(xdrs, *vpp) )
                              return(-1) ;
                  }
            }
          handle->numrecs = stash->numrecs ;
          if(!xdr_numrecs(handle->xdrs, handle) )
              return(-1) ;
      }

#ifdef EDEBUG
	NCadvise(NC_NOERR, "begin %d, recsize %d, numrecs %d",
             handle->begin_rec, handle->recsize, handle->numrecs) ;
#endif /* EDEBUG */

	if(!(handle->flags & NC_CREAT)) /* redefine */
      {
          char realpath[FILENAME_MAX + 1] ;
          strcpy(realpath, stash->path) ;

          /* close stash */
/*                NC_free_cdf(stash) ; */
#ifdef DOS_FS
          xdr_destroy(handle->xdrs) ; /* close handle */
          if( remove(realpath) != 0 )
              nc_serror("couldn't remove filename \"%s\"", realpath) ;
#endif
          if( rename(handle->path, realpath) != 0)
            {
                nc_serror("rename %s -> %s failed", handle->path, realpath) ;
                /* try to restore state prior to redef */
                _cdfs[cdfid] = stash ;
                _cdfs[handle->redefid] = NULL ;
                if(handle->redefid == _ncdf - 1)
                    _ncdf-- ;
                NC_free_cdf(handle) ;
                return(-1) ;
            }
          (void) strncpy(handle->path, realpath, FILENAME_MAX) ;
#ifdef DOS_FS
          if( NCxdrfile_create( handle->xdrs, handle->path, NC_WRITE ) < 0)
              return -1 ;
#endif
          NC_free_cdf(stash) ;
          _cdfs[handle->redefid] = NULL ;
          if(handle->redefid == _ncdf - 1)
              _ncdf-- ;
          handle->redefid = -1 ;
      }

done:
	handle->flags &= ~(NC_CREAT | NC_INDEF | NC_NDIRTY | NC_HDIRTY) ;
	return(0) ;
}


int ncendef( cdfid )
int cdfid ;
{
	NC *handle ;

	cdf_routine_name = "ncendef" ;

	handle = NC_check_id(cdfid) ; 
	if(handle == NULL)
		return(-1) ;
	if( !NC_indefine(cdfid,TRUE) )
		return(-1) ;
	return( NC_endef(cdfid, handle) ) ;
}

/*
 * This routine is called by SDend()? -GV
 */
int ncclose( cdfid )
int cdfid ;
{
	NC *handle ;

	cdf_routine_name = "ncclose" ;

	handle = NC_check_id(cdfid) ; 
	if(handle == NULL)
		return(-1) ;

	if( handle->flags & NC_INDEF)
      {
          if( NC_endef(cdfid, handle) == -1 )
            {
                return( ncabort(cdfid) ) ;
            }
      }
	else if(handle->flags & NC_RDWR)
      {
          handle->xdrs->x_op = XDR_ENCODE ;
          if(handle->flags & NC_HDIRTY)
            {
                if(!xdr_cdf(handle->xdrs, &handle) )
                    return(-1) ;
            }
          else if(handle->flags & NC_NDIRTY)
            {
                if(!xdr_numrecs(handle->xdrs, handle) )
                    return(-1) ;
            }
      }

#ifdef HDF
    if(handle->file_type == HDF_FILE) 
        hdf_close(handle);
#endif

    NC_free_cdf(handle) ; /* calls fclose */

	_cdfs[cdfid] = NULL ;

	if(cdfid == _ncdf - 1)
		_ncdf-- ;

	return(0) ;
}

int
ncsetfill(id, fillmode)
int id ;
int fillmode ;
{
	NC *handle ;
	int ret = 0 ;

	cdf_routine_name = "ncsetfill" ;

	handle = NC_check_id(id) ; 
	if(handle == NULL)
		return(-1) ;

	if(!(handle->flags & NC_RDWR))
      {
          /* file isn't writable */
          NCadvise(NC_EPERM, "%s is not writable", handle->path) ;
          return -1 ;
      }

	ret = (handle->flags & NC_NOFILL) ? NC_NOFILL : NC_FILL ;

	if(fillmode == NC_NOFILL)
		handle->flags |= NC_NOFILL ;
	else if(fillmode == NC_FILL)
      {
          if(handle->flags & NC_NOFILL)
            {
                /*
                 * We are changing back to fill mode
                 * so do a sync
                 */
#ifdef HDF       /* save the original x_op  */
                enum xdr_op  xdr_op = handle->xdrs->x_op;
                 
                if (handle->flags & NC_RDWR)   /* make sure we can write */
                    handle->xdrs->x_op = XDR_ENCODE; /*  to the file */
#endif
                if(handle->flags & NC_HDIRTY)
                  {
                      if(!xdr_cdf(handle->xdrs, &handle) )
                          return(-1) ;
                      handle->flags &= ~(NC_NDIRTY | NC_HDIRTY) ;
                  }
                else if(handle->flags & NC_NDIRTY)
                  {
                      if(!xdr_numrecs(handle->xdrs, handle) )
                          return(-1) ;
#ifdef HDF
                      if (handle->file_type != HDF_FILE)
                          handle->flags &= ~(NC_NDIRTY) ;
#else              
                      handle->flags &= ~(NC_NDIRTY) ;
#endif
                  }
                handle->flags &= ~NC_NOFILL ;
#ifdef HDF                /* re-store the x_op  */
                handle->xdrs->x_op = xdr_op;
#endif
            }
      }
	else
      {
          NCadvise(NC_EINVAL, "Bad fillmode") ;
          return -1 ;
      }


	return ret ;
}
