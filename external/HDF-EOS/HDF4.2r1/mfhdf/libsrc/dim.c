/*
 *	Copyright 1993, University Corporation for Atmospheric Research
 *      See netcdf/COPYRIGHT file for copying and redistribution conditions.
 */
/*	$Id: dim.c,v 1.12 1997/11/17 02:29:30 sxu Exp $ */

#include	<string.h>
#include	"local_nc.h"
#include	"alloc.h"


NC_dim *
NC_new_dim(name,size)
const char *name ;
long size ;
{
	NC_dim *ret ;

	ret = (NC_dim *)HDmalloc(sizeof(NC_dim)) ;
	if( ret == NULL )
		goto alloc_err ;

	ret->name = NC_new_string((unsigned)strlen(name),name) ;
	if( ret->name == NULL)
		goto alloc_err ;

	ret->size = size ;
#ifdef HDF
        ret->count = 1;
/*        ret->dim00_compat = (size == NC_UNLIMITED)? 0 : 1;  */
        ret->dim00_compat = 0;
#endif /* HDF */
	return(ret) ;
alloc_err :
	nc_serror("NC_new_dim") ;
	return(NULL) ;
}


/*
 * Free dim
 *
 * NOTE: Changed return value to return 'int' 
 *       If successful returns SUCCEED else FAIL -GV 9/19/97
 */
int
NC_free_dim(dim)
NC_dim *dim ;
{
    int ret_value = SUCCEED;

	if(dim != NULL)
      {
#ifdef HDF
          if (dim->count > 1)
            {
                dim->count -=  1;
                ret_value = SUCCEED;
                goto done;
            }
#endif /* HDF */

          if (NC_free_string(dim->name) == FAIL)
            {
                ret_value = FAIL;
                goto done;
            }
          Free(dim) ;
      }

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
     /* Normal cleanup */

    return ret_value;
}


int ncdimdef(cdfid, name, size)
int cdfid ;
const char *name ;
long size ;
{
	NC *handle ;
	NC_dim *dim[1] ;
	NC_dim **dp ;
	int ii ;
	int len ;

	cdf_routine_name = "ncdimdef" ;

	if( !NC_indefine(cdfid,TRUE) )
		return(-1) ;

	handle = NC_check_id(cdfid) ;
	if(handle == NULL)
		return(-1) ;

	if(size < 0) /* NC_UNLIMITED #define'd as 0 */
	{
		NCadvise(NC_EINVAL, "Invalid size %d", size) ;
		return(-1) ;
	}

	if(handle->dims == NULL) /* first time */
	{
		dim[0] = NC_new_dim(name, size) ;
		if(dim[0] == NULL)
			return(-1) ;
		handle->dims = NC_new_array(NC_DIMENSION,(unsigned)1,
			(Void *)dim) ;
		if(handle->dims == NULL)
			return(-1) ;
	} else if(handle->dims->count >= MAX_NC_DIMS)
	{
		NCadvise(NC_EMAXDIMS, "maximum number of dimensions %d exceeded",
			handle->dims->count ) ;
		return(-1) ;
	} else {
		/* check for name in use */
		len = strlen(name) ;
		dp = (NC_dim**)handle->dims->values ;
		for(ii = 0 ; ii < handle->dims->count ; ii++, dp++)
		{
			if( len == (*dp)->name->len &&
				strncmp(name, (*dp)->name->values, len) == 0)
			{
				NCadvise(NC_ENAMEINUSE, "dimension \"%s\" in use with index %d",
					(*dp)->name->values, ii) ;
				return(-1) ;
			}
			if((*dp)->size == NC_UNLIMITED && size == NC_UNLIMITED)
			{
				NCadvise(NC_EUNLIMIT, "NC_UNLIMITED size already in use: dimension \"%s\" (index %d)",
					(*dp)->name->values, ii) ;
				return(-1) ;
			}
		}

		dim[0] = NC_new_dim(name, size) ;
		if(dim[0] == NULL)
			return(-1) ;
		if( NC_incr_array(handle->dims, (Void *)dim) == NULL)
			return(-1) ;
	}
	return(handle->dims->count -1) ;
}

int NC_dimid( handle, name)
NC *handle;
char *name;
{
  int ii;
  int len;
  NC_dim **dp;

  len = strlen(name) ;
  dp = (NC_dim**)handle->dims->values ;
  for(ii = 0 ; ii < handle->dims->count ; ii++, dp++)
    {
      if( len == (*dp)->name->len &&
         strncmp(name, (*dp)->name->values, len) == 0)
        return(ii) ;
    }
  NCadvise(NC_EBADDIM, "dim \"%s\" not found", name) ;
  return(-1) ;
}

int ncdimid( cdfid, name)
int cdfid ;
const char *name ;
{
	NC *handle ;
	NC_dim **dp ;
	int ii ;
	int len ;

	cdf_routine_name = "ncdimid" ;

	handle = NC_check_id(cdfid) ;
	if(handle == NULL)
		return(-1) ;
	if(handle->dims == NULL)
		return(-1) ;
	len = strlen(name) ;
	dp = (NC_dim**)handle->dims->values ;
	for(ii = 0 ; ii < handle->dims->count ; ii++, dp++)
	{
            if( len == (*dp)->name->len &&
               strncmp(name, (*dp)->name->values, len) == 0)
                return(ii) ;
	}
	NCadvise(NC_EBADDIM, "dim \"%s\" not found", name) ;
	return(-1) ;
}


int ncdiminq( cdfid, dimid, name, sizep)
int cdfid ;
int dimid ;
char *name ;
long *sizep ;
{
	NC *handle ;
	NC_dim **dp ;

	cdf_routine_name = "ncdiminq" ;

	handle = NC_check_id(cdfid) ;
	if(handle == NULL)
		return(-1) ;
	if(handle->dims == NULL)
		return(-1) ;
	if(dimid >= handle->dims->count)
		return(-1) ;

	dp = (NC_dim**)handle->dims->values ;
	dp += dimid ;

	if(name != NULL)
	{
#ifdef HDF
		(void)memcpy( name, (*dp)->name->values, 
			(size_t)(*dp)->name->len) ;
#else
		(void)strncpy( name, (*dp)->name->values, 
			(size_t)(*dp)->name->len) ;
#endif
		name[(*dp)->name->len] = 0 ;
	}
	if(sizep != 0)
	{
		if((*dp)->size == NC_UNLIMITED)
			*sizep = handle->numrecs ;
		else
			*sizep = (*dp)->size ;	
	}
	return(dimid) ;
}


int ncdimrename(cdfid, dimid, newname)
int cdfid ;
int dimid ; 
const char *newname ;
{
	
	NC *handle ;
	NC_dim **dp ;
	NC_string *old, *new ;
	int ii ;
	int len ;

	cdf_routine_name = "ncdimrename" ;

	handle = NC_check_id(cdfid) ;

	if(handle == NULL)
		return(-1) ;
	if(!(handle->flags & NC_RDWR))
		return(-1) ;
	if(handle->dims == NULL)
		return(-1) ;

	/* check for name in use */
	len = strlen(newname) ;
	dp = (NC_dim**)handle->dims->values ;
	for(ii = 0 ; ii < handle->dims->count ; ii++, dp++)
	{
		if( len == (*dp)->name->len  &&
			strncmp(newname, (*dp)->name->values, len) == 0)
		{
			NCadvise(NC_ENAMEINUSE, "dimension \"%s\" in use with index %d",
				(*dp)->name->values, ii) ;
			return(-1) ;
		}
	}

	dp = (NC_dim**)handle->dims->values ;
	dp += dimid ;

	old = (*dp)->name ;
	if( NC_indefine(cdfid,FALSE) )
	{
		new = NC_new_string((unsigned)strlen(newname),newname) ;
		if( new == NULL)
			return(-1) ;
		(*dp)->name = new ;
		NC_free_string(old) ;
		return(dimid) ;
	} /* else */
	new = NC_re_string(old, (unsigned)strlen(newname),newname) ;
	if( new == NULL)
		return(-1) ;
	(*dp)->name = new ;
	if(handle->flags & NC_HSYNC)
	{
		handle->xdrs->x_op = XDR_ENCODE ;
		if(!xdr_cdf(handle->xdrs, &handle) )
			return(-1) ;
		handle->flags &= ~(NC_NDIRTY | NC_HDIRTY) ;
	} else
		handle->flags |= NC_HDIRTY ;
	return(dimid) ;
}


bool_t
xdr_NC_dim(xdrs, dpp)
	XDR *xdrs;
	NC_dim **dpp;
{
	if( xdrs->x_op == XDR_FREE)
	{
		NC_free_dim((*dpp)) ;
		return(TRUE) ;
	}

	if( xdrs->x_op == XDR_DECODE )
	{
		*dpp = (NC_dim *)HDmalloc(sizeof(NC_dim)) ;
		if( *dpp == NULL )
		{
			nc_serror("xdr_NC_dim") ;
			return(FALSE) ;
		}
	}

#ifdef HDF
    /* hmm...what does this do? */
    if( xdrs->x_op == XDR_DECODE )
        (*dpp)->count = 0;
#endif

	if( !xdr_NC_string(xdrs, &((*dpp)->name)))
		return(FALSE) ;
	return( xdr_long(xdrs, &((*dpp)->size)) ) ;
}


/*
 * How much space will the xdr'd dim take.
 */
int NC_xlen_dim(dpp)
NC_dim **dpp ;
{
	int len = 4 ;
	if(*dpp!=NULL)
	{
		len += NC_xlen_string((*dpp)->name) ;
	}
	return(len) ;
}
