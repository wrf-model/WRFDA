/*
 *	Copyright 1993, University Corporation for Atmospheric Research
 *      See netcdf/COPYRIGHT file for copying and redistribution conditions.
 */
/*	$Id: attr.c,v 1.10 2000/08/29 13:56:51 koziol Exp $ */

#include	<string.h> 
#include	"local_nc.h"
#include	"alloc.h"


NC_attr *
NC_new_attr(name,type,count,values)
const char *name ;
nc_type type ;
unsigned count ;
const void *values ;
{
	NC_attr *ret ;

	ret = (NC_attr *)HDmalloc(sizeof(NC_attr)) ;
	if( ret == NULL )
		goto alloc_err ;

	ret->name = NC_new_string((unsigned)strlen(name),name) ;
	if( ret->name == NULL)
		goto alloc_err ;

	ret->data = NC_new_array(type, count, values) ;
	if( ret->data == NULL)
		goto alloc_err ;
#ifdef HDF
    ret->HDFtype = hdf_map_type(type);
#endif
	return(ret) ;
alloc_err :
	nc_serror("NC_new_attr") ;
	return(NULL) ;
}


/*
 * Free attr
 *
 * NOTE: Changed return value to return 'int' 
 *       If successful returns SUCCEED else FAIL -GV 9/19/97
 */
int
NC_free_attr(attr)
NC_attr *attr ;
{
    int ret_value = SUCCEED;

	if(attr != NULL)
      {
          if (NC_free_string(attr->name) == FAIL)
            {
                ret_value = FAIL;
                goto done;
            }
              
          if (NC_free_array(attr->data) == FAIL)
            {
                ret_value = FAIL;
                goto done;
            }

          Free(attr) ;
      }

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
     /* Normal cleanup */

    return ret_value;
}


/*
 *  Verify that this is a user nc_type
 */
bool_t
NCcktype(datatype)
nc_type datatype ;
{
	switch(datatype){
	case NC_BYTE :
	case NC_CHAR :
	case NC_SHORT :
	case NC_LONG :
	case NC_FLOAT :
	case NC_DOUBLE :
		return(TRUE) ;
    default:
        break;
	}
	NCadvise(NC_EBADTYPE, "Unknown type %d", datatype) ;
	return(FALSE) ;
}


/*
 * Given cdfid and varid, return handle to array of attributes
 *  else NULL on error
 */
static NC_array **
NC_attrarray( cdfid, varid )
int cdfid ;
int varid ;
{
	NC *handle ;
	NC_array **ap ;

	handle = NC_check_id(cdfid) ;
	if(handle == NULL)
		return(NULL) ;

	if(varid == NC_GLOBAL) /* Global attribute, attach to cdf */
	{
		ap = &(handle->attrs) ;
	}else if(handle->vars != NULL && varid >= 0 && varid < handle->vars->count)
	{
		ap = (NC_array **)handle->vars->values ;
		ap += varid ;
		ap = &(((NC_var *)(*ap))->attrs) ; /* Whew! */
	} else {
		NCadvise(NC_EINVAL, "%d is not a valid variable id", varid) ;
		ap = NULL ;
	}
	return(ap) ;
}


/*
 * Step thru NC_ATTRIBUTE array, seeking match on name.
 *  return match or NULL if Not Found.
 */
NC_attr **
NC_findattr(ap, name)
NC_array **ap ;
const char *name ;
{
    NC_attr **attr ;
    int attrid ;
    size_t len ;
    
    if(*ap == NULL)
        return(NULL) ;
    
    attr = (NC_attr **) (*ap)->values ;
    
    len = strlen(name) ;
    
    for(attrid = 0 ; attrid < (*ap)->count ; attrid++, attr++)
	{
            if( len == (*attr)->name->len && 
               strncmp(name, (*attr)->name->values, len) == 0)
                {
                    return(attr) ; /* Normal return */
                }
        }
    return(NULL) ;
}


/*
 * Look up by cdfid, varid and name, return NULL if not found
 */
static NC_attr **
NC_lookupattr( cdfid, varid, name, verbose)  
int cdfid ;
int varid ;
const char *name ; /* attribute name */
bool_t verbose ;
{
	NC_array **ap ;
	NC_attr **attr ;

	ap = NC_attrarray(cdfid, varid) ;
	if(ap == NULL)
		return(NULL) ;

	attr = NC_findattr(ap, name ) ;
	if(verbose && attr == NULL)
		NCadvise(NC_ENOTATT, "attribute \"%s\" not found",name) ;
	return( attr ) ;
}


/*
 * Common code for attput and attcopy
 */
static int
NC_aput(cdfid, ap, name, datatype, count, values)
int cdfid ;
NC_array **ap ;
const char *name ;
nc_type datatype ;
unsigned count ;
const void *values ;
{
	NC *handle ;
	NC_attr *attr[1] ;
	NC_attr **atp, *old ;

	handle = NC_check_id(cdfid) ;
	if(handle == NULL)
		return(-1) ;
	if(!(handle->flags & NC_RDWR))
		return(-1) ;

	if(*ap == NULL ) /* first time */
	{
		if( !NC_indefine(cdfid,TRUE) )
			return(-1) ;

		attr[0] = NC_new_attr(name,datatype,count,values) ;
		if(attr[0] == NULL)
			return(-1) ;
		*ap = NC_new_array(NC_ATTRIBUTE,(unsigned)1, (Void*)attr) ;
		if(*ap == NULL)
			return(-1) ;
		return((*ap)->count -1) ;
	}
	/* else */
	if( (atp = NC_findattr(ap, name)) != NULL) /* name in use */
	{
		if( NC_indefine(cdfid,FALSE) )
		{
			old = *atp ;
			*atp = NC_new_attr(name,datatype,count,values) ;
			if(*atp == NULL)
			{
				*atp = old ;
				return(-1) ;
			}
			NC_free_attr(old) ;
			return((*ap)->count -1) ;
		} 
		/* else */
		if( NC_re_array((*atp)->data, datatype,count,values) == NULL) 
		{
		NCadvise(NC_ENOTINDEFINE, "Can't increase size unless in define mode") ;
			return(-1) ;
		}
		/* else */
#ifdef HDF
                (*atp)->HDFtype = hdf_map_type(datatype);
#endif
		if(handle->flags & NC_HSYNC)
		{
			handle->xdrs->x_op = XDR_ENCODE ;
			if(!xdr_cdf(handle->xdrs, &handle) )
				return(-1) ;
			handle->flags &= ~(NC_NDIRTY | NC_HDIRTY) ;
		} else
			handle->flags |= NC_HDIRTY ;
		return((*ap)->count -1) ;
	} 
	/* else */
	if((*ap)->count >= MAX_NC_ATTRS)
	{
		NCadvise(NC_EMAXATTS, "maximum number of attributes %d exceeded",
			(*ap)->count ) ;
		return(-1) ;
	}
	/* else */
	if( NC_indefine(cdfid,TRUE) )
	{
		attr[0] = NC_new_attr(name,datatype,count,values) ;
		if(attr[0] == NULL)
			return(-1) ;
		if( NC_incr_array((*ap), (Void *)attr) == NULL)
			return(-1) ;
		return((*ap)->count -1) ;
	}
	/* else */
	return(-1) ;
}


int ncattput(cdfid, varid, name, datatype, count, values)
int cdfid ;
int varid ;
const char *name ;
nc_type datatype ;
int count ;
const ncvoid *values ;
{
	NC_array **ap ;

	cdf_routine_name = "ncattput" ;

	ap = NC_attrarray(cdfid, varid) ;
	if(ap == NULL)
		return(-1) ;

	if(count < 0 )
	{
		NCadvise(NC_EINVAL, "Invalid length %d", count) ;
		return(-1) ;
	}

	if(!NCcktype(datatype))
		return(-1) ;

	return( NC_aput(cdfid, ap, name, datatype, (unsigned)count,
		values) ) ;
}


int ncattname( cdfid, varid, attnum, name)
int cdfid ;
int varid ;
int attnum ;
char *name ;
{
	NC_array **ap ;
	NC_attr **attr ;

	cdf_routine_name = "ncattname" ;

	ap = NC_attrarray(cdfid, varid) ;
	if(ap == NULL || *ap == NULL)
		return(-1) ;

	if( attnum < 0 || attnum >= (*ap)->count)
	{
		NCadvise(NC_ENOTATT, "%d is not a valid attribute id", attnum) ;
		return(-1) ;
	}

	attr = (NC_attr **) (*ap)->values ;
	attr += attnum ;
#ifdef HDF
	(void)memcpy( name, (*attr)->name->values, (*attr)->name->len) ;
#else
	(void)strncpy( name, (*attr)->name->values, (*attr)->name->len) ;
#endif
	name[(*attr)->name->len] = 0 ;

	return(attnum) ;
}


int ncattinq( cdfid, varid, name, datatypep, countp)  
int cdfid ;
int varid ;
const char *name ; /* input, attribute name */
nc_type *datatypep ;
int *countp ;
{
	NC_attr **attr ;

	cdf_routine_name = "ncattinq" ;

	attr = NC_lookupattr(cdfid,varid,name,TRUE) ;
	if(attr == NULL)
		return(-1) ;

	if(datatypep != 0)
		*datatypep = (*attr)->data->type ;
	if(countp != 0)
		*countp = (*attr)->data->count ;
	return(1) ;
}


int ncattrename(cdfid, varid, name, newname)
int cdfid ;
int varid ; 
const char *name ;
const char *newname ;
{
	
	NC *handle ;
	NC_attr **attr ;
	NC_string *new, *old ;

	cdf_routine_name = "cdfattrrename" ;

	handle = NC_check_id(cdfid) ;
	if(handle == NULL)
		return(-1) ;
	if(!(handle->flags & NC_RDWR))
		return(-1) ;

	attr = NC_lookupattr(cdfid,varid,name,TRUE) ;
	if(attr == NULL)
		return(-1) ;

	if( NC_lookupattr(cdfid,varid,newname,FALSE) != NULL) /* name in use */
		return(-1) ;

	old = (*attr)->name ;
	if( NC_indefine(cdfid,FALSE) )
	{
		new = NC_new_string((unsigned)strlen(newname),newname) ;
		if( new == NULL)
			return(-1) ;
		(*attr)->name = new ;
		NC_free_string(old) ;
		return(1) ;
	} /* else */
	new = NC_re_string(old, (unsigned)strlen(newname),newname) ;
	if( new == NULL)
		return(-1) ;
	(*attr)->name = new ;
	if(handle->flags & NC_HSYNC)
	{
		handle->xdrs->x_op = XDR_ENCODE ;
		if(!xdr_cdf(handle->xdrs, &handle) )
			return(-1) ;
		handle->flags &= ~(NC_NDIRTY | NC_HDIRTY) ;
	} else
		handle->flags |= NC_HDIRTY ;
	return(1) ;
}


int ncattcopy( incdf, invar, name, outcdf, outname)
int incdf ;
int invar ;
const char *name ;
int outcdf ;
int outname ;
{
	NC_attr **attr ;
	NC_array **ap ;

	cdf_routine_name = "ncattcopy" ;

	attr = NC_lookupattr(incdf,invar,name,TRUE) ;
	if(attr == NULL)
		return(-1) ;
	
	ap = NC_attrarray(outcdf, outname) ;
	if(ap == NULL)
		return(-1) ;

	return( NC_aput(outcdf, ap, name,
		(*attr)->data->type, (*attr)->data->count,
		(*attr)->data->values) ) ;
}


int ncattdel(cdfid, varid, name)
int cdfid ;
int varid ; 
const char *name ;
{
	
	NC_array **ap ;
	NC_attr **attr ;
	NC_attr *old = NULL ;
	int attrid ;
	int len ;

	cdf_routine_name = "ncattdel" ;

	if( !NC_indefine(cdfid,TRUE) )
		return(-1) ;

	ap = NC_attrarray(cdfid, varid) ;
	if(ap == NULL || *ap == NULL)
		return(-1) ;

	attr = (NC_attr **) (*ap)->values ;

	len = strlen(name) ;
	for(attrid = 0 ; attrid < (*ap)->count ; attrid++, attr++)
	{
		if( len == (*attr)->name->len &&
			strncmp(name, (*attr)->name->values, len) == 0)
		{
			old = *attr ;
			break ;
		}
	}
	if( attrid == (*ap)->count )
	{
		NCadvise(NC_ENOTATT, "attribute \"%s\" not found",name) ;
		return(-1) ;
	}
	/* shuffle down */
	for(attrid++ ; attrid < (*ap)->count ; attrid++)
	{
		*attr = *(attr + 1) ;
		attr++ ;
	}
	/* decrement count */
	(*ap)->count-- ;

	NC_free_attr(old) ;

	return(1) ;
}


int ncattget(cdfid, varid, name, values)
int cdfid ;
int varid ; 
const char *name ;
ncvoid *values ;
{
	
	NC_attr **attr ;

	cdf_routine_name = "ncattget" ;

	attr = NC_lookupattr(cdfid,varid,name,TRUE) ;
	if(attr == NULL)
		return(-1) ;

	NC_copy_arrayvals((char *)values, (*attr)->data) ;

	return(1) ;
}


bool_t
xdr_NC_attr(xdrs, app)
	XDR *xdrs;
	NC_attr **app;
{
    bool_t ret_value;

	if( xdrs->x_op == XDR_FREE)
	{
		NC_free_attr((*app)) ;
		return(TRUE) ;
	}

	if( xdrs->x_op == XDR_DECODE )
	{
		*app = (NC_attr *)HDmalloc(sizeof(NC_attr)) ;
		if( *app == NULL )
		{
			nc_serror("xdr_NC_attr") ;
			return(FALSE) ;
		}
	}

	if( !xdr_NC_string(xdrs, &((*app)->name)))
		return(FALSE) ;
	ret_value = xdr_NC_array(xdrs, &((*app)->data)) ;
#ifdef HDF
        (*app)->HDFtype = hdf_map_type(((*app)->data)->type);
#endif
	return ret_value;
}


/*
 * How much space will the xdr'd attr take.
 */
int NC_xlen_attr(app)
NC_attr **app ;
{
	int len ;

	if(*app == NULL)
		return(4) ;

	len = NC_xlen_string((*app)->name) ;
	len += NC_xlen_array((*app)->data) ;

	return(len) ;
}
