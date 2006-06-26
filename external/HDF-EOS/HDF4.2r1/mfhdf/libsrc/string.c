/*
 *	Copyright 1993, University Corporation for Atmospheric Research
 *      See netcdf/COPYRIGHT file for copying and redistribution conditions.
 */
/*	$Id: string.c,v 1.12 1997/11/05 19:40:32 koziol Exp $ */

#include	<string.h>
#include	"local_nc.h"
#include	"alloc.h"

#ifdef HDF
static uint32 
compute_hash(unsigned count, 
             const char *str)
{
    uint32 ret=0;
    uint32 temp;

     /* check if string is NULL */
     if (str == NULL)
         return ret;

    while(count > sizeof(uint32))
      {
          HDmemcpy(&temp,str,sizeof(uint32));
          ret   += temp;
          str   += sizeof(uint32);
          count -= sizeof(uint32);
      } /* end while */

    if(count > 0)
      {
          temp=0;
          HDmemcpy(&temp,str,count);
          ret+=temp;
      } /* end if */
    return(ret);
} /* end compute_hash() */
#endif /* HDF */

NC_string *
NC_new_string(count, str)
unsigned count ;
const char *str ;
{
	NC_string *ret ;
	size_t memlen ;

	if(count > MAX_NC_NAME)
      {
          NCadvise(NC_EMAXNAME,
                   "string \"%c%c%c%c%c%c ...\"  length %d exceeds %d",
                   str[0], str[1], str[2], str[3], str[4], str[5],
                   count,  MAX_NC_NAME ) ;
          return NULL ;
      }


	ret = (NC_string *)HDmalloc(sizeof(NC_string)) ;
	if( ret == NULL )
		goto alloc_err ;
	ret->count = count ;
    ret->len   = count ;
#ifdef HDF
    ret->hash = compute_hash(count,str);
#endif /* HDF */
	if(count != 0 ) /* allocate */
      {
          memlen = count + 1 ;
          ret->values = (char *)HDmalloc(memlen) ;
          if(ret->values == NULL)
              goto alloc_err ;
          if(str != NULL)
            {
#ifdef HDF
                memcpy(ret->values, str, (size_t)count) ;
#else
                (void)strncpy(ret->values, str, count) ;
#endif
                ret->values[count] = 0 ;
            }
      } else { /* use what what you were given */
          ret->values = NULL ;
      }
	
	return(ret) ;
    alloc_err :
        nc_serror("NC_new_string") ;
	if(ret != NULL) HDfree(ret) ;
	return(NULL) ;
}


/*
 * Free string, and, if needed, its values.
 *
 * NOTE: Changed return value to return 'int' 
 *       If successful returns SUCCEED else FAIL -GV 9/19/97
 */
int
NC_free_string(cdfstr)
NC_string *cdfstr ;
{
    int ret_value = SUCCEED;

	if(cdfstr != NULL)
      {
          if(cdfstr->values != NULL)
              Free(cdfstr->values) ;
          Free(cdfstr) ;
      }

#ifdef LATER
done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
     /* Normal cleanup */
#endif /* LATER */

    return ret_value;
}


NC_string *
NC_re_string(old, count, str)
NC_string *old ;
unsigned count ;
const char *str ;
{
	if(old->count < count) /* punt */
      {
          NCadvise(NC_ENOTINDEFINE,
                   "Must be in define mode to increase name length %d",
                   old->count) ;
          return(NULL) ;
      }

    if (str == NULL)
        return NULL;

	(void)memcpy(old->values, str, count) ;
	(void)memset(old->values + count, 0, (int)old->count - (int)count +1) ;
        
    /* make sure len is always == to the string length */
    old->len = count ;
#ifdef HDF
    old->hash = compute_hash(count,str);
#endif /* HDF */

	return(old) ;
}

bool_t
xdr_NC_string(xdrs, spp)
	XDR *xdrs;
	NC_string **spp;
{
	u_long count ;
        int status ;

	switch (xdrs->x_op) {
	case XDR_FREE:
		NC_free_string((*spp)) ;
		return(TRUE) ;
	case XDR_DECODE:
		/* need the length to pass to new */
		if (! xdr_u_long(xdrs, &count)) {
			return (FALSE);
		}
		if( count == 0)
		{
		   *spp = NULL ;
		   return(TRUE) ;
		} /* else */
		(*spp) = NC_new_string((unsigned)count, (const char *)NULL) ;
		if((*spp) == NULL)
			return(FALSE) ;
		(*spp)->values[count] = 0 ;
		/* then deal with the characters */
		status = xdr_opaque(xdrs, (*spp)->values, (*spp)->count);

                /* might be padded */
                (*spp)->len = strlen((*spp)->values);
                return(status);
	case XDR_ENCODE:
		/* first deal with the length */
		if(*spp == NULL)
		{
			count = 0 ;
			return( xdr_u_long(xdrs, &count) ) ;
		} /* else */
		count = (*spp)->count ;
		if (! xdr_u_long(xdrs, &count) ) {
			return (FALSE);
		}
		/* then deal with the characters */
		return (xdr_opaque(xdrs, (*spp)->values, (*spp)->count));
	}
	return(FALSE) ;
}


/*
 * How much space will the xdr'd string take.
 *
 */
int NC_xlen_string(cdfstr)
NC_string *cdfstr ;
{
	int len = 4 ;
	int rem ;
	if(cdfstr!=NULL)
	{
		len += cdfstr->count ;
		if( (rem = len%4) != 0)
			len += 4 - rem ;
	}
	return(len) ;
}
