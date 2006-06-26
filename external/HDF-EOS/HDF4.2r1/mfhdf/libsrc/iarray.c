/*
 *	Copyright 1993, University Corporation for Atmospheric Research
 *      See netcdf/COPYRIGHT file for copying and redistribution conditions.
 */
/*	$Id: iarray.c,v 1.6 1997/11/05 19:40:20 koziol Exp $ */

#include	"local_nc.h"
#include	"alloc.h"


NC_iarray *
NC_new_iarray(count, values)
unsigned count ;
const int *values ;           /* VAX C doesn't like values[] */
{
	NC_iarray *ret ;
	int *ip ;
	size_t memlen ;

	ret = (NC_iarray *)HDmalloc(sizeof(NC_iarray)) ;
	if( ret == NULL )
		goto alloc_err ;
	ret->count = count ;
	if(count != 0 ) /* allocate */
      {
          memlen = count * sizeof(int) ;
          ret->values = (int *)HDmalloc(memlen) ;
          if(ret->values == NULL)
              goto alloc_err ;
          if(values != NULL) /* copy them in */
            {
                for(ip = ret->values ; count > 0; count--)
                    *ip++ = *values++ ;
            }
      } else {
          ret->values = NULL ;
      }
	
	return(ret) ;
    alloc_err :
        nc_serror("NC_new_iarray") ;
	return(NULL) ;
}


/*
 * Free iarray, and, if needed, its values.
 *
 * NOTE: Changed return value to return 'int' 
 *       If successful returns SUCCEED else FAIL -GV 9/19/97
 */
int
NC_free_iarray(iarray)
NC_iarray *iarray ;
{
    int ret_value = SUCCEED;

	if(iarray != NULL)
      {
          if(iarray->values != NULL)
              Free(iarray->values) ;
          Free(iarray) ;
      }

    return ret_value;
}


bool_t
xdr_NC_iarray(xdrs, ipp)
	XDR *xdrs;
	NC_iarray **ipp;
{
	int *ip ;
	u_long count ;
	bool_t stat = TRUE ;

	switch (xdrs->x_op) {
	case XDR_FREE:
		NC_free_iarray((*ipp)) ;
		return(TRUE) ;
	case XDR_DECODE:
		/* need the length to pass to new */
		if (! xdr_u_long(xdrs, &count)) {
			return (FALSE);
		}
		(*ipp) = NC_new_iarray((unsigned)count, (int *)NULL) ;
		if((*ipp) == NULL)
			return(FALSE) ;
		/* then deal with the array */
		for( ip = (*ipp)->values ; (count > 0 ) && stat ; count-- )
			stat = xdr_int(xdrs, ip++ ) ;
		return(stat) ;
	case XDR_ENCODE:
		/* first deal with the length */
		count = (*ipp)->count ;
		if (! xdr_u_long(xdrs, &count) ) {
			return (FALSE);
		}
		/* then deal with the array */
		for(ip = (*ipp)->values  ; (count > 0 ) && stat ; count--)
			stat = xdr_int(xdrs, ip++ ) ;
		return(stat) ;
	}
	return(FALSE) ;
}


/*
 * How much space will the xdr'd iarray take.
 */
int NC_xlen_iarray(iarray)
NC_iarray *iarray ;
{
	int len = 4 ;
	if(iarray!=NULL)
	{
		len += iarray->count * 4 ;
	}
	return(len) ;
}
