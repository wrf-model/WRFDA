/*
 *	Copyright 1993, University Corporation for Atmospheric Research
 *      See netcdf/COPYRIGHT file for copying and redistribution conditions.
 */
/*	$Id: sharray.c,v 1.2 1993/05/03 21:25:15 chouck Exp $ */

#include	"local_nc.h"
#include	"alloc.h"

/* you may wish to tune this: big on a cray, small on a PC? */
#define NC_SHRT_BUFSIZ 8192
#define NC_NSHRTS_PER (NC_SHRT_BUFSIZ/2) /* number of netshorts the buffer holds */

/*
 * internal function, bulk xdr of an even number of shorts, less than NC_NSHRTS_PER
 */
static
bool_t
NCxdr_shortsb(xdrs, sp, nshorts)
	XDR *xdrs;
	short *sp;
	u_int nshorts ;
{
	unsigned char buf[NC_SHRT_BUFSIZ] ;
	unsigned char *cp ;
	unsigned int nbytes = nshorts * 2;

	/* assert(nshorts <= NC_NSHRTS_PER) ; */
	/* assert(nshorts > 0) ; */

	if(xdrs->x_op == XDR_ENCODE)
	{
		for(cp = buf ; cp < &buf[nbytes] ; sp++, cp += 2 )
		{
			*(cp +1) = *sp % 256 ;
			*cp = (*sp >> 8) ;
		}
	}

	if(!xdr_opaque(xdrs, (caddr_t)buf, nbytes))
		return FALSE ;
	
	if(xdrs->x_op == XDR_DECODE)
	{
		for(cp = buf ; cp < &buf[nbytes] ; sp++, cp += 2 )
		{
			*sp = ((*cp & 0x7f) << 8) + *(cp +1) ;
			if(*cp & 0x80)
			{
				/* extern is neg */
				*sp -= 0x8000 ;
			}
		}
	}

	return TRUE ;
}


/*
 * Translate an array of cnt short integers at sp.
 */
bool_t
xdr_shorts(xdrs, sp, cnt)
	XDR *xdrs;
	short *sp;
	u_int cnt ;
{
	int odd ; /* 1 if cnt is odd, 0 otherwise */

	if(cnt == 0)
		return TRUE ;	/* ? */

	odd = cnt % 2 ;
	if(odd) 
		cnt-- ;
	/* cnt is even, odd is set if apropos */

	while(cnt > NC_NSHRTS_PER)
	{
		if(!NCxdr_shortsb(xdrs, sp, NC_NSHRTS_PER))
			return FALSE ;
		/* else */
		sp += NC_NSHRTS_PER ;
		cnt -= NC_NSHRTS_PER ;
	}

	/* we know cnt <= NC_NSHRTS_PER at this point */

	if(cnt != 0)
	{
		if(!NCxdr_shortsb(xdrs, sp, cnt))
			return FALSE ;
		/* else */
		sp += cnt ;
		cnt = 0 ;
	}

	if(odd)
		if(!xdr_NCvshort(xdrs, 0, sp))
			return FALSE ;

	return TRUE ;
}
