/*
 *	Copyright 1993, University Corporation for Atmospheric Research
 *      See netcdf/COPYRIGHT file for copying and redistribution conditions.
 */
/*	$Id: putget.c,v 1.78 2003/12/10 21:15:13 epourmal Exp $ */

#include	<string.h>
#include	"local_nc.h"
#include	"alloc.h"
#ifdef HDF
#include    "hfile.h"    /* Ugh!  We need the defs for HI_READ and HI_SEEK */

/* Local function prototypes */
static bool_t nssdc_xdr_NCvdata
       (NC *handle,NC_var *vp,u_long where,nc_type type,uint32 count,
        void * values);

static intn hdf_xdr_NCvdata
    (NC *handle,NC_var *vp,u_long where,nc_type type,uint32 count,
        void * values);

static intn hdf_xdr_NCv1data
    (NC *handle,NC_var *vp,u_long where,nc_type type,void * values);

int32 hdf_get_vp_aid
    (NC *handle, NC_var *vp);

static intn SDIresizebuf(void * *buf,int32 *buf_size,int32 size_wanted);

#endif /* HDF */

static const long *NCvcmaxcontig(NC *, NC_var *, const long *, const long *);

/*
 * If you use ./xdrstdio.c rather than ./xdrposix.c as
 * your bottom layer, the you may need to #define XDRSTDIO
 */

#ifndef HDF
#define MIN(mm,nn) (((mm) < (nn)) ? (mm) : (nn))
#define MAX(mm,nn) (((mm) > (nn)) ? (mm) : (nn))
#endif /* HDF */

/* #define VDEBUG */
/* #define DEBUG */
/* #define CDEBUG */

#ifdef VDEBUG
/*
 * Print the values of an array of longs
 */
int arrayp(label, count, array)
const char *label ;
unsigned count ;
const long array[] ;
{
	fprintf(stderr, "%s", label) ;
	fputc('\t',stderr) ;	
	for(; count > 0 ; count--, array++)
		fprintf(stderr," %ld", *array) ;
	fputc('\n',stderr) ;	
}
#endif /* VDEBUG */

#define xdr_NCsetpos(xdrs, pos) xdr_setpos((xdrs),(pos))

/*
 * At the current position, add a record containing the fill values.
 */
static bool_t
NCfillrecord(xdrs, vpp, numvars)
XDR *xdrs ;
NC_var **vpp ;
unsigned numvars ;
{
	int ii ;

	for(ii = 0 ; ii < numvars ; ii++, vpp++)
	{
		if( !IS_RECVAR(*vpp) )
		{
			continue ;	/* skip non-record variables */
		}

		/* compute sub size */
		if( !xdr_NC_fill(xdrs, *vpp) )
			return(FALSE) ;
	}
	return(TRUE) ;
}


/* 
 * Check whether coords are valid for the variable.
 * For 'record' variables add and fill records out
 * to the requested recno == coords[0] as a side effect.
 */
bool_t
NCcoordck(handle, vp, coords)
NC *handle ;
NC_var *vp ; 
const long *coords ;
{
	const long *ip ;
	unsigned long *up ;
	const long *boundary ;
	long unfilled ;

	if( IS_RECVAR(vp) )
      {
          boundary = coords + 1 ;
          if(*coords < 0)
              goto bad ;
      } else
          boundary = coords ;
	
	up = vp->shape + vp->assoc->count - 1 ;
	ip = coords + vp->assoc->count - 1 ;
#ifdef CDEBUG
	fprintf(stderr,"	NCcoordck: coords %p, *coords %ld, count %ld, ip %p, boundary %p, *ip %ld\n",
            coords, *coords, vp->assoc->count, ip , boundary, *ip) ;
#endif /* CDEBUG */
	for( ; ip >= boundary ; ip--, up--)
      {
#ifdef CDEBUG
          fprintf(stderr,"	NCcoordck: ip %p, *ip %ld, up %p, *up %lu\n",
                  ip, *ip, up, *up ) ;
#endif /* CDEBUG */
          if( *ip < 0 || *ip >= *up )
              goto bad ;
      }

#ifdef HDF
    if(handle->file_type == HDF_FILE && IS_RECVAR(vp)) 
      {
          void     *strg = NULL;
          void     *strg1 = NULL;
          NC_attr **attr = NULL;
          int count, byte_count;
          int len;
            
          if((unfilled = *ip - vp->numrecs) < 0) 
              return TRUE;   

#ifdef CDEBUG
fprintf(stderr, "NCcoordck: check 3.6, unfilled=%d\n",unfilled);
#endif /* CDEBUG */
          /* check to see if we are trying to read beyond the end */
          if(handle->xdrs->x_op != XDR_ENCODE)
              goto bad ;

          /* else */
          if ((handle->flags & NC_NOFILL) == 0) 
            {
                /* make sure we can write to this sucker */
                if(vp->aid == FAIL && hdf_get_vp_aid(handle, vp) == FAIL) 
                    return(FALSE);
            
                /*
                 * Set up the array strg to hold the fill values
                 */
                len = (vp->len / vp->HDFsize) * vp->szof;
                strg = (Void *) HDmalloc(len);
                strg1 = (Void *) HDmalloc(len);
                if (NULL == strg || NULL == strg1)
                    return FALSE;

                attr = NC_findattr(&vp->attrs, _FillValue);

                if(attr != NULL)
                    HDmemfill(strg,(*attr)->data->values,vp->szof,(vp->len/vp->HDFsize));
                else 
                    NC_arrayfill(strg, len, vp->type);

#ifdef DEBUG
                fprintf(stderr, "Going to fill in record %d for variable %s\n", *ip,
                        vp->name->values);
#endif

#ifdef OLD_WAY
                count = vp->dsizes[0] / NC_typelen(vp->type);
#endif

                /*
                 * Seek to correct location
                 */
                byte_count = vp->len;
                count = byte_count / vp->HDFsize;

                if (FAIL == Hseek(vp->aid, (vp->numrecs) * byte_count, DF_START))
                    return FALSE;

#ifdef DEBUG
                fprintf(stderr,"Filling %d bytes starting at %d\n", 
                       byte_count * unfilled, (vp->numrecs) * byte_count);
#endif  

                /*
                 * Write out the values
                 */
                if (FAIL == DFKconvert(strg, strg1, vp->HDFtype, count, DFACC_WRITE, 0, 0))
                    return FALSE;

                for(; unfilled >= 0; unfilled--, vp->numrecs++)
                  {
                      if (FAIL == Hwrite(vp->aid, byte_count, (uint8 *) strg1))
                          return FALSE;
                  }

#ifdef DEBUG
                fprintf(stderr, "WROTE %d values at location %d (numrecs = %d)\n",
                        count, *ip * count, vp->numrecs);
#endif
                HDfree(strg);
                HDfree(strg1);
            } /* !SD_NOFILL  */

        vp->numrecs = MAX(vp->numrecs, (*ip + 1));    /* if NOFILL  */
#ifdef CDEBUG
fprintf(stderr, "NCcoordck: check 10.0, vp->numrecs=%d\n",vp->numrecs);
#endif /* CDEBUG */
        if((*ip + 1) > handle->numrecs) 
          {
              handle->numrecs = *ip + 1;
              handle->flags |= NC_NDIRTY;
          }

        return (TRUE);
    }
#endif /* HDF */


	if( IS_RECVAR(vp) && (unfilled = *ip - handle->numrecs) >= 0 )
      {
          /* check to see if we are trying to read beyond the end */
          if(handle->xdrs->x_op != XDR_ENCODE)
              goto bad ;
          /* else */

          handle->flags |= NC_NDIRTY ;
          if( handle->flags & NC_NOFILL )
            {
                /* Go directly to jail, do not pass go */
                handle->numrecs = *ip + 1 ;
            }
          else
            {
                /* fill out new records */
                if( !xdr_NCsetpos(handle->xdrs,
                                  handle->begin_rec + handle->recsize*handle->numrecs) )
                  {
                      nc_serror("NCcoordck seek, var %s", vp->name->values) ;
                      return(FALSE) ;
                  }
                for( ; unfilled >= 0 ; unfilled--, handle->numrecs++)
                  {
                      if( !NCfillrecord(handle->xdrs,
                                        (NC_var **)handle->vars->values, handle->vars->count) )
                        {
                            nc_serror("NCcoordck fill, var %s, rec %ld",
                                      vp->name->values, handle->numrecs) ;
                            return(FALSE) ;
                        }
                  }
            }
          if(handle->flags & NC_NSYNC) /* write out header->numrecs NOW */
            {
                if(!xdr_numrecs(handle->xdrs, handle) )
                    return(FALSE) ;
                handle->flags &= ~NC_NDIRTY ;
            }
      }

	return(TRUE) ;
bad:
#ifdef VDEBUG
	arrayp("\t\tcoords", vp->assoc->count, coords) ;
	arrayp("\t\tmax", vp->assoc->count, vp->shape) ;
#endif /* VDEBUG */
	NCadvise(NC_EINVALCOORDS, "%s: Invalid Coordinates", vp->name->values) ;
	return(FALSE) ;
}


/* 
 * Translate the (variable, coords) pair into a seek index
 */
static u_long
NC_varoffset(handle, vp, coords)
NC *handle ;
NC_var *vp ; 
const long *coords ;
{
    u_long offset ;
    const long *ip  ;
    unsigned long *up ;
    const long *boundary ;
#ifdef HDF
    vix_t * vix;
    intn    i;
#endif
    
    if(vp->assoc->count == 0) /* 'scaler' variable */
        return(vp->begin) ;
    
    if( IS_RECVAR(vp) )
        boundary = coords + 1 ;
    else
        boundary = coords ;
    
    up = vp->dsizes + vp->assoc->count - 1 ;
    ip = coords + vp->assoc->count - 1 ;
    for(offset = 0 ; ip >= boundary ; ip--, up--)
        offset += *up * *ip ;
    
    if( IS_RECVAR(vp) ) 
      {
#ifdef HDF
          switch(handle->file_type) 
            {
            case HDF_FILE:
                return( vp->dsizes[0] * *coords + offset) ;
            case netCDF_FILE:
                return( vp->begin + handle->recsize * *coords + offset) ;
            case CDF_FILE:
#ifdef DEBUG
                fprintf(stderr, "Yow!  Don't do CDF records yet\n");
#endif
                return (0);
            }
#else /* !HDF */
          return( vp->begin + handle->recsize * *coords + offset) ;
#endif /* !HDF */
      } 
    else 
      {
#ifdef HDF
          switch(handle->file_type) 
            {
            case HDF_FILE:
                return (offset);
            case netCDF_FILE:
                return (vp->begin + offset);
            case CDF_FILE:
                if((vix = vp->vixHead) == NULL)
                    return (-1);
            
                /* 
                 * Record data is stored in chunks.  the firstRec and lastRec
                 *  fields give the indicies of the first and last records
                 *  stored in a given chunk and the offset gives the offset in
                 *  the file of where that chunk starts.  The local variable
                 *  'offset' gives the offset into the entire variable space
                 *  where we want to read.  To map find the correct location
                 *  we need to find the correct chunk and then get our offset
                 *  within that chunk
                 */
                while(vix) 
                  {
                      for(i = 0; i < vix->nUsed; i++) 
                        {
                            if((vix->firstRec[i] <= *coords) && (vix->lastRec[i] >= *coords)) {
                                /* found the record we want */
                                return (offset + vix->offset[i] - vix->firstRec[i] * vp->dsizes[0] + 8);
                            }
                        } /* loop over user entries in current vix record */
                      vix = vix->next;
                  } /* loop over all vix records */
                break;
            }
#else /* !HDF */
          return (vp->begin + offset);
#endif /* !HDF */
      }
    
    /* should never get to here */
    return (0);
    
} /* NC_varoffset */


/*
 * xdr 1 - 3 bytes, leaving adjoining bytes within the word ok.
 * (minimum unit of io is 4 bytes)
 */
static bool_t
xdr_NCvbyte(xdrs, rem, count, values)
XDR *xdrs ;
unsigned rem ;
unsigned count ;
char *values ;
{
	char buf[4] ;
	u_long origin=0 ;
    enum xdr_op  x_op = xdrs->x_op ; /* save state */

	if(x_op == XDR_ENCODE)
      {
          /*
           * Since we only read/write multiples of four bytes,
           * We will read in the word to change one byte in it.
           */
          origin = xdr_getpos( xdrs ) ;
#ifdef XDRSTDIO
          /*
           * N.B. : "a file positioning function must be called between
           * a write and a read or vice versa"
           *		- limitations of stdio, open for update
           */
          if( !xdr_setpos(xdrs, origin) ) 
              return(FALSE) ;
#endif /* XDRSTDIO */
          /* next op is a get */
          xdrs->x_op = XDR_DECODE ;
      }

    if(!xdr_opaque(xdrs, buf, 4))
      {
          /* get failed, assume we are trying to read off the end */
#ifdef XDRSTDIO
          /*
           * N.B. 2 : Violates layering,
           * assumes stdio under xdr.
           * This clause could be safely replaced with
           * just the 'memset' line.
           */
          if(feof((FILE*)xdrs->x_private)) /* NC_NOFILL */
            {
                /* failed because we tried to read
                 * beyond EOF
                 */
                clearerr((FILE*)xdrs->x_private) ;
                (void)HDmemset(buf, 0, sizeof(buf)) ;
            }
          else
            {
                NCadvise(NC_EXDR, "xdr_NCvbyte") ;
                xdrs->x_op = x_op ;
                return(FALSE) ;
            }
#else
          (void)HDmemset(buf, 0, sizeof(buf)) ;
#endif /* XDRSTDIO */
      }

    if(x_op == XDR_ENCODE) /* back to encode */
        xdrs->x_op = x_op ;

	while(count-- != 0)
      {
          if(x_op == XDR_ENCODE)
              buf[rem] = *values ;
          else
              *values = buf[rem] ;
	
          rem++ ;
          values++ ;
      }

	if(x_op == XDR_ENCODE)
      {
          if( !xdr_setpos(xdrs, origin) )
              return(FALSE) ;
          if( !xdr_opaque(xdrs, buf, 4))
              return(FALSE) ;
      }

	return(TRUE) ;
}


/*
 * xdr a short leaving adjoining short within the word ok.
 * (minimum unit of io is 4 bytes)
 */
bool_t
xdr_NCvshort(xdrs, which, values)
XDR *xdrs ;
unsigned which ;
short *values ;
{
    unsigned char buf[4] ; /* unsigned is important here */
    u_long origin=0;
    enum xdr_op  x_op = xdrs->x_op ; /* save state */

    if(x_op == XDR_ENCODE)
      {
          origin = xdr_getpos( xdrs ) ;
#ifdef XDRSTDIO
          /* See N.B. above */
          if( !xdr_setpos(xdrs, origin) )
              return(FALSE) ;
#endif /* XDRSTDIO */
          /* next op is a get */
          xdrs->x_op = XDR_DECODE ;
      }

    if(!xdr_opaque(xdrs, (caddr_t)buf, 4))
      {
          /* get failed, assume we are trying to read off the end */
#ifdef XDRSTDIO
          /* See N.B. 2 above */
          if(feof((FILE*)xdrs->x_private)) /* NC_NOFILL */
            {
                /* failed because we tried to read
                 * beyond EOF
                 */
                clearerr((FILE*)xdrs->x_private) ;
                (void)memset(buf, 0, sizeof(buf)) ;
            }
          else
            {
                NCadvise(NC_EXDR, "xdr_NCvbyte") ;
                xdrs->x_op = x_op ;
                return(FALSE) ;
            }
#else
          (void)HDmemset(buf, 0, sizeof(buf)) ;
#endif /* XDRSTDIO */
      }

    if(x_op == XDR_ENCODE) /* back to encode */
        xdrs->x_op = x_op ;
 
    if(which != 0) which = 2 ;

    if(xdrs->x_op == XDR_ENCODE)
      {
          buf[which +1] = *values % 256 ;
          buf[which] = (*values >> 8) ;

          if( !xdr_setpos(xdrs, origin) )
              return(FALSE) ;
          if( !xdr_opaque(xdrs, (caddr_t)buf, 4))
              return(FALSE) ;
      }
    else
      {
          *values = ((buf[which] & 0x7f) << 8) + buf[which + 1] ;
          if(buf[which] & 0x80)
            {
                /* extern is neg */
                *values -= 0x8000 ;
            }
      }
    return(TRUE) ;
}


/*
 * xdr a single datum of type 'type' at 'where'
 */
static bool_t 
xdr_NCv1data(xdrs, where, type, values)
XDR *xdrs ;
u_long where ;
nc_type type ;
Void *values ;
{
	u_long rem=0 ;

	switch(type){
	case NC_BYTE :
	case NC_CHAR :
	case NC_SHORT :
		rem = where%4 ;
		where -= rem ; /* round down to nearest word */
		break ;
    default:
        break;
	}
	if( !xdr_NCsetpos(xdrs, where) )
		return(FALSE) ;

	switch(type){
	case NC_BYTE :
	case NC_CHAR :
		return( xdr_NCvbyte(xdrs, (unsigned)rem, (unsigned)1,
                            (char *)values) ) ;
	case NC_SHORT :
		return( xdr_NCvshort(xdrs, (unsigned)rem/2, (short *)values) ) ;
	case NC_LONG :
#if defined _CRAYMPP
		return( xdr_short(xdrs, (nclong *)values) ) ;
#elif defined __alpha || (_MIPS_SZLONG == 64) || defined __ia64 || (defined __sun && defined _LP64) || defined AIX5L64 || defined __x86_64__
		return( xdr_int(xdrs, (nclong *)values) ) ;
#else
		return( xdr_long(xdrs, (nclong *)values) ) ;
#endif
	case NC_FLOAT :
		return( xdr_float(xdrs, (float *)values) ) ;
	case NC_DOUBLE : 
		return( xdr_double(xdrs, (double *)values) ) ;
    default:
        break;
	}
	return(FALSE) ;
}

/*****************************************************************************
* 
*			NCSA HDF / netCDF Project
*			       May, 1993
*
* NCSA HDF / netCDF source code and documentation are in the public domain.  
* Specifically, we give to the public domain all rights for future
* licensing of the source code, all resale rights, and all publishing rights.
* 
* We ask, but do not require, that the following message be included in all
* derived works:
* 
* Portions developed at the National Center for Supercomputing Applications at
* the University of Illinois at Urbana-Champaign.  Funding for this project 
* has come primarily from the National Science Foundation.
* 
* THE UNIVERSITY OF ILLINOIS GIVES NO WARRANTY, EXPRESSED OR IMPLIED, FOR THE
* SOFTWARE AND/OR DOCUMENTATION PROVIDED, INCLUDING, WITHOUT LIMITATION,
* WARRANTY OF MERCHANTABILITY AND WARRANTY OF FITNESS FOR A PARTICULAR PURPOSE
* 
******************************************************************************
*
* Please report all bugs / comments to hdfhelp@ncsa.uiuc.edu
*
*****************************************************************************/

#ifdef HDF

PRIVATE int32 tBuf_size = 0;
PRIVATE int32 tValues_size = 0;
PRIVATE int8  *tBuf = NULL;
PRIVATE int8  *tValues = NULL;

/* ------------------------------ SDPfreebuf ------------------------------ */
/*
    Throw away the temporary buffer we've allocated 
*/
intn 
SDPfreebuf()
{
    int ret_value = SUCCEED;

    if(tBuf != NULL)
      {
          HDfree(tBuf);
          tBuf = NULL;
          tBuf_size = 0;
      } /* end if */

    if(tValues != NULL)
      {
          HDfree(tValues);
          tValues = NULL;
          tValues_size = 0;
      } /* end if */

#ifdef LATER
done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
     /* Normal cleanup */
#endif /* LATER */

    return ret_value;
}

/* ------------------------------ SDIresizebuf ------------------------------ */
/*
    Resize a temporary buffer to the proper size
*/
static intn 
SDIresizebuf(void * *buf,int32 *buf_size,int32 size_wanted)
{
    intn ret_value = SUCCEED;

    if(*buf_size < size_wanted)
      {
        if(*buf)
            HDfree(*buf);
        *buf_size = size_wanted;
        *buf = HDmalloc(size_wanted);
        if (*buf == NULL) 
          {
            *buf_size=0;
            ret_value = FAIL;
            goto done;
          } /* end if */
      } /* end if */

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
     /* Normal cleanup */

    return ret_value;
} /* end SDIresizebuf() */

#define MAX_SIZE 1000000

/* ------------------------- hdf_get_data ------------------- */
/*
 * Given a variable vgid return the id of a valid data storage
 * If no data storage is found, hdf_get_data returns DFREF_NONE(0).
 * OLD WAY: Create and fill in the VS as a side effect if it doesn't
 *          exist yet <- not any more
 *
 * NEW WAY: we delay filling until data is  written out -QAK
 * 
 */
intn 
hdf_get_data(handle, vp)
NC *handle;
NC_var *vp;
{
    int32     vg = FAIL;
    int32     vsid = DFREF_NONE;
    int32     tag, t, n;
    int       ret_value = DFREF_NONE;
    
#ifdef DEBUG 
    fprintf(stderr, "hdf_get_data I've been called\n");
#endif
    
    if(NULL == handle) 
      {
          ret_value = DFREF_NONE;
          goto done;
      }

    if(NULL == vp) 
      {
          ret_value = DFREF_NONE;
          goto done;
      }

    /* 
     * if it is stored as NDGs we can't do any better than what was
     *    originally stored in vp->data_ref
     */
    if(vp->data_tag == DFTAG_SDS)
      {
          ret_value = vp->data_ref;
          goto done;
      }
    
    if(vp->vgid) 
      {
          /* attach to the variable's Vgroup */
          vg = Vattach(handle->hdf_file, vp->vgid, "r");
          if(FAIL == vg)
            {
                ret_value = DFREF_NONE;
                goto done;
            }
        
          /* loop through looking for a data storage object */
          n = Vntagrefs(vg);
          if(FAIL == n)
            {
                ret_value = DFREF_NONE;
                goto done;
            }

          for(t = 0; t < n; t++) 
            {
                if (FAIL == Vgettagref(vg, t, &tag, &vsid))
                  {
                      ret_value = DFREF_NONE;
                      goto done;
                  }

                if(tag == DATA_TAG) 
                  {   /* detach */
                      if (FAIL == Vdetach(vg))
                        {
                            ret_value = DFREF_NONE;
                            goto done;
                        }
                      ret_value = vsid;
                      goto done;
                  } /* end if */
            }
          /* don't forget to let go of vgroup */
          if (FAIL == Vdetach(vg))
            {
                ret_value = DFREF_NONE;
                goto done;
            }
      }
    
    /* are we only in read-only mode? */
    if(handle->hdf_mode == DFACC_RDONLY)
      { /* yes, not good */
          ret_value = DFREF_NONE;
          goto done;
      }
  
    /* 
     * create a new data storage object
     */
#ifdef DEBUG
    fprintf(stderr, "--- Creating new data storage (len = %d) --- \n", vp->len);
    fprintf(stderr, "shape[0]= %d shape[1]= %d\n", vp->shape[0], vp->shape[1]);
    fprintf(stderr, "dsize[0]= %d dsize[1]= %d\n", vp->dsizes[0], vp->dsizes[1]);
#endif  
    
    
    /* --------------------------------------
     *
     * allocate new data storage 
     *
     * --------------------------------------
     */

#ifdef NOT_YET
    vsid = Htagnewref(handle->hdf_file,DATA_TAG);
#else /* NOT_YET */
    vsid = Hnewref(handle->hdf_file);
#endif /* NOT_YET */
#ifdef DEBUG
    fprintf(stderr, "--- Allocating new data storage szof=%d, to_do=%d\n",(int)vp->szof, (int)to_do);
    fprintf(stderr, "byte_count=%d\n", (int)byte_count);
#endif  

    /* if it is a record var might as well make it linked blocks now */
    if(IS_RECVAR(vp)) 
      {
          int32 block_size; /* the size of the linked blocks to use */

          /* The block size is calculated according to the following heuristic: */
          /*   First, the block size the user set is used, if set. */
          /*   Second, the block size is calculated according to the size being */
          /*           written times the BLOCK_MULT value, in order to make */
          /*           bigger blocks if the slices are very small. */
          /*   Third, the calculated size is check if it is bigger than the */
          /*           MAX_BLOCK_SIZE value so that huge empty blocks are not */
          /*           created.  If the calculated size is greater than */
          /*           MAX_BLOCK_SIZE, then MAX_BLOCK_SIZE is used */
          /* These are very vague heuristics, but hopefully they should avoid */
          /* some of the past problems... -QAK */
          if(vp->block_size != (-1)) /* use value the user provided, if available */
              block_size = vp->block_size;
          else 
            { /* try figuring out a good value using some heuristics */
                block_size = vp->len*BLOCK_MULT;
                if (block_size > MAX_BLOCK_SIZE)
                    block_size = MAX_BLOCK_SIZE;
            } /* end else */

          vp->aid = HLcreate(handle->hdf_file, DATA_TAG, vsid, block_size,
                             BLOCK_COUNT);
          if(vp->aid == FAIL) 
            {
                ret_value = DFREF_NONE;
                goto done;
            }

          if(Hendaccess(vp->aid) == FAIL) 
            {
                ret_value = DFREF_NONE;
                goto done;
            }
      }

    if(vp->vgid) 
      {
          /* attach to the variable's Vgroup */
          vg = Vattach(handle->hdf_file, vp->vgid, "w");
          if(vg == FAIL)
            {
                ret_value = DFREF_NONE;
                goto done;
            }
        
          /* add new Vdata to existing Vgroup */
          if (FAIL == Vaddtagref(vg, (int32) DATA_TAG, (int32) vsid))
            {
                ret_value = DFREF_NONE;
                goto done;
            }

        
          /* detach from the variable's VGroup --- will no longer need it */
          if (FAIL == Vdetach(vg))
            {
                ret_value = DFREF_NONE;
                goto done;
            }
      }
        
#ifdef DEBUG 
    fprintf(stderr, "Done with the DATA Vdata returning id %d\n", vsid);
#endif

    vp->aid = FAIL;

    /* added a new object -- make sure we flush the header */
    handle->flags |= NC_HDIRTY;
            
    ret_value = vsid;

done:
    if (ret_value == DFREF_NONE)
      { /* Failure cleanup */
          if (vg != FAIL)
            {                
                Vdetach(vg); /* no point in catch error here if we fail */
            }
      }
     /* Normal cleanup */

    return ret_value;
} /* hdf_get_data */


/* ---------------------------- hdf_get_vp_aid ---------------------------- */
/*

  Return an AID for the current variable.  Return FAIL on error SUCCEED on success

*/
int32
hdf_get_vp_aid(handle, vp)
NC        * handle;
NC_var    * vp;
{
    int32 ret_value = SUCCEED;

    /* attach to proper data storage*/
    if(!vp->data_ref)
        vp->data_ref = hdf_get_data(handle, vp);

    /*
     * Fail if there is no data
     */
    if(vp->data_ref == DFREF_NONE) 
      {
          ret_value = FAIL;
          goto done;
      }

    if(handle->hdf_mode == DFACC_RDONLY)
        vp->aid = Hstartread(handle->hdf_file, vp->data_tag, vp->data_ref);
    else
      {
#ifdef OLD_WAY
          vp->aid = Hstartwrite(handle->hdf_file, vp->data_tag, vp->data_ref, 0);
#else /* OLD_WAY */
        if(!IS_RECVAR(vp)) {
          vp->aid = Hstartaccess(handle->hdf_file, vp->data_tag, vp->data_ref, DFACC_WRITE);
          if(vp->set_length==TRUE) {
              Hsetlength(vp->aid,vp->len);
              vp->set_length=FALSE;
          }
        }
        else
          vp->aid = Hstartaccess(handle->hdf_file, vp->data_tag, vp->data_ref, DFACC_WRITE|DFACC_APPENDABLE);
#endif /* OLD_WAY */
      }
    
    ret_value = vp->aid;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
     /* Normal cleanup */

    return ret_value;
} /* hdf_get_vp_aid */

 
/* --------------------------- hdf_xdr_NCvdata ---------------------------- */
/*
 *  Read / write 'count' items of contiguous data of type 'type' at 'where'
 *
 * If we are not already attached to the VData representing the
 *  data attach to it now.  Since attaching / detaching is so
 *  slow, stay attached for future reads / writes.  As a result,
 *  we must always attach with write access.
 *
 * The calling routine is responsible for calling DFKsetNT() as required.
 */
static intn
hdf_xdr_NCvdata(NC *handle, 
                NC_var *vp, 
                u_long where, 
                nc_type type, 
                uint32 count, 
                void * values)
{
    NC_attr **attr = NULL;  /* pointer to the fill-value attribute */
    int32   status;
    int32  byte_count;	/* total # of bytes of data to be processed */
    int32  elements_left;/* number of elements still left to be processed */
    int32  data_size;	/* size of data block being processed in bytes */
    int32  new_count;	/* computed by dividing number of elements 'count' by 2 since 'count' is too big to allocate temporary buffer */
    int32  bytes_left;
    int32  elem_length;	/* length of the element pointed to */
    int8   platntsubclass;  /* the machine type of the current platform */
    int8   outntsubclass;   /* the data's machine type */
    uintn  convert;         /* whether to convert or not */
    int16  isspecial;
    intn   ret_value = SUCCEED;
    int32 alloc_status = FAIL;	/* no successful allocation yet */

#ifdef DEBUG
    fprintf(stderr, "hdf_xdr_NCvdata I've been called : %s\n", vp->name->values);
#endif

#ifdef DEBUG 
    fprintf(stderr, "Where = %d  count = %d\n", where, count);
#endif
    

    if(vp->aid == FAIL 
       && hdf_get_vp_aid(handle, vp) == FAIL) 
      {
          /*
           * Fail if there is no data *AND* we were trying to read...
           * Otherwise, we should fill with the fillvalue
           */
#ifdef DEBUG
          fprintf(stderr, "hdf_xdr_NCvdata creating new data, check for fill value, vp->data_ref=%d\n",(int)vp->data_ref);
#endif
          if(vp->data_ref == 0) 
            {
                if(handle->hdf_mode == DFACC_RDONLY) 
                  {
                      if(vp->data_tag == DATA_TAG || vp->data_tag == DFTAG_SDS) 
                        {
                            if((attr = NC_findattr(&vp->attrs, _FillValue))!= NULL)
                                HDmemfill(values,(*attr)->data->values,vp->szof,count);
                            else 
                                NC_arrayfill(values, count * vp->szof, vp->type);
                        }

                      ret_value = SUCCEED;
                      goto done;
                  } 
                else 
                  {
                      ret_value =  FAIL;
                      goto done;
                  }
            }
      }

    /* 
       Figure out if the tag/ref is a compressed special-element with no data.
       This "template" tag/ref is treated as if the tag/ref doesn't exist at
       all:  reading from it fills a memory buffer and returns it to the user
       and writing to it fills up the buffer around the block to write.
    */
    if(Hinquire(vp->aid,NULL,NULL,NULL,&elem_length,NULL,NULL,NULL,&isspecial) == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }

#ifdef DEBUG 
    fprintf(stderr, "vp->aid=%d, length=%ld, byte_count=%ld\n", 
            (int)vp->aid, (long)elem_length, (long)byte_count);
#endif
    /* Check for zero-length compressed special element, i.e. a template */
    if(elem_length <= 0)
      {
          attr = NC_findattr(&vp->attrs, _FillValue);

          /* Check for reading from template & fill memory buffer with fill-value */
          if(handle->xdrs->x_op == XDR_DECODE) 
            {
                if(attr != NULL)
                    HDmemfill(values,(*attr)->data->values,vp->szof,count);
                else 
                    NC_arrayfill(values, count * vp->szof, vp->type);

                ret_value = SUCCEED; /* we are done */
                goto done;
            } /* end if */
      } /* end if */

    /* Collect all the number-type size information, etc. */
    byte_count = count * vp->HDFsize;

#if 0 /* old way */
    platntsubclass = DFKgetPNSC(vp->HDFtype, DF_MT); 
    outntsubclass = DFKisnativeNT(vp->HDFtype) ? DFKgetPNSC(vp->HDFtype, DF_MT)
	    : (DFKislitendNT(vp->HDFtype) ? DFNTF_PC : DFNTF_HDFDEFAULT);
#else /* new way */
    if (FAIL == (platntsubclass = DFKgetPNSC(vp->HDFtype, DF_MT)))
      {
          ret_value = FAIL;
          goto done;
      }

    if (DFKisnativeNT(vp->HDFtype))
      {
          if (FAIL == (outntsubclass = DFKgetPNSC(vp->HDFtype, DF_MT)))
            {
                ret_value = FAIL;
                goto done;
            }
      }
    else
      {
          outntsubclass = DFKislitendNT(vp->HDFtype) ? DFNTF_PC : DFNTF_HDFDEFAULT;
      }

#endif
    convert= (uintn)(platntsubclass!=outntsubclass);

/* BMR - bug#268: removed the block here that attempted to allocation
large amount of space and failed.  The allocation is not incorporated
in the reading values, writing values, and writing fill values parts in
this routine */

#ifdef DEBUG
    fprintf(stderr, "hdf_xdr_NCvdata: tBuf_size=%d, tBuf=%p\n",(int)tBuf_size,tBuf);
#endif

    /* 
     * It may be the case that the current does NOT begin at the start of the
     *   data-object which is storing it.  In that case compute the correct 
     *   location.
     * QAK: This shouldn't be an issue for compressed template objects.
     */
#ifdef DEBUG
    fprintf(stderr, "hdf_xdr_NCvdata: vp->data_offset=%d, where=%d\n",(int)vp->data_offset,(int)where);
#endif

    if(vp->data_offset > 0) 
      {
          where += vp->data_offset;
          /* if the dataset doesn't exist yet, we need to fill in the dimension scale info */
          if(elem_length <= 0 && (handle->flags & NC_NOFILL) == 0)
          {
	  /* BMR: work around for the low memory problem by repeatedly 
             processing smaller amount blocks of data */

             /* compute the data block size and the # of elements*/
             data_size = MAX( byte_count, where ); 
             new_count = vp->data_offset/vp->HDFsize; 

	     /* attempt to allocate the entire amount needed first, data_size bytes */
	     alloc_status = SDIresizebuf((void **)&tBuf, &tBuf_size, data_size );

	     /* if fail to allocate, repeatedly calculate a new amount
	     and allocate until success or until no more memory available */
	     while( alloc_status == FAIL )
		{
		   new_count = new_count / 2;	/* try smaller number of elements */
                   if( new_count <= 0 )  /* unable to allocate any memory */
                   {
                      ret_value = FAIL;
                      goto done;
                   }
		  /* re-calculate the size of the data block using smaller # of elements */
		   data_size = new_count * vp->szof;
		   alloc_status = SDIresizebuf((void **)&tBuf,&tBuf_size, data_size);
		} /* while trying to allocate */

                /* assume that all elements are to be processed */
		elements_left = vp->data_offset/vp->HDFsize;

		/* repeatedly read, convert, and store blocks of data_size 
                   bytes of data into the user's buffer until no more elements 
                   left */
		while( elements_left > 0 )
		{
                   /* Fill the temporary buffer with the fill-value */
                   if(attr != NULL)
                      HDmemfill(tBuf,(*attr)->data->values,vp->szof, new_count);
                   else 
                      NC_arrayfill(tBuf, data_size, vp->type);

                   /* convert the fill-values, if necessary */
                   if(convert) 
                   {
                      if (FAIL == DFKconvert(tBuf, tBuf, vp->HDFtype, (uint32)new_count, DFACC_WRITE, 0, 0))
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                   } /* end if convert */

                   /* Write the fill-values out */
                   status = Hwrite(vp->aid, data_size, tBuf);
                   if (data_size == status)
                   {
                      ret_value = FAIL;
                      goto done;
                   }

		   /* compute the number of elements left to be processed */
		   elements_left = elements_left - new_count;

		   /* adjust the # of elements in the final block and
		      compute that block's size if necessary */
		   if( elements_left > 0 && elements_left < new_count )
		   {
		      new_count = elements_left; 
		      data_size = new_count * vp->szof;
		   }
	        } /* while more elements left to be processed */

	        SDPfreebuf();  /* free tBuf and tValues if any exists */
	       /* end of BMR part */
            } /* end if */
      } /* end if */
    
#ifdef DEBUG
    fprintf(stderr, "hdf_xdr_NCvdata vp->aid=%d, where=%d\n",
            (int)vp->aid,(int)where);
#endif
    /* if we get here and the length is 0, we need to fill in the initial set of fill-values */
    if(elem_length <= 0 && where > 0)
      { /* fill in the lead sequence of bytes with the fill values */
          if((handle->flags & NC_NOFILL)==0 || isspecial==SPECIAL_COMP)
            {
                int32 buf_size = where;
                int32 chunk_size;
		int32 tempbuf_size;	/* size to allocate buffer tBuf */
                uint8 *write_buf = NULL;
                uint32 fill_count;	/* number of fill values */

		/* this block is to work around the failure caused by
		   allocating a large chunk for the temporary buffers.
		   First, try to allocate the desired chunk for both
		   buffers; if any allocation fails, reduce the chunk size
		   in half and try again until both buffers are
		   successfully allocated - BMR */ 

		chunk_size = MIN(buf_size, MAX_SIZE);   /* initial chunk size */
		alloc_status = FAIL;

                /* while any allocation fails */
		while( alloc_status == FAIL )
		{
		/* try to allocate the buffer to hold the fill values after conversion */
		   alloc_status = SDIresizebuf((void * *)&tValues,&tValues_size,chunk_size);
		   /* then, if successful, try to allocate the temporary
		   buffer that holds the fill values before conversion */
                   if( alloc_status != FAIL)
		   {
		      /* calculate the size needed to allocate tBuf by
		         first calculating the number of fill values that
		         cover the chunk in buffer tValues after conversion... */
		      fill_count = chunk_size/vp->HDFsize;

		      /* then use that number to compute the size of
		         the buffer to hold fill_count fill values of type
		         vp->szof, i.e., before conversion */
		      tempbuf_size = fill_count * vp->szof;
		      alloc_status = SDIresizebuf((void **)&tBuf,&tBuf_size, tempbuf_size);
		   } /* if first allocation successes */
					
		   if( alloc_status == FAIL )      /* if any allocations fail */
		      chunk_size = chunk_size / 2;  /* try smaller chunk size */

                   if( chunk_size <= 0 )  /* unable to allocate any memory */
                   {
                      ret_value = FAIL;
                      goto done;
                   }
		} /* while any allocation fails */

		/* Fill the temporary buffer tBuf with the fill-value
		  specified in the attribute if one exists, otherwise,
		  with the default value */ 
                if(attr != NULL)
                    HDmemfill(tBuf,(*attr)->data->values, vp->szof, fill_count);
                else 
                    NC_arrayfill(tBuf, tempbuf_size, vp->type);

		/* convert the fill-values, if necessary, and store
		them in the buffer tValues */
                if(convert)
                {
                   if (FAIL == DFKconvert(tBuf, tValues, vp->HDFtype, fill_count, DFACC_WRITE, 0, 0))
                     {
                         ret_value = FAIL;
                         goto done;
                     }    
                   write_buf=(uint8 *)tValues;
                } /* end if */
                else
                   write_buf=(uint8 *)tBuf;

                do {
                    /* Write the fill-values out */
                    status = Hwrite(vp->aid, chunk_size, write_buf);
                    if (status != chunk_size)
                      {
                          ret_value = FAIL;
                          goto done;
                      }    

                    /* reduce the bytes to be written */
                    buf_size -= chunk_size;

		    /* to take care of the last piece of data */
		    chunk_size = MIN( chunk_size, buf_size );
                } while (buf_size > 0);
            } /* end if */
          else
            { /* don't write fill values, just seek to the correct location */
                if(Hseek(vp->aid, where, DF_START) == FAIL)
                  {
                      ret_value = FAIL;
                      goto done;
                  }    
            } /* end else */
      } /* end if */
    else
      { /* position ourselves correctly */

#ifdef DEBUG
          fprintf(stderr, "hdf_xdr_NCvdata: Check 2.0\n");
#endif
          if(elem_length > 0)
            {
                if( Hseek(vp->aid, where, DF_START) == FAIL)
                  {
                      ret_value = FAIL;
                      goto done;
                  }    
            }
    
#ifdef DEBUG
    fprintf(stderr, "hdf_xdr_NCvdata after Hseek(), byte_count=%d\n",(int)byte_count);
#endif
    
      } /* end else */

    /* Read or write the data into / from values */
    if(handle->xdrs->x_op == XDR_DECODE)  /* the read case */
    {
       if(convert) /* if data need to be converted for this platform */
       {
          data_size = byte_count; /* use data_size; preserve the byte count */
          new_count = count;     /* use new_count; preserve the # of elements */

          /* attempt to allocate the entire amount needed first */
          alloc_status = SDIresizebuf((void **)&tBuf,&tBuf_size, data_size );

	  /* if fail to allocate, repeatedly calculate a new amount and
	     allocate until success or until no memory available */
          while( alloc_status == FAIL )
          {
             new_count = new_count / 2;  /* try smaller number of elements */
             if( new_count <= 0 )  /* unable to allocate any memory */
             {
                ret_value = FAIL;
                goto done;
             }

             /* re-calculate the size of the data block */
             data_size = new_count * vp->szof;
             alloc_status = SDIresizebuf((void **)&tBuf,&tBuf_size, data_size);
          }

	  /* repeatedly read, convert, and store blocks of data_size
	     bytes of data into the user's buffer until no more elements
	     left */
             elements_left = count;  /* 'count' is number of elements to
be processed */
             while( elements_left > 0 )
             {
                status = Hread(vp->aid, data_size, tBuf);
                if(status != data_size)  /* amount read != amount specified */
                  {
                      ret_value = FAIL;
                      goto done;
                  }    
                /* convert and store new_count elements in tBuf into 
                   the buffer values */
                if (FAIL == DFKconvert(tBuf, values, vp->HDFtype, (uint32) new_count, DFACC_READ, 0, 0))
                  {
                      ret_value = FAIL;
                      goto done;
                  }

		/* compute the number of elements left to be processed */
                elements_left = elements_left - new_count;

		/* adjust the # of elements in the final block and
                   compute that block's size if necessary */
                if( elements_left > 0 && elements_left < new_count )
                {
                   new_count = elements_left; 
                   data_size = new_count * vp->szof;
                }
              } /* while more elements left to be processed */
              SDPfreebuf();
            } /* end if convert */
          else  /* no convert, read directly into the user's buffer */
            {
                status = Hread(vp->aid, byte_count, values);
                if(status != byte_count)
                  {
                      ret_value = FAIL;
                      goto done;
                  }    
            } /* end else */
      } /* end if XDR_DECODE */
    else 
      {/* XDR_ENCODE */
          if(convert) /* if data need to be converted for this platform */
          {
             data_size = byte_count; /* use data_size; preserve the byte count*/
             new_count = count;  /* use new_count; preserve the # of elements */

             /* attempt to allocate the entire amount needed first */
             alloc_status = SDIresizebuf((void **)&tBuf,&tBuf_size, data_size);

             /* if fail to allocate, repeatedly calculate a new amount and 
                allocate until success or no more memory left */
             while( alloc_status == FAIL )
             {
                new_count = new_count / 2;  /* try smaller number of elements */
                if( new_count <= 0 )  /* unable to allocate any memory */
                {
                   ret_value = FAIL;
                   goto done;
                }

                /* re-calculate the size of the data block */ 
                data_size = new_count * vp->HDFsize;
                alloc_status = SDIresizebuf((void **)&tBuf,&tBuf_size, data_size);
             }

            /* repeatedly convert, store blocks of data_size bytes of data
               from the user's buffer into the temporary buffer, and write 
               out the temporary buffer until no more bytes left */
            elements_left = count;   /* all elements are left to be processed */
            while( elements_left > 0 )
            {
               /* convert new_count elements in the user's buffer values and 
                  write them into the temporary buffer */
               if (FAIL == DFKconvert(values, tBuf, vp->HDFtype, (uint32) new_count, DFACC_WRITE, 0, 0))
               {
                   ret_value = FAIL;
                   goto done;
               }    
               status = Hwrite(vp->aid, data_size, tBuf);
               if(status != data_size) 
               {
                   ret_value = FAIL;
                   goto done;
               }    
		/* compute the number of elements left to be processed */
                elements_left = elements_left - new_count;

		/* adjust the # of elements in the final block and
                   compute that block's size if necessary */
                if( elements_left > 0 && elements_left < new_count )
                {
                   new_count = elements_left; 
                   data_size = new_count * vp->szof;
                }
              } /* while more elements left to be processed */
            SDPfreebuf();  /* free tBuf and tValues if any exist */
            } /* end if convert */
          else 
          { /* no convert, write directly from the user's buffer */
              status = Hwrite(vp->aid, byte_count, values);

#ifdef DEBUG
          fprintf(stderr, "hdf_xdr_NCvdata: status=%d\n",(int)status);
          if(status==FAIL)
              HEprint(stdout,0);
#endif
          if(status != byte_count) 
            {
                ret_value = FAIL;
                goto done;
            }
          } /* no convert */
      } /* XDR_ENCODE */

    /* if we get here and the length is 0, we need to finish writing out the fill-values */
    bytes_left = vp->len - (where + byte_count);
#ifdef DEBUG
    fprintf(stderr, "hdf_xdr_NCvdata: bytes_left=%d\n",(int)bytes_left);
#endif
    if(elem_length <= 0 && bytes_left > 0)
      {
          if((handle->flags & NC_NOFILL) == 0 || isspecial == SPECIAL_COMP)
          {
             int32 buf_size = bytes_left;
             int32 chunk_size;
             int32 tempbuf_size;    /* num of bytes to allocate buffer tBuf */
             uint8 *write_buf = NULL;
             uint32 fill_count;     /* number of fill values */

	     /* this block is to work around the failure caused by
	        allocating a large chunk for the temporary buffers.
	        First, try to allocate the desired chunk for both buffers;
	        if any allocation fails, reduce the chunk size in half and
	        try again until both buffers are successfully allocated - BMR */

		chunk_size = MIN(buf_size, MAX_SIZE); /* initial chunk size */

		/* while any allocation fails */ 
		alloc_status = FAIL;
                while( alloc_status == FAIL ) 
                {
                   /* first, try to allocate the buffer to hold the fill 
                      values after conversion */
                   alloc_status = SDIresizebuf((void * *)&tValues,&tValues_size,chunk_size);

		   /* then, if successful, try to allocate the temporary
		      buffer that holds the fill values before conversion */
		      if( alloc_status != FAIL)
                   {
		      /* calculate the size needed to allocate tBuf
		         by first calculating the number of fill values that
			 cover the chunk in buffer tValues after conversion...*/
		      fill_count = chunk_size/vp->HDFsize;

                      /* then use that number to compute the size of the 
                         buffer to hold fill_count fill values of type 
			 vp->szof, i.e., before conversion */
                      tempbuf_size = fill_count * vp->szof;
                      alloc_status = SDIresizebuf((void **)&tBuf,&tBuf_size, tempbuf_size);
                   } /* if first allocation successes */
                                        
                   if( alloc_status == FAIL ) /* if any allocations fail */
                      chunk_size = chunk_size / 2; /* try smaller chunk size */

                   if( chunk_size <= 0 )  /* unable to allocate any memory */
                   {
                      ret_value = FAIL;
                      goto done;
                   }

                } /* while any allocation fails */

                /* Fill the temporary buffer tBuf with the fill-value specified                    in the attribute if one exists, otherwise, with the default value */
                if(attr != NULL)
                    HDmemfill(tBuf,(*attr)->data->values, vp->szof, fill_count);
                else 
                    NC_arrayfill(tBuf, tempbuf_size, vp->type);

                /* convert the fill-values, if necessary, and store them in the buffer tValues */
                if(convert) 
                  {
                      if (FAIL == DFKconvert(tBuf, tValues, vp->HDFtype, fill_count, DFACC_WRITE, 0, 0))
                        {
                            ret_value = FAIL;
                            goto done;
                        }    
                      write_buf=(uint8 *)tValues;
                  } /* end if */
                else
                    write_buf=(uint8 *)tBuf;

                do {
                    /* Write the fill-values out */
                    status = Hwrite(vp->aid, chunk_size, write_buf);
                    if (status != chunk_size)
                      {
                          ret_value = FAIL;
                          goto done;
                      }    

                    /* reduce the bytes still to be written */
                    buf_size -= chunk_size;

                    /* to take care of the last piece of data */
                    chunk_size = MIN( chunk_size, buf_size );
                } while (buf_size > 0);
            } /* end if */
      } /* end if */

#ifdef DEBUG
    fprintf(stderr, " * * * Done with call to xdr_NCvdata * * *\n");
#endif

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
     /* Normal cleanup */

    return ret_value;
} /* hdf_xdr_NCvdata */


/* ------------------------- hdf_xdr_NCv1data ------------------- */
/*
 * read / write a single datum of type 'type' at 'where'
 * This is designed to replace the xdr based routine of the
 *  similar name
 * Return TRUE if everything worked, else FALSE
 */
static intn
hdf_xdr_NCv1data(handle, vp, where, type, values)
NC      * handle;
NC_var  * vp;
u_long    where;
nc_type   type;
void *values;
{

    intn ret_value = SUCCEED;

    
    if (FAIL == DFKsetNT(vp->HDFtype))
      {
          ret_value = FAIL;
          goto done;
      }

    ret_value = hdf_xdr_NCvdata(handle, vp, where, type, 1, values); 

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
     /* Normal cleanup */

    return ret_value;
} /* hdf_xdr_NCv1data */

/* -------------------------- nssdc_xdr_NCvdata --------------------------- */
/*
 *  Read / write 'count' items of contiguous data of type 'type' at 'where'
 *    out of a CDF file
 *
 * The calling routine is responsible for calling DFKsetNT() as required.
 *
 * NOTE: Do we really care about CDF anymore since we don't support 
 *       it 100 percent -GV
 */
static bool_t
nssdc_xdr_NCvdata(NC *handle, 
                  NC_var *vp, 
                  u_long where, 
                  nc_type type, 
                  uint32 count, 
                  void * values)
{
    int32 status;
    int32 byte_count;

#ifdef DEBUG
    fprintf(stderr, "nssdc_xdr_NCvdata I've been called : %s\n", vp->name->values);
    fprintf(stderr, "Where = %d  count = %d\n", where, count);
    fprintf(stderr, "nssdc_xdr_NCvdata I've been called : %s reading %d from %d\n", 
            vp->name->values, count, where);
#endif

    /* position ourselves correctly */
    status = HI_SEEK((hdf_file_t) handle->cdf_fp, where);
    if(status == FAIL)
        return(FALSE);
    
    /* make sure our tmp buffer is big enough to hold everything */
    byte_count = count * vp->HDFsize;
    if(SDIresizebuf((void * *)&tBuf,&tBuf_size,byte_count)==FAIL)
        return(FALSE);

#ifdef DEBUG
    fprintf(stderr, "\tbyte_count %d   vp->HDFsize %d\n", byte_count, vp->HDFsize);
#endif

#ifdef QAK
    /* Read or write the data into / from values */
    if(handle->xdrs->x_op == XDR_DECODE) {
        status = HI_READ((hdf_file_t) handle->cdf_fp, tBuf, byte_count);
        if(status == FAIL) return FALSE;
        
        /* convert tBuf into values */
        DFKnumin(tBuf, values, (uint32) count, 0, 0);
        
    } else {

#ifdef CDF_WRITE      
        /*  convert values into tBuf */
        DFKnumout(values, tBuf, (uint32) count, 0, 0);
        
        status = Hwrite(vp->aid, byte_count, tBuf);
        if(status != byte_count) return FALSE;
#endif /* CDF_WRITE */

    }
#endif /* QAK */
    

#ifdef DEBUG
    fprintf(stderr, " * * * Done with call to nssdc_xdr_NCvdata * * *\n");
#endif
    
    return(TRUE);
    
} /* nssdc_xdr_NCvdata */


#endif /* HDF */


static
int NCvar1io(handle, varid, coords, value)
NC *handle ;
int varid ;
const long *coords ;
Void *value ;
{
	NC_var *vp ;
	u_long offset ;

	if(handle->flags & NC_INDEF )
		return(-1) ;
	if(handle->vars == NULL)
		return(-1) ;

	vp = NC_hlookupvar( handle, varid ) ;
	if(vp == NULL)
		return(-1) ;

	if(vp->assoc->count == 0) /* 'scaler' variable */
      {
#ifdef HDF
          switch(handle->file_type) 
            {
            case HDF_FILE:
           
                if (FAIL == hdf_xdr_NCv1data(handle, vp, vp->begin, vp->type, value))
                    return -1;
                else
                    return 0;
            case netCDF_FILE:
                return(xdr_NCv1data(handle->xdrs, vp->begin, vp->type, value) ?
                       0 : -1 ) ;
            }
#else /* !HDF */
          return(
              xdr_NCv1data(handle->xdrs, vp->begin, vp->type, value) ?
              0 : -1 ) ;
#endif /* !HDF */
      }

	if( !NCcoordck(handle, vp, coords) )
		return(-1) ;

	offset = NC_varoffset(handle, vp, coords) ;
		
#ifdef VDEBUG
	NCadvise(NC_NOERR, "%s offset %d, numrecs %d",
             vp->name->values, offset, vp->numrecs) ;
	arrayp("shape", vp->assoc->count, vp->shape) ;
	arrayp("coords", vp->assoc->count, coords) ;
#endif /* VDEBUG */
        
#ifdef HDF
    switch(handle->file_type) {
    case HDF_FILE:
        if(FAIL == hdf_xdr_NCv1data(handle, vp, offset, vp->type, value))
            return(-1) ;
        break;
    case netCDF_FILE:
        if( !xdr_NCv1data(handle->xdrs, offset, vp->type, value))
            return(-1) ;
        break;
    }
#else /* !HDF */
    if( !xdr_NCv1data(handle->xdrs, offset, vp->type, value))
        return(-1) ;
#endif /* !HDF */
        
	return(0) ;
}


int ncvarput1(cdfid, varid, coords, value)
int cdfid ;
int varid ;
const long *coords ;
const ncvoid *value ;
{
	NC *handle ;

	cdf_routine_name = "ncvarput1" ;

	handle = NC_check_id(cdfid) ;
	if(handle == NULL)
		return(-1) ;

    if(!(handle->flags & NC_RDWR))
      {
          NCadvise(NC_EPERM, "%s: NC_NOWRITE", handle->path) ;
          return(-1) ;
      }
	handle->xdrs->x_op = XDR_ENCODE ;

	return( NCvar1io(handle, varid, coords, value) ) ;
}


int ncvarget1(cdfid, varid, coords, value)
int cdfid ;
int varid ;
const long *coords ;
ncvoid *value ;
{
	NC *handle ;

	cdf_routine_name = "ncvarget1" ;

	handle = NC_check_id(cdfid) ;
	if(handle == NULL)
		return(-1) ;

	handle->xdrs->x_op = XDR_DECODE ;

	return( NCvar1io(handle, varid, coords, (Void *)value) ) ;
}


/*
 * xdr 'count' items of contiguous data of type 'type' at 'where'
 */
static bool_t 
xdr_NCvdata(xdrs, where, type, count, values)
XDR *xdrs ;
u_long where ;
nc_type type ;
unsigned count ;
Void *values ;
{
	u_long rem = 0 ;
	bool_t (*xdr_NC_fnct)() ;
	bool_t stat ;
	size_t szof ;

	switch(type){
	case NC_BYTE :
	case NC_CHAR :
	case NC_SHORT :
		rem = where%4 ;
		where -= rem ; /* round down to nearest word */
		break ;
    default:
        break;
	}
	if( !xdr_NCsetpos(xdrs, where) )
		return(FALSE) ;

	switch(type){
	case NC_BYTE :
	case NC_CHAR :
		if(rem != 0)
		{
			unsigned vcount = MIN(count, 4 - rem) ;
			if(!xdr_NCvbyte(xdrs, (unsigned)rem, vcount, values) )
				return(FALSE) ;
			values += vcount ;
			count -= vcount ;
		}

		rem = count%4 ; /* tail remainder */
		count -= rem ;
		if(!xdr_opaque(xdrs, values, count))
			return(FALSE) ;

		if(rem != 0)
		{
			values += count ;
			if( !xdr_NCvbyte(xdrs, (unsigned)0, (unsigned)rem ,
				values) )
				return(FALSE) ;
		 	return(TRUE) ;	
		} /* else */
		return(TRUE) ;
	case NC_SHORT :
		if(rem != 0)
		{
			if(!xdr_NCvshort(xdrs, (unsigned)1, (short *)values) )
				return(FALSE) ;
			values += sizeof(short) ;
			count -= 1 ;
		}
		rem = count%2 ; /* tail remainder */
		count -= rem ;
		if(!xdr_shorts(xdrs, (short *)values, count))
			return(FALSE) ;
		if(rem != 0)
		{
			values += (count * sizeof(short)) ;
			return( xdr_NCvshort(xdrs, (unsigned)0,
				(short *)values) ) ;
		} /* else */
		return(TRUE) ;
	case NC_LONG :
#if defined _CRAYMPP
                xdr_NC_fnct = xdr_short;
#elif defined __alpha || (_MIPS_SZLONG == 64) || defined __ia64 || (defined __sun && defined _LP64) || defined AIX5L64 || defined __x86_64__
		xdr_NC_fnct = xdr_int ;
#else
		xdr_NC_fnct = xdr_long ;
#endif
		szof = sizeof(nclong) ;
		break ;
	case NC_FLOAT :
		xdr_NC_fnct = xdr_float ;
		szof = sizeof(float) ;
		break ;
	case NC_DOUBLE : 
		xdr_NC_fnct = xdr_double ;
		szof = sizeof(double) ;
		break ;
	default :
		return(FALSE) ;
	}
	for(stat = TRUE ; stat && (count > 0) ; count--)
	{
		stat = (*xdr_NC_fnct)(xdrs,values) ;	
		values += szof ;
	}
	return(stat) ;
}


/*
 *  For a "hypercube" put/get, compute the largest contiguous block
 */
static const long *
NCvcmaxcontig(handle, vp, origin, edges)
NC *handle ;
NC_var *vp ;
const long *origin ;
const long *edges ;
{
	const long *edp, *orp ;
	unsigned long *boundary, *shp ;
    int partial=0;

	if( IS_RECVAR(vp) )
	{
		/*     one dimensional   &&  the only 'record' variable  */
		if(vp->assoc->count == 1 && handle->recsize <= vp->len)
		{
				return(edges) ;
		} /* else */
		boundary = vp->shape +1 ;
	}
	else
		boundary = vp->shape ;

	/* find max contiguous */
	shp = vp->shape + vp->assoc->count - 1 ;
	edp = edges + vp->assoc->count - 1 ;
	orp = origin + vp->assoc->count - 1 ;
	for( ; shp >= boundary ; shp--,edp--,orp--)
	{
		if(*edp > *shp - *orp || *edp < 0 )
		{
			NCadvise(NC_EINVAL, "Invalid edge length %d", *edp) ;
			return(NULL) ;
		}
		if(*edp < *shp )
          {
            partial=1;
			break ;
          }
	}
	if(shp < boundary) /* made it all the way */
		edp++ ;

    /*
     *   This little check makes certain that if complete "slices" of the
     *  regular dimensions of an unlimited dimension dataset are being written
     *  out, it's ok to write out a "block" of all those slices at once. -QAK
     */
/*	if( IS_RECVAR(vp) && (edp-1==edges) && !partial)
        edp=edges;
*/

	/* shp, edp reference last index s.t. shape[ii] == edge[ii] */
	return(edp) ;
}


static
int NCsimplerecio(handle, vp, start, edges, values)
NC *handle ;
NC_var *vp ;
const long *start ;
const long *edges ;
Void *values ;
{
	long offset ;
	long newrecs ;

	/* 'start' should be verified as valid upon prior to entry to this
	 * routine
	 */
	if(*edges <= 0)
      {
          NCadvise(NC_EINVALCOORDS, "%s: Invalid edge length %ld",
                   vp->name->values, *edges) ;
          return -1 ;
      }

	/* check to see if we are trying to read beyond the end */
	newrecs = (*start + *edges) - handle->numrecs ;
	if(handle->xdrs->x_op != XDR_ENCODE && newrecs > 0)
      {
          NCadvise(NC_EINVALCOORDS, "%s: Invalid Coordinates",
                   vp->name->values) ;
          return -1 ;
      }

	offset = NC_varoffset(handle, vp, start) ;
#ifdef VDEBUG
	fprintf(stderr, "\t\t %s offset %ld, *edges %lu\n",
            vp->name->values, offset, *edges ) ;
	arrayp("\t\t coords", vp->assoc->count, start) ;
#endif

	if(newrecs > 0)
		handle->flags |= NC_NDIRTY ;

#ifdef HDF
    switch(handle->file_type) 
      {
      case HDF_FILE:
          DFKsetNT(vp->HDFtype);
          if(FAIL == hdf_xdr_NCvdata(handle, vp,
                                     offset, vp->type, 
                                     (uint32)*edges, values))
              return(-1) ;
          break;
      case CDF_FILE:
          DFKsetNT(vp->HDFtype);
          if(!nssdc_xdr_NCvdata(handle, vp,
                                offset, vp->type, 
                                (uint32)*edges, values))
              return(-1) ;
          break;
      case netCDF_FILE:
          if(!xdr_NCvdata(handle->xdrs,
                          offset, vp->type, 
                          (unsigned)*edges, values))
              return(-1) ;
          break;
      }
#else /* !HDF */
    if(!xdr_NCvdata(handle->xdrs,
                    offset, vp->type, 
                    (unsigned)*edges, values))
        return(-1) ;
#endif /* !HDF */
        
#ifdef HDF
	if(newrecs > 0)
      {
          handle->numrecs += newrecs ;
	  vp->numrecs += newrecs;
          if(handle->flags & NC_NSYNC) /* write out header->numrecs NOW */
            {
                if(!xdr_numrecs(handle->xdrs, handle) )
                    return(-1) ;
                handle->flags &= ~NC_NDIRTY ;
            }
      }
#endif
	return(0) ;
}


/*
 * The following routine is not `static' because it is used by the `putgetg'
 * module for generalized hyperslab access.
 */
int NCvario(handle, varid, start, edges, values)
NC *handle ;
int varid ;
const long *start ;
const long *edges ;
void *values ;
{

	NC_var *vp ;
	const long *edp0, *edp ;
	unsigned long iocount ;

	if(handle->flags & NC_INDEF)
		return(-1) ;

	/* find the variable */
	if(handle->vars == NULL)
		return(-1) ;
	vp = NC_hlookupvar( handle, varid ) ;
	if(vp == NULL)
		return(-1) ;

#ifdef VDEBUG
	fprintf(stderr, "Entering NCvario, variable %s\n", vp->name->values ) ;
	arrayp("start", vp->assoc->count, start) ;
	arrayp("edges", vp->assoc->count, edges) ;
#endif /* VDEBUG */

#ifdef HDF
    if(handle->file_type != netCDF_FILE)
      {
          if (FAIL == DFKsetNT(vp->HDFtype))
              return -1;
      }
#endif

	if(vp->assoc->count == 0) /* 'scaler' variable */
      {
#ifdef HDF
          switch(handle->file_type) 
            {
            case HDF_FILE:
                if (FAIL == hdf_xdr_NCv1data(handle, vp, vp->begin, vp->type, values))
                    return -1;
                else
                    return 0;
            case netCDF_FILE:
                return(xdr_NCv1data(handle->xdrs, vp->begin, vp->type, values) ?
                       0 : -1 ) ;
            }
#else /* !HDF */
          return(xdr_NCv1data(handle->xdrs, vp->begin, vp->type, values) ?
                 0 : -1 ) ;
#endif /* !HDF */
      }

	if( !NCcoordck(handle, vp, start) )
		return(-1) ;

	if( IS_RECVAR(vp) 
		&& vp->assoc->count == 1
		&& handle->recsize <= vp->len)
      {
          /* one dimensional   &&  the only 'record' variable  */
          return(NCsimplerecio(handle, vp, start, edges, values)) ;
      }

	/* find max contiguous, check sanity of edges */
	edp0 = NCvcmaxcontig(handle, vp, start, edges) ;
	if(edp0 == NULL)
		return(-1) ;
#ifdef VDEBUG
	fprintf(stderr, "edp0\t%ld\n", (unsigned long)edp0 - (unsigned long)edges) ;
#endif /* VDEBUG */

	/* now accumulate max count for a single io operation */
	edp = edges + vp->assoc->count - 1 ; /* count is > 0 at this point */
	iocount = 1 ;
#ifdef VDEBUG
	fprintf(stderr, "edp\t%ld\n", (unsigned long)edp - (unsigned long)edges) ;
#endif /* VDEBUG */
	for( ; edp >= edp0 ; edp--)
		iocount *= *edp ;
	/* now edp = edp0 - 1 */

	{ /* inline */
        long  coords[MAX_VAR_DIMS], upper[MAX_VAR_DIMS];
        long  *cc ;
        const long *mm ;
        u_long offset ;
        size_t szof = nctypelen(vp->type) ;

        /* copy in starting indices */
        cc = coords ;
        mm = start ;
        while(cc < &coords[vp->assoc->count] )
            *cc++ = *mm++ ;
#ifdef VDEBUG
        arrayp("coords", vp->assoc->count, coords) ;
#endif

        /* set up in maximum indices */
        cc = upper ;
        mm = coords ;
        edp = edges ;
        while(cc < &upper[vp->assoc->count] )
            *cc++ = *mm++ + *edp++ ;
#ifdef VDEBUG
        arrayp("upper", vp->assoc->count, upper) ;
#endif

        /* ripple counter */
        cc = coords ;
        mm = upper ;
        while(*coords < *upper)
          {
#ifdef VDEBUG
              fprintf(stderr, "\t*cc %ld, *mm %ld\n",
                      *cc, *mm) ;
#endif /* VDEBUG */
              while( *cc < *mm )
                {
#ifdef VDEBUG
                    fprintf(stderr, "\t\tedp0 %p, edges %p, mm %p, &upper[] %p\n",
                            edp0, edges, mm, &upper[edp0-edges-1]) ;
#endif /* VDEBUG */
                    if(edp0 == edges || mm == &upper[edp0-edges-1])
                      {
                          /* doit */
                          if( !NCcoordck(handle, vp, coords) )
                              return(-1) ;
                          offset = NC_varoffset(handle, vp, coords) ;
#ifdef VDEBUG
                          fprintf(stderr, "\t\t %s offset %lu, iocount %lu\n",
                                  vp->name->values, offset, iocount ) ;
                          arrayp("\t\t coords", vp->assoc->count, coords) ;
#endif

#ifdef HDF
                          switch(handle->file_type) 
                            {
                            case HDF_FILE:
                                if(FAIL == hdf_xdr_NCvdata(handle, vp,
                                                           offset, vp->type, 
                                                           (uint32)iocount, values))
                                    return(-1) ;
                                break;
                            case CDF_FILE:
                                if(!nssdc_xdr_NCvdata(handle, vp,
                                                      offset, vp->type, 
                                                      (uint32)iocount, values))
                                    return(-1) ;
                                break;
                            case netCDF_FILE:
                                if(!xdr_NCvdata(handle->xdrs,
                                                offset, vp->type, 
                                                (unsigned)iocount, values))
                                    return(-1) ;
                                break;
                            }
#else /* !HDF */
                          if(!xdr_NCvdata(handle->xdrs,
                                          offset, vp->type, 
                                          (unsigned)iocount, values))
                              return(-1) ;
#endif /* !HDF */
                                
                          values = (void *)((const uint8 *)values + iocount * szof);
                          (*cc) += (edp0 == edges ? iocount : 1) ;
#ifdef VDEBUG
                          fprintf(stderr, "\t\t *cc %ld, *mm %ld  continue\n",
                                  *cc, *mm) ;
#endif /* VDEBUG */
                          continue ;
                      }
                    cc++ ;
                    mm++ ;
#ifdef VDEBUG
                    fprintf(stderr, "\t\t*cc %ld, *mm %ld\n",
                            *cc, *mm) ;
#endif /* VDEBUG */
                }
#ifdef VDEBUG
              fprintf(stderr, "\tcc %p, coords %p\n",
                      cc, coords) ;
#endif /* VDEBUG */
              if(cc == coords)
                {
#ifdef VDEBUG
                    fprintf(stderr, "\t break\n") ;
#endif /* VDEBUG */
                    break ;
                }
              *cc = start[ cc - coords ] ;
              cc-- ;
              mm-- ;
              (*cc)++ ;
#ifdef VDEBUG
              fprintf(stderr, "\t*coords %ld, *upper %ld\n",
                      *coords, *upper) ;
#endif
          }
#ifdef VDEBUG
        arrayp("coords", vp->assoc->count, coords) ;
        arrayp("upper", vp->assoc->count, upper) ;
        fprintf(stderr,"vp->numrecs=%d\n",vp->numrecs);
        fprintf(stderr,"upper[0]=%d\n",upper[0]);
#endif
        /*
         * This is a kludge to work around the fact the NCcoordck() doesn't
         * get the upper limits on the slab to write out -QAK
         */
        if(upper[0] > vp->numrecs)
            vp->numrecs=upper[0];
	} /* end inline */

#ifdef NOTNOW
    /* Albert and I agree that this check below makes perfect sense, but it
     * causes the ncdiminq test to fail for unlimited length dimensions.
     * Perhaps someone with more time can look into this later.  -QAK
     */
	if (handle->numrecs < vp->numrecs)
	    handle->numrecs = vp->numrecs;
#endif /* NOTNOW */

#ifdef VDEBUG
	fprintf(stderr, "Exiting NCvario\n") ;
#endif /* VDEBUG */
	return(0) ;
}


int ncvarput(cdfid, varid, start, edges, values)
int cdfid ;
int varid ;
const long *start ;
const long *edges ;
ncvoid *values ;
{
	NC *handle ;

	cdf_routine_name = "ncvarput" ;

	handle = NC_check_id(cdfid) ;
	if(handle == NULL)
		return(-1) ;

    if(!(handle->flags & NC_RDWR))
      {
          NCadvise(NC_EPERM, "%s: NC_NOWRITE", handle->path) ;
          return(-1) ;
      }
	handle->xdrs->x_op = XDR_ENCODE ;

	return( NCvario(handle, varid, start, edges, values) ) ;
}


int ncvarget(cdfid, varid, start, edges, values)
int cdfid ;
int varid ;
const long *start ;
const long *edges ;
ncvoid *values ;
{
	NC *handle ;

	cdf_routine_name = "ncvarget" ;

	handle = NC_check_id(cdfid) ;
	if(handle == NULL)
		return(-1) ;

	handle->xdrs->x_op = XDR_DECODE ;

	return( NCvario(handle, varid, start, edges, (Void *)values) ) ;
}

/* Begin recio */

/*
 * Returns number of record variables in an open netCDF file,
 * Optionally fills an array of record variable handles.
 * Optionally fills an array of record variable ids.
 * Returns -1 on error.
 */
static int
NCnumrecvars(handle, vpp, recvarids)
     NC *handle;
     NC_var **vpp;
	 int *recvarids;
{
	NC_var **dp ;
	int ii ;
	int nrecvars = 0;

	if(handle->vars == NULL)
		return -1 ;

	dp = (NC_var**)handle->vars->values ;
	for(ii = 0 ; ii < handle->vars->count ; ii++, dp++)
	{
		if(IS_RECVAR((*dp)))
		{
			if(vpp != NULL)
				vpp[nrecvars] = *dp ;
			if(recvarids != NULL)
				recvarids[nrecvars] = ii ;
			nrecvars++ ;
		}
	}
	return nrecvars ;
}


static long
NCelemsPerRec(vp)
NC_var *vp ;
{
	long nelems = 1 ;
	int jj ;
	for(jj = 1 ; jj < vp->assoc->count ; jj++)	
		nelems *= vp->shape[jj] ;
	return nelems ;
}


/*
 * Retrieves the number of record variables, the record variable ids, and the
 * record size of each record variable.  If any pointer to info to be returned
 * is null, the associated information is not returned.  Returns -1 on error.
 */
int
ncrecinq(cdfid, nrecvars, recvarids, recsizes)
int cdfid ;
int *nrecvars ;
int *recvarids ;
long *recsizes ;
{
	NC *handle ;
	int nrvars ;
	NC_var *rvp[MAX_NC_VARS] ; 

	cdf_routine_name = "ncrecinq" ;

	handle = NC_check_id(cdfid) ;
	if(handle == NULL)
		return(-1) ;
	
	nrvars = NCnumrecvars(handle, rvp, recvarids) ;
	if(nrvars == -1)
		return -1 ;

	if(nrecvars != NULL)
		*nrecvars = nrvars;
	
	if(recsizes != NULL)
	{
		int ii ;
		for(ii = 0 ; ii < nrvars ; ii++)
		{
			recsizes[ii] = nctypelen(rvp[ii]->type) * NCelemsPerRec(rvp[ii]) ;
		}
	}
	return nrvars ;
}


static int
NCrecio(handle, recnum, datap)
NC *handle ;
long recnum ;
Void **datap ;
{
	int nrvars ;
	NC_var *rvp[MAX_NC_VARS] ; 
	int ii ;
	long coords[MAX_VAR_DIMS];
	u_long offset ;
	unsigned iocount ;

	nrvars = NCnumrecvars(handle, rvp, (int*) NULL) ;
	if(nrvars == -1)
		return -1 ; /* TODO: what error message ?*/

	memset(coords, 0, sizeof(coords)) ;
	coords[0] = recnum ;
	for(ii = 0 ; ii < nrvars ; ii++)
      {
          if(datap[ii] == NULL)
              continue ;
          /* else */
          offset = NC_varoffset(handle, rvp[ii], coords) ;
          iocount = NCelemsPerRec(rvp[ii]) ;

#ifdef HDF
          switch(handle->file_type) 
            {
            case HDF_FILE:
                DFKsetNT(rvp[ii]->HDFtype);
                if(FAIL == hdf_xdr_NCvdata(handle, rvp[ii],
                                           offset, rvp[ii]->type, 
                                           (uint32)iocount, datap[ii]))
                    return(-1) ;
                break;
            case CDF_FILE:
                DFKsetNT(rvp[ii]->HDFtype);
                if(!nssdc_xdr_NCvdata(handle, rvp[ii],
                                      offset, rvp[ii]->type, 
                                      (uint32)iocount, datap[ii]))
                    return(-1) ;
                break;
            case netCDF_FILE:
                if(!xdr_NCvdata(handle->xdrs,
                                offset, rvp[ii]->type, 
                                iocount, datap[ii]))
                    return(-1) ;
                break;
            }
#else /* !HDF */
          if(!xdr_NCvdata(handle->xdrs,
                          offset, rvp[ii]->type, 
                          iocount, datap[ii]))
              return(-1) ;
#endif /* !HDF */

      }
	return 0 ;
}


/*
 * Write one record's worth of data, except don't write to variables for which
 * the address of the data to be written is null.  Return -1 on error.
 */
int
ncrecput(cdfid, recnum, datap)
int cdfid ;
long recnum ;
ncvoid * *datap ;
{
	NC *handle ;
	long unfilled ;

	cdf_routine_name = "ncrecput" ;

	handle = NC_check_id(cdfid) ;
	if(handle == NULL)
		return(-1) ;
	if(handle->flags & NC_INDEF )
		return(-1) ;

	if((unfilled = recnum - handle->numrecs) >= 0)
	{
		handle->flags |= NC_NDIRTY ;
		if( handle->flags & NC_NOFILL )
		{
			/* Go directly to jail, do not pass go */
			handle->numrecs = recnum + 1 ;
		}
		else
		{
			/* fill out new records */
			if( !xdr_NCsetpos(handle->xdrs,
					handle->begin_rec + handle->recsize*handle->numrecs) )
			{
				nc_serror("seek, rec %ld", handle->numrecs) ;
				return(FALSE) ;
			}
			for( ; unfilled >= 0 ; unfilled--, handle->numrecs++)
			{
				if( !NCfillrecord(handle->xdrs,
						(NC_var **)handle->vars->values, handle->vars->count) )
				{
					nc_serror("NCfillrec, rec %ld", handle->numrecs) ;
					return(FALSE) ;
				}
			}
		}
		if(handle->flags & NC_NSYNC) /* write out header->numrecs NOW */
		{
			if(!xdr_numrecs(handle->xdrs, handle) )
				return(FALSE) ;
			handle->flags &= ~NC_NDIRTY ;
		}
	}

	handle->xdrs->x_op = XDR_ENCODE ;

	return( NCrecio(handle, recnum, (Void **)datap) ) ;
}


/*
 * Read one record's worth of data, except don't read from variables for which
 * the address of the data to be read is null.  Return -1 on error;
 */
int
ncrecget(cdfid, recnum, datap)
int cdfid ;
long recnum ;
ncvoid **datap ;
{
	NC *handle ;

	cdf_routine_name = "ncrecget" ;

	handle = NC_check_id(cdfid) ;
	if(handle == NULL)
		return(-1) ;
	if(handle->flags & NC_INDEF )
		return(-1) ;

	handle->xdrs->x_op = XDR_DECODE ;

	return( NCrecio(handle, recnum, (Void **)datap) ) ;
}
