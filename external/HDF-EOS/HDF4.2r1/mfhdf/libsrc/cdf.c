/*
 *	Copyright 1993, University Corporation for Atmospheric Research
 *      See netcdf/COPYRIGHT file for copying and redistribution conditions.
 */
/*	$Id: cdf.c,v 1.79 2003/12/10 21:15:10 epourmal Exp $ */

#include	"local_nc.h"
#include	"alloc.h"

#ifdef HDF
#include    "hfile.h"
extern intn  hdf_xdr_cdf
   PROTO((XDR *xdrs, NC**handlep));

/* A couple of local prototypes to HDF section*/
intn hdf_cdf_clobber(NC *handle);

intn hdf_close(NC *handle);

static intn
hdf_num_attrs(NC *handle,/* IN: handle to SDS */
              int32 vg   /* IN: ref of top Vgroup */);

#endif /* HDF */

static bool_t NC_xdr_cdf(XDR *xdrs, NC **handlep);

static int NC_free_xcdf(NC *);

/* hmm we write the NDG out always for now */
#define WRITE_NDG 1

/* Debugging: define for each function you want debugging printfs */
/* #define HDF_READ_VARS
#define HDF_READ_ATTRS
#define HDF_NUM_ATTRS 
#define HDF_XDR_CDF 
#define HDF_VG_CLOBBER 
#define HDF_CDF_CLOBBER 
#define HDF_CLOSE */

/*
 * free the stuff that xdr_cdf allocates
 *
 * NOTE: Modified to return SUCCEED / FAIL and to catch errors
 *       -GV 9/16/97
 */
static int
NC_free_xcdf(handle)
NC *handle ;
{
    int ret_value = SUCCEED;

    if(handle != NULL)
      {
          if (NC_free_array(handle->dims) == FAIL)
            {
                ret_value = FAIL;
                goto done;
            }
          if (NC_free_array(handle->attrs) == FAIL)
            {
                ret_value = FAIL;
                goto done;
            }
          if (NC_free_array(handle->vars) ==FAIL)
            {
                ret_value = FAIL;
                goto done;
            }
      }

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */

    return ret_value;
}

/*
 * NOTE: Modified to return SUCCEED / FAIL and to catch errors
 *       -GV 9/16/97
 */
int
NC_free_cdf(handle)
NC *handle ;
{
    int ret_value = SUCCEED;

      if(handle != NULL)
        {
            if (NC_free_xcdf(handle) == FAIL) 
              {
                  ret_value = FAIL;
                  goto done;
              }
            
            /* destroy xdr struct */
            xdr_destroy(handle->xdrs);
            Free(handle->xdrs);

#ifdef HDF
            if(handle->file_type == HDF_FILE) 
              {
                if (Vend(handle->hdf_file) == FAIL)
                  {
                      ret_value = FAIL;
                      goto done;
                  }

                if (Hclose(handle->hdf_file) == FAIL)
                  {
                      ret_value = FAIL;
                      goto done;
                  }

              }
#endif /* HDF */

            Free(handle);
        }

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
     /* Normal cleanup */

    return ret_value;
}


#ifdef HDF 
/******************************************************************************/

/*
  From NASA CDF Source
*/
#define V2_MAGIC_NUMBER  0x0000FFFF   /* Written twice at the beginning of file */
#define V2_MAGIC_OFFSET  0

/* -------------------------------- Hiscdf -------------------------------- */
/*
  Return TRUE/FALSE depending on if the given file is a NASA CDF file
*/
PRIVATE intn
Hiscdf(filename)
const char * filename;
{
    
    static const char *FUNC = "Hiscdf";
    hdf_file_t fp;
    uint8      b[4];
    uint8    * bb = NULL;
    int32      magic_num;
    intn       ret_value = TRUE;
  
    fp = (hdf_file_t)HI_OPEN(filename, DFACC_READ);
    if (OPENERR(fp)) 
      {
          ret_value = FALSE;
          goto done;
      } 
    else 
      {
        if(HI_SEEK(fp, V2_MAGIC_OFFSET) == FAIL) 
          {
            HERROR(DFE_SEEKERROR);
            ret_value = FALSE;
            goto done;
          }
        
        if(HI_READ(fp, b, 4) == FAIL) 
          {
            HERROR(DFE_READERROR);
            ret_value = FALSE;
            goto done;
          }

        bb = &b[0];

        INT32DECODE(bb, magic_num); 

        if(magic_num == V2_MAGIC_NUMBER) 
            ret_value = TRUE;
        else 
            ret_value = FALSE;

        HI_CLOSE(fp);
      }

done:
    if (ret_value == FALSE)
      { /* FALSE cleanup ?*/

      }
     /* Normal cleanup */

    return ret_value;
}


/*
  Model after Hiscdf
*/
/* -------------------------------- Hisnetcdf -------------------------------- */
/*
  Return TRUE if the given file is a netCDF file, FALSE otherwise.
*/
PRIVATE intn
Hisnetcdf(filename)
const char * filename;
{
    
    static const char *FUNC = "Hisnetcdf";
    hdf_file_t fp;
    uint8      b[4];
    uint8    * bb = NULL;
    int32      magic_num;
    intn       ret_value = TRUE;
  
    fp = (hdf_file_t)HI_OPEN(filename, DFACC_READ);
    if (OPENERR(fp)) 
      {
        ret_value = FALSE;
        goto done;
      } 
    else 
      {
        if(HI_READ(fp, b, 4) == FAIL) 
          {
              HERROR(DFE_READERROR);
              HI_CLOSE(fp);
              ret_value = FALSE;
              goto done;
          }

        bb = &b[0];

        INT32DECODE(bb, magic_num); 

        if(magic_num == NCMAGIC) 
            ret_value = TRUE;
        else 
            ret_value = FALSE;

        HI_CLOSE(fp);
    }

done:
    if (ret_value == FALSE)
      { /* FALSE cleanup? */

      }
     /* Normal cleanup */

    return ret_value;
}
/******************************************************************************/
#endif /* HDF */


/*
 * NOTE: Cleaned up to catch errors - GV 9/19/97
 */
NC *
NC_new_cdf(name, mode)
const char *name ;
int mode ;
{
#ifdef HDF
    int32 hdf_mode =  DFACC_RDWR; /* default */
#endif
	NC   *cdf = NULL;
    static const char *FUNC = "NC_new_cdf";
    NC   *ret_value = NULL;

    /* allocate an NC struct */
	cdf = (NC *)HDcalloc(1,sizeof(NC)) ;
	if( cdf == NULL )
      {
          nc_serror("NC_new_cdf") ;
          ret_value = NULL;
          goto done;
      }

	cdf->flags = mode ;

	cdf->xdrs = (XDR *)HDmalloc(sizeof(XDR)) ;
	if( cdf->xdrs == NULL)
      {
          nc_serror("NC_new_cdf: xdrs") ;
          ret_value = NULL;
          goto done;
      } /* else */
	
#ifdef HDF
    /*
     * See what type of file we are looking at.
     * If we are creating a new file it will be an HDF file
     */
    if(mode & NC_CREAT) 
      {
          cdf->file_type = HDF_FILE;
      } 
    else 
      {
          if(Hishdf(name))
              cdf->file_type = HDF_FILE;
          else if(Hiscdf(name))
              cdf->file_type = CDF_FILE;
          else if(Hisnetcdf(name))
              cdf->file_type = netCDF_FILE;
          else
            {
                ret_value = NULL;
                goto done;
            }
            
#ifdef DEBUG
          if(cdf->file_type == CDF_FILE)
              printf("Yow!  found a CDF file\n");
#endif
      }

    /*
     * Set up the XDR functions that some of the netCDF old code uses
     */
    switch(cdf->file_type) 
      {
      case HDF_FILE:
          hdf_xdrfile_create(cdf->xdrs, mode); /* return type is 'void' */
          break;
      case netCDF_FILE:
          if( NCxdrfile_create( cdf->xdrs, name, mode ) < 0) 
            {
                ret_value = NULL;
                goto done;
            } 
          break;
      case CDF_FILE:
          /* CDF_xdrfile_create(); */
          /* try this, I bet it will be sufficient */
          hdf_xdrfile_create(cdf->xdrs, mode);
          break;
      }
        
#else /* !HDF */
    if( NCxdrfile_create( cdf->xdrs, name, mode ) < 0) 
      {
          ret_value = NULL;
          goto done;
      } 
#endif /* !HDF */

	cdf->dims = NULL ;
	cdf->attrs = NULL ;
	cdf->vars = NULL ;
	cdf->begin_rec = 0 ;
	cdf->recsize = 0 ;
	cdf->numrecs = 0 ;
	cdf->redefid = -1 ;

#ifdef HDF 
    /* 
     * determine the HDF access mode 
     */
    switch(mode) 
      {
      case NC_CLOBBER   :
          hdf_mode = DFACC_CLOBBER; break;
      case NC_NOCLOBBER :
          /* will handle below */
          break;
      case NC_WRITE     :
          hdf_mode = DFACC_RDWR;    break;
      case NC_NOWRITE   :
          hdf_mode = DFACC_RDONLY;  break;
      default:
          hdf_mode = DFACC_RDWR;
      }
        
        
    /*
     * Do file type specific setup
     */
    switch(cdf->file_type) 
      {
      case HDF_FILE:  /* HDF stuff */

          /* see if the file exists */
          if(mode == NC_NOCLOBBER) 
            {
                if((int) Hishdf(name))
                  { /* Need to free allocated structures.
                       This will happen on failure cleanup. */
                      xdr_destroy(cdf->xdrs) ; /* destroy xdr struct first */
                      ret_value = NULL;
                      goto done;
                  }
                hdf_mode = DFACC_RDWR;
            }

          /* open the file */
          cdf->hdf_file = (int32) Hopen(name, hdf_mode, 200);
          if(cdf->hdf_file == FAIL) 
            {
                ret_value = NULL;
                goto done;
            }

          /* start Vxx access */
          if (Vstart(cdf->hdf_file) == FAIL)
            {
                ret_value = NULL;
                goto done;
            }

          cdf->hdf_mode = hdf_mode;
          cdf->vgid = 0; /* invalid ref */

          HDstrncpy(cdf->path, name, FILENAME_MAX);
#if 0            
          HDmemcpy(cdf->path, name, FILENAME_MAX);
#endif
            
#ifdef DEBUG
          printf("value returned from Hopen() : %d\n", cdf->hdf_file);
#endif
          break;
      case netCDF_FILE:
          /* Nothing */
          break;
      case CDF_FILE:
#ifdef DEBUG
          fprintf(stderr, "About to do CDF file set up\n");
#endif
          cdf->cdf_fp = (hdf_file_t) HI_OPEN(name, hdf_mode);
          if (OPENERR(cdf->cdf_fp)) 
              HRETURN_ERROR(DFE_DENIED,NULL);
          break;
      }
#endif /* HDF */

    /*
     * Read in the contents
     */
    if(cdf->xdrs->x_op == XDR_DECODE) /* Not NC_CREAT */
      {
          if(!xdr_cdf(cdf->xdrs, &cdf) )
            {   /* free cdf struct. This cleanup is different than
                   NC_free_xcdf(). */
                NC_free_cdf(cdf); /* free memory, close structues,files etc*/
                cdf = NULL;
                ret_value = NULL ;
                goto done;
            }

          if(NC_computeshapes(cdf) == -1)
            {
                ret_value = NULL;
                goto done;
            }
      }

    ret_value = cdf ;

done:
    if (ret_value == NULL)
      { /* Failure cleanup */
          if (cdf != NULL)
            { /* handles case other than one for NC_free_cdf().
                 These routines only free up allocted memory. */
                NC_free_xcdf(cdf); /* no point in catching error here */
                if (cdf->xdrs != NULL)
                    Free(cdf->xdrs);
                Free(cdf) ;
            }
      }
     /* Normal cleanup */

    return ret_value;
}


/* 
 * Duplicate a description structure.
 * Can only be called for 'old' extant on disk, eg, old in DATA mode.
 *
 * NOTE:  Cleaned up to catch errors - GV 9/19/97
 */
NC *
NC_dup_cdf(name, mode, old)
const char *name ;
int mode ;
NC *old ;
{
	NC *cdf = NULL;
    NC *ret_value = NULL;

    cdf = (NC *)HDmalloc(sizeof(NC)) ;
	if( cdf == NULL )
      {
          nc_serror("NC_dup_cdf") ;
          ret_value = NULL;
          goto done;
      }

	cdf->flags = old->flags | NC_INDEF ;

	cdf->xdrs = (XDR *)HDmalloc(sizeof(XDR)) ;
	if( cdf->xdrs == NULL)
      {
          nc_serror("NC_dup_cdf: xdrs") ;
          ret_value = NULL;
          goto done;
      } 

	cdf->dims = NULL ;
	cdf->attrs = NULL ;
	cdf->vars = NULL ;
	cdf->begin_rec = 0 ;
	cdf->recsize = 0 ;
	cdf->numrecs = 0 ;

#ifdef HDF
    cdf->file_type = old->file_type;
#endif

	if(NCxdrfile_create( cdf->xdrs, name, mode) < 0)
      {
          ret_value = NULL ;
          goto done;
      } 

	old->xdrs->x_op = XDR_DECODE ;
	if(!xdr_cdf(old->xdrs, &cdf) )
      {
          ret_value = NULL;
          goto done;
      }
	if( NC_computeshapes(cdf) == -1)
      {
          ret_value = NULL;
          goto done;
      }

	ret_value = cdf;

done:
    if (ret_value == NULL)
      { /* Failure cleanup */
          if (cdf != NULL)
            { /* free up allocated structures */
                if (cdf->xdrs != NULL)
                    Free(cdf->xdrs);

                NC_free_xcdf(cdf); /* don't catch error here */
                Free(cdf);
            }

      }
     /* Normal cleanup */

    return ret_value;
}


int ncinquire(cdfid, ndimsp, nvarsp, nattrsp, xtendimp)
int cdfid ;
int *ndimsp ;
int *nvarsp ;
int *nattrsp ;
int *xtendimp ;
{
	NC *handle ;

	cdf_routine_name = "ncinquire" ;

	handle = NC_check_id(cdfid) ;
	if(handle == NULL)
		return(-1) ;
	
	if(nvarsp != NULL)
		*nvarsp = (handle->vars != NULL) ? handle->vars->count : 0 ;
	if(nattrsp != NULL)
		*nattrsp = (handle->attrs != NULL) ? handle->attrs->count : 0 ;
	if(handle->dims != NULL)
      {
          NC_dim **dp ;
          int ii ;

          if(ndimsp != NULL)
              *ndimsp = handle->dims->count ;
          if(xtendimp != NULL) {
              *xtendimp = -1 ;

              dp = (NC_dim**)handle->dims->values ;
              for(ii = 0 ; ii < handle->dims->count ; ii++, dp++)
                {
                    if((*dp)->size == NC_UNLIMITED)
                      {
                          *xtendimp = ii ;
                      }
                }
          }
      } else {
          if(ndimsp != NULL)
              *ndimsp = 0 ;
          if(xtendimp != NULL)
              *xtendimp = -1 ;
      }

	return(cdfid) ;
}

/*
 *  NOTE: modfied how errors were caught and reported - GV 9/19/97
 */
bool_t
xdr_cdf(xdrs, handlep)
	XDR *xdrs;
	NC **handlep;
{
    bool_t ret_value = TRUE;

#ifdef HDF    
    switch((*handlep)->file_type) 
      {
      case HDF_FILE:
          if (hdf_xdr_cdf(xdrs, handlep) == FAIL)
              ret_value = FALSE;
          break;
      case netCDF_FILE:
          ret_value = NC_xdr_cdf(xdrs, handlep);
          break;
      case CDF_FILE:
          ret_value = nssdc_xdr_cdf(xdrs, handlep);
          break;
      default:
          ret_value = FALSE;
          break;
      }
#else /* !HDF */
    ret_value = NC_xdr_cdf(xdrs, handlep);
#endif /* !HDF */

    return ret_value;
}


static bool_t
NC_xdr_cdf(xdrs, handlep)
	XDR *xdrs;
	NC **handlep;
{

	u_long	magic;

	if( xdrs->x_op == XDR_FREE)
      {
          NC_free_xcdf(*handlep) ;
          return(TRUE) ;
      }
	
	if( xdr_getpos(xdrs) != 0)
      {
          if( !xdr_setpos(xdrs, 0) )
            {
                nc_serror("Can't set position to begin") ;
                return(FALSE) ;
            }
      }

	/* magic number */
	if( !xdr_u_long(xdrs, &magic) )
      {
          if( xdrs->x_op == XDR_DECODE)
            {
                NCadvise(NC_ENOTNC,
                         "Not a netcdf file (Can't read magic number)") ;
            }
          else
            {
                /* write error */
                nc_serror("xdr_cdf: xdr_u_long") ;
            }
          return(FALSE) ;
      }

	if( xdrs->x_op == XDR_DECODE && magic != NCMAGIC )
      {
          if(magic == NCLINKMAGIC)
            {
                NCadvise(NC_NOERR, "link file not handled yet") ;
                return(FALSE) ;
            } /* else */
          NCadvise(NC_ENOTNC, "Not a netcdf file") ;
          return(FALSE) ;
      }

	if( !xdr_numrecs(xdrs, *handlep))
      {
          NCadvise(NC_EXDR, "xdr_numrecs") ;
          return(FALSE) ;
      }
	if( !xdr_NC_array(xdrs, &((*handlep)->dims)))
      {
          NCadvise(NC_EXDR, "xdr_cdf:dims") ;
          return(FALSE) ;
      }
	if( !xdr_NC_array(xdrs, &((*handlep)->attrs)))
      {
          NCadvise(NC_EXDR, "xdr_cdf:attrs") ;
          return(FALSE) ;
      }
	if( !xdr_NC_array(xdrs, &((*handlep)->vars)))
      {
          NCadvise(NC_EXDR, "xdr_cdf:vars") ;
          return(FALSE) ;
      }	
	return(TRUE) ;
}

#ifdef HDF
/*****************************************************************************
* 
*			NCSA HDF / netCDF Project
*			        May, 1993
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


/* ----------------------------------------------------------------
** Map an NC_<type> to an HDF type
*/
int
hdf_map_type(type)
nc_type type;
{

    switch(type) 
      {
      case NC_UNSPECIFIED : /* be like netCDF */
      case NC_CHAR   :
          return DFNT_CHAR;
      case NC_BYTE   :
          return DFNT_INT8;
      case NC_SHORT   :
          return DFNT_INT16;
      case NC_LONG   :
          return DFNT_INT32;
      case NC_FLOAT   :
          return DFNT_FLOAT32;
      case NC_DOUBLE   :
          return DFNT_FLOAT64;
      default:
          return DFNT_NONE;
      }

} /* hdf_map_type */


/* ----------------------------------------------------------------
**  UnMap a data type.  I.e. go from an HDF type to an NC_<type>
**  The HDF type may be in DFNT_NATIVE mode, so only look at the 
**    bottom bits
*/
nc_type
hdf_unmap_type(type)
int type;
{
    switch(type & 0xff) 
      {
      case DFNT_CHAR        :
      case DFNT_UCHAR       :
          return NC_CHAR;
      case DFNT_INT8        :
      case DFNT_UINT8       :
          return NC_BYTE;
      case DFNT_INT16       :
      case DFNT_UINT16      :
          return NC_SHORT;
      case DFNT_INT32       :
      case DFNT_UINT32      :
          return NC_LONG;
      case DFNT_FLOAT32     :
          return NC_FLOAT; 
      case DFNT_FLOAT64     :
          return NC_DOUBLE;
      default:
          return (nc_type)FAIL;	/* need a better legal nc_type value */
      }

} /* hdf_unmap_type */

/* -----------------------------------------------------------------
** Given a dimension id number return its hdf_ref number (Vgroup id)
*/
int
hdf_get_ref(handle, i)
NC *handle;
int i;
{
  NC_array  *tmp = NULL;
  NC_dim   **d = NULL;
  Void      *dims = NULL;
  
  tmp  = handle->dims;
  dims = handle->dims->values;
  dims += i * tmp->szof;
  
  d = (NC_dim **) dims;
  return (*d)->vgid; /* return ref of vgroup */
} /* get_hdf_ref */


/* ----------------------------------------------------------------
** Given a dimension pointer return the ref of a Vdata which was
**   newly created to represent the values the dimension takes on
** If there is a variable with the same name as our dimension then
**   the values the variable takes on are the values for the
**   'steps' in the dimension.
** Otherwise, the dimension takes on the values 0..(size -1)
**
** NOTE:  This may cause conflicts cuz we may get called before
**   the variable's values are set???
**
** NOTE2: Someone should update the comments here. They no longer
**        seem valid -GV 9/19/97
*/
int 
hdf_create_dim_vdata(xdrs, handle, dim)
XDR *xdrs;
NC *handle;
NC_dim *dim;
{
#ifdef LATER
    CONSTR(FUNC,"hdf_create_dim_vdata"); 
#endif /* LATER */
  int   found = FALSE;
  int   ref;
  int32 val;
  long  dsize;
  int   ret_value = FAIL;

#if DEBUG
  fprintf(stderr, "hdf_create_dim_vdata I've been called\n");
  fprintf(stderr, "handle->hdf_file = %d\n", handle->hdf_file);
#endif

#if 0
  /* look for variable with the given name */
  if(handle->vars) 
    {
        dp = (NC_var **) handle->vars->values;
        for(ii = 0; ii < handle->vars->count; ii++, dp++)
            if(HDstrcmp(dim->name->values, (*dp)->name->values) == 0) 
              {
                  /* found it */
                  found = TRUE;
                  break;
              }
    }
#endif

  if(found) 
    {
        /* load in the variable's values */
#if DEBUG
        fprintf(stderr, "Found real values for dimension %s\n", dim->name->values);
#endif
    } 
  else 
    {
        dsize = 1;
        val = (dim->size != NC_UNLIMITED) ? dim->size : (int32)handle->numrecs;
        ref = VHstoredata(handle->hdf_file, "Values", (const uint8 *)&val, dsize,
                          DFNT_INT32, dim->name->values, DIM_VALS01);
      
        if(ref == FAIL) 
          {
#ifdef DEBUG
              fprintf(stderr, "FAILed creating Vdata %s\n", dim->name->values);
#endif
              ret_value = FAIL;
              goto done;
          }
    }
  
#if DEBUG
  fprintf(stderr, "Returning vdata pointer %d\n", ref);
#endif
  
  ret_value = ref;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
     /* Normal cleanup */

    return ret_value;
} /* hdf_create_dim_vdata */

/* ----------------------------------------------------------------
** Given a dimension pointer return the ref of an older version
**   dim Vdata which was newly created to represent the values
**   the dimension takes on, for backward compatibility.
**
**   For DIM_VALS, the values the variable takes on are the values
**     for the 'steps' in the dimension.
**
*/
int
hdf_create_compat_dim_vdata(XDR *xdrs, 
                            NC *handle, 
                            NC_dim *dim, 
                            int32 dimval_ver)
{
    static const char  *FUNC = "hdf_create_compat_dim_vdata";
    int    i;
    int    ref;
    long   dsize;
    int32 *val = NULL;
    int    ret_value = FAIL;

#ifdef DEBUG
    fprintf(stderr, "hdf_create_compat_dim_vdata I've been called\n");
    fprintf(stderr, "handle->hdf_file = %d\n", handle->hdf_file);
    fprintf(stderr, "dim_ver = %d\n", dim_ver);
#endif

    if (dimval_ver != DIMVAL_VERSION00)
      {
          ret_value = FAIL;
          goto done;
      }

    dsize = (dim->size == NC_UNLIMITED)? 1 : dim->size;
    if (dsize < 0)
      {
          ret_value = FAIL;
          goto done;
      }
    /* create a fake one */
#ifdef DEBUG
    fprintf(stderr, "Creating fake dim  ::::%s::: (%d)\n",
            dim->name->values, dsize);
#endif
    /* allocate space */
    val = (int32 *) HDmalloc(dsize * sizeof(int32));
    if(!val) 
      {
          HERROR(DFE_NOSPACE);
          ret_value = FAIL;
          goto done;
      }

    if (dim->size == NC_UNLIMITED)  
      {
          *val = handle->numrecs;
      }
    else
      {
          for(i = 0; i < dsize; i++) val[i] = i;
      }

    ref = VHstoredata(handle->hdf_file, "Values", (const uint8 *)val,
                      dsize, DFNT_INT32, dim->name->values, DIM_VALS);
    if(ref == FAIL) 
      {
#ifdef DEBUG
          fprintf(stderr, "FAILed creating Vdata %s\n", dim->name->values);
#endif
          ret_value =  FAIL;
          goto done;
      }


#ifdef DEBUG
    fprintf(stderr, "Returning vdata pointer %d\n", ref);
#endif

    ret_value = ref;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
     /* Normal cleanup */
    if (val != NULL)
        HDfree(val);

    return ret_value;
} /* hdf_create_compat_dim_vdata */

/* ----------------------------------------------------------------
** Write out a vdata representing an attribute
*/
int
hdf_write_attr(xdrs, handle, attr)
XDR *xdrs;
NC *handle;
NC_attr **attr;
{
    char *name = NULL;
    Void *values = NULL;
    int size;
    int type;
    int order;
    int ret_value = SUCCEED;

    name = (*attr)->name->values;
    values = (*attr)->data->values;
    size = (*attr)->data->count;
    type = (*attr)->HDFtype;

#if DEBUG
    fprintf(stderr, "hdf_write_attr I've been called\n");
    fprintf(stderr, "The attribute is called %s\n", name);
    fprintf(stderr, "Type = %d (%d)  size  %d\n", type, (*attr)->HDFtype, size);
    fprintf(stderr, "Value: ");
    switch(type) 
      {
      case DFNT_CHAR :fprintf(stderr, " (char) %s\n", (char *) values); break;
      case DFNT_UINT8 :fprintf(stderr, " (uint8) %d\n", (char *) values); break;
      case DFNT_INT8 :fprintf(stderr, " (int8) %d\n", (char *) values); break;
      case DFNT_UINT16 :fprintf(stderr, " (uint16) %d\n", (int *) values); break;
      case DFNT_INT16 :fprintf(stderr, " (int16) %d\n", (int *) values); break;
      case DFNT_UINT32 :fprintf(stderr, " (uint32) %d\n", (int *) values); break;
      case DFNT_INT32 :fprintf(stderr, " (int32) %d\n", (int *) values); break;
      case DFNT_FLOAT32 :fprintf(stderr, " (float32) %f\n", (float *) values); break;
      case DFNT_FLOAT64 :fprintf(stderr, " (float64) %f\n", (double *) values); break;
      default:fprintf(stderr, "???\n");
      }
#endif

    if(type == DFNT_CHAR) 
      {
          order = size;
          size = 1;
      } 
    else 
      { 
          order = 1;
      }
      
    ret_value = VHstoredatam(handle->hdf_file, ATTR_FIELD_NAME, 
                          (unsigned char *) values, size, 
                          type, name, _HDF_ATTRIBUTE, order);

#if DEBUG
    fprintf(stderr, "hdf_write_attr returning %d\n", ret_value);
#endif

#ifdef LATER
done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
     /* Normal cleanup */
#endif /* LATER */

    return ret_value;
} /* hdf_write_attr */

/* ----------------------------------------------------------------
** Write out a group representing a dimension
*/
int32
hdf_write_dim(XDR *xdrs, NC *handle, NC_dim **dim, int32 cnt)
{
    int32 tags[100];
    int32 refs[100];
    int32 count;
    const char  *class = NULL;
    char  name[MAX_NC_NAME] = "";
    int32 ret_value = SUCCEED;

#if DEBUG
    fprintf(stderr, "hdf_write_dim I've been called\n");
    fprintf(stderr, "The name is -- %s -- \n", (*dim)->name->values);
#endif

    /*
     * Look up to see if there is a variable of the same name
     *  giving values
     */
    count = 0;
    tags[count] = DFTAG_VH;
    refs[count] = hdf_create_dim_vdata(xdrs, handle, (*dim));
    if(refs[count] == FAIL) 
      {
          ret_value = FAIL;
          goto done;
      }
    count++;

    /* do we need to create compatible dimension? */
    if ((*dim)->dim00_compat) 
      {
          tags[count] = DFTAG_VH;
          refs[count] = hdf_create_compat_dim_vdata(xdrs, handle, (*dim), DIMVAL_VERSION00);
          if(refs[count] == FAIL) 
            {
                ret_value = FAIL;
                goto done;
            }
          count++;
      } 

    /* check if UNLIMITED dimension */
    if((*dim)->size == NC_UNLIMITED) 
        class = _HDF_UDIMENSION;
    else
        class = _HDF_DIMENSION;
  
    if(HDstrncmp((*dim)->name->values, "fakeDim", 7) == 0)
        sprintf(name, "fakeDim%d", (int)cnt);
    else
        HDstrcpy(name, (*dim)->name->values);

    /* write out the dimension group? */
    (*dim)->vgid = VHmakegroup(handle->hdf_file, tags, refs, count, 
                               name, class);

    ret_value = (*dim)->vgid; /* ref of vgroup of dimension */

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
     /* Normal cleanup */

    return ret_value;
} /* hdf_write_dim */


/* ----------------------------------------------------------------
** Write out a group representing a variable
** If successfull, return the id of the created Vgroup else
**  return NULL
*/
int32
hdf_write_var(xdrs, handle, var)
XDR *xdrs;
NC *handle;
NC_var **var;
{
    NC_array  *  attrs = NULL;
    NC_iarray *  assoc = NULL;
    uint8        ntstring[4];
    uint16       ref;
    int8         outNT;
    uint8 tbuf[2+((MAX_VAR_DIMS+1)*8)];   /* temporary buffer */
    int32 tags[MAX_NC_ATTRS + MAX_VAR_DIMS + 2];
    int32 refs[MAX_NC_ATTRS + MAX_VAR_DIMS + 10];
    uint16       nt_ref, rank;
    int32     GroupID, val;
    uint8     *bufp = NULL;
#ifdef LATER
    CONSTR(FUNC,"hdf_write_var"); 
#endif /* LATER */
    int32      ret_value = SUCCEED;
    register int  i, count;
    register Void *attribute = NULL;

    count = 0;
    assoc = (*var)-> assoc;
    attrs = (*var)-> attrs;

#if DEBUG
    fprintf(stderr, "hdf_write_var I've been called\n");
    fprintf(stderr, "handle->hdf_file = %d\n", handle->hdf_file);
    fprintf(stderr, "The name is -- %s -- \n", (*var)->name->values);

    if(assoc && assoc->count) 
      {
          fprintf(stderr, "value of assoc %d\n", assoc);
          fprintf(stderr, " assoc->count %d\n", assoc->count);
          fprintf(stderr, " asc[0] %d asc[1] %d\n", assoc->values[0], assoc->values[1]);
      }
#endif

    /*
     *  Get the dimension information
     */
    for(i = 0; i < assoc->count; i++) 
      {
          tags[count] = DIM_TAG;
          refs[count] = hdf_get_ref(handle, assoc->values[i]);
          count++;
      }

    /* 
     * Add info for the attributes
     */
    if(attrs) 
      {
          attribute = attrs->values;
          for(i = 0; i < attrs->count; i++) 
            {
                tags[count] = ATTR_TAG;
                refs[count] = hdf_write_attr(xdrs, handle, (NC_attr **)attribute);
                if (refs[count] == FAIL)
                  {
                      ret_value = FAIL;
                      goto done;
                  }
                attribute += attrs->szof;
                count++;
            }
      }
  
    /*
     * If we already have data written out include that too
     *   (this might happen after a redef() cuz we will leave
     *   the data sitting on the disk but clear out all the 
     *   meta-data)
     */
    if((*var)->data_ref) 
      {
          tags[count] = (int32) DFTAG_SD;
          refs[count] = (*var)->data_ref;
#if DEBUG
          fprintf(stderr, " ---- Carrying forward data with ref %d ---- \n", (*var)->data_ref);
#endif 
          count++;
      } 
  
  
    /*
     * Write out a number type tag so that we can recover this 
     *   variable's type later on
     *
     * by default numbers are converted to IEEE otherwise we need to save the 
     *   machine type in the NT object
     */

    /* somone unwrap this statement....*/
    outNT = ((*var)->HDFtype & DFNT_NATIVE)?  
        DFKgetPNSC((*var)->HDFtype, DF_MT) : ((*var)->HDFtype & DFNT_LITEND)? 
        DFNTF_PC :  DFNTF_IEEE;

#ifdef NOT_YET
    ref = Htagnewref(handle->hdf_file,DFTAG_NT);
#else /* NOT_YET */
    ref = Hnewref(handle->hdf_file);
#endif /* NOT_YET */
    ntstring[0] = DFNT_VERSION;                    /* version */
    ntstring[1] = (uint8)((*var)->HDFtype & 0xff); /* type */
    ntstring[2] = (uint8)((*var)->HDFsize * 8);    /* width (in bits) */
    ntstring[3] = outNT;                           /* class: IEEE or machine class */
    if(Hputelement(handle->hdf_file, DFTAG_NT, ref, ntstring, (int32) 4) == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }
    tags[count] = DFTAG_NT;
    refs[count] = ref;
    nt_ref = (uint16) ref;
    count++;
  
#ifdef WRITE_NDG
    /* prepare to start writing ndg   */
    if ((GroupID = DFdisetup(10)) < 0)
      {
          ret_value = FAIL;
          goto done;
      }
  
    /* write SD record */
    if((*var)->data_ref)
      {
          if (DFdiput(GroupID, DFTAG_SD, (uint16) (*var)->data_ref) == FAIL)
            {
                ret_value = FAIL;
                goto done;
            }
      }
  
    /* write NT tag/ref */
    if (DFdiput(GroupID, DFTAG_NT, (uint16) ref) == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* put rank & dimensions in buffer */
    bufp = tbuf;
    rank = assoc->count;
    UINT16ENCODE(bufp, rank);
    for(i = 0; i < (int)rank; i++) 
      {
          val = (int32) (*var)->shape[i];

          /* need to fake the size of the record dimension */
          if(val == NC_UNLIMITED) 
            {
                if(handle->file_type == HDF_FILE)
                    val = (*var)->numrecs;
                else
                    val = handle->numrecs;
            }

          INT32ENCODE(bufp, val);
      }
  
    /* "<=" used to put 1 data NT + rank scale NTs in buffer */
    for (i = 0; i <= (int)rank; i++) 
      {  /* scale NTs written even if no scale!*/
          UINT16ENCODE(bufp, DFTAG_NT);
          UINT16ENCODE(bufp, nt_ref);
      } 
  
    /* write out SDD record */
    if(Hputelement(handle->hdf_file, DFTAG_SDD, ref, tbuf, (int32) (bufp-tbuf)) == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }
  
    /* write dimension record tag/ref */
    if (DFdiput(GroupID, DFTAG_SDD,(uint16) ref) == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }

    tags[count] = DFTAG_SDD;
    refs[count] = ref;
    count++;

    /* Add a bogus tag so we know this NDG is really a variable */
    if (DFdiput(GroupID, BOGUS_TAG,(uint16) ref) == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* write out NDG */
    if (DFdiwrite(handle->hdf_file, GroupID, DFTAG_NDG, (*var)->ndg_ref) < 0) 
      {
          ret_value = FAIL;
          goto done;
      }
  
    tags[count] = DFTAG_NDG;
    refs[count] = (*var)->ndg_ref;
    count++;

#endif /* WRITE_NDG */

    /* write the vgroup for the variable ? */
    (*var)->vgid = VHmakegroup(handle->hdf_file, tags, refs, count, 
                               (*var)->name->values, _HDF_VARIABLE);

#ifdef DEBUG
    if((*var)->vgid == FAIL) 
      {
          fprintf(stderr, "Failed to write variable %s\n", (*var)->name->values);
          fprintf(stderr, "count = %d\n", count);
          for(i = 0; i < count; i++)
              fprintf(stderr, "i = %d   tag = %d ref = %d\n", i, tags[i], refs[i]);
          
          HEprint(stdout, 0);
      }  
#endif

    ret_value =  (*var)->vgid; /* ref of vgroup of variable */

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
     /* Normal cleanup */

    return ret_value;  
} /* hdf_write_var */


/* ----------------------------------------------------------------
** Write out a cdf structure
*/
intn
hdf_write_xdr_cdf(xdrs, handlep)
XDR *xdrs;
NC **handlep;
{
    int32 count;
    int sz, i, j, status, done;
    int32    *tags = NULL;
    int32    *refs = NULL;
    NC_dim  **dims = NULL;
    NC_dim  **dims1 = NULL;
    NC_array *tmp = NULL;
    long     *dim_size_array = NULL;
    long     *tsizeptr = NULL;
    long      tsize;
    uint32   *dim_hash_array = NULL;
    uint32   *thashptr = NULL;
    uint32   thash;
    Void     *vars = NULL;
    Void     *attrs = NULL;
    intn      ret_value = SUCCEED;

#if DEBUG
    fprintf(stderr, "hdf_write_xdr_cdf i've been called op = %d \n", xdrs->x_op);
#endif

   /* Convert old scales into coordinate var values before writing
      out any header info */
    status = hdf_conv_scales(handlep);
    if (status == FAIL) 
      {
          ret_value = FAIL;
          goto done;
      }

    /* count size of tag / ref arrays */
    sz = 0;
    if((*handlep)->dims)  
        sz += (*handlep)->dims->count;
    if((*handlep)->vars)  
        sz += (*handlep)->vars->count;
    if((*handlep)->attrs) 
        sz += (*handlep)->attrs->count;

#if DEBUG
    fprintf(stderr, "sz = %d\n", sz);
#endif

    /* allocate tag / ref arrays */
    tags = (int32 *) HDmalloc(sz * sizeof(int32) + 1);
    refs = (int32 *) HDmalloc(sz * sizeof(int32) + 1);
    if(NULL == tags || NULL == refs) 
      {
#ifdef DEBUG
          fprintf(stderr, "Out of memory line %d file %s\n", __LINE__, __FILE__);
#endif
          ret_value = FAIL;
          goto done;
      }

    /* 
    ** write out dimension arrays 
    */
    count = 0;
    if((*handlep)->dims) 
      {
          tmp = (*handlep)->dims; 
          dims = (NC_dim **) (*handlep)->dims->values;

          tsizeptr = dim_size_array =(long *)HDmalloc(sizeof(long)*(size_t)tmp->count);
          thashptr = dim_hash_array =(uint32 *)HDmalloc(sizeof(uint32)*(size_t)tmp->count);

          if(NULL == dim_size_array || NULL == dim_hash_array) 
            {
#ifdef DEBUG
                fprintf(stderr, "Out of memory line %d file %s\n", __LINE__, __FILE__);
#endif
                ret_value = FAIL;
                goto done;
            }

          for(i = 0; i < tmp->count; i++,dims++) 
            {
                *tsizeptr++=(*dims)->size;
                *thashptr++=(*dims)->name->hash;
            } /* end for */

          dims = (NC_dim **) (*handlep)->dims->values;
          for(i = 0; i < tmp->count; i++) 
            {

                /* this is really ugly and should be handled another way */
                /* make sure we don't duplicate dimensions */
                done = FALSE;
                dims1 = (NC_dim **) (*handlep)->dims->values;
                tsize = dim_size_array[i];
                thash = dim_hash_array[i];
                tsizeptr = dim_size_array;
                thashptr = dim_hash_array;

                for(j = 0; j < i; j++) 
                  {
                      /* this order on the test is faster -QAK */
                      if( thash == *thashptr 
                          && tsize == *tsizeptr 
                          && NC_compare_string((*dims)->name,(*dims1)->name) == 0 ) 
                        {
                            done = TRUE;
                            break;
                        }
                      tsizeptr++;
                      thashptr++;
                      dims1++;
                  }

                if(!done) 
                  {
                      tags[count] = (int32) DIM_TAG;
                      refs[count] = (int32) hdf_write_dim(xdrs, (*handlep), dims, count);
                      if(refs[count] == FAIL) 
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                      count++;
                  }
                dims++;
            }
      } /* end if handle->dims */
  
    /* 
    ** write out variable info 
    */
    if((*handlep)->vars) 
      {
          tmp = (*handlep)->vars; 
          vars = (*handlep)->vars->values;
          for(i = 0; i < tmp->count; i++) 
            {
                tags[count] = (int32) VAR_TAG;
                refs[count] = (int32) hdf_write_var(xdrs, (*handlep), (NC_var **)vars);
                if(refs[count] == FAIL) 
                  {
                      ret_value = FAIL;
                      goto done;
                  }
                vars += tmp->szof;
                count++;
            }
      }

    /*
     * write global attribute information
     */
    if((*handlep)->attrs) 
      {
          tmp = (*handlep)->attrs; 
          attrs = (*handlep)->attrs->values;
          for(i = 0; i < tmp->count; i++) 
            {
                tags[count] = (int32) ATTR_TAG;
                refs[count] = (int32) hdf_write_attr(xdrs, (*handlep), (NC_attr **)attrs);
                if(refs[count] == FAIL) 
                  {
                      ret_value = FAIL;
                      goto done;
                  }
                attrs += tmp->szof;
                count++;
            }
      }

#if DEBUG
    fprintf(stderr, "About to write top level VG with %d elements\n", count);
    {
        int i; 
        for(i = 0; i < count; i++) 
            fprintf(stderr, "%d :=> %d %d\n", i, tags[i], refs[i]);
    }
#endif

    /* write out final VGroup thang */
    /* set the top level CDF VGroup pointer */
    (*handlep)->vgid = VHmakegroup((*handlep)->hdf_file, tags, refs, count, 
                                   (*handlep)->path, _HDF_CDF);


    ret_value = (*handlep)->vgid; /* ref of final vgroup  */

#ifdef DEBUG
    fprintf(stderr, "======= Have finished writing top level VGroup #%d\n", ret_value);
#endif

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
     /* Normal cleanup */
    if (dim_size_array != NULL)
        HDfree(dim_size_array);
    if (dim_hash_array != NULL)
        HDfree(dim_hash_array);
    if (tags != NULL)
        HDfree(tags);
    if (refs != NULL)
        HDfree(refs);

    return ret_value;
} /* hdf_write_xdr_cdf */

/* --------------------------------------------------------------
** hdf_conv_scales converts old scale values into coord var values.
** Searchs through var list for DFTAG_SDS, reads in the scale data,
** change the ref to ndg_ref and the tag to DATA_TAG, writes the 
** data out. 
*/
intn 
hdf_conv_scales(handlep)
NC **handlep;
{
    int i, status, scaleref, scaletag, scalelen;
    NC_var   **vars = NULL;
    NC_array  *tmp = NULL;
    uint8     *scalebuf = NULL;
    uint8     *datap = NULL;
    intn    ret_value = SUCCEED;
#ifdef LATER
    CONSTR(FUNC, "hdf_conv_scales");
#endif /* LATER */

    if ((*handlep)->vars) 
      {
          tmp = (*handlep)->vars;
          vars = (NC_var **)tmp->values;
          for (i = 0; i < tmp->count; i++)   
            {
                if ( ((*vars)->data_tag == DFTAG_SDS) 
                     && ((*vars)->data_ref != (*vars)->ndg_ref)) 
                  {
                      /* read in scale values */
                      scaleref = (*vars)->data_ref;
                      scaletag = (*vars)->data_tag;
                      scalelen = Hlength((*handlep)->hdf_file, scaletag, scaleref);
                      if (scalelen == FAIL) 
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                      if ((*vars)->data_offset == -1)  
                        { /* this dim has no scale values */
                            (*vars)->data_ref = 0;
                            (*vars)->data_tag = DATA_TAG;
                        }
                      else 
                        {    /* has scale values */
                            scalebuf = (uint8 *)HDmalloc((uint32)scalelen);
                            if (scalebuf == NULL) 
                              {
                                  ret_value = FAIL;
                                  goto done;
                              }
                            status = Hgetelement((*handlep)->hdf_file, scaletag, 
                                                 scaleref, scalebuf); 
                            if (status == FAIL) 
                              {
                                  ret_value = FAIL;
                                  goto done;
                              }

                            (*vars)->data_tag = DATA_TAG;
                            (*vars)->data_ref = (*vars)->ndg_ref; /* Try to stick
                                                                     with the current way. If this ref conflicts with 
                                                                     existing SDS, call Hnewref to get a new one. 3/25/97 */
                            datap = scalebuf + (*vars)->data_offset;
                            status = Hputelement((*handlep)->hdf_file, DATA_TAG,
                                                 (*vars)->data_ref, datap, (*vars)->len);
                            if (status == FAIL) 
                              {
                                  (*vars)->data_tag = scaletag;
                                  (*vars)->data_ref = scaleref;
                                  ret_value = FAIL;
                                  goto done;
                              }
                        } /* has scale values */
                  }  /* DFTAG_SDS */
                vars ++;
            }
      }

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
     /* Normal cleanup */
    if (scalebuf != NULL)
        HDfree(scalebuf);

    return ret_value;
}
  

/* ----------------------------------------------------------------
** Read in the dimensions out of a cdf structure
** Return FAIL if something goes wrong
*/
intn 
hdf_read_dims(XDR *xdrs, NC *handle, int32 vg)
{
    char vgname[MAX_NC_NAME] = "";
    char vsclass[MAX_NC_CLASS] = "";
    char vgclass[MAX_NC_CLASS] = "";
    int      id, count, i, found;
    int      sub_id;
    int32    dim_size;
    NC_dim **dimension = NULL;
    int32    dim, entries;
    int32    vs;
    intn      ret_value = SUCCEED;

    found = FALSE;
    count = 0;
    id = -1;

#if DEBUG
    fprintf(stderr, "hdf_read_dims I've been called, handle->hdf_file = %d\n", handle->hdf_file);
#endif

    /*
     * Allocate enough space in case everything is a dimension
     */
    count = 0;
    dimension = (NC_dim **) HDmalloc(sizeof(NC_dim *) * Vntagrefs(vg) + 1);
    if(NULL == dimension) 
      {
          /* replace it with NCadvice or HERROR?? */
#ifdef DEBUG
          fprintf(stderr, "Out of memory line %d file %s\n", __LINE__, __FILE__);
#endif
          ret_value = FAIL;
          goto done;
      }

    /*
     * Look through for a Vgroup of class _HDF_DIMENSION
     */
    while((id = Vgetnext(vg, id)) != FAIL) 
      {
          if(Visvg(vg, id)) 
            {
                dim = Vattach(handle->hdf_file, id, "r");
                if(dim == FAIL) 
                    continue; /* why do we continue? does this failure here
                                 not matter? -GV */
                if (Vgetclass(dim, vgclass) == FAIL)
                  {
                      ret_value = FAIL;
                      goto done;
                  }

                if(!HDstrcmp(vgclass, _HDF_DIMENSION) 
                   || !HDstrcmp(vgclass, _HDF_UDIMENSION)) 
                  {
                      int is_dimval, is_dimval01;
                      /* init both flags to FALSE  */
                      is_dimval = FALSE;
                      is_dimval01 = FALSE;

                      if (Vinquire(dim, &entries, vgname) == FAIL)
                        {
                            ret_value = FAIL;
                            goto done;
                        }
      	
                      /* 
                       * look through for a Vdata of class DIM_VALS01 and/or DIM_VALS 
                       * to get size 
                       */
                      sub_id = -1;
                      while(((sub_id = Vgetnext(dim, sub_id)) != FAIL) ) 
                        {
                            if(Visvs(dim, sub_id)) 
                              {
                                  vs = VSattach(handle->hdf_file, sub_id, "r");
                                  if(vs == FAIL) 
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }
      
                                  if (VSgetclass(vs, vsclass) == FAIL)
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }
      
                                  if(!HDstrcmp(vsclass, DIM_VALS)) 
                                    {
                                        is_dimval = TRUE;
                                        if (HDstrcmp(vgclass, _HDF_UDIMENSION))  /* not unlimited di
                                                                                    m */
                                          {
                                              if (VSQuerycount(vs, &dim_size) == FAIL)
                                                {
                                                    ret_value = FAIL;
                                                    goto done;
                                                }
                                          }
                                    }
                                  if ((!HDstrcmp(vsclass, DIM_VALS01)) 
                                      || (!HDstrcmp(vgclass, _HDF_UDIMENSION))) 
                                    { /* DIM_VALS && _HDF_UDIMENSION */
                                        int32 val;	/* needs a temp var since handle->numrecs */
                                        /* may not be an int32 */
       /* 
          The call to VSsetfields fails for the files created with the library
          version 3.3r1. This call is not necessary since handle vs is
          obtained by specifying class name. 
          Elena Pourmal 2/17/99

                                        if (VSsetfields(vs, "Values") == FAIL)
                                          {
                                              ret_value = FAIL;
                                              goto done;
                                          }
        */

                                        if (VSseek(vs, 0) == FAIL)
                                          {
                                              ret_value = FAIL;
                                              goto done;
                                          }

                                        /*
                                         * This is highly dangerous since there might be multiple
                                         * unlimited dimensions
                                         */
                                        if(VSread(vs, (uint8 *) &val, 1, FULL_INTERLACE) != 1)
                                          {
                                              ret_value = FAIL;
                                              goto done;
                                          }
                                        if (!HDstrcmp(vgclass, _HDF_UDIMENSION))   
                                          {
                                              dim_size = NC_UNLIMITED;
                                              handle->numrecs = val;
                                          }
                                        else  
                                            dim_size = val;
                                    }  /*  */

                                  if (!HDstrcmp(vsclass, DIM_VALS01)) /* dimval01  */
                                      is_dimval01 = TRUE;

                                  if (VSdetach(vs) == FAIL)
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    } 

                                  /* Is it the second dim vs of a compatible dim? */
                                  found = FALSE;
                                  for (i = count-1; ((i >= 0) && (!found)); i--)  
                                    {
                                        if (!HDstrcmp(vgname, dimension[i]->name->values) &&
                                            (dim_size == dimension[i]->size))     {
                                            /* vgname is the dim name. vgname may be diff from vsname */
                                            if (is_dimval01 == TRUE && is_dimval == TRUE)
                                                dimension[i]->dim00_compat = 1;
                                            found = TRUE;   /* the second vs */
                                        }
                                    }  /* for */

                                  if (!found) 
                                    {
                                        dimension[count] = NC_new_dim(vgname, dim_size);
                                        if(NULL == dimension[count]) 
                                          {
#ifdef DEBUG
                                              /* replace it with NCadvice or HERROR?? */
                                              fprintf(stderr, "Can't create new dimension #%d\n", count);
#endif
                                              ret_value = FAIL;
                                              goto done;
                                          }  /*  dimension[count]  */
#if DEBUG
                                        fprintf(stderr, "Dimension <%s> has size %d\n", vgname, dim_size);
#endif
                                        if (!HDstrcmp(vsclass, DIM_VALS01)) /* dimvals01 only  */
                                            dimension[count]->dim00_compat = 0;
                                        count++;
                                    }  /* found */
                              }    /* is vs  */
                        }      /* while in dimension vg  */
                  }        /* is vg  */

                if (Vdetach(dim) == FAIL)
                  {
                      ret_value = FAIL;
                      goto done;
                  }
            }  /* while */
      }

    if(count)
      {
          handle->dims = NC_new_array(NC_DIMENSION, count, (Void *) dimension);
          if (handle->dims == NULL)
            {
                ret_value = FAIL;
                goto done;
            }
      }
    else
        handle->dims = NULL;

#if DEBUG
    fprintf(stderr, "Created dimension array %d \n", handle->dims);
#endif

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
          if (handle->dims != NULL)
            {
                NC_free_array(handle->dims);
                handle->dims = NULL;
            }
      }
     /* Normal cleanup */
    if (dimension != NULL)
        HDfree(dimension);

    return ret_value;
} /* hdf_read_dims */

/******************************************************************************
 NAME 
   hdf_num_attrs

 DESCRIPTION
   Determine number of attributes in vgroup i.e. of the SDS

 RETURNS
   returns number of attributes in vgroup if successful and FAIL
   otherwise.
*******************************************************************************/
static intn
hdf_num_attrs(NC *handle,/* IN: handle to SDS */
              int32 vg   /* IN: ref of top Vgroup */)
{
    int       count = 0;
    int       t, n;
    int32     vs, tag;
    int32     id = -1;
    char      class[MAX_NC_CLASS] = "";
    intn      ret_value = FAIL;

#ifdef HDF_NUM_ATTRS
    fprintf(stderr, "hdf_num_attrs: I've been called, handle->hdf_file = %d\n", handle->hdf_file);
#endif

    n = Vntagrefs(vg);
    if (n == FAIL)
      {
#ifdef HDF_NUM_ATTRS
          fprintf(stderr,"hdf_read_attrs: Vntagrefs failed \n");
#endif
          ret_value = FAIL;
          goto done;
      }

#ifdef HDF_NUM_ATTRS
    fprintf(stderr,"hdf_num_attrs: Vntagrefs returned =%d \n",n);
#endif

    /*
     * look through for a Vdata of class _HDF_ATTRIBUTE
     */
    for (t = 0; t < n; t++) 
      {
          if (Vgettagref(vg, t, &tag, &id) == FAIL)
            {
                ret_value = FAIL;
                goto done;
            }
      
          if(tag == DFTAG_VH) 
            {
                vs = VSattach(handle->hdf_file, id, "r");
                if(vs == FAIL) 
                  {
                      ret_value = FAIL;
                      goto done;
                  }
          
                if (VSgetclass(vs, class) == FAIL)
                  {
                      ret_value = FAIL;
                      goto done;
                  }

                if(!HDstrcmp(class, _HDF_ATTRIBUTE)) 
                    count++;

                if (VSdetach(vs) == FAIL)
                  {
                      ret_value = FAIL;
                      goto done;
                  }
            }
      }
  
#ifdef HDF_NUM_ATTRS
    fprintf(stderr, "hdf_num_attrs: number of attributes is %d \n", count);
#endif

    ret_value = count;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
     /* Normal cleanup */

    return ret_value;
} /* hdf_num_attrs */

/* ----------------------------------------------------------------
** Read in all attributes of the current vg
** Return NULL if something goes wrong
** Return a pointer to the array of attributes if all goes well
*/
NC_array *
hdf_read_attrs(XDR *xdrs, NC *handle, int32 vg)
{
    int       count, t, n;
    int32     vs, tag, id, vsize, attr_size, nt;
    nc_type   type;
    char      vsname[MAX_NC_NAME] = "";
    char      fields[100] = "" ;
    char      class[MAX_NC_CLASS] = "";
    char     *values = NULL;
    NC_attr **attributes = NULL;
    NC_array *Array = NULL;
    NC_array *ret_value = NULL;

    count = 0;
    id = -1;

#if DEBUG
    fprintf(stderr, "hdf_read_attrs I've been called, handle->hdf_file = %d\n", handle->hdf_file);
#endif

    n = Vntagrefs(vg);
    if (n == FAIL)
      {
#ifdef HDF_READ_ATTRS
          fprintf(stderr,"hdf_read_attrs: Vntagrefs failed \n");
#endif
          ret_value = NULL;
          goto done;
      }

#ifdef HDF_READ_ATTRS
    fprintf(stderr,"hdf_read_attrs: Vntagrefs returned =%d \n",n);
#endif

    /*
     * Allocate enough space in case everything is an attribute
     */
    count = 0;
    attributes = (NC_attr **) HDmalloc(sizeof(NC_attr *) * n + 1);
    if(NULL == attributes) 
      {
          /* replace it with NCAdvice or HERROR? */
#ifdef HDF_READ_ATTRS
          fprintf(stderr, "Out of memory line %d file %s\n", __LINE__, __FILE__);
#endif
          ret_value = NULL;
          goto done;
      }

    /*
     * look through for a Vdata of class _HDF_ATTRIBUTE
     */
    for (t = 0; t < n; t++) 
      {
          if (Vgettagref(vg, t, &tag, &id) == FAIL)
            {
                ret_value = NULL;
                goto done;
            }
      
          if(tag == DFTAG_VH) 
            {
                vs = VSattach(handle->hdf_file, id, "r");
                if(vs == FAIL) 
                  {
                      ret_value = NULL;
                      goto done;
                  }
          
                if (VSgetclass(vs, class) == FAIL)
                  {
                      ret_value = NULL;
                      goto done;
                  }

                if(!HDstrcmp(class, _HDF_ATTRIBUTE)) 
                  {
                      if (VSinquire(vs, &attr_size, NULL, fields, &vsize, vsname) == FAIL)
                        {
                            ret_value = NULL;
                            goto done;
                        }

                      if ((nt = VFfieldtype(vs, 0)) == FAIL)
                        {
                            ret_value = NULL;
                            goto done;
                        }

                      if ((type = hdf_unmap_type(nt)) == FAIL)
                        {
                            ret_value = NULL;
                            goto done;
                        }

                      values = (char *) HDmalloc(vsize * attr_size + 1);
                      if (NULL == values)
                        {
                            ret_value = NULL;
                            goto done;
                        }

                      if (VSsetfields(vs, fields) == FAIL)
                        {
                            ret_value = NULL;
                            goto done;
                        }

                      if (VSread(vs, (uint8 *) values, attr_size, FULL_INTERLACE) == FAIL)
                        {
                            ret_value = NULL;
                            goto done;
                        }

              
                      if(type == NC_CHAR) 
                        {
                            if ((attr_size = VFfieldorder(vs, 0)) == FAIL)
                              {
                                  ret_value = NULL;
                                  goto done;
                              }

                            ((char *) values)[attr_size] = '\0';
                        }

                      attributes[count] = 
                          (NC_attr *) NC_new_attr(vsname, type, attr_size, values);
                      if(NULL == attributes[count]) 
                        {
                            /* replace it with NCadvice or HERROR? */
#ifdef HDF_READ_ATTRS
                            fprintf(stderr, "hdf_read_attrs: Can't create new attribute #%d\n", count);
#endif
                            ret_value = NULL;
                            goto done;
                        }
                      attributes[count]->HDFtype = nt;
              
#ifdef HDF_READ_ATTRS
                      fprintf(stderr, "hdf_read_attrs: Attribute <%s> has type %d and size %d\n", 
                              vsname, type, attr_size);
#endif
                      /* free values and reset to NULL */
                      HDfree(values);
                      values = NULL;
                      count++;
                  } /* end if attribute ? */

                if (VSdetach(vs) == FAIL)
                  {
                      ret_value = NULL;
                      goto done;
                  }
            } /* end if DFTAG_VH */
      } /* end for */
  
    /* create array of attributes */
    if(count) 
        Array = NC_new_array(NC_ATTRIBUTE, count, (Void *) attributes);

#ifdef HDF_READ_ATTRS
    fprintf(stderr, "hdf_read_attrs: Created attribute array %d \n", Array);
#endif

    ret_value = Array; /* return array of attributes */

done:
    if (ret_value == NULL)
      { /* Failure cleanup */
          if (Array != NULL)
              NC_free_array(Array);
      }
     /* Normal cleanup */
    if (values != NULL)
        HDfree(values);
    if (attributes != NULL)
        HDfree(attributes);

    return ret_value;
} /* hdf_read_attrs */

/* ----------------------------------------------------------------
** Read in the variables out of a cdf structure
** Return FAIL if something goes wrong
**
** Important:  We must already assume that handle->dims is set
**   so that we can do a call to NC_var_shape() so that we can
**   set the numrecs fields of variables (so we can fill record
**   variables intelligently)
*/
intn 
hdf_read_vars(XDR *xdrs, 
              NC *handle, 
              int32 vg)
{
    char     vgname[MAX_NC_NAME] = "";
    char     subname[MAX_NC_NAME] = "";
    char     class[MAX_NC_CLASS] = "";
    NC_var **variables = NULL;
    NC_var  *vp = NULL;
    int      ndims, *dims = NULL;
    uint8    ntstring[4];
    int      data_ref, is_rec_var, vg_size, count;
    int32    data_count;
    int32    HDFtype = FAIL;
    int32    tag;
    int32    id;
    int32    n;
    int32    sub_id;
    int32    entries;
    int32    ndg_ref = 0;
    int32    rag_ref = 0;
    intn     nattrs;
    register int     t, i;
    register nc_type type;
    register int32   var, sub;
    intn     ret_value = SUCCEED;
#ifdef LATER
    CONSTR(FUNC,"hdf_read_vars"); 
#endif /* LATER */

    count = 0;
    id = -1;

#ifdef HDF_READ_VARS
    fprintf(stderr, "hdf_read_vars: I've been called, handle->hdf_file = %d\n", handle->hdf_file);
#endif

    /*
     * Allocate enough space in case everything is a variable
     */
    count = 0;
    variables = (NC_var **) HDmalloc(sizeof(NC_var *) * Vntagrefs(vg) + 1);
    if(NULL == variables) 
      {
#ifdef HDF_READ_VARS
          fprintf(stderr, "hdf_read_vars:Out of memory line %d file %s\n", __LINE__, __FILE__);
#endif
          ret_value = FAIL;
          goto done;
      }

    /*
     * Allocate enough space in case lots of dimensions
     */
    dims = (int *) HDmalloc(sizeof(int) * Vntagrefs(vg) + 1);
    if(NULL == dims) 
      {
#ifdef HDF_READ_VARS
          fprintf(stderr, "hdf_read_vars:Out of memory line %d file %s\n", __LINE__, __FILE__);
#endif
          ret_value = FAIL;
          goto done;
      }

    /*
     * Look through for a Vgroup of class _HDF_VARIABLE
     */
    if ((vg_size = Vntagrefs(vg)) == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }

    for(i = 0; i < vg_size; i++) 
      {
          if (Vgettagref(vg, i, &tag, &id) == FAIL)
            {
#ifdef HDF_READ_VARS
                fprintf(stderr, "hdf_read_vars:Vgettagref failed\n");
#endif
                ret_value = FAIL;
                goto done;
            }

          if(tag == DFTAG_VG) 
            {
                var = Vattach(handle->hdf_file, id, "r");
                if(var == FAIL) 
                    continue; /* isn't this bad? -GV */
          
                if (Vgetclass(var, class) == FAIL)
                  {
#ifdef HDF_READ_VARS
                      fprintf(stderr, "hdf_read_vars:Vgetclass failed\n");
#endif
                      ret_value = FAIL;
                      goto done;
                  }

                if(!HDstrcmp(class, _HDF_VARIABLE)) 
                  {
              
                      /*
                       * We have found a VGroup representing a Variable
                       */
                      ndims = 0;
                      type = NC_UNSPECIFIED;
                      data_ref = 0;
                      data_count = 0;
                      rag_ref = 0;
                      is_rec_var = FALSE;

                      if (Vinquire(var, &n, vgname) == FAIL)
                        {
#ifdef HDF_READ_VARS
                            fprintf(stderr, "hdf_read_vars:Vinquire failed\n");
#endif
                            ret_value = FAIL;
                            goto done;
                        }
          
                      /*
                       * Loop through contents looking for dimensions
                       */
                      for (t = 0; t < n; t++) 
                        {
                            if (Vgettagref(var, t, &tag, &sub_id) == FAIL)
                              {
#ifdef HDF_READ_VARS
                                  fprintf(stderr, "hdf_read_vars:Vgettagref failed\n");
#endif
                                  ret_value = FAIL;
                                  goto done;
                              }

                            switch(tag) 
                              {
                              case DFTAG_VG :   /* ------ V G R O U P ---------- */
                                  sub = Vattach(handle->hdf_file, sub_id, "r");
                                  if (FAIL == sub)
                                    {
#ifdef HDF_READ_VARS
                                        fprintf(stderr, "hdf_read_vars:Vattach failed\n");
#endif
                                        ret_value = FAIL;
                                        goto done;
                                    }

                                  if (FAIL == Vgetclass(sub, class))
                                    {
#ifdef HDF_READ_VARS
                                        fprintf(stderr, "hdf_read_vars:Vgetclass failed\n");
#endif
                                        ret_value = FAIL;
                                        goto done;
                                    }

                                  if(!HDstrcmp(class, _HDF_DIMENSION) 
                                     || !HDstrcmp(class, _HDF_UDIMENSION)) 
                                    {
                          
                                        if(!HDstrcmp(class, _HDF_UDIMENSION))
                                            is_rec_var = TRUE;
                          
                                        if (FAIL == Vinquire(sub, &entries, subname))
                                          {
#ifdef HDF_READ_VARS
                                        fprintf(stderr, "hdf_read_vars:Vinquire failed\n");
#endif
                                              ret_value = FAIL;
                                              goto done;
                                          }
                          
                                        dims[ndims] = (int) NC_dimid( handle, subname);
                                        if (-1 == dims[ndims]) /* should change to FAIL */
                                          {
#ifdef HDF_READ_VARS
                                        fprintf(stderr, "hdf_read_vars:NC_dimid failed\n");
#endif
                                              ret_value = FAIL;
                                              goto done;
                                          }

                                        ndims++;
                                    }
                                  if (FAIL == Vdetach(sub))
                                    {
#ifdef HDF_READ_VARS
                                        fprintf(stderr, "hdf_read_vars:Vdetach failed\n");
#endif
                                        ret_value = FAIL;
                                        goto done;
                                    }

                                  break;
                              case DFTAG_VH :   /* ----- V D A T A ----- */
                                  break;
                              case DFTAG_NDG :  /* ----- NDG Tag for HDF 3.2 ----- */
                                  ndg_ref = sub_id;
                                  break;
                              case DFTAG_SD :   /* ------- Data Storage ------ */
                                  data_ref = sub_id;
                                  /* Note: apparently Hlength will fail in certain cases, but
                                           but this okay since I believe this is because 
                                           the data does not exist yet in the file? 
                                           So we can't catch this error -GV*/
                                  data_count = Hlength(handle->hdf_file, DATA_TAG, sub_id);
#ifdef HDF_READ_VARS
                                  fprintf(stderr, "hdf_read_vars:Hlength returned %d\n",data_count);
#endif

                                  break;
                              case DFTAG_SDRAG : /* ----- Ragged Array index ----- */
                                  rag_ref = sub_id;
#ifdef HDF_READ_VARS
                                  printf("hdf_read_vars:Lookout!  Found a ragged array element\n");
#endif
                                  break;
                              case DFTAG_NT :   /* ------- Number type ------- */
                                  if(Hgetelement(handle->hdf_file, tag, sub_id, ntstring) == FAIL)
                                    {
#ifdef HDF_READ_VARS
                                        fprintf(stderr, "hdf_read_vars:Hgetlement failed\n");
#endif
                                        ret_value = FAIL;
                                        goto done;
                                    }

                                  HDFtype = ntstring[1];
                                  if ((type = hdf_unmap_type(HDFtype)) == FAIL)
                                    {
#ifdef HDF_READ_VARS
                                        fprintf(stderr, "hdf_read_vars:hdf_unmap_teyp failed\n");
#endif
                                        ret_value = FAIL;
                                        goto done;
                                    }

                                  /*
                                   * Check if data was stored in native format
                                   * And make sure the numbertype version numbers are the same
                                   */
                                  if((ntstring[0] != DFNT_VERSION) 
                                     || ((ntstring[3] != DFNTF_NONE) 
                                         && (ntstring[3] != DFNTF_IEEE))) 
                                    {
                          
                                        /* check if in native mode for a different type of machine  or external data file is LITEND */
                                        if (ntstring[3] == DFNTF_PC)
                                            HDFtype |= DFNT_LITEND;
                                        else
                                          {
                                              if(ntstring[3] != (uint8)DFKgetPNSC(HDFtype, DF_MT)) 
                                                {
                                                    /* 
                                                     * OK, we have a problem here --- is in native mode
                                                     * for a different machine.  PUNT
                                                     */
                                                    goto bad_number_type; /* GOTO */
#ifdef HDF_READ_VARS
                                                    fprintf(stderr, "hdf_read_vars: BAD number type \n");
#endif
                                                } 
                                              else 
                                                {
                                                    /*
                                                     * Is in native mode but its OK --- same machine type
                                                     */
                                                    HDFtype |= DFNT_NATIVE;
                                                }
                                          }
                                    }
                      
                                  break;
                              default:
                                  /* Do nothing */
                                  break;
                              }
                        }
          
                      variables[count] = NC_new_var(vgname, type, ndims, dims);
		      /* BMR: put back hdf type that was set wrong by 
			 NC_new_var; please refer to the cvs history of 
			 bug #172 for reason on this statement - 4/17/2001 
		      */
		      variables[count]->HDFtype = HDFtype;

                      vp = variables[count];
                      if(NULL == vp) 
                        {
#ifdef HDF_READ_VARS
                            fprintf(stderr, "hdf_read_vars:Can't read new variable %s\n", vgname);
#endif
                            ret_value = FAIL;
                            goto done;
                        }
              
#ifdef HDF_READ_VARS
                      fprintf(stderr,"hdf_read_vars:Created a variable called %s   (id %d) \n", vgname, id);
#endif
                      /* Read in the attributes if any */
                      if ((nattrs = hdf_num_attrs(handle, var)) > 0)
                          vp->attrs = hdf_read_attrs(xdrs, handle, var);
                      else
                          vp->attrs = NULL;

#ifdef HDF_READ_VARS
                      fprintf(stderr,"hdf_read_vars:read in %d attributes \n",nattrs );
#endif              
                      /* set up for easy access later */
                      vp->vgid     = id;
                      vp->data_ref = data_ref;
                      vp->data_tag = DATA_TAG;
                      vp->HDFtype  = HDFtype;
                      vp->ndg_ref  = (uint16) ndg_ref;
                      vp->cdf = handle; /* for NC_var_shape */

                      /* need to process the ragged array info here */
                      /* QUESTION:  Load the whole rag_fill list in now??????? */
                      if(rag_ref) 
                        {
                            vp->is_ragged = TRUE;
                        }
              
                      if(vp->data_ref) 
                        {
                            /*
                             * We have already seen data for this variable so now
                             *  we need to worry about its numrecs field
                             */
                  
                            if(is_rec_var) 
                              {
                                  /*
                                   * Call NC_var_shape() so we can figure out how many
                                   *  records have been written.  This is horribly 
                                   *  inefficient, but the separation-of-powers gets
                                   *  really mucked up if we wait till later...
                                   */
                      
                                  if(NC_var_shape(vp, handle->dims) == -1)
                                    {
#ifdef HDF_READ_VARS
                                        fprintf(stderr,"hdf_read_vars:NC_var_shape failed \n" );
#endif              
                                        ret_value = FAIL;
                                        goto done;
                                    }
                      
                                  /*
                                   * Now figure out how many recs have been written
                                   * For a while there was a -1 at the end of this
                                   *   equation.  I don't remember why its there
                                   *   (4-Nov-93)
                                   */
                                  vp->numrecs = data_count / vp->dsizes[0];
                      
#ifdef HDF_READ_VARS
                                  fprintf(stderr, "hdf_read_vars:I have set numrecs to %d\n", vp->numrecs);
#endif                  
                                  /*
                                   * Deallocate the shape info as it will be recomputed
                                   *  at a higher level later
                                   */
                                  if(vp->shape != NULL)
                                      HDfree(vp->shape);
                                  if(vp->dsizes != NULL)
                                      HDfree(vp->dsizes);
				  /* Reset these two pointers to NULL after 
				     freeing.  BMR 4/11/01 */
				  vp->shape = NULL;
				  vp->dsizes = NULL;
                              } 
                            else 
                              {
                                  /* Not a rec var, don't worry about it */
                                  vp->numrecs = 1;
                              }
                        }  /* end vp->data_ref */
                      count++;
                  } /* end if vgroup class is variable */

bad_number_type: /* ? */
          
                if (FAIL == Vdetach(var))
                  {
                      ret_value = FAIL;
                      goto done;
                  }

            } /* end if DTAG_VG */
      } /* end for vg_size */

    /* create array of variables */
    if(count)
      {
          handle->vars = NC_new_array(NC_VARIABLE, count, (Void *) variables);
          if (NULL == handle->vars)
            {
                ret_value = FAIL;
                goto done;
            }
      }
    else
        handle->vars = NULL;

#ifdef HDF_READ_VARS
    fprintf(stderr, "hdf_read_vars: Created variable array %d \n", handle->vars);
#endif

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
#ifdef HDF_READ_VARS
          fprintf(stderr, "hdf_read_vars: failed to created variable array \n");
#endif

          if (handle->vars != NULL)
              NC_free_array(handle->vars);
      }
     /* Normal cleanup */
    if (variables != NULL)
        HDfree(variables);
    if (dims != NULL)
        HDfree(dims);

    return ret_value;
} /* hdf_read_vars */


/* ----------------------------------------------------------------
** Read in a cdf structure
*/
intn
hdf_read_xdr_cdf(xdrs, handlep)
XDR *xdrs;
NC **handlep;
{
#if DEBUG
  char            vgname[MAX_NC_NAME];
  int32           entries;
#endif
  register int32  cdf_vg = FAIL;
  register int    vgid = 0;
  int             status;
#ifdef OLD_WAY
  register int    found;
  char            class[MAX_NC_CLASS];
#endif /* OLD_WAY */
  CONSTR(FUNC,"hdf_read_xdr_cdf");
  intn            ret_value = SUCCEED;

#if DEBUG
 fprintf(stderr, "hdf_read_xdr_cdf i've been called %d\n", (*handlep)->hdf_file);
#endif

#ifdef OLD_WAY
  /* find first thing of type _HDF_CDF */
  vgid = -1;
  found = FALSE;
  while(!found 
        && ((vgid = Vgetid((*handlep)->hdf_file, vgid)) != FAIL)) 
    {
        cdf_vg = Vattach((*handlep)->hdf_file, vgid, "r");
        if(cdf_vg == FAIL) 
          {
            HERROR(DFE_CANTATTACH);
            ret_value = FAIL;
            goto done;
          }
        if (Vgetclass(cdf_vg, class) == FAIL)
          { 
              ret_value = FAIl;
              goto done;
          }

        if(!HDstrcmp(class, _HDF_CDF)) 
            found = TRUE;
        else 
          {
            if (Vdetach(cdf_vg) == FAIL)
              { 
                  ret_value = FAIL;
                  goto done;
              }
          }
    }

  if(!found)
    {
      ret_value = FAIL;
      goto done;
    }
  
#else /* new way */

    if((vgid = Vfindclass((*handlep)->hdf_file,_HDF_CDF))!=FAIL) 
      {
          cdf_vg = Vattach((*handlep)->hdf_file, vgid, "r");
          if(cdf_vg == FAIL) 
            {
              HERROR(DFE_CANTATTACH);
              ret_value = FAIL;
              goto done;
            }
      } /* end if */
    else
      {
        ret_value = FAIL;
        goto done;
      }

#endif /* new way */

  (*handlep)->vgid = vgid; /* ref of vgroup */

#if DEBUG
  Vinquire(cdf_vg, &entries, vgname);
  fprintf(stderr, "Found _HDF_CDF : %s  (%d entries)\n", vgname, entries);
#endif

  /* read in dimensions */
  status = hdf_read_dims(xdrs, (*handlep), cdf_vg);
  if(status == FAIL)
    {
        ret_value = FAIL;
        goto done;
    }

  /* read in variables */
  status = hdf_read_vars(xdrs, (*handlep), cdf_vg);
  if(status == FAIL)
    {
        ret_value = FAIL;
        goto done;
    }

  /* read in attributes */
  if(hdf_num_attrs((*handlep), cdf_vg) > 0 )
      (*handlep)->attrs = hdf_read_attrs(xdrs, (*handlep), cdf_vg);
  else
      (*handlep)->attrs = NULL;

  /* deatch from cdf vgroup */
  if (FAIL == Vdetach(cdf_vg))
    {
        ret_value = FAIL;
        goto done;
    }

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
          if (cdf_vg != FAIL)
              Vdetach(cdf_vg);
      }
     /* Normal cleanup */

    return ret_value;
} /* hdf_read_xdr_cdf */

/* -------------------------------------------------------------------
** Read or write a CDF structure
**
** If we are reading, first try to read the information out of netCDF
**    object stored explicitly in HDF files as netCDF objects.  If
**    that fails try to read SDSs out of the HDF file and interpret
**    them as netCDF information.
*/
intn
hdf_xdr_cdf(xdrs, handlep)
XDR *xdrs;
NC **handlep;
{
    CONSTR(FUNC,"hdf_xdr_cdf"); /* for HERROR */
    intn status;
    intn ret_value = SUCCEED;

#ifdef HDF_XDR_CDF
    fprintf(stderr, "hdf_xdr_cdf: i've been called op = %d \n", xdrs->x_op);
#endif

    switch(xdrs->x_op) 
      {
      case XDR_ENCODE :
          if((*handlep)->vgid) 
            {
                if (FAIL == hdf_cdf_clobber((*handlep)))
                  {
                      ret_value = FAIL; 
                      goto done;
                  }
            }
          status = hdf_write_xdr_cdf(xdrs, handlep);
          if (FAIL == status)
            {
#ifdef HDF_XDR_CDF
                fprintf(stderr, "hdf_xdr_cdf: hdf_write_xdr_cdf failed \n");
#endif
                ret_value = FAIL;
                goto done;
            }
          break;
      case XDR_DECODE :
          if(FAIL == (status = hdf_read_xdr_cdf(xdrs, handlep))) 
            {
#ifdef HDF_XDR_CDF
                fprintf(stderr, "hdf_xdr_cdf: hdf_read_xdr_cdf failed \n");
                fprintf(stderr,"               going to hdf_read_sds \n");
#endif
                status = hdf_read_sds_cdf(xdrs, handlep);
                if(FAIL == status)
                  {
#ifdef HDF_XDR_CDF
                      fprintf(stderr, "hdf_xdr_cdf: hdf_read_sds failed \n");
#endif
                      HERROR(DFE_BADNDG);
                      ret_value = FAIL;
                      goto done;
                  }
            } /* end if */
          break;
      case XDR_FREE   :
          if (FAIL == NC_free_cdf((*handlep)))
              ret_value = FAIL;
          else
              ret_value = SUCCEED;
          break;
      default:
          ret_value = FAIL;
      }

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
     /* Normal cleanup */

    return ret_value;  
} /* hdf_xdr_cdf */

/* ---------------------- hdf_vg_clobber --------------- */
/*
  Delete a VGroup that is on the disk.  Basically, we will want to
  just trash everything inside of it, making sure that any VDatas
  with class == DATA are saved
*/
intn 
hdf_vg_clobber(handle, id)
NC *handle;
int id;
{
    int   t, n;
    int32 vg, tag, ref;
    int32 status;
    intn  ret_value = SUCCEED;

#ifdef HDF_VG_CLOBBER
    fprintf(stderr, "hdf_vg_clobber: has been called for vgroup ref=%d\n",id);
#endif

    /* loop through and Clobber all top level VGroups */

    /* attach to top level vgroup with read access */
    vg = Vattach(handle->hdf_file, id, "r");
    if(FAIL == vg) 
      {
#ifdef HDF_VG_CLOBBER
          fprintf(stderr,"hdf_vg_clobber: Vattach failed for vgroup ref =%d\n",id);
          HEprint(stderr,0);
#endif
          ret_value = FAIL;
          goto done;
      }

    /* get number of members in vgroup */
    n = Vntagrefs(vg);
    if (FAIL == n)
      {
#ifdef HDF_VG_CLOBBER
          fprintf(stderr,"hdf_vg_clobber: Vntagrefs failed \n");
#endif
          ret_value = FAIL;
          goto done;
      }

    /* Loop though and kill stuff */
    for (t = 0; t < n; t++) 
      {   /* get tag/ref of elment in vgroup */
          if (FAIL == Vgettagref(vg, t, &tag, &ref))
            {
#ifdef HDF_VG_CLOBBER
                fprintf(stderr,"hdf_vg_clobber: Vgettagref failed \n");
#endif
                ret_value = FAIL;
                goto done;
            }


#ifdef HDF_VG_CLOBBER
          fprintf(stderr, "hdf_vg_clobber: Looking at <%d, %d> in vgroup\n", tag, ref);
#endif
          /* switch on the type of element: vgroup, vdata, data, 
             everyting else */
          switch(tag) 
            {
            case DFTAG_VG : /* recursive call */
#ifdef HDF_VG_CLOBBER
                fprintf(stderr,"hdf_vg_clobber: found a vgroup ref %d in vgroup %d\n",ref,id);
#endif
                /* check if vgroup exists in file before trying to delete
                   it's members */
                if (vexistvg(handle->hdf_file, ref) != FAIL)
                  {
                      if (FAIL == hdf_vg_clobber(handle, ref))
                        {
#ifdef HDF_VG_CLOBBER
                            fprintf(stderr,"hdf_vg_clobber: hdf_vg_clobber failed member whose vgroup ref=%d\n",ref);
#endif
                            ret_value = FAIL;
                            goto done;
                        }
                  }
                break;
            case DFTAG_VH :
                /* check if vdata exists in file before trying to delete it */
                if (vexistvs(handle->hdf_file, ref) != FAIL)
                  {
                      status = VSdelete(handle->hdf_file, (int32) ref);
                      if (FAIL == status)
                        {
#ifdef HDF_VG_CLOBBER
                            fprintf(stderr,"hdf_vg_clobber: VSdelete failed for vdata ref=%d\n",ref);
#endif
                            ret_value = FAIL;
                            goto done;
                        }
                  }
#ifdef HDF_VG_CLOBBER
                fprintf(stderr,"hdf_vg_clobber: VSdelete deleted vdata ref=%d\n",ref);
#endif
                break;
            case DFTAG_SD :
                /*
                 * Don't delete actual numeric data
                 * I guess this means we save it? -GV
                 */
                break;
            default: /* delete other objects given tag/ref in file */
                if (FAIL == Hdeldd(handle->hdf_file, (uint16) tag, (uint16) ref))
                  {
#ifdef HDF_VG_CLOBBER
                      fprintf(stderr,"hdf_vg_clobber: Hdeldd failed \n");
#endif
                      ret_value = FAIL;
                      goto done;
                  }

#ifdef HDF_VG_CLOBBER
                fprintf(stderr,"hdf_vg_clobber: Hdeldd deleted tag/ref=%d/%d\n",tag,ref);
#endif
                break;
            }
      }

    ret_value = Vdetach(vg);

#ifdef HDF_VG_CLOBBER
    fprintf(stderr, "hdf_vg_clobber: Vdetach, ret_value=%d \n",ret_value);
#endif

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
#ifdef HDF_VG_CLOBBER
          fprintf(stderr, "hdf_vg_clobber: failed \n");
#endif
      }
     /* Normal cleanup */

    return ret_value;  
} /* hdf_vg_clobber */


/* --------------------------- hdf_cdf_clobber ---------------------------- */
/*
  Delete a netCDF structure that has been already written to disk
*/
intn 
hdf_cdf_clobber(handle)
NC *handle;
{
    int32  vg, tag, ref;
    int    n, t, status;
    intn   ret_value = SUCCEED;

    if(!handle->vgid) 
      { /* okay right? */
          ret_value = SUCCEED; /* hmm...since ref of vgroup is zero? */
          goto done;
      }

    /* Close open VData pointers */
    if (FAIL == hdf_close(handle))
      {
#ifdef HDF_CDF_CLOBBER
          fprintf(stderr,"hdf_cdf_clobber: hdf_close failed \n");
#endif
          ret_value = FAIL;
          goto done;
      }

#ifdef HDF_CDF_CLOBBER
    fprintf(stderr,"hdf_cdf_clobber: closed all open vdata handles \n");
#endif

    /* loop through and Clobber all top level VGroups */
    vg = Vattach(handle->hdf_file, handle->vgid, "r");
    if(vg == FAIL) 
      {
#ifdef HDF_CDF_CLOBBER
          fprintf(stderr,"hdf_cdf_clobber: Vattach failed for vgroup ref=%d\n",
                  handle->vgid);
#endif
          ret_value = FAIL;
          goto done;
      }

    /* get number of members of Vgroup */
    n = Vntagrefs(vg);
    if (FAIL == n)
      {
#ifdef HDF_CDF_CLOBBER
          fprintf(stderr,"hdf_cdf_clobber: Vntagrefs failed \n");
#endif
          ret_value = FAIL;
          goto done;
      }

    /* Loop though and just kill everyone */
    for (t = 0; t < n; t++) 
      {
          if (FAIL == Vgettagref(vg, t, &tag, &ref))
            {
#ifdef HDF_CDF_CLOBBER
                fprintf(stderr,"hdf_cdf_clobber: Vgettagref failed for vgroup %d\n",
                        handle->vgid);
#endif
                ret_value = FAIL;
                goto done;
            }

          /* if this member is a vgroup destroy everything in it */
          if(tag == DFTAG_VG) 
            {
#ifdef HDF_CDF_CLOBBER
                fprintf(stderr,"hdf_cdf_clobber: member of vgroup is a vgroup,");
                fprintf(stderr,"deleteing everything in vgroup %d \n",ref);
#endif
                /* check if vgroup exists in file */
                if (vexistvg(handle->hdf_file, ref) != FAIL)
                  {
                      hdf_vg_clobber(handle, ref);
                  }
            }

          switch(tag) 
            {
            case DFTAG_VG:
                status = Vdelete(handle->hdf_file, (int32) ref);
                if (FAIL == status)
                  {
#ifdef HDF_CDF_CLOBBER
                      fprintf(stderr,"hdf_cdf_clobber: Vdelete failed for vgroup %d\n",
                              ref);
#endif
                      ret_value = FAIL;
                      goto done;
                  }

#ifdef HDF_CDF_CLOBBER
                fprintf(stderr,"hdf_cdf_clobber: Vdelete deleted vgroup %d\n",
                        ref);
#endif
                break;
            case DFTAG_VH:
                status = VSdelete(handle->hdf_file, (int32) ref);
                if (FAIL == status)
                  {
#ifdef HDF_CDF_CLOBBER
                      fprintf(stderr,"hdf_cdf_clobber: VSdelete failed for vdata %d\n",
                              ref);
#endif
                      ret_value = FAIL;
                      goto done;
                  }

#ifdef HDF_CDF_CLOBBER
                fprintf(stderr,"hdf_cdf_clobber: VSdelete deleted vdata %d\n",
                        ref);
#endif
                break;
            default:
                status = Hdeldd(handle->hdf_file, (uint16) tag, (uint16) ref);
                if (FAIL == status)
                  {
#ifdef HDF_CDF_CLOBBER
                      fprintf(stderr,"hdf_cdf_clobber: Hdeldd failed for tag/ref %d/%d\n",
                              tag,ref);
#endif
                      ret_value = FAIL;
                      goto done;
                  }

#ifdef HDF_CDF_CLOBBER
                fprintf(stderr,"hdf_cdf_clobber: Hdeldd deleted tag/ref %d/%d\n",
                        tag,ref);
#endif
                break;
            } /* end switch tag */
      }/* end for every member in vgroup */

    if (FAIL == Vdetach(vg))
      {
#ifdef HDF_CDF_CLOBBER
          fprintf(stderr,"hdf_cdf_clobber: Vdetach failed for vgroup %d\n",
                  handle->vgid);
#endif
          ret_value = FAIL;
          goto done;
      }

    status = Vdelete(handle->hdf_file, (int32) handle->vgid);
    if (FAIL == status)
      {
#ifdef HDF_CDF_CLOBBER
          fprintf(stderr,"hdf_cdf_clobber: Vdelete failed for vgroup %d\n",
                  handle->vgid);
#endif
          ret_value = FAIL;
          goto done;
      }

#ifdef HDF_CDF_CLOBBER
    fprintf(stderr,"hdf_cdf_clobber:Clobbering VGroup %d\n\n", handle->vgid);
#endif

    handle->vgid = 0; /* reset ref of SDS vgroup to invalid ref */

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */
#ifdef HDF_CDF_CLOBBER
        fprintf(stderr,"hdf_cdf_clobber: Failed to Clobber VGroup %d\n\n", handle->vgid);
#endif
    } /* end if */

  /* Normal function cleanup */
  return ret_value;    
} /* hdf_cdf_clobber */

/* -------------------------- hdf_close --------------------- */
/*
  We're about to close the file, do last minute HDF cleanup
  Also dump the number of records currently instatiated into the
  unlimited dimensions.  

  BUG:  All unlimited dimensions will have the same size
*/
intn 
hdf_close(handle)
    NC *handle;
{
    NC_array  *tmp = NULL;
    NC_var   **vp = NULL;
    Void      *vars = NULL;
    register   int i;
    int        id, sub_id;
    int32      vg, dim;
    int32      vs;
    char       class[MAX_NC_CLASS] = "";
    intn       ret_value = SUCCEED;
#ifdef LATER
    CONSTR(FUNC,"hdf_close"); 
#endif /* LATER */

#ifdef HDF_CLOSE
    fprintf(stderr,"hdf_close: I've been called\n");
#endif
    
    /* loop through and detach from variable data VDatas */
    if(handle->vars) 
      {
        tmp = handle->vars; 
        vars = handle->vars->values;
        
        for(i = 0; i < tmp->count; i++) 
          {
            vp = (NC_var **) vars;

            if((*vp)->aid != FAIL)
              {
                if (FAIL == Hendaccess((*vp)->aid))
                  {
#ifdef HDF_CLOSE
                      fprintf(stderr,"hdf_close: Hendaccess failed for vdata aid %d\n",
                              (*vp)->aid);
#endif
                      ret_value = FAIL;
                      goto done;
                  }
              }

            (*vp)->aid = FAIL; /* reset access id */
            vars += tmp->szof;
          } /* end for each variable */
      }
    
    /* loop through top level looking for unlimited dimensions.
       we write them out? -GV */
    if(handle->flags & NC_NDIRTY) 
      {
        id = -1;
        vg = Vattach(handle->hdf_file, handle->vgid, "r");
        if (FAIL == vg)
          {
#ifdef HDF_CLOSE
              fprintf(stderr,"hdf_close: Vattach failed for vgroup ref %d\n",
                      handle->vgid);
#endif
              ret_value = FAIL;
              goto done;
          }

        /* go through vgroup hierachy */
        while((id = Vgetnext(vg, id)) != FAIL) 
          {
            if(Visvg(vg, id)) 
              {
                dim = Vattach(handle->hdf_file, id, "r");
                if (FAIL == dim)
                  {
#ifdef HDF_CLOSE
                      fprintf(stderr,"hdf_close: Vattach failed for vgroup ref %d\n",
                              id);
#endif
                      ret_value = FAIL;
                      goto done;
                  }


                if (FAIL == Vgetclass(dim, class))
                  {
#ifdef HDF_CLOSE
                      fprintf(stderr,"hdf_close: Vgetclass failed for vgroup ref %d\n",
                              id);
#endif
                      ret_value = FAIL;
                      goto done;
                  }

                /* look for proper vgroup */
                if(!HDstrcmp(class, _HDF_UDIMENSION)) 
                  {
                    sub_id = -1;
                    /* look for vdata in vgroup */
                    while((sub_id = Vgetnext(dim, sub_id)) != FAIL) 
                      {
                        if(Visvs(dim, sub_id)) 
                          { /* yes, attach to vdata */
                            vs = VSattach(handle->hdf_file, sub_id, "w");
                            if(vs == FAIL) 
                              {
#ifdef HDF_CLOSE
                                  fprintf(stderr,"hdf_close: VSattach failed for vdata ref %d\n",
                                          sub_id);
#endif
                                  ret_value = FAIL;
                                  goto done;
                                  /* HEprint(stdout, 0); */
                              }
                            /* get class of vdata */
                            if (FAIL == VSgetclass(vs, class))
                              {
#ifdef HDF_CLOSE
                                  fprintf(stderr,"hdf_close: VSgetclass failed for vdata ref %d\n",
                                          sub_id);
#endif
                                  ret_value = FAIL;
                                  goto done;
                              }

                            /* are these dimension vdatas? */
                            if(!HDstrcmp(class, DIM_VALS) 
                               || !HDstrcmp(class, DIM_VALS01)) 
                              { /* yes */
                                int32 val = handle->numrecs;

                                if (FAIL == VSsetfields(vs, "Values"))
                                  {
#ifdef HDF_CLOSE
                                      fprintf(stderr,"hdf_close: VSsetfields failed for vdata ref %d\n",
                                              sub_id);
#endif
                                      ret_value = FAIL;
                                      goto done;
                                  }

                                if (FAIL == VSseek(vs, 0))
                                  {
#ifdef HDF_CLOSE
                                      fprintf(stderr,"hdf_close: VSseek failed for vdata ref %d\n",
                                              sub_id);
#endif
                                      ret_value = FAIL;
                                      goto done;
                                  }

                                /* write out dimension vdatas? */
                                if(VSwrite(vs, (uint8 *)&val, 1, FULL_INTERLACE) != 1)
                                  {
#ifdef HDF_CLOSE
                                      fprintf(stderr,"hdf_close: VSwrite failed for vdata ref %d\n",
                                              sub_id);
#endif
                                      ret_value = FAIL;
                                      goto done;
                                  }

                            }

                            /* detach from vdata */
                            if (FAIL == VSdetach(vs))
                              {
#ifdef HDF_CLOSE
                                  fprintf(stderr,"hdf_close: VSdetach failed for vdata ref %d\n",
                                          sub_id);
#endif
                                  ret_value = FAIL;
                                  goto done;
                              }

                        } /* end if vdata */
                    } /* end while looking for vdata in vgroup */
                } /* end if UNLIMTED dimension vgroup */

                if (FAIL == Vdetach(dim))
                  {
#ifdef HDF_CLOSE
                      fprintf(stderr,"hdf_close: Vdetach failed for vgroup ref %d\n",
                              id);
#endif
                      ret_value = FAIL;
                      goto done;
                  }

            }/* end if vgroup */
        } /* end if looking through toplevel vgroup hierachy */

        if (FAIL == Vdetach(vg))
          {
#ifdef HDF_CLOSE
              fprintf(stderr,"hdf_close: Vdetach failed for vgroup ref %d\n",
                      handle->vgid);
#endif
              ret_value = FAIL;
              goto done;
          }

    } /* end if we need to flush out unlimited dimensions? */

#ifdef QAK
    /* Free dimension array if necessary */
    if(handle->dims) 
        NC_free_array(handle->dims); /* Might need to close Vdatas? -QAK */
#endif /* QAK */

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */
  return ret_value;    
} /* hdf_close */

/*******************************************************************************/                
#endif /* HDF */

/*
 * How much space will the xdr'd NC description take.
 *
 */
int NC_xlen_cdf(cdf)
NC *cdf ;
{
	int len = 8 ;

	if(cdf == NULL)
		return(0) ;

	len += NC_xlen_array(cdf->dims) ;
	len += NC_xlen_array(cdf->attrs) ;
	len += NC_xlen_array(cdf->vars) ;

	return(len) ;
}


#define RECPOS	4L 	/* seek index of numrecs value */
bool_t
xdr_numrecs(xdrs, handle)
	XDR *xdrs;
	NC *handle;
{

#ifdef HDF
    if(handle->file_type == HDF_FILE) 
        return TRUE; /* hmm...why? */
#endif

	if( (handle->flags & NC_NOFILL)
		&& xdrs->x_op == XDR_ENCODE
		&& handle->begin_rec > 0)
      {
          /*
           * we need to write something just beyond the last 
           * record so we can successfully read back the 
           * entire last record.
           */
          if( !xdr_setpos(xdrs, handle->begin_rec
                          +  handle->numrecs * handle->recsize) )
            {
                nc_serror("Can't set position to EOF") ;
                return(FALSE) ;
            }
#ifdef RDEBUG
          fprintf(stderr,"\txdr_numrecs %ld = %d + %ld * %d\n",
                  xdr_getpos(xdrs), 
                  handle->begin_rec, handle->numrecs, handle->recsize) ;
#endif /*  RDEBUG */
          if( !xdr_u_long(xdrs, &(handle->numrecs)) )
              return(FALSE) ;
      }

	if( !xdr_setpos(xdrs, RECPOS) )
      {
          nc_serror("Can't set position to RECPOS") ;
          return(FALSE) ;
      }
	return( xdr_u_long(xdrs, &(handle->numrecs)) ) ;
}

static bool_t
xdr_4bytes(xdrs, cp)
XDR *xdrs ;
char *cp ; /* at least 4 valid bytes */
{
      return xdr_opaque(xdrs, cp, 4) ;
}

static bool_t
xdr_2shorts(xdrs, sp)
XDR *xdrs ;
short *sp ; /* at least 2 valid shorts */
{
      return xdr_shorts(xdrs, sp, 2) ;
}

bool_t
xdr_NC_fill(xdrs, vp)
XDR *xdrs ;
NC_var *vp ;
{
	char fillp[2*sizeof(double)] ;
	bool_t stat ;
	bool_t (*xdr_NC_fnct)() ;
	u_long alen = vp->len ;
	NC_attr **attr = NULL ;


	/*
	 * set up fill value
	 */
	/* start with the default */
	NC_arrayfill((Void *)fillp, (size_t)2*sizeof(double),
                 vp->type) ;

	/* 
	 * if there is a valid user defined value, use it instead
	 */
	attr = NC_findattr(&vp->attrs, _FillValue) ;
	if( attr != NULL )
      {
          if( (*attr)->data->type != vp->type || (*attr)->data->count != 1 )
              NCadvise(NC_EBADTYPE, "var %s: _FillValue type mismatch",
                       vp->name->values) ;
          else
            {
                int len = NC_typelen(vp->type) ;
                char *cp = fillp ;
                while(cp < &fillp[sizeof(fillp) -1])
                  {
                      NC_copy_arrayvals(cp, (*attr)->data) ;
                      cp += len ;
                  }
            }
      }

    switch(vp->type){
    case NC_BYTE :
    case NC_CHAR :
        alen /= 4 ;
        xdr_NC_fnct = xdr_4bytes ;
        break ;
    case NC_SHORT :
        alen /= 4 ;
        xdr_NC_fnct = xdr_2shorts ;
        break ;
    case NC_LONG :
        alen /= 4 ;
#if defined _CRAYMPP
        xdr_NC_fnct = xdr_short;
#elif defined __alpha || (_MIPS_SZLONG == 64) || defined __ia64 || (defined __sun && defined _LP64) || defined AIX5L64 || defined __x86_64__
        xdr_NC_fnct = xdr_int ;
#else
        xdr_NC_fnct = xdr_long ;
#endif
        break ;	
    case NC_FLOAT :
        alen /= 4 ;
        xdr_NC_fnct = xdr_float ;
        break ;
    case NC_DOUBLE : 
        alen /= 8 ;
        xdr_NC_fnct = xdr_double ;
        break ;
    default :
        NCadvise(NC_EBADTYPE, "bad type %d", vp->type) ;
        return(FALSE) ;
    }

    /* write out fill values */
    for(stat = TRUE ; stat && (alen > 0) ; alen--)
      {
          stat = (*xdr_NC_fnct)(xdrs,fillp) ;	
      }

    if(!stat)
      {
          NCadvise(NC_EXDR, "xdr_NC_fill") ;
          return(FALSE) ;
      }
		
    return(TRUE) ;
}

