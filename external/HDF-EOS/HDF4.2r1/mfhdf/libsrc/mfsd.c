/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

#ifdef RCSID
static char RcsId[] = "@(#)$Revision: 1.147 $";
#endif

/* $Id: mfsd.c,v 1.147 2005/02/09 21:12:54 bmribler Exp $ */

/******************************************************************************
file - mfsd.c

  This file contains the HDF/netCDF based multi-file interface for SDSs

  All of the routines that make up this interface have names beginning
  with SD.  Routines beginning with SDI are internal routines and
  should not be used outside of this module.

  Defining SDDEBUG will print status messages to stderr

Reading interface:
------------------

fid    = SDstart(file name, access);

        --- get the number of data-sets in the file ---
num    = SDnumber(fid);
        
sdsid  = SDselect(fid, i, ...);   0 <= i < num

        --- return the name, rank, dimsizes, #of attr, datatype ---
status = SDgetinfo(sdsid, ...); 

status = SDreaddata(sdsid, ...);

status = SDgetrange(sdsid, ...);

status = SDend(fid);

status = SDisdimval_bwcomp(dimid);

status = SDcheckempty(sdsid, emptySDS);

        --- take an id and determine if it is an SD id, SDS id, dim id, ---
        --- or none of the above ---
id_type =  SDidtype(an_id);

NOTE: This file needs to have the comments cleaned up for most of the
       functions here. -GV 9/10/97

******************************************************************************/

#include "local_nc.h"
#ifdef VMS
#include <stat.h>
#endif

#ifdef HDF
#include "mfhdf.h"
#include "hfile.h"

#ifdef H4_HAVE_LIBSZ          /* we have the library */
#include "szlib.h"
#ifndef MIN
#define MIN(a,b)    (((a)<(b)) ? (a) : (b))
#endif
#endif
/* for Chunk debugging */
/*
#define CHK_DEBUG
*/

/* Local function prototypes */

PRIVATE NC_dim * SDIget_dim
    (NC *handle, int32 id);

PRIVATE NC * SDIhandle_from_id 
    (int32 id, intn typ);

PRIVATE NC_var *SDIget_var
    (NC *handle, int32 sdsid);

PRIVATE intn SDIputattr 
    (NC_array **ap, const char *name, int32 nt, intn count, const void * data);

PRIVATE int32 SDIgetcoordvar 
    (NC *handle, NC_dim *dim, int32 id, int32 nt);

PRIVATE int32 SDIfreevarAID 
    (NC * handle, int32 index);

PRIVATE intn SDIapfromid
    (int32 id, NC ** handlep, NC_array *** app);

/* Whether we've installed the library termination function yet for this interface */
PRIVATE intn library_terminate = FALSE;

/******************************************************************************
 NAME
	SDIhandle_from_id -- get the handle from this object

 DESCRIPTION
    Map an ID to the handle for this file

 RETURNS
    NULL or the handle

******************************************************************************/
PRIVATE NC *
SDIhandle_from_id(int32 id, /* IN: an object (file, dim, dataset) ID */
                  intn  typ /* IN: IN: the type of ID this is */)
{
    int32 tmp;
    NC   *ret_value = NULL;

    /* check that it is the proper type of id */
    tmp = (id >> 16) & 0xff;
    if(tmp != typ)
      {
        ret_value = NULL;
        goto done;
      }

    /* get the file from top 8 bits*/
    tmp = (id >> 24) & 0xff;
    ret_value = NC_check_id((int)tmp);

done:
    if (ret_value == NULL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */

    return ret_value;
} /* SDIhandle_from_id */


/******************************************************************************
 NAME
	SDIget_var -- get the variable record

 DESCRIPTION
    Map an ID and a handle to the NC_var object for this dataset

 RETURNS
    NULL or the variable object

******************************************************************************/
PRIVATE NC_var *
SDIget_var(NC   *handle, /* IN: the handle for this file */
           int32 sdsid   /* IN: a dataset ID */)
{
    int32      varid;
    NC_array **ap = NULL;
    NC_var    *ret_value = NULL;

    /* varid is low 16bits of sdsid */
    varid = sdsid & 0xffff;

    if(handle->vars != NULL 
       && varid >= 0 
       && varid < handle->vars->count) 
      {
        ap = (NC_array **)handle->vars->values;
        ap += varid;
      } 
    else 
      {
        ret_value = NULL;
        goto done;
      }
    
    ret_value = ((NC_var *)*ap);

done:
    if (ret_value == NULL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */

    return ret_value;    
} /* SDIget_var */


/******************************************************************************
 NAME
	SDIget_dim -- get the dimension record

 DESCRIPTION
    Map an ID and a handle to the NC_dim object for this dimension

 RETURNS
    NULL or the variable object

******************************************************************************/
PRIVATE NC_dim *
SDIget_dim(NC   *handle,/* IN: the handle for this file */
           int32 id     /* IN: a dimension ID */)
{
    int32      dimid;
    NC_array **ap = NULL;
    NC_dim    *ret_value = NULL;

    /* dimid is low 16bits of id */
    dimid = id & 0xffff;

    if(handle->dims != NULL 
       && dimid >= 0 
       && dimid < handle->dims->count) 
      {
        ap = (NC_array **)handle->dims->values;
        ap += dimid;
      } 
    else 
      {
        ret_value = NULL;
        goto done;
      }
    
    ret_value = ((NC_dim *)*ap);

done:
    if (ret_value == NULL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */

    return ret_value;    
} /* SDIget_dim */


/******************************************************************************
 NAME
	SDIstart -- initialize the SD interface

 DESCRIPTION
    Register the atexit callback function, etc.

 RETURNS
    SUCCEED/FAIL
******************************************************************************/
static intn
SDIstart(void)
{
    CONSTR(FUNC, "SDIstart");    /* for HGOTO_ERROR */
    intn        ret_value = SUCCEED;

    /* Don't call this routine again... */
    library_terminate = TRUE;

    /* Install atexit() library cleanup routine */
    if (HPregister_term_func(&SDPfreebuf) != 0)
          HGOTO_ERROR(DFE_CANTINIT, FAIL);

done:
    if(ret_value == FAIL)
    { /* Error condition cleanup */

    } /* end if */

    /* Normal function cleanup */
    return(ret_value);
} /* end SDIstart() */


/******************************************************************************
 NAME
	SDI_can_clobber -- check permission on the file

 DESCRIPTION
    Check the file permissions.  If OK to clobber the file, return 1,
    else return 0. 
    Called by SDstart.

 RETURNS
    1 if OK to clobber
    0 if not OK to overwrite

******************************************************************************/
int SDI_can_clobber(const char *name)
{
int res;
struct stat buf;
FILE *ff;

    res = stat(name, &buf);

    if (res < 0) {
        /* no such file, OK to try to create it */
	return(1);
    }

    ff = HI_OPEN(name, DFACC_RDWR);

    if (ff != NULL) {
        /* OK to open for write, so OK to clobber it */
        HI_CLOSE(ff);
	return(1);
    } 

    /* no permission to write, don't do the create */
    return(0);
}

/******************************************************************************
 NAME
	SDstart -- open a file

 DESCRIPTION
    Open a file by calling ncopen() or nccreate() and return a
    file ID to the file.

 RETURNS
    A file ID or FAIL

******************************************************************************/
int32
SDstart(const char *name,   /* IN: file name to open */
        int32       HDFmode /* IN: access mode to open file with */)
{
    CONSTR(FUNC, "SDstart");    /* for HGOTO_ERROR */
    intn    cdfid;
    int32   fid;
    intn    NCmode;
    NC     *handle = NULL;
    int32   ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDstart: I've been called\n");
#endif

    /* turn off annoying crash on error stuff */
    ncopts = 0;

    /* Perform global, one-time initialization */
    if (library_terminate == FALSE)
        if(SDIstart()==FAIL)
            HGOTO_ERROR(DFE_CANTINIT, FAIL);

    /* check access mode */
    if(HDFmode & DFACC_WRITE)
        NCmode = NC_RDWR;
    else
        NCmode = NC_NOWRITE;

    if(HDFmode & DFACC_CREATE) 
      { /* create file */
        if(!SDI_can_clobber(name)) 
            HGOTO_ERROR(DFE_DENIED, FAIL);
        cdfid = nccreate(name, NC_CLOBBER);
      } 
    else 
      { /* open the file */
        cdfid = ncopen(name, NCmode);
      }

    /* check if bad create/open */
    if(cdfid == -1) 
        HGOTO_ERROR(DFE_BADOPEN, FAIL);

    /* hmm.....*/
    handle = NC_check_id(cdfid);
    if(handle == NULL) 
        HGOTO_ERROR(DFE_ARGS, FAIL);

    /* set in 'define' mode? */
    handle->flags &= ~(NC_INDEF);

    /* create file id to return */
    fid = (((int32) cdfid) << 24) + (((int32) CDFTYPE) << 16) + cdfid;

    ret_value = fid;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;
} /* SDstart */


/******************************************************************************
 NAME
	SDend -- close a file

 DESCRIPTION
    Close the file

 RETURNS
    SUCCEED / FAIL

******************************************************************************/
intn
SDend(int32 id /* IN: file ID of file to close */)
{
    intn  cdfid;
    NC   *handle = NULL;
    intn  ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDend: I've been called\n");
#endif


    /* get id? */
    cdfid = (intn)id & 0xffff;

#ifndef SYNC_ON_EACC

    /* get the handle */
    handle = SDIhandle_from_id(id, CDFTYPE);
    if(handle == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    /* make sure we can write to the file */
    if(handle->flags & NC_RDWR) 
      {

        handle->xdrs->x_op = XDR_ENCODE;

        /* see if the meta-data needs to be updated */
        if(handle->flags & NC_HDIRTY) 
          {
            if(!xdr_cdf(handle->xdrs, &handle))
              {
                ret_value = FAIL;
                goto done;
              }

            handle->flags &= ~(NC_NDIRTY | NC_HDIRTY);
          } 
        else 
          {

            /* see if the numrecs info needs updating */
              if(handle->flags & NC_NDIRTY) 
                {
                    if(!xdr_numrecs(handle->xdrs, handle))
                      {
                          ret_value = FAIL;
                          goto done;
                      }

                    if (handle->file_type != HDF_FILE)
                        handle->flags &= ~(NC_NDIRTY);
                }
          }
      }

#endif /* SYNC_ON_EACC */

    /* call netCDF close */
    ret_value = ncclose(cdfid);

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;
} /* SDend */


/******************************************************************************
 NAME
	SDfileinfo -- get info about an open file

 DESCRIPTION
    Return the number of datasets and global attributes in the file.
    NOTE:  the number of datasets includes coordinate variable
    datasets.  The routine SDiscoordvar() should be used if the
    distinction is important.

 RETURNS
    SUCCEED / FAIL

******************************************************************************/
intn
SDfileinfo(int32  fid,     /* IN:  file ID */
           int32 *datasets,/* OUT: number of datasets in the file */
           int32 *attrs    /* OUT: number of global attributes */)
{
    NC   *handle = NULL;
    intn  ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDnumber: I've been called\n");
#endif


    /* check that fid is valid and get file structure */
    handle = SDIhandle_from_id(fid, CDFTYPE);
    if(handle == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

#ifdef SDDEBUG
    fprintf(stderr, "SDnumber: looked up handle as %d\n", handle);
#endif

    /* get number of data sets and global attributes */
    *(int32 *)datasets = ((handle->vars != NULL) ? handle->vars->count : 0);
    *(int32 *)attrs    = ((handle->attrs != NULL) ? handle->attrs->count : 0);


done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;
} /* SDfileinfo */


/******************************************************************************
 NAME
	SDselect -- get a dataset ID

 DESCRIPTION
    Return an id for the index-th data set in the file

    We've got 32bits, the current ID looks like:

    sdsID:
        
        32       24       16               0
        ------------------------------------
        |  fid   | id-type| position index |
        ------------------------------------
        
    fid is the netCDF based file ID (i.e. from ncopen).  ID type
    is SDSTYPE defined in mfhdf.h and position index is the 
    position in the file of this dataset.

    The position index is zero based

 RETURNS
   An ID to a dataset else FAIL
        
******************************************************************************/
int32
SDselect(int32 fid,  /* IN: file ID */
         int32 index /* IN: index of dataset to get ID for */)
{
    NC    *handle = NULL;
    int32  sdsid;         /* the id we're gonna build */
    int32  ret_value = FAIL;

#ifdef SDDEBUG
    fprintf(stderr, "SDselect: I've been called (index: %d) \n", index);
#endif


    /* check that fid is valid */
    handle = SDIhandle_from_id(fid, CDFTYPE);
    if(handle == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    /* check that a data set with this index exists */
    if(handle->vars == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    if(handle->vars->count < index)
      {
        ret_value = FAIL;
        goto done;
      }

    /* create SDS id to return */
    sdsid  = (((int32) fid & 0xffff) << 24) + (((int32) SDSTYPE) << 16) + index;

    ret_value = sdsid;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;
} /* SDselect */

/******************************************************************************
 NAME
	SDgetinfo -- get info about a dataset

 DESCRIPTION
    The user is repsonsible for allocating space to hold
    the dataset name.  It can be at most MAX_NC_NAME 
    characters in length.  NULL can be passed for the name
    if it is not required.

    dimsizes should be an array to hold the dimension sizes
    a dataset can have at most MAX_VAR_DIMS dimensions.

 RETURNS
        SUCCEED / FAIL

******************************************************************************/ 
intn
SDgetinfo(int32  sdsid,   /* IN:  dataset ID */
          char  *name,    /* OUT: name of the dataset */
          int32 *rank,    /* OUT: rank of the dataset */
          int32 *dimsizes,/* OUT: array of dimension siszes */ 
          int32 *nt,      /* OUT: number type of data */
          int32 *nattr    /* OUT: the number of local attributes */)
{
    CONSTR(FUNC, "SDgetinfo");    /* for HGOTO_ERROR */
    intn    i;
    NC     *handle = NULL;
    NC_var *var = NULL;
    intn    ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDgetinfo: I've been called\n");
#endif


    if( rank == NULL || dimsizes == NULL || nt == NULL || nattr == NULL)
	HGOTO_ERROR(DFE_ARGS, FAIL);

    handle = SDIhandle_from_id(sdsid, SDSTYPE);
    if(handle == NULL) 
      {
        ret_value = FAIL;
        goto done;
      }

    if(handle->vars == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    var = SDIget_var(handle, sdsid);
    if(var == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    if(name != NULL) 
      {
          HDmemcpy(name, var->name->values, var->name->len);
#if 0
          HDstrncpy(name, var->name->values, var->name->len);
#endif
          name[var->name->len] = '\0';
      }

    *rank  = var->assoc->count;
    if(!var->HDFtype)
        *nt    = hdf_map_type(var->type);
    else
        *nt    = var->HDFtype;

    *nattr = (var->attrs ? var->attrs->count : 0);

    for(i = 0; i < *rank; i++)
        dimsizes[i] = (int32) var->shape[i];

    if(dimsizes[0] == NC_UNLIMITED) 
      {
          if(handle->file_type == HDF_FILE)
              dimsizes[0] = var->numrecs;
          else
              dimsizes[0] = handle->numrecs;
      }

#ifdef SDDEBUG
    fprintf(stderr, "SDgetinfo: looked up handle as %d\n", handle);
#endif

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;
} /* SDgetinfo */


/******************************************************************************
 NAME
	SDreaddata -- read a hyperslab of data

 DESCRIPTION
    Read a hyperslab of data from the given variable.

 RETURNS
     SUCCEED / FAIL
 
******************************************************************************/
intn
SDreaddata(int32  sdsid,  /* IN:  dataset ID */
           int32 *start,  /* IN:  coords of starting point */
           int32 *stride, /* IN:  stride along each dimension */
           int32 *end,    /* IN:  number of values to read per dimension */
           void *  data    /* OUT: data buffer */)
{
    CONSTR(FUNC, "SDreaddata");    /* for HGOTO_ERROR */
    NC     *handle = NULL;
    NC_dim *dim = NULL;
    intn    varid;
    int32   status;
    comp_coder_t comp_type;
    comp_info c_info;
    uint32  comp_config;
    NC_var * tvar;
#ifdef BIG_LONGS
    long    Start[MAX_VAR_DIMS];
    long    End[MAX_VAR_DIMS];
    long    Stride[MAX_VAR_DIMS];
#else
    long    *Start = NULL;
    long    *End   = NULL;
    long    *Stride = NULL;
#endif
    intn    ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDreaddata: I've been called\n");
#endif

    
    if((start == NULL) || (end == NULL) || (data == NULL))
      {
        ret_value = FAIL;
        goto done;
      }

    handle = SDIhandle_from_id(sdsid, SDSTYPE);
    if(handle == NULL) 
      {
          handle = SDIhandle_from_id(sdsid, DIMTYPE);
          if(handle == NULL) 
            {
              ret_value = FAIL;
              goto done;
            }
          dim = SDIget_dim(handle, sdsid);
      }

    if(handle->vars == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    /* Check compression method is enabled */
    tvar = SDIget_var(handle, sdsid);
    if(tvar == NULL)
     {
            ret_value = FAIL;
            goto done;
     }
    status = HCPgetcompress(handle->hdf_file, tvar->data_tag, tvar->data_ref, 
		&comp_type, &c_info);

    if (status != FAIL) {
	    HCget_config_info( comp_type , &comp_config);
	    if ((comp_config & COMP_DECODER_ENABLED|COMP_ENCODER_ENABLED) == 0) {
		/* coder not present?? */
		    HGOTO_ERROR(DFE_NOENCODER, FAIL);
	    }
	    if ((comp_config & COMP_DECODER_ENABLED) == 0) {
		/* decoder not present?? */
		HGOTO_ERROR(DFE_BADCODER, FAIL);
	    }
    }

    /* get ready to read */
    handle->xdrs->x_op = XDR_DECODE ;
   
    /* 
     * figure out the index of the variable to read from
     * the user might have passed us a dimension, in which
     * case we want to reade from its coordinate variable
     */
    if(dim) 
      {
          varid = SDIgetcoordvar(handle, dim, (int32)(sdsid & 0xffff), (int32) 0);
      } 
    else 
      {
          /* oops, how do we know this ? */
          varid = (intn)sdsid & 0xffff;
      }

    /*
     * In general, (long) == int32 
     * In cases where it doesn't we need to convert
     */
#ifdef BIG_LONGS
    {
        int i;
        NC_var * var = SDIget_var(handle, sdsid);
        if(var == NULL)
          {
            ret_value = FAIL;
            goto done;
          }
        
        for(i = 0; i < var->assoc->count; i++) 
          {
            Start[i]  = (long) start[i];
            End[i]    = (long) end[i];
            if(stride) 
                Stride[i] = (long) stride[i];
          }
    }

#else

    Start  = (long *)start;
    End    = (long *)end;
    Stride = (long *)stride;

#endif

    /* call the readg routines if a stride is given */
    if(stride == NULL)
        status = NCvario(handle, varid, Start, End, (Void *)data);
    else
        status = NCgenio(handle, varid, Start, End, Stride, NULL, (Void *)data);

    if(status == -1)
        ret_value = FAIL;
    else
        ret_value = SUCCEED;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;
} /* SDreaddata */


/******************************************************************************
 NAME
	SDnametoindex -- map a dataset name to an index

 DESCRIPTION
    Given a data set name return the index (not ID) of the 
    first data set whose name matches.

    There can be multiple data sets with the same name.  In 
    such a case we only ever return the index of the first
    such dataset.
    Wildcards are not supported

 RETURNS
        Index of a dataset or FAIL

******************************************************************************/
int32
SDnametoindex(int32 fid,  /* IN: file ID */
              const char *name  /* IN: name of dataset to search for */)
{
    intn     ii;
    intn     len;
    NC      *handle = NULL;
    NC_var **dp = NULL;
    int32    ret_value = FAIL;

#ifdef SDDEBUG
    fprintf(stderr, "SDnametoindex: I've been called\n");
#endif


    /* check that fid is valid */
    handle = SDIhandle_from_id(fid, CDFTYPE);
    if(handle == NULL) 
      {
        ret_value = FAIL;
        goto done;
      }

    if(handle->vars == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    len = HDstrlen(name) ;
    dp = (NC_var**)handle->vars->values ;
    for(ii = 0 ; ii < handle->vars->count ; ii++, dp++) 
      {
        if( len == (*dp)->name->len 
            && HDstrncmp(name, (*dp)->name->values, len) == 0) 
          {
            ret_value = ii;
            goto done;
          }
      }

    ret_value = FAIL;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */

    return ret_value;    
} /* SDnametoindex */



/******************************************************************************
 NAME
	SDgetrange -- simulate a call to DFSDgetrange

 DESCRIPTION
    If a "valid_range" attribute is provided return its 
    values in pmax and pmin.  Else if both a "valid max" 
    AND a "vaild min" exist return their values in pmax and pmin.

    Arrgghh, in HDF it was assumed that the max and min values 
    were of the same data type as the rest of the data.  So 
    the user would know the amount of storage to send to get 
    back the max and min values.  This is not the case with 
    netCDF.  This routine will only work if they are already 
    the same number types.

 RETURNS
    On error or missing attributes return FAIL else SUCCEED.

******************************************************************************/
intn
SDgetrange(int32 sdsid, /* IN:  dataset ID */
           void * pmax,  /* OUT: valid max */
           void * pmin   /* OUT: valid min */)
{
    NC       *handle = NULL;
    NC_var   *var = NULL;
    NC_attr **attr = NULL;
    NC_attr **attr1 = NULL;
    NC_attr **attr2 = NULL;
    NC_array *array = NULL;
    intn      ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDgetrange: I've been called\n");
#endif


    handle = SDIhandle_from_id(sdsid, SDSTYPE);
    if(handle == NULL) 
      {
        ret_value = FAIL;
        goto done;
      }

    var = SDIget_var(handle, sdsid);
    if(var == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    attr = (NC_attr **) NC_findattr(&(var->attrs), _HDF_ValidRange);
    if((attr != NULL) && ((*attr)->data->type == var->type)) 
      {
        /* BUG: this may be a pointer to a pointer */
        array = (NC_array *) (*attr)->data;
        HDmemcpy(pmin, array->values, array->szof);
        HDmemcpy(pmax, array->values + array->szof, array->szof);
      } 
    else 
      {
        attr1 = (NC_attr **) NC_findattr(&(var->attrs), "valid_max");
        attr2 = (NC_attr **) NC_findattr(&(var->attrs), "valid_min");
        
        if((attr1 == NULL) || (attr2 == NULL)) 
          {
#ifdef SDDEBUG
            fprintf(stderr, "No dice on range info (missing at least one)\n");
#endif   
            ret_value = FAIL;
            goto done;
          }

        if(((*attr1)->HDFtype != var->HDFtype) 
           || ((*attr2)->HDFtype != var->HDFtype)) 
          {
#ifdef SDDEBUG
            fprintf(stderr, "No dice on range info (wrong types)\n");
#endif   
            ret_value = FAIL;
            goto done;
          }

        NC_copy_arrayvals((char *)pmax, (*attr1)->data) ;
        NC_copy_arrayvals((char *)pmin, (*attr2)->data) ;
      }

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;
} /* SDgetrange */


/* -------------------------- CREATION INTERFACE -------------------------- */
/*

        --- create a new data set ---
sdsid   = SDcreate(fid, name, numbertype, rank, dimsizes);

        --- associate a name with a dimension.  If a prev sdsid is ---
        --- provided then it is assumed that the current dimension is the ---
        --- same as the dimension with the same name of the previous sds ---
status  = SDsetdim(sdsid, dimnumber, dimname, [prev sdsid] );

        --- note that it will be possible to store values for a ---
        --- dimension without having to name it ---
status  = SDsetdimvalues(sdsid, dimnumber, numbertype, count, data);

        --- set the units and format strings ---
status  = SDsetdimstrs(sdsid, dimnumber, unitstr, formatstr);

        --- we will need an SDendaccess() so that we know when it ---
        --- is safe to write the information out in this case ---
status  = SDendaccess(sdsid);

        --- set fill mode for a file open for write
cur_mode = SDsetfillmode(fid, fillmode);

        --- set dimval backward compatible  for write
status  = SDsetdimval_comp(dimid, compt_mode);
 
*/

/******************************************************************************
 NAME
	SDcreate -- create a new dataset

 DESCRIPTION
    Simulate a call to ncvardef without having to be in 
    define mode.  name can be at most MAX_NC_NAME
    characters.  Rank can be at most MAX_VAR_DIMS

    It looks like for the call to NC_new_var() we need to 
    have dimension IDs already.  So I guess we should just 
    create the fake dimensions now and when optional 
    information comes in (i.e.  name, tying to other 
    dimensions) we'll go in and modify the structure in place.
    This is gonna be heinous.  Please do not attempt this at home
    See SDselect() for a discussion on how SDS IDs are set up.

 RETURNS
    An ID to the new dataset else FAIL

******************************************************************************/
int32
SDcreate(int32  fid,      /* IN: file ID */
         const char  *name, /* IN: dataset name */
         int32  nt,       /* IN: dataset number type */
         int32  rank,     /* IN: rank of dataset */
         int32 *dimsizes  /* IN: array of dimension sizes */)
{
    intn     i;
    NC      *handle = NULL;
    NC_var  *var = NULL;
    NC_dim  *newdim = NULL;
    int32    sdsid;
    nc_type  nctype;
    char     dimname[MAX_NC_NAME];
    intn     num;
    intn    *dims = NULL;
    intn     is_ragged;
    int32    ret_value = FAIL;

#ifdef SDDEBUG
    fprintf(stderr, "SDcreate: I've been called\n");
#endif


    /* check that fid is valid */
    handle = SDIhandle_from_id(fid, CDFTYPE);
    if(handle == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    /* fudge the name since its optional */
    if((name == NULL) || (name[0] == ' ') || (name[0] == '\0'))
        name = "DataSet";

    /* check if its a ragged array.
       Why is this code still here? -GV */
    if((rank > 1) && dimsizes[rank - 1] == SD_RAGGED) 
      {
#ifdef DEBUG
        printf("YOW!  We have a ragged array kids: %s\n", name);
#endif
        rank--;
        is_ragged = TRUE;
      } 
    else 
      {
        is_ragged = FALSE;        
      }

    /* make fake dimensions which may or may not be over-ridden later */
    dims = (intn *) HDmalloc(rank * sizeof(intn));
    if(dims == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    if(rank > MAX_VAR_DIMS)
      {
        ret_value = FAIL;
        goto done;
      }

    for(i = 0; i < rank; i++) 
      {

          num = (handle->dims ? handle->dims->count : 0);
          sprintf(dimname, "fakeDim%d", num);

          newdim = (NC_dim *) NC_new_dim(dimname, dimsizes[i]);
          if(newdim == NULL) 
            {
                ret_value = FAIL;
                goto done;
            }

          if(handle->dims == NULL) 
            { /* first time */
                handle->dims = NC_new_array(NC_DIMENSION,(unsigned)1, (Void *)&newdim);
                if(handle->dims == NULL)
                  {
                      ret_value = FAIL;
                      goto done;
                  }
            } 
          else 
            {
                if( NC_incr_array(handle->dims, (Void *)&newdim) == NULL)
                  {
                      ret_value = FAIL;
                      goto done;
                  }
            }

          dims[i] = (intn) handle->dims->count -1;

      } /* end for 'i < rank' */

    /* create the actual variable */
    if ((nctype = hdf_unmap_type((int)nt)) == FAIL)
      {
#ifdef SDDEBUG
          /* replace it with NCAdvice or HERROR? */
          fprintf(stderr "SDcreate: hdf_unmap_type failed for %d\n", nt);
#endif
          ret_value = FAIL;
          goto done;
      }

    var = (NC_var *) NC_new_var(name, nctype, (int)rank, dims);
    if(var == NULL)
      {
          ret_value = FAIL;
          goto done;
      }
    
    /* Set the "newly created" & "set length" flags for use in SDwritedata */
    var->created=TRUE;
    var->set_length=FALSE;

    /* NC_new_var strips off "nativeness" add it back in if appropriate */
    var->HDFtype = nt;
    if (FAIL == (var->HDFsize = DFKNTsize(nt)))
      {
          ret_value    = FAIL;
          goto done;
      } 

    var->cdf     = handle; /* set cdf before calling NC_var_shape */
    /* get a new NDG ref for this sucker */
#ifdef NOT_YET
    var->ndg_ref = Htagnewref(handle->hdf_file,DFTAG_NDG);
#else /* NOT_YET */
    var->ndg_ref = Hnewref(handle->hdf_file);
#endif /* NOT_YET */

    /* set ragged status. Why is this still here -GV */
    var->is_ragged = is_ragged;
    
    /* no ragged array info stored yet */
    if(var->is_ragged) 
      {
        var->rag_list = NULL;
        var->rag_fill = 0;
      }

    /* add it to the handle */
    if(handle->vars == NULL) 
      { /* first time */
          handle->vars = NC_new_array(NC_VARIABLE,(unsigned)1, (Void *)&var);
          if(handle->vars == NULL)
            {
                ret_value = FAIL;
                goto done;
            }
      } 
    else 
      {
          if(handle->vars->count >= MAX_NC_VARS) 
            {
                ret_value = FAIL;
                goto done;
            } 
          else 
            {
                if( NC_incr_array(handle->vars, (Void *)&var) == NULL)
                  {
                      ret_value = FAIL;
                      goto done;
                  } 
            }
      }

    /* compute all of the shape information */
    if(NC_var_shape(var, handle->dims) == -1)
      {
          ret_value = FAIL;
          goto done;
      } 

    /* create a handle we can give back to the user */
    sdsid  = (((int32) fid) << 24) + (((int32) SDSTYPE) << 16);
    sdsid += handle->vars->count -1;

    /* make sure it gets reflected in the file */
    handle->flags |= NC_HDIRTY;

    /* free dims */
    HDfree(dims);

    ret_value = sdsid;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */

    return ret_value;
} /* SDcreate */


/******************************************************************************
 NAME
	SDgetdimid -- get a dimension ID

 DESCRIPTION
    Given an sdsid and a dimension number return a 
    dimid.  Index is a ZERO based quantity

    The dimID looks similar to the sdsID except DIMTYPE 
    is substituted for SDSTYPE as the id-type:

    dimID:
        
        32       24       16               0
        ------------------------------------
        |  fid   | id-type| position index |
        ------------------------------------

 RETURNS
        An ID to the dimension else FAIL
        
******************************************************************************/
int32
SDgetdimid(int32 sdsid,  /* IN: dataset ID */
           intn  number  /* IN: index of dimension */)
{
    NC     *handle = NULL;
    NC_var *var = NULL;
    int32   id;
    int32   dimindex;
    int32   ret_value = FAIL;

#ifdef SDDEBUG
    fprintf(stderr, "SDgetdimid: I've been called\n");
#endif


    /* get the handle */
    handle = SDIhandle_from_id(sdsid, SDSTYPE);
    if(handle == NULL) 
      {
        ret_value = FAIL;
        goto done;
      }

    /* get the variable */
    var = SDIget_var(handle, sdsid);
    if(var == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    /* check if enough / too many dims */
    if((var->assoc == NULL) || (var->assoc->count < number))
      {
        ret_value = FAIL;
        goto done;
      }

    /* get the dim number out of the assoc array */
    if (var->assoc->values == NULL)
      {
        ret_value = FAIL;
        goto done;
      }
    dimindex = var->assoc->values[number];

    /* build the dim id */
    id  = (sdsid & 0xff000000) + (((int32) DIMTYPE) << 16) + dimindex;

    ret_value = id;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */

    return ret_value;
} /* SDgetdimid */


/******************************************************************************
 NAME
	SDsetdimname -- give a name to a dimension

 DESCRIPTION
    Set the name of a dimension -- at most MAX_NC_NAME characters.  
    If this name is already in use we should point to the 
    existing dimension with that name.  If the sizes are 
    different return an error.  If this dimension already has
    a name throw it out and use the new one.

 RETURNS
    SUCCEED / FAIL

******************************************************************************/
intn
SDsetdimname(int32  id,   /* IN: dataset ID */
             const char  *name  /* IN: dimension name */)
{
    NC         *handle = NULL;
    NC_dim     *dim = NULL;
    NC_dim    **dp = NULL;
    NC_string  *old = NULL;
    NC_string  *new = NULL;
    NC_array  **ap = NULL;
    int32       len;
    int32       ii;
    intn        ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDsetdimname: I've been called\n");
#endif


    /* get the handle */
    handle = SDIhandle_from_id(id, DIMTYPE);
    if(handle == NULL) 
      {
        ret_value = FAIL;
        goto done;
      }

    /* get the dimension structure */
    dim = SDIget_dim(handle, id);
    if(dim == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    /* check for name in use */
    len = HDstrlen(name) ;
    dp = (NC_dim**)handle->dims->values ;
    for(ii = 0 ; ii < handle->dims->count ; ii++, dp++) 
      {
          if( len == (*dp)->name->len 
              && HDstrncmp(name, (*dp)->name->values, (size_t)len) == 0) 
            {
                if(dim != (*dp)) 
                  {
                      /* a dimension with this name already exists */
                      /* so change to point to it */
                      if(dim->size != (*dp)->size)
                        {
                            ret_value = FAIL;
                            goto done;
                        }

                      ap = (NC_array **) handle->dims->values;
                      ap += id & 0xffff;
                      NC_free_dim(dim);
                      (*dp)->count += 1;
                      (*ap) = (NC_array *) (*dp);
                      ret_value = SUCCEED;
                      goto done;
                  }
            }
      }
    
    /* throw out the old name if it exists and create a new one */
    old = dim->name;
    new = NC_new_string((unsigned)HDstrlen(name),name);
    if(new == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    dim->name = new;
    NC_free_string(old);
    
    /* make sure it gets reflected in the file */
    handle->flags |= NC_HDIRTY;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */

    return ret_value;
} /* SDsetdimname */


/******************************************************************************
 NAME
	SDendaccess -- close a sds ID

 DESCRIPTION
    Close down this access ID to a data object

    Usually, this will do nothing.  However, if the meta-data 
    has changed and SYNC_ON_EACC is defiend flush it all out 
    to disk.

 RETURNS
        SUCCEED / FAIL          

******************************************************************************/
intn
SDendaccess(int32 id /* IN: dataset ID */)
{
    NC     *handle;
    int32   ret_value = SUCCEED;
	
#ifdef SDDEBUG
    fprintf(stderr, "SDendaccess: I've been called\n");
#endif


    /* get the handle */
    handle = SDIhandle_from_id(id, SDSTYPE);
    if(handle == NULL) 
      {
        ret_value = FAIL;
        goto done;
      }

#ifdef SYNC_ON_EACC

    /* make sure we can write to the file */
    if(handle->flags & NC_RDWR) 
      {
          handle->xdrs->x_op = XDR_ENCODE;

          /* see if the meta-data needs to be updated */
          if(handle->flags & NC_HDIRTY) 
            {
                if(!xdr_cdf(handle->xdrs, &handle) )
                  {
                      ret_value = FAIL;
                      goto done;
                  }

                handle->flags &= ~(NC_NDIRTY | NC_HDIRTY);
            } 
          else 
            {
                /* see if the numrecs info needs updating */
                if(handle->flags & NC_NDIRTY) 
                  {
                      if(!xdr_numrecs(handle->xdrs, handle) )
                        {
                            ret_value = FAIL;
                            goto done;
                        }

                      handle->flags &= ~(NC_NDIRTY);
                  }
            }
      }

#else 

    /* free the AID */
    ret_value = SDIfreevarAID(handle, id & 0xffff);
     
#endif /* SYNC_ON_EACC */
  
done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDendaccess */

/******************************************************************************
 NAME
	SDIputattr -- put an attribute in an attribute list

 DESCRIPTION
    Common code for adding an attribute to an attribute list.
    The list gets created if it had previously been empty

 RETURNS
     SUCCEED / FAIL

******************************************************************************/
PRIVATE intn
SDIputattr(NC_array **ap,   /* IN/OUT: attribute list */
           const char *name, /* IN:     attribute name */
           int32      nt,   /* IN:     attribute number type */
           intn       count,/* IN:     number of attribute values */
           const void *      data  /* IN:     attribute values */)
{
    NC_attr *attr = NULL;
    NC_attr **atp = NULL;
    NC_attr *old = NULL;
    nc_type  type;   /* unmap -- HDF type to NC type */
    intn     ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDIputattr: I've been called\n");
#endif
    
    if ((type = hdf_unmap_type((int)nt)) == FAIL)
      {
#ifdef SDDEBUG
          /* replace it with NCAdvice or HERROR? */
          fprintf(stderr "SDIputattr: hdf_unmap_type failed for %d\n", nt);
#endif
          ret_value = FAIL;
          goto done;
      }

    if(*ap == NULL) 
      { /* first time */
          attr = (NC_attr *) NC_new_attr(name,type,(unsigned)count,data) ;
          if(attr == NULL)
            {
                ret_value = FAIL;
                goto done;
            }

          attr->HDFtype = nt; /* Add HDFtype  */
          *ap = NC_new_array(NC_ATTRIBUTE,(unsigned)1, (Void*)&attr) ;
          if(*ap == NULL)
            {
                ret_value = FAIL;
                goto done;
            }
      }
    else
      {
          if((atp = NC_findattr(ap, name)) != NULL) 
            { /* name in use */
                old = *atp ;
                *atp = (NC_attr *) NC_new_attr(name,type,(unsigned)count,data);
                if(*atp == NULL) 
                  {
                      *atp = old;
                      ret_value = FAIL;
                      goto done;
                  }
                (*atp)->HDFtype = nt; /* Add HDFtype  */
                NC_free_attr(old);
            }
          else   
            {
                if((*ap)->count >= MAX_NC_ATTRS) 
                  {  /* Too many */
                      ret_value = FAIL;
                      goto done;
                  }

                /* just add it */
                attr = (NC_attr *) NC_new_attr(name,type,(unsigned)count,data);
                attr->HDFtype = nt; /* Add HDFtype  */
                if(attr == NULL)
                  {
                      ret_value = FAIL;
                      goto done;
                  }

                if(NC_incr_array((*ap), (Void *)&attr) == NULL)
                  {
                      ret_value = FAIL;
                      goto done;
                  }
            }
      }
        
done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */

    return ret_value;    
} /* SDIputattr */


/******************************************************************************
 NAME
	SDsetrange -- simulate a call to DFSDsetrange

 DESCRIPTION
    Store range info for this variable in the valid_range 
    attribute.  If that attribute already exists overwrite 
    the current values.  It is assumed that the values are 
    the same type as the data set.

    It is up to the user to decide what is meant by the
    "valid" max and min.

 RETURNS
    On error FAIL else SUCCEED.

******************************************************************************/
intn
SDsetrange(int32 sdsid, /* IN: dataset ID */
           void * pmax,  /* IN: valid max */
           void * pmin   /* IN: valid min */)
{
    NC      *handle = NULL;
    NC_var  *var = NULL;
    uint8    data[80];
    intn     sz;
    intn     ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDsetrange: I've been called\n");
#endif


    handle = SDIhandle_from_id(sdsid, SDSTYPE);
    if(handle == NULL) 
      {
          ret_value = FAIL;
          goto done;
      }

    var = SDIget_var(handle, sdsid);
    if(var == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    if((pmax == NULL) || (pmin == NULL))
      {
          ret_value = FAIL;
          goto done;
      }

    /* move data values over */
    if (FAIL == (sz = DFKNTsize(var->HDFtype | DFNT_NATIVE)))
      {
          ret_value    = FAIL;
          goto done;
      } 

    HDmemcpy(data, pmin, sz);
    HDmemcpy(data + sz, pmax, sz);

    /* call common code */
    if(SDIputattr(&var->attrs, _HDF_ValidRange, var->HDFtype, (intn) 2, data) == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }
    
    /* make sure it gets reflected in the file */
    handle->flags |= NC_HDIRTY;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDsetrange */


/******************************************************************************
 NAME
	SDIapfromid -- get the attribute list

 DESCRIPTION
    Given a ID figure out what the handle and attribute 
    list pointer are for that object.

 RETURNS
     On error FAIL else SUCCEED.

******************************************************************************/
PRIVATE intn
SDIapfromid(int32       id,      /* IN:  object ID */
            NC        **handlep, /* IN:  handle for this file */
            NC_array ***app      /* OUT: attribute list */)
{
    NC     *handle = NULL;
    NC_var *var = NULL;
    NC_dim *dim = NULL;
    int32   varid;
    intn    ret_value = SUCCEED;

    /* see if its a variable ID */
    handle = SDIhandle_from_id(id, SDSTYPE);
    if(handle != NULL) 
      { 
          /* find the variable */
          var = SDIget_var(handle, id);
          if(var == NULL)
            {
                ret_value = FAIL;
                goto done;
            }

          (*app) = &(var->attrs);
          (*handlep) = handle;
          ret_value = SUCCEED;
          goto done;
      } 

    /* see if its a file ID */
    handle = SDIhandle_from_id(id, CDFTYPE);
    if(handle != NULL) 
      {
          (*app) = &(handle->attrs);
          (*handlep) = handle;
          ret_value = SUCCEED;
          goto done;
      }

    /* see if its a dimension ID */
    handle = SDIhandle_from_id(id, DIMTYPE);
    if(handle != NULL) 
      {
          /* find the dimension */
          dim = SDIget_dim(handle, id);
          if(dim == NULL)
            {
                ret_value = FAIL;
                goto done;
            }

          /* get index of coordinate variable */
          varid = SDIgetcoordvar(handle, dim, (int32)(id & 0xffff), (int32) 0);

          /* get the variable object */
          var = NC_hlookupvar(handle, varid);
          if(var == NULL)
            {
                ret_value = FAIL;
                goto done;
            }


          (*app) = &(var->attrs);
          (*handlep) = handle;
          ret_value = SUCCEED;
          goto done;
      }

    ret_value = FAIL;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */

    return ret_value;    
} /* SDIapfromid */


/******************************************************************************
 NAME
   SDsetattr -- user level function to create and set an attribute

 DESCRIPTION
   Given an ID and an attribute defintion attach the atrribute 
   to the thing represented by the ID.  For starters, the valid 
   IDs could be variable, file or dimesnion IDs

 RETURNS
   On error FAIL else SUCCEED.

******************************************************************************/
intn
SDsetattr(int32 id,    /* IN: object ID */
          const char *name,  /* IN: attribute name */
          int32 nt,    /* IN: attribute number type */
          int32 count, /* IN: number of attribute values */
          const void * data   /* IN: attribute values */)
{
    NC_array **ap = NULL;
    NC        *handle = NULL;
    intn       sz;
    intn       ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDsetattr: I've been called\n");
#endif


    /* sanity check args */
    if(name == NULL) 
      {
          ret_value = FAIL;
          goto done;
      }

    /* This release doesn't support native number types for attr  */
    if (nt & DFNT_NATIVE) 
      {
          ret_value = FAIL;
          goto done;
      }

    /* Make sure that count is less than MAX_ORDER(Vdata)
           and total size is less than MAX_FIELD_SIZE(Vdata) */
    if (FAIL == (sz = DFKNTsize(nt)))
      {
          ret_value = FAIL;
          goto done;
      }

    if ((count > MAX_ORDER) ||
        ((count * sz) > MAX_FIELD_SIZE))
      {
          ret_value = FAIL;
          goto done;
      }

    /* determine what type of ID we've been given */
    if(SDIapfromid(id, &handle, &ap) == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* still no handle ? */
    if(handle == NULL)
      {
          ret_value = FAIL;
          goto done;
      }
    
    /* hand over to SDIputattr */
         
    if(SDIputattr(ap, name, nt, count, data) == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }
    
    /* make sure it gets reflected in the file */
    handle->flags |= NC_HDIRTY;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;        
} /* SDsetattr */


/******************************************************************************
 NAME
	SDattrinfo -- get info about an attribute

 DESCRIPTION
    Inquire about an attribute.  Attribute indexes are zero based.

    Given the ID of the attribute's parent and the attribute's 
    index return the number type, name and count of the attribute 
    so the user knows how much space to provide to read it

 RETURNS
        On error FAIL else SUCCEED.
******************************************************************************/
intn
SDattrinfo(int32  id,    /* IN:  object ID */
           int32  index, /* IN:  attribute index */
           char  *name,  /* OUT: attribute name */
           int32 *nt,    /* OUT: attribute number type */
           int32 *count  /* OUT: number of attribute values */)
{
    CONSTR(FUNC, "SDattrinfo");    /* for HGOTO_ERROR */
    NC_array  *ap = NULL;
    NC_array **app = NULL;
    NC_attr  **atp = NULL;
    NC        *handle = NULL;
    intn       ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDattrinfo: I've been called\n");
#endif


    /* sanity check args */
    if((name == NULL) || (nt == NULL) || (count == NULL))
        HGOTO_ERROR(DFE_ARGS, FAIL);

    /* determine what type of ID we've been given */
    if(SDIapfromid(id, &handle, &app) == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }

    ap = (*app);
    if((ap == NULL) || (index >= ap->count))
      {
          ret_value = FAIL;
          goto done;
      }

    /* 
     * ap is the proper attribute list now look up something with this
     *  name
     */
    atp = (NC_attr **) ((char *)ap->values + index * ap->szof);
    if(*atp == NULL) 
      {
          ret_value = FAIL;
          goto done;
      }

    /* move the information over */
    if(name != NULL) 
      {
#if 0
          HDstrncpy(name, (*atp)->name->values, (*atp)->name->len);
#endif
          HDmemcpy(name, (*atp)->name->values, (*atp)->name->len);
          name[(*atp)->name->len] = '\0';
      }

    *count = (*atp)->data->count;
    *nt = (*atp)->HDFtype;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDattrinfo */


/******************************************************************************
 NAME
	SDreadattr -- read an attribute's values

 DESCRIPTION
    Read the actual contents of the given attribute
    Assume that the user has called SDinqattr() and so 
    has allocated sufficient space

 RETURNS
    On error FAIL else SUCCEED.

******************************************************************************/
intn
SDreadattr(int32 id,    /* IN:  object ID */
           int32 index, /* IN:  attribute index */
           void * buf    /* OUT: data buffer  */)
{
    CONSTR(FUNC, "SDreadattr");    /* for HGOTO_ERROR */
    NC_array  *ap = NULL;
    NC_array **app = NULL;
    NC_attr  **atp = NULL;
    NC        *handle = NULL;
    intn       ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDreadattr: I've been called\n");
#endif


    /* sanity check args */
    if(buf == NULL)
        HGOTO_ERROR(DFE_ARGS, FAIL);

    /* determine what type of ID we've been given */
    if(SDIapfromid(id, &handle, &app) == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }

    ap = (*app);
    if((ap == NULL) || (index >= ap->count))
      {
          ret_value = FAIL;
          goto done;
      }

    /* 
     * ap is the proper attribute list now look up something with this
     *  index
     */
    atp = (NC_attr **) ((char *)ap->values + index * ap->szof);
    if(*atp == NULL) 
      {
          ret_value = FAIL;
          goto done;
      }

    /* move the information over */
    HDmemcpy(buf, (*atp)->data->values, (*atp)->data->count * (*atp)->data->szof);

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDreadattr */


/******************************************************************************
 NAME
	SDwritedata -- write a hyperslab of data

 DESCRIPTION
    Write out a chunk o data.  Except for the line setting the 
    XDR op-code this is exactly the same as SDreaddata().  The 
    two routines should really be combined at some point

 RETURNS
    SUCCEED / FAIL
 
******************************************************************************/
intn
SDwritedata(int32  sdsid,  /* IN: dataset ID */
            int32 *start,  /* IN: coords of starting point */
            int32 *stride, /* IN: stride along each dimension */
            int32 *end,    /* IN: number of values to write per dimension */
            void *  data    /* IN: data buffer */)
{
    CONSTR(FUNC, "SDwritedata");    /* for HGOTO_ERROR */
    intn    varid;
    int32   status;
    comp_coder_t comp_type;
    comp_info c_info;
    uint32  comp_config;
    NC_var *tvar;
    NC     *handle = NULL;
    NC_dim *dim = NULL;
#ifdef BIG_LONGS
    long    Start[MAX_VAR_DIMS];
    long    End[MAX_VAR_DIMS];
    long    Stride[MAX_VAR_DIMS];
#else
    long   *Start = NULL;
    long   *End = NULL;
    long   *Stride = NULL;
#endif
    intn    no_strides = 0;
    intn    ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDwritedata: I've been called\n");
#endif


    if((start == NULL) || (end == NULL) || (data == NULL))
        HGOTO_ERROR(DFE_ARGS, FAIL);

    handle = SDIhandle_from_id(sdsid, SDSTYPE);
    if(handle == NULL) 
      {
        handle = SDIhandle_from_id(sdsid, DIMTYPE);
        if(handle == NULL) 
          {
              ret_value = FAIL;
              goto done;
          }

        dim = SDIget_dim(handle, sdsid);
      }

#ifdef QAK
    fprintf(stderr, "SDwritedata: check 1.0\n");
#endif
    if(handle->vars == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

      tvar = SDIget_var(handle, sdsid);

      if(tvar == NULL)
         {
              ret_value = FAIL;
              goto done;
      }

    /* disallow writing SDS with rank = 0 - BMR, bug #1045 */
    if(tvar->shape == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    /* Check compression method is enabled */
    status = HCPgetcompress(handle->hdf_file, tvar->data_tag, tvar->data_ref, 
		&comp_type, &c_info);

    if (status != FAIL) {
	    HCget_config_info( comp_type , &comp_config);
	    if ((comp_config & COMP_DECODER_ENABLED|COMP_ENCODER_ENABLED) == 0) {
		/* coder not present?? */
		    HGOTO_ERROR(DFE_NOENCODER, FAIL);
	    }
	    if ((comp_config & COMP_ENCODER_ENABLED) == 0) {
		/* encoder not present?? */
		HGOTO_ERROR(DFE_BADCODER, FAIL);
	    }
    }

    /* get ready to write */
    handle->xdrs->x_op = XDR_ENCODE;
    
    /* 
     * figure out the index of the variable to write to
     * the user might have passed us a dimension, in which
     * case we want to write to its coordinate variable
     */
    if(dim) 
      {
        varid = SDIgetcoordvar(handle, dim, (int32)(sdsid & 0xffff), (int32) 0);
      } 
    else 
      {
        /* oops, how do we know this ? */
        varid = (intn)sdsid & 0xffff;
      }

#ifdef QAK
    fprintf(stderr, "SDwritedata: check 2.0\n");
#endif
    /* Check for strides all set to '1', so it acts like NULL was passed */
    if(stride!=NULL)
      {
        int i;
        NC_var *var = SDIget_var(handle, sdsid);

        if(var == NULL)
          {
              ret_value = FAIL;
              goto done;
          }

        no_strides=1;
        /* if the stride for any dim. is not '1', real stride processing has to occur */
        for(i = 0; i < var->assoc->count; i++) 
          {
              if(stride[i]!=1)    
                  no_strides=0;
          }
      } /* end if */

    /*
     * In general, (long) == int32 
     * In cases where it doesn't we need to convert
     */
#ifdef BIG_LONGS

    {
        int i;
        NC_var *var = SDIget_var(handle, sdsid);

        if(var == NULL)
          {
              ret_value = FAIL;
              goto done;
          }
        
        for(i = 0; i < var->assoc->count; i++) 
          {
            Start[i]  = (long) start[i];
            End[i]    = (long) end[i];
            if(stride) 
                Stride[i] = (long) stride[i];
          }
    }

#else

    Start  = (long *)start;
    End    = (long *)end;
    Stride = (long *)stride;

#endif

    /* Check if this data is being written out to a newly created dataset */
    {
        NC_var *var = SDIget_var(handle, sdsid);

        if(var->created) {
            if(!IS_RECVAR(var) && (handle->flags & NC_NOFILL) ) {
              var->set_length=TRUE;
            } /* end if */
            var->created=FALSE;
        } /* end if */
    }

    /* call the writeg routines if a stride is given */
    if(stride == NULL || no_strides==1)
        status = NCvario(handle, varid, Start, End, (Void *)data);
    else
        status = NCgenio(handle, varid, Start, End, Stride, NULL, data);

    if(status == -1)
        ret_value = FAIL;
    else
        ret_value = SUCCEED;
            
done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDwritedata */


/******************************************************************************
 NAME
	SDsetdatastrs -- set "data strings"

 DESCRIPTION
    Store information about the 'label', 'units', 'format' and 
    'cordsys' attributes of a dataset.  All of the values 
    are optional.  If no string is desired NULL should be passed 
    in its place.  

 RETURNS
    SUCCEED / FAIL 

******************************************************************************/
intn
SDsetdatastrs(int32 sdsid, /* IN: dataset ID */
              const char *l,     /* IN: label string ("long_name") */
              const char *u,     /* IN: units string ("units") */
              const char *f,     /* IN: format string ("format") */
              const char *c      /* IN: coordsys string ("coordsys") */)
{
    NC     *handle = NULL;
    NC_var *var = NULL;
    intn    ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDsetdatastrs: I've been called\n");
#endif
    

    handle = SDIhandle_from_id(sdsid, SDSTYPE);
    if(handle == NULL) 
      {
          ret_value = FAIL;
          goto done;
      }

    if(handle->vars == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    var = SDIget_var(handle, sdsid);
    if(var == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    if(l && l[0] != '\0') 
      {
        if(SDIputattr(&var->attrs, _HDF_LongName, DFNT_CHAR, 
                      (intn) HDstrlen(l), l) == FAIL)
          {
              ret_value = FAIL;
              goto done;
          }
      }

    if(u && u[0] != '\0') 
      {
        if(SDIputattr(&var->attrs, _HDF_Units, DFNT_CHAR, 
                      (intn) HDstrlen(u), u) == FAIL)
          {
              ret_value = FAIL;
              goto done;
          }
      }

    if(f && f[0] != '\0') 
      {
        if(SDIputattr(&var->attrs, _HDF_Format, DFNT_CHAR, 
                      (intn) HDstrlen(f), f) == FAIL)
          {
              ret_value = FAIL;
              goto done;
          }
      }
    
    if(c && c[0] !='\0') 
      {
        if(SDIputattr(&var->attrs, _HDF_CoordSys, DFNT_CHAR, 
                      (intn) HDstrlen(c), c) == FAIL)
          {
              ret_value = FAIL;
              goto done;
          }
      }
    
    /* make sure it gets reflected in the file */
    if(l || u || f || c)
        handle->flags |= NC_HDIRTY;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDsetdatastrs */


/******************************************************************************
 NAME
	SDsetcal -- set calibration information

 DESCRIPTION
    Store calibration information.  What is the formula? Good question -GV

 RETURNS
    SUCCEED / FAIL

******************************************************************************/
intn
SDsetcal(int32   sdsid,/* IN: dataset ID */
         float64 cal,  /* IN: multiplicative factor */
         float64 cale, /* IN: multiplicative factor error */
         float64 ioff, /* IN: integer offset */
         float64 ioffe,/* IN: integer offset error */
         int32   nt    /* IN: number type of uncalibrated data */)
{
    NC     *handle = NULL;
    NC_var *var = NULL;
    intn    ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDsetcal: I've been called\n");
#endif

    
    handle = SDIhandle_from_id(sdsid, SDSTYPE);
    if(handle == NULL) 
      {
          ret_value = FAIL;
          goto done;
      }

    if(handle->vars == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    var = SDIget_var(handle, sdsid);
    if(var == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    if(SDIputattr(&var->attrs, _HDF_ScaleFactor, DFNT_FLOAT64, 
                  (intn) 1, &cal) == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }

    if(SDIputattr(&var->attrs, _HDF_ScaleFactorErr, DFNT_FLOAT64, 
                  (intn) 1, &cale) == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }

    if(SDIputattr(&var->attrs, _HDF_AddOffset, DFNT_FLOAT64, 
                  (intn) 1, &ioff) == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }

    if(SDIputattr(&var->attrs, _HDF_AddOffsetErr, DFNT_FLOAT64, 
                  (intn) 1, &ioffe) == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }

    if(SDIputattr(&var->attrs, _HDF_CalibratedNt, DFNT_INT32, 
                  (intn) 1, &nt) == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* make sure it gets reflected in the file */
    handle->flags |= NC_HDIRTY;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDsetcal */


/******************************************************************************
 NAME
	SDsetfillvalue -- set the fill value

 DESCRIPTION
    Set the fill value for this data set.  The fill value 
    is assumed to have the same number type as the dataset

 RETURNS
    SUCCEED / FAIL 

******************************************************************************/
intn
SDsetfillvalue(int32 sdsid, /* IN: dataset ID */
               void * val    /* IN: fillvalue */)
{
    NC     *handle = NULL;
    NC_var *var = NULL;
    intn    ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDsetfillvalue: I've been called\n");
#endif
    

    handle = SDIhandle_from_id(sdsid, SDSTYPE);
    if(handle == NULL) 
      {
          ret_value = FAIL;
          goto done;
      }

    if(handle->vars == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    var = SDIget_var(handle, sdsid);
    if(var == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    if(SDIputattr(&var->attrs, _FillValue, var->HDFtype, 
                  (intn) 1, val) == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }
    
    /* make sure it gets reflected in the file */
    handle->flags |= NC_HDIRTY;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDsetfillvalue */


/******************************************************************************
 NAME
	SDgetfillvalue -- get the fill value

 DESCRIPTION
    Retreive the fill value for this data set if one has been
    stored.  The fill value has the same number type as the
    dataset

 RETURNS
    SUCCEED / FAIL
 
******************************************************************************/
intn
SDgetfillvalue(int32 sdsid, /* IN:  dataset ID */
               void * val    /* OUT: fillvalue */)
{
    CONSTR(FUNC, "SDgetfillvalue");    /* for HGOTO_ERROR */
    NC       *handle = NULL;
    NC_var   *var = NULL;
    NC_attr **attr = NULL;
    intn      ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDgetfillvalue: I've been called\n");
#endif
    

    /* sanity check args */
    if(val == NULL)
        HGOTO_ERROR(DFE_ARGS, FAIL);

    handle = SDIhandle_from_id(sdsid, SDSTYPE);
    if(handle == NULL) 
      {
          ret_value = FAIL;
          goto done;
      }

    if(handle->vars == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    var = SDIget_var(handle, sdsid);
    if(var == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    attr = (NC_attr **) NC_findattr(&(var->attrs), _FillValue);
    if(attr == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    NC_copy_arrayvals((char *)val, (*attr)->data) ;    
    
done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDgetfillvalue */


/******************************************************************************
 NAME
	SDgetdatastrs -- get "data strings"

 DESCRIPTION
    Read information about the 'label', 'units', 'format' and 
    'cordsys' attributes of a dataset.  All of the values 
     are optional.  If no string is desired NULL should be passed 
     in its place.  Assume all buffers are len bytes long.

 RETURNS
    SUCCEED / FAIL
 
******************************************************************************/
intn
SDgetdatastrs(int32 sdsid, /* IN:  dataset ID */
              char *l,     /* OUT: label string ("long_name") */
              char *u,     /* OUT: units string ("units") */
              char *f,     /* OUT: format string ("format") */
              char *c,     /* OUT: coordsys string ("coordsys") */
              intn  len    /* IN:  buffer length */)
{
    NC       *handle = NULL;
    NC_var   *var = NULL;
    NC_attr **attr = NULL;
    intn      ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDgetdatastrs: I've been called\n");
#endif
    

    handle = SDIhandle_from_id(sdsid, SDSTYPE);
    if(handle == NULL) 
      {
          ret_value = FAIL;
          goto done;
      }

    if(handle->vars == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    var = SDIget_var(handle, sdsid);
    if(var == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    if(l) 
      {
          attr = (NC_attr **) NC_findattr(&(var->attrs), _HDF_LongName);
          if(attr != NULL) 
            {
                if((*attr)->data->count < len)
                  {
                      HDstrncpy((char *)l, (*attr)->data->values,(*attr)->data->count );
                      l[(*attr)->data->count] = '\0';
                  }
                else
                    HDstrncpy((char *)l, (*attr)->data->values,len );
            } 
          else 
              l[0] = '\0';
      }

    if(u) 
      {
          attr = (NC_attr **) NC_findattr(&(var->attrs), _HDF_Units);
          if(attr != NULL) 
            {
                if((*attr)->data->count < len)
                  {
                      HDstrncpy((char *)u, (*attr)->data->values,(*attr)->data->count );
                      u[(*attr)->data->count] = '\0';
                  }
                else
                    HDstrncpy((char *)u, (*attr)->data->values, len);

            } 
          else 
              u[0] = '\0';
      }

    if(f) 
      {
          attr = (NC_attr **) NC_findattr(&(var->attrs), _HDF_Format);
          if(attr != NULL) 
            {
                if((*attr)->data->count < len)
                  {
                      HDstrncpy((char *)f, (*attr)->data->values, (*attr)->data->count);
                      f[(*attr)->data->count] = '\0';
                  }
                else
                    HDstrncpy((char *)f, (*attr)->data->values, len);
            } 
          else 
              f[0] = '\0';
      }

    if(c) 
      {
          attr = (NC_attr **) NC_findattr(&(var->attrs), _HDF_CoordSys);
          if(attr != NULL) 
            {
                if((*attr)->data->count < len)
                  {
                      HDstrncpy((char *)c, (*attr)->data->values, (*attr)->data->count);
                      c[(*attr)->data->count] = '\0';
                  }
                else
                    HDstrncpy((char *)c, (*attr)->data->values, len);

            } 
          else 
              c[0] = '\0';
      }
        
done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDgetdatastrs */


/******************************************************************************
 NAME
	SDgetcal -- get calibration information

 DESCRIPTION
    Retreive calibration information.  What is the formula?

 RETURNS
    SUCCEED / FAIL

******************************************************************************/
intn
SDgetcal(int32    sdsid, /* IN:  dataset ID */
         float64 *cal,   /* OUT: multiplicative factor */
         float64 *cale,  /* OUT: multiplicative factor error */
         float64 *ioff,  /* OUT: integer offset  */
         float64 *ioffe, /* OUT: integer offset error */
         int32   *nt     /* OUT: number type of uncalibrated data */)
{
    NC       *handle = NULL;
    NC_var   *var    = NULL;
    NC_attr **attr   = NULL;
    intn      ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDgetcal: I've been called\n");
#endif
    

    handle = SDIhandle_from_id(sdsid, SDSTYPE);
    if(handle == NULL) 
      {
          ret_value = FAIL;
          goto done;
      }

    if(handle->vars == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    var = SDIget_var(handle, sdsid);
    if(var == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    attr = (NC_attr **) NC_findattr(&(var->attrs), _HDF_ScaleFactor);
    if(attr == NULL)
      {
          ret_value = FAIL;
          goto done;
      }
    NC_copy_arrayvals((char *)cal, (*attr)->data) ;    

    attr = (NC_attr **) NC_findattr(&(var->attrs), _HDF_ScaleFactorErr);
    if(attr == NULL)
      {
          ret_value = FAIL;
          goto done;
      }
    NC_copy_arrayvals((char *)cale, (*attr)->data) ;    

    attr = (NC_attr **) NC_findattr(&(var->attrs), _HDF_AddOffset);
    if(attr == NULL)
      {
          ret_value = FAIL;
          goto done;
      }
    NC_copy_arrayvals((char *)ioff, (*attr)->data) ;    

    attr = (NC_attr **) NC_findattr(&(var->attrs), _HDF_AddOffsetErr);
    if(attr == NULL)
      {
          ret_value = FAIL;
          goto done;
      }
    NC_copy_arrayvals((char *)ioffe, (*attr)->data) ;    

    attr = (NC_attr **) NC_findattr(&(var->attrs), _HDF_CalibratedNt);
    if(attr == NULL)
      {
          ret_value = FAIL;
          goto done;
      }
    NC_copy_arrayvals((char *)nt, (*attr)->data) ;    
        
done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDgetcal */


/******************************************************************************
 NAME
	SDgetcoordvar -- get index of coordinate variable

 DESCRIPTION
    Given a dimension return the index of its coordinate variable
    creating one if needed.  If we need to create a variable and an nt
    is not supplied (i.e. is equal to zero) use 32bit floats.
        
    If the variable already exists and the existing nt is different
    from the supplied one (and the supplied one is not zero) replace
    the nt by the new one.  ID is needed so that we can set the 
    dimension of the variable correctly if we need to.  Yuck.

 RETURNS
     A variable index or FAIL on error

******************************************************************************/ 
PRIVATE int32
SDIgetcoordvar(NC     *handle, /* IN: file handle */
               NC_dim *dim,    /* IN: dimension to find coord var of */
               int32   id,     /* IN: dimension ID */
               int32   nt      /* IN: number type to use if new variable*/)
{ 
    int32      ii;
    int32      len;
    nc_type    nctype;
    intn       dimindex;
    NC_string *name = NULL;
    NC_var   **dp = NULL;
    NC_var    *var = NULL;
    int32      ret_value = FAIL;

    /* look for a variable with the same name */
    name = dim->name;
    len = dim->name->len;
    dp = (NC_var**)handle->vars->values ;
    for(ii = 0 ; ii < handle->vars->count ; ii++, dp++) 
      {
          if( len == (*dp)->name->len 
              && HDstrncmp(name->values, (*dp)->name->values, (size_t)len) == 0) 
            {
                /* see if we need to change the number type */
                if((nt != 0) && (nt != (*dp)->type)) 
                  {
#ifdef SDDEBUG
                      fprintf(stderr, "SDIgetcoordvar redefining type\n");
#endif
                      if (((*dp)->type = hdf_unmap_type((int)nt)) == FAIL)
                        {
#ifdef SDDEBUG
                            /* replace it with NCAdvice or HERROR? */
                            fprintf(stderr "SDIgetcoordvar: hdf_unmap_type failed for %d\n", nt);
#endif
                            ret_value = FAIL;
                            goto done;
                        }

                      (*dp)->HDFtype = nt;
                      (*dp)->cdf = handle;
                      /* don't forget to reset the sizes  */
                      (*dp)->szof = NC_typelen((*dp)->type);
                      if (FAIL == ((*dp)->HDFsize = DFKNTsize(nt)))
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                
                      /* recompute all of the shape information */
                      /* BUG: this may be a memory leak ??? */
                      if(NC_var_shape((*dp), handle->dims) == -1)
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                  }

                ret_value = ii; /* found it? */
                goto done;
            }
      }

    /* create a new var with this dim as only coord */
    if(nt == 0) 
        nt = DFNT_FLOAT32;

    if ((nctype = hdf_unmap_type((int)nt)) == FAIL)
      {
#ifdef SDDEBUG
          /* replace it with NCAdvice or HERROR? */
          fprintf(stderr "SDIgetcoordvar: hdf_unmap_type failed for %d\n", nt);
#endif
          ret_value = FAIL;
          goto done;
      }

    dimindex = (intn)id;
    var = (NC_var *) NC_new_var(name->values, nctype, (unsigned)1, &dimindex);
    if(var == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* BMR: put back hdf type that was set wrong by NC_new_var; please refer
       to the cvs history of bug #172 for reason on this statement - 4/17/01*/
    var->HDFtype = nt;

    /* get a new NDG ref for this sucker */
#ifdef NOT_YET
    var->ndg_ref = Htagnewref(handle->hdf_file,DFTAG_NDG);
#else /* NOT_YET */
    var->ndg_ref = Hnewref(handle->hdf_file);
#endif /* NOT_YET */

    /* add it to the handle */
    if(handle->vars->count >= MAX_NC_VARS)
      {
          ret_value = FAIL;
          goto done;
      }

    var->cdf = handle; /* set cdf before calling NC_var_shape */
    /* compute all of the shape information */
    if(NC_var_shape(var, handle->dims) == -1)
      {
          ret_value = FAIL;
          goto done;
      }

    if(NC_incr_array(handle->vars, (Void *)&var) == NULL)
      {
          ret_value = FAIL;
          goto done;
      }
    
    ret_value = handle->vars->count - 1;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */

    return ret_value;    
} /* SDIgetcoordvar */


/******************************************************************************
 NAME
	SDsetdimstrs -- set "dimension strings"

 DESCRIPTION
    Store information about the 'label', 'units' and 'format' 
    attributes of a dimension.  All three of the values are optional.  
    If no string is desired NULL should be passed in its place.

 RETURNS
    SUCCEED / FAIL

******************************************************************************/ 
intn
SDsetdimstrs(int32 id, /* IN: dimension ID */
             const char *l,  /* IN: label string ("long_name") */
             const char *u,  /* IN: units string ("units") */
             const char *f   /* IN: format string ("format") */)
{
    intn       varid;
    NC        *handle = NULL;
    NC_dim    *dim = NULL;
    NC_var    *var = NULL;
    intn       ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDsetdimstrs: I've been called\n");
#endif


    /* get the handle */
    handle = SDIhandle_from_id(id, DIMTYPE);
    if(handle == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* get the dimension structure */
    dim = SDIget_dim(handle, id);
    if(dim == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* look for a variable with the same name */
    varid = (intn)SDIgetcoordvar(handle, dim, (int32)(id & 0xffff), (int32)0);
    if(varid == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* get the variable object */
    var = NC_hlookupvar(handle, varid);
    if(var == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* set the attributes */
    if(l && l[0] != '\0')
      {
        if(SDIputattr(&var->attrs, _HDF_LongName, DFNT_CHAR,
                      (intn) HDstrlen(l), l) == FAIL)
          {
              ret_value = FAIL;
              goto done;
          }
      }

    if(u && u[0] != '\0')
      {
        if(SDIputattr(&var->attrs, _HDF_Units, DFNT_CHAR,
                      (intn) HDstrlen(u), u) == FAIL)
          {
              ret_value = FAIL;
              goto done;
          }
      }

    if(f && f[0] != '\0')
      {
        if(SDIputattr(&var->attrs, _HDF_Format, DFNT_CHAR,
                      (intn) HDstrlen(f), f) == FAIL)
          {
              ret_value = FAIL;
              goto done;
          }
      }

    /* make sure it gets reflected in the file */
    handle->flags |= NC_HDIRTY;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDsetdimstrs */


/******************************************************************************
 NAME
	SDIfreevarAID -- free a variables AID

 DESCRIPTION
    Free the AID of the variable with the given index

 RETURNS
    SUCCEED / FAIL 

******************************************************************************/
PRIVATE int32
SDIfreevarAID(NC   *handle, /* IN: file handle */
              int32 index   /* IN: variable index */)
{
    NC_array **ap = NULL;
    NC_var    *var = NULL;
    int32      ret_value = SUCCEED;

    if(handle == NULL || !handle->vars)
      {
          ret_value = FAIL;
          goto done;
      }

    if(index < 0 || index > handle->vars->count)
      {
          ret_value = FAIL;
          goto done;
      }

    ap = (NC_array **)handle->vars->values;
    ap += index;

    var = (NC_var *) *ap;

    if(var->aid != 0 && var->aid != FAIL)
      {
        if (Hendaccess(var->aid) == FAIL)
          {
              ret_value = FAIL;
              goto done;
          }
      }

    var->aid = FAIL;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */

    return ret_value;    
} /* SDIfreevarAID */
 

/******************************************************************************
 NAME
	SDsetdimscale -- store scale information for the dimension

 DESCRIPTION
    Store information about the 'scales' of a dimension.  Dimensions
    do not have to have the same number type as the dataset.

 RETURNS
    SUCCEED / FAIL

******************************************************************************/ 
intn
SDsetdimscale(int32 id,    /* IN: dimension ID */
              int32 count, /* IN: number of values */
              int32 nt,    /* IN: number type of data */
              void * data   /* IN: scale values */)
{
    NC        *handle = NULL;
    NC_dim    *dim = NULL;
    int32      status;
    intn       varid;
    long       start[1];
    long       end[1];
    intn       ret_value = SUCCEED;
	
#ifdef SDDEBUG
    fprintf(stderr, "SDsetdimscales: I've been called\n");
#endif


    /* get the handle */
    handle = SDIhandle_from_id(id, DIMTYPE);
    if(handle == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* get the dimension structure */
    dim = SDIget_dim(handle, id);
    if(dim == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* sanity check, if not SD_UNLIMITED */
    if( dim->size != 0 && count != dim->size)
      {
          ret_value = FAIL;
          goto done;
      }

    /* look for a variable with the same name */
    varid = (intn)SDIgetcoordvar(handle, dim, id & 0xffff, nt);
    if(varid == -1)
      {
          ret_value = FAIL;
          goto done;
      }

    /* store the data */
    handle->xdrs->x_op = XDR_ENCODE;
    start[0] = 0;
    end[0]   = count;
    status = NCvario(handle, varid, start, end, (Void *)data);
    if(status == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* free the AID */
    status = SDIfreevarAID(handle, varid);
    if(status == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* make sure it gets reflected in the file */
    handle->flags |= NC_HDIRTY;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDsetdimsacle */


/******************************************************************************
 NAME
	SDgetdimscale -- get scale information for the dimension

 DESCRIPTION
    Retreive the scale information stored with a dimension.  It is 
    assumed that the user has called SDdiminfo() and that the data 
    array is long enough to hold the values.

 RETURNS
    SUCCEED / FAIL

******************************************************************************/ 
intn 
SDgetdimscale(int32 id,   /* IN:  dimension ID */
              void * data  /* OUT: scale values */)
{
    NC        *handle = NULL;
    NC_dim    *dim = NULL;
    NC_var    *vp = NULL;
    int32      status;
    intn       varid;
    long       start[1];
    long       end[1];
    intn       ret_value = SUCCEED;
	
#ifdef SDDEBUG
    fprintf(stderr, "SDgetdimscale: I've been called\n");
#endif


    /* sanity check args */
    if(data == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* get the handle */
    handle = SDIhandle_from_id(id, DIMTYPE);
    if(handle == NULL) 
      {
          ret_value = FAIL;
          goto done;
      }

    /* get the dimension structure */
    dim = SDIget_dim(handle, id);
    if(dim == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* look for a variable with the same name */
    /* assume type Float32 can be over-ridden later */
    varid = (intn)SDIgetcoordvar(handle, dim, (int32)(id & 0xffff), (int32)0);
    if(varid == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* store the data */
    handle->xdrs->x_op = XDR_DECODE;
    start[0] = 0;
    if (dim->size != 0)
        end[0] = dim->size;
    else   
      {
        if (handle->file_type != HDF_FILE)
            end[0] = handle->numrecs;
        else   
          {
            vp = SDIget_var(handle, varid);
            if (vp == NULL)
              {
                  ret_value = FAIL;
                  goto done;
              }

            end[0] = vp->numrecs;
          }
      }
  
    status = NCvario(handle, varid, start, end, (Void *)data);
    if(status == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* free the AID */
    status = SDIfreevarAID(handle, varid);
    if(status == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDsetdimsacle */


/******************************************************************************
 NAME
	SDdiminfo -- get info about a dimension

 DESCRIPTION
    Return basic information about a dimension (name, sizes, number
    of attributes, number type, etc...) The user is repsonsible for 
    allocating space to hold the dataset name.  It can be at most 
    MAX_NC_NAME characters in length.  NULL can be passed for the 
    name if it is not required.

 RETURNS
    SUCCEED / FAIL

******************************************************************************/ 
intn
SDdiminfo(int32  id,    /* IN:  dimension ID */
          char  *name,  /* OUT: name of the dimension */
          int32 *size,  /* OUT: size of the dimension */
          int32 *nt,    /* OUT: number type of scales */
          int32 *nattr  /* OUT: the number of local attributes */)
{
    NC      *handle = NULL;
    NC_dim  *dim = NULL;
    NC_var **dp = NULL;
    intn     ii;
    intn     len;
    int      ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDdiminfo: I've been called\n");
#endif


    handle = SDIhandle_from_id(id, DIMTYPE);
    if(handle == NULL)
      {
          ret_value = FAIL;
          goto done;
      }
 
    if(handle->dims == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    dim = SDIget_dim(handle, id);
    if(dim == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    if(name != NULL) 
      {
#if 0
        HDstrncpy(name, dim->name->values, dim->name->len);
#endif
        HDmemcpy(name, dim->name->values, dim->name->len);
        name[dim->name->len] = '\0';
      } 
    else 
        name = dim->name->values;

    *size  = dim->size;

    if(handle->vars) 
      {
          len = dim->name->len;
          dp = (NC_var**)handle->vars->values;
          for(ii = 0 ; ii < handle->vars->count ; ii++, dp++) 
            {
                if( len == (*dp)->name->len 
                    && HDstrncmp(name, (*dp)->name->values, (*dp)->name->len) == 0)
                  {
                      if (handle->file_type == HDF_FILE)
                          *nt = ((*dp)->numrecs ? (*dp)->HDFtype : 0);
                      else 
                          *nt = (*dp)->HDFtype;

                      *nattr = ((*dp)->attrs ? (*dp)->attrs->count : 0);
                      ret_value = SUCCEED;
                      goto done;
                  }
            }
      }

    /* no var so return NULL values */
    *nt    = 0;
    *nattr = 0;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDdiminfo */


/******************************************************************************
 NAME
	SDgetdimstrs -- get "data strings"

 DESCRIPTION
    Read as many of the dimension strings as possible.  Assume that 
    if a pointer is not NULL that we have len bytes that we can use 
    to return the values

 RETURNS
     SUCCEED / FAIL 

******************************************************************************/ 
intn
SDgetdimstrs(int32 id,  /* IN:  dataset ID */
             char *l,   /* OUT: label string ("long_name") */
             char *u,   /* OUT: units string ("units") */
             char *f,   /* OUT: format string ("format") */
             intn  len  /* IN:  buffer length */)
{
    NC       *handle = NULL;
    NC_var   *var = NULL;
    NC_var  **dp = NULL;
    NC_dim   *dim = NULL;
    NC_attr **attr = NULL;
    char     *name = NULL;
    int32     ii;
    int32     namelen;
    intn      ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDgetdimstrs: I've been called\n");
#endif
    

    handle = SDIhandle_from_id(id, DIMTYPE);
    if(handle == NULL) 
      {
          ret_value = FAIL;
          goto done;
      }

    if(handle->vars == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    dim = SDIget_dim(handle, id);
    if(dim == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* need to get a pointer to the var now */
    var = NULL;
    if(handle->vars) 
      {
          name = dim->name->values;
          namelen = HDstrlen(name);
          dp = (NC_var**)handle->vars->values;
          for(ii = 0 ; ii < handle->vars->count ; ii++, dp++) 
            {
                if( namelen == (*dp)->name->len 
                    && HDstrncmp(name, (*dp)->name->values, (size_t)namelen) == 0)
                  {
                      var = (*dp);
                  }
            }
      }

    if(!var)
      {
          ret_value = FAIL;
          goto done;
      }

    if(l) 
      {
          attr = (NC_attr **) NC_findattr(&(var->attrs), _HDF_LongName);
          if(attr != NULL) 
            {
                intn minlen;
                minlen = (len > (*attr)->data->count)? (*attr)->data->count: len;
                HDstrncpy((char *)l, (*attr)->data->values, minlen);
                if((*attr)->data->count < len)
                    l[(*attr)->data->count] = '\0';
            } 
          else 
              l[0] = '\0';
      }

    if(u) 
      {
          attr = (NC_attr **) NC_findattr(&(var->attrs), _HDF_Units);
          if(attr != NULL) 
            {
                intn minlen;
                minlen = (len > (*attr)->data->count)? (*attr)->data->count: len;
                HDstrncpy((char *)u, (*attr)->data->values, minlen);
                if((*attr)->data->count < len)
                    u[(*attr)->data->count] = '\0';
            } 
          else 
              u[0] = '\0';
      }

    if(f) 
      {
          attr = (NC_attr **) NC_findattr(&(var->attrs), _HDF_Format);
          if(attr != NULL) 
            {
                intn minlen;
                minlen = (len > (*attr)->data->count)? (*attr)->data->count: len;
                HDstrncpy((char *)f, (*attr)->data->values, minlen);
                if((*attr)->data->count < len)
                    f[(*attr)->data->count] = '\0';
            } 
          else 
              f[0] = '\0';
      }

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDgetdimstrs */


/******************************************************************************
 NAME
	SDsetexternalfile -- store info in a separate file
 USAGE
	int32 SDsetexternalfile(id, filename, offset)
        int32   id;                  
        const char  * filename;            
        int32   offset;              

 DESCRIPTION
    Specify that the actual data for this dataset be stored in a 
    separate file (and "external file" in HDF terms).

    Only the data (as in SDwritedata()) will be stored externally.  
    Attributes and such will still be in the main file

    IMPORTANT:  It is the user's responsibility to see that the 
    separate files are transported when the main file is moved.

    IMPORTANT:  This can only be called *once* for a given dataset.  
    The HDF utility 'hdfpack' may be able to undo it.

    IMPORTANT:  This will only work on datasets stored in HDF files.

    FORTRAN	- sfsextf

 RETURNS
    SUCCEED/FAIL

******************************************************************************/ 
intn 
SDsetexternalfile(int32 id,       /* IN: dataset ID */
                  const char *filename, /* IN: name of external file */
                  int32 offset    /* IN: offset in external file */)
{
    NC       *handle = NULL;
    NC_var   *var = NULL;
    intn      status;
    int       ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDsetexternalfile: I've been called\n");
#endif


    if(NULL == filename || offset < 0)
      {
          ret_value = FAIL;
          goto done;
      }

    handle = SDIhandle_from_id(id, SDSTYPE);
    if(handle == NULL || handle->file_type != HDF_FILE)
      {
          ret_value = FAIL;
          goto done;
      }

    if(handle->vars == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    var = SDIget_var(handle, id);
    if(var == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* already exists */
    if(var->data_ref) 
      {
          /* no need to give a length since the element already exists */
          status = (intn)HXcreate(handle->hdf_file, (uint16)DATA_TAG, 
                                  (uint16) var->data_ref,
                                  filename, offset, (int32)0);
      } 
    else 
      {
          int32   length;

          /* look up the length */
          length = var->len;

          /* element doesn't exist so we need a reference number */
#ifdef NOT_YET
          var->data_ref = Htagnewref(handle->hdf_file,DATA_TAG);
#else /* NOT_YET */
          var->data_ref = Hnewref(handle->hdf_file);
#endif /* NOT_YET */
          if(var->data_ref == 0)
            {
                ret_value = FAIL;
                goto done;
            }

          /* need to give a length since the element does not exist yet */
          status = (intn)HXcreate(handle->hdf_file, (uint16)DATA_TAG, 
                                  (uint16) var->data_ref,
                                  filename, offset, length);

      }

    if(status != FAIL) 
      {
          if((var->aid != 0) && (var->aid != FAIL))
            {
                if (Hendaccess(var->aid) == FAIL)
                  {
                      ret_value = FAIL;
                      goto done;
                  }

            }

          var->aid = status;
          ret_value = SUCCEED;
      }
    else
        ret_value = FAIL;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDsetexternalfile */


/******************************************************************************
 NAME
	SDsetnbitdataset -- Create/convert a dataset to n-bit representation


 DESCRIPTION
    Specify that the actual data for this dataset be represented as a
    n-bit dataset internally in the HDF file.

    The start_bit parameter determines the lowest bit to write out,
    the bit_len parameter determines how many bits to write out.  The
    bits in the data include the lowest bit (start_bit) and count up
    bit_len-1 bits to write out.  For example, starting at bit 2
    and writing 4 bits from the following bit data, "01111011", would
    write out the bit data, "1110", to the dataset on disk.

    The sign_ext parameter determines whether the top bit (highest bit #)
    is used to sign extend the bits whether data is read back from the
    disk.  The fill_one parameter is used to determine whether to
    fill the "background bits" (the bits not in the data written to the
    file) with 1's or 0's when the data is read back from the file.

    Only the data (as in SDwritedata()) will be stored in n-bit
    representation.  Attributes and such will still be stored normally.

    IMPORTANT:  This will only work on datasets stored in HDF files.

    NOTE: n-bit "compression" is described more fully in the cnbit.c file.

 RETURNS
    SUCCEED/FAIL

******************************************************************************/ 
intn 
SDsetnbitdataset(int32 id,       /* IN: dataset ID */
                 intn start_bit, /* IN: starting bit offset (lowest=0) */
                 intn bit_len,   /* IN: # of bits to write */
                 intn sign_ext,  /* IN: Whether to sign extend */
                 intn fill_one   /* IN: Whether to fill background w/1's */)
{
    NC        *handle = NULL;
    NC_var    *var = NULL;
    model_info m_info;  /* modeling information for the HCcreate() call */
    comp_info  c_info;  /* "compression" information for the HCcreate() call */
    intn       status;
    intn       ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDsetnbitdataset: I've been called\n");
#endif


    if(start_bit < 0 || bit_len <= 0)
      {
          ret_value = FAIL;
          goto done;
      }

    handle = SDIhandle_from_id(id, SDSTYPE);
    if(handle == NULL || handle->file_type != HDF_FILE)
      {
          ret_value = FAIL;
          goto done;
      }

    if(handle->vars == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    var = SDIget_var(handle, id);
    if(var == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* set up n-bit parameters */
    c_info.nbit.nt        = var->HDFtype;
    c_info.nbit.sign_ext  = sign_ext;
    c_info.nbit.fill_one  = fill_one;
    c_info.nbit.start_bit = start_bit;
    c_info.nbit.bit_len   = bit_len;

#ifdef SDDEBUG
    printf("SDsetnbitdata(): nt=%d, sign_ext=%d, fill_one=%d, start_bit=%d, bit_len=%d\n",(intn)c_info.nbit.nt,(intn)c_info.nbit.sign_ext,(intn)c_info.nbit.fill_one,(intn)c_info.nbit.start_bit,(intn)c_info.nbit.bit_len);
#endif
    if(!var->data_ref) 
      {   /* doesn't exist */
#ifdef SDDEBUG
          printf("SDsetnbitdata(): dataset doesn't exist\n");
#endif

          /* element doesn't exist so we need a reference number */
#ifdef NOT_YET
          var->data_ref=Htagnewref(handle->hdf_file,DATA_TAG);
#else /* NOT_YET */
          var->data_ref=Hnewref(handle->hdf_file);
#endif /* NOT_YET */
          if(var->data_ref == 0)
            {
                ret_value = FAIL;
                goto done;
            }
      } /* end if */

    status=(intn)HCcreate(handle->hdf_file,(uint16)DATA_TAG,
                          (uint16) var->data_ref,COMP_MODEL_STDIO,&m_info,
                          COMP_CODE_NBIT, &c_info);

#ifdef SDDEBUG
    printf("SDsetnbitdata(): HCcreate() status=%d\n",(intn)status);
#endif
    if(status != FAIL) 
      {
          if((var->aid != 0) && (var->aid != FAIL))
            {
                if (Hendaccess(var->aid) == FAIL)
                  {
                      ret_value = FAIL;
                      goto done;
                  }
            }

          var->aid = status;
      } /* end if */

    ret_value = status;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDsetnbitdataset */

#ifdef H4_HAVE_LIBSZ          /* we have the library */

/******************************************************************************
 NAME
	SDsetup_szip_parms -- check and set parameters for szip compression
              int32 id;            IN: the  varid
              NC *handle;          IN: the SD handle
              comp_info *c_info;   IN/OUT: the compression settings 
              int32 *cdims;        IN: the chunk dims, NULL if not chunked

 DESCRIPTION
     Collect the parameters and call HCPcszip_setup_parms to set the
     computed szip paramters.

 RETURNS
    SUCCEED/FAIL

******************************************************************************/ 
intn 
SDsetup_szip_parms( int32 id, NC *handle, comp_info *c_info, int32 *cdims)
{
    NC_dim    *dim;     /* to check if the dimension is unlimited */
    int32      dimindex;/* to obtain the NC_dim record */
    NC_var    *var; 
    int32 ndims;
    int i;
    int32 xdims[MAX_VAR_DIMS];
    int32 nt;
    intn       ret_value = SUCCEED;

    if(handle->vars == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    var = SDIget_var(handle, id);
    if(var == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    ndims = var->assoc->count; 
	for (i = 0; i < ndims; i++) {
		dimindex = var->assoc->values[i];
		dim = SDIget_dim(handle, dimindex);
		xdims[i] = dim->size;
	}

    nt = var->HDFtype;
	
    ret_value = HCPsetup_szip_parms( c_info, nt, 1, ndims, xdims, cdims);

done:
	return(ret_value);
}
#endif
/******************************************************************************
 NAME
	SDsetcompress -- Create/convert a dataset to compressed representation

 DESCRIPTION
    Specify a compression scheme for an SD dataset.

    Valid compression types available for this interface are listed in
    hcomp.h as COMP_nnnn.

    IMPORTANT:  This will only work on datasets stored in HDF files.

 RETURNS
    SUCCEED/FAIL

******************************************************************************/ 
intn 
SDsetcompress(int32 id,                /* IN: dataset ID */
              comp_coder_t comp_type,  /* IN: the type of compression to 
					  perform on the next image */
              comp_info *c_info        /* IN: ptr to compression info struct*/)
{
    CONSTR(FUNC, "SDsetcompress");    /* for HGOTO_ERROR */
    NC        *handle;
    NC_var    *var;
    NC_dim    *dim;     /* to check if the dimension is unlimited */
    int32      dimindex;/* to obtain the NC_dim record */
    model_info m_info;  /* modeling information for the HCcreate() call */
    comp_info c_info_x;  /* local copy */
    uint32  comp_config;
    intn       status = FAIL;
    intn       ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDsetcompress: I've been called\n");
#endif /* SDDEBUG */


    if (comp_type < COMP_CODE_NONE || comp_type >= COMP_CODE_INVALID)
      {
          ret_value = FAIL;
          goto done;
      }

    HCget_config_info(comp_type, &comp_config);
    if ((comp_config & COMP_DECODER_ENABLED|COMP_ENCODER_ENABLED) == 0) {
	/* coder not present?? */
	HGOTO_ERROR(DFE_BADCODER, FAIL);
    }
    if ((comp_config & COMP_ENCODER_ENABLED) == 0) {
	/* encoder not present?? */
	HGOTO_ERROR(DFE_NOENCODER, FAIL);
    }
    HDmemcpy(&c_info_x,c_info,sizeof(comp_info));

    handle = SDIhandle_from_id(id, SDSTYPE);
    if(handle == NULL || handle->file_type != HDF_FILE)
      {
          ret_value = FAIL;
          goto done;
      }

    if(handle->vars == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    var = SDIget_var(handle, id);
    if(var == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* disallow setting compress for SDS with rank = 0 - BMR, bug #1045 */
    if(var->shape == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    /* unlimited dimensions don't work with compression */
        /* Get the index of the SDS' first dimension from the list of indices
         * branching out from NC_var.  This index indicates where this dim
         * is in the "dims" list branching out from NC. */
        dimindex = var->assoc->values[0];

        /* Retrieve the NC_dim record to check for unlimited dimension */
        dim = SDIget_dim(handle, dimindex);
        if(dim == NULL)
        {
            ret_value = FAIL;
            goto done;
        }

        /* If this dimension is unlimited, then return FAIL; the subsequent
         * writing of this SDS will write uncompressed data */
        if (dim->size == SD_UNLIMITED)
        {
            ret_value = FAIL;
            goto done;
        }
#ifdef H4_HAVE_LIBSZ          /* we have the library */
	if (comp_type == COMP_CODE_SZIP) {
	   if (SDsetup_szip_parms( id, handle, &c_info_x, NULL) == FAIL) {
		HGOTO_ERROR(DFE_INTERNAL, FAIL);
	    }
	}
#else
	/* no SZIP */
/* probably covered by test at start */
	if (comp_type == COMP_CODE_SZIP) {
		HGOTO_ERROR(DFE_BADCODER, FAIL);
	}
#endif /* H4_HAVE_LIBSZ          */

#ifdef SDDEBUG
    printf("SDsetcompress(): var->data_ref=%d\n",(int)var->data_ref);
#endif /* SDDEBUG */
    if(!var->data_ref) 
      {   /* doesn't exist */
#ifdef SDDEBUG
          printf("SDsetcompress(): dataset doesn't exist\n");
#endif /* SDDEBUG */

          /* element doesn't exist so we need a reference number */
#ifdef NOT_YET
          var->data_ref=Htagnewref(handle->hdf_file,DATA_TAG);
#else /* NOT_YET */
          var->data_ref=Hnewref(handle->hdf_file);
#endif /* NOT_YET */
          if(var->data_ref == 0)
            {
                ret_value = FAIL;
                goto done;
            }
      } /* end if */

    status=(intn)HCcreate(handle->hdf_file,(uint16)DATA_TAG,
                          (uint16) var->data_ref,COMP_MODEL_STDIO,&m_info,
                          comp_type, &c_info_x);

#ifdef SDDEBUG
    printf("SDsetcompress(): HCcreate() status=%d\n",(intn)status);
    if(status==FAIL)
        HEprint(stderr,0);
#endif /* SDDEBUG */

    if(status != FAIL) 
      {
          if((var->aid != 0) && (var->aid != FAIL))
            {
                if (Hendaccess(var->aid) == FAIL)
                  {
                      ret_value = FAIL;
                      goto done;
                  }
            }

          var->aid = status;
      } /* end if */

    /* Insert data tag/ref into the variable's Vgroup */
    if(var->vgid) 
      {
          int32 vg;

          /* attach to the variable's Vgroup */
          vg = Vattach(handle->hdf_file, var->vgid, "w");
          if(vg == FAIL)
            {
                ret_value = FAIL;
                goto done;
            }
        
          /* add new Vdata to existing Vgroup */
          if (Vaddtagref(vg, (int32) DATA_TAG, (int32) var->data_ref) == FAIL)
            {
                ret_value = FAIL;
                goto done;
            }
        
          /* detach from the variable's VGroup --- will no longer need it */
          if (Vdetach(vg) == FAIL)
            {
                ret_value = FAIL;
                goto done;
            }
      }

    /* added a new object -- make sure we flush the header */
    handle->flags |= NC_HDIRTY;
            
    ret_value = (status != FAIL ? SUCCEED : FAIL);

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDsetcompress */

/******************************************************************************
 NAME
	SDgetcompress -- Retrieves compression information of a dataset

 DESCRIPTION
    This routine uses HCPgetcompress to retrieve the compression type
    and the compression information of the identified dataset.

 RETURNS
    SUCCEED/FAIL

 MODIFICATION
    July 2001: Added to fix bug #307 - BMR 

******************************************************************************/ 
intn 
SDgetcompress(int32     id,     /* IN: dataset ID */
              comp_coder_t*    comp_type,   /* OUT: the type of compression */
              comp_info* c_info)/* OUT: ptr to compression information 
				structure for storing the retrieved info */
{
    CONSTR(FUNC, "SDgetcompress");    /* for HGOTO_ERROR */
    NC        *handle;
    NC_var    *var;
    intn       status = FAIL;
    intn       ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDgetcompress: I've been called\n");
#endif /* SDDEBUG */


    /* clear error stack */
    HEclear();

    if(comp_type == NULL || c_info == NULL)
	HGOTO_ERROR(DFE_ARGS, FAIL);

    handle = SDIhandle_from_id(id, SDSTYPE);
    if(handle == NULL || handle->file_type != HDF_FILE)
	HGOTO_ERROR(DFE_ARGS, FAIL);
    if(handle->vars == NULL)
	HGOTO_ERROR(DFE_ARGS, FAIL);

    var = SDIget_var(handle, id);
    if(var == NULL) HGOTO_ERROR(DFE_ARGS, FAIL);

#ifdef SDDEBUG
    printf("SDgetcompress(): var->data_ref=%d, var->aid=%d\n",(int)var->data_ref, (int)var->aid);
#endif /* SDDEBUG */
    if(!var->data_ref) 
	HGOTO_ERROR(DFE_ARGS, FAIL);

    /* use lower-level routine to get the compression information */
    status = HCPgetcompress(handle->hdf_file, var->data_tag, var->data_ref, 
		comp_type, c_info);
    if(status==FAIL) HGOTO_ERROR(DFE_INTERNAL, FAIL);

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */

    return ret_value;    
} /* SDgetcompress */

/******************************************************************************
 NAME
	SDfindattr -- find an attribute's index by name

 DESCRIPTION
    Given an ID to an object and an attribute name return the index 
    of the attribute with that name.  This does not support any
    form of wildcards / regular expressions

 RETURNS
     An attribute index or FAIL

******************************************************************************/ 
int32
SDfindattr(int32 id,       /* IN: object ID */
           const char *attrname  /* IN: attribute name */)
{
    NC_array  *ap = NULL;
    NC_array **app = NULL;
    NC_attr  **attr = NULL;
    NC        *handle = NULL;
    int32      attrid;
    int32      len;
    int32      ret_value = FAIL;


    /* determine what type of ID we've been given */
    if(SDIapfromid(id, &handle, &app) == FAIL)
      {
          ret_value = FAIL;
          goto done;
      }

    ap = (*app);
    if(ap == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* 
     * ap is the proper attribute list now look up something with this
     *  name
     */

    attr = (NC_attr **) ap->values;
    len = HDstrlen(attrname);
    
    for(attrid = 0 ; attrid < ap->count ; attrid++, attr++)
      {
          if( len == (*attr)->name->len 
              && HDstrncmp(attrname, (*attr)->name->values, (size_t)len) == 0)
            {
                ret_value = attrid ; /* found it */
                goto done;
            }
      }

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;        
} /* SDfindattr */


/******************************************************************************
 NAME
	SDidtoref -- get a unique reference number for this dataset

 DESCRIPTION
    Given an index return the ref of the associated NDG for 
    inclusion in Vgroups and annotations

 RETURNS
     A reference number or FAIL

******************************************************************************/
int32
SDidtoref(int32 id /* IN: dataset ID */)
{
    NC       *handle = NULL;
    NC_var   *var = NULL;
    int32     ret_value = FAIL;

#ifdef SDDEBUG
    fprintf(stderr, "SDidtoref: I've been called\n");
#endif
    

    handle = SDIhandle_from_id(id, SDSTYPE);
    if(handle == NULL || handle->file_type != HDF_FILE) 
      {
          ret_value = FAIL;
          goto done;
      }

    if(handle->vars == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    var = SDIget_var(handle, id);
    if(var == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    ret_value = (int32) var->ndg_ref;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDidtoref */


/******************************************************************************
 NAME
	SDreftoindex -- map a reference number to a dataset index

 DESCRIPTION
    Given a ref number return the index of the cooresponding dataset

 RETURNS
    A dataset index or FAIL

******************************************************************************/
int32
SDreftoindex(int32 fid, /* IN: file ID */
             int32 ref  /* IN: reference number */)
{
    NC       *handle = NULL;
    NC_var  **dp = NULL;
    intn      ii;
    int32     ret_value = FAIL;

#ifdef SDDEBUG
    fprintf(stderr, "SDreftoindex: I've been called\n");
#endif
    

    handle = SDIhandle_from_id(fid, CDFTYPE);
    if(handle == NULL || handle->file_type != HDF_FILE) 
      {
          ret_value = FAIL;
          goto done;
      }

    if(handle->vars == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    dp = (NC_var**) handle->vars->values;
    for(ii = 0 ; ii < handle->vars->count ; ii++, dp++)
      {
        if((*dp)->ndg_ref == ref)
          {
            ret_value = ii;
            goto done;
          }
      }
    
    ret_value = FAIL;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDreftoindex */


/******************************************************************************
 NAME
	SDisrecord -- check is var is a record variable

 DESCRIPTION
    Return TRUE if the dataset in question is a record variable
    else FALSE

 RETURNS
    TRUE/FALSE

******************************************************************************/
int32
SDisrecord(int32 id /* IN: dataset ID */)
{
    NC       *handle;
    NC_var   *var;
    int32     ret_value = TRUE;

#ifdef SDDEBUG
    fprintf(stderr, "SDisrecord: I've been called\n");
#endif
    

    handle = SDIhandle_from_id(id, SDSTYPE);
    if(handle == NULL)
      {
        ret_value = FALSE;
        goto done;
      }

    if(handle->vars == NULL)
      {
        ret_value = FALSE;
        goto done;
      }

    var = SDIget_var(handle, id);
    if(var == NULL)
      {
        ret_value = FALSE;
        goto done;
      }

    if(var->shape == NULL)
      {
        ret_value = TRUE; /* EP thinks it should return true - BMR, bug #1045 */
        goto done;
      }

    if(var->shape[0] == SD_UNLIMITED)
        ret_value = TRUE;
    else
        ret_value = FALSE;

done:
    if (ret_value == FALSE)
      { /* FALSE cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDisrecord */


/******************************************************************************
 NAME
	SDiscoordvar -- check is var is a coord var

 DESCRIPTION
    Return TRUE if the dataset in question is a coordinate variable

 RETURNS
     TRUE/FALSE

******************************************************************************/
intn
SDiscoordvar(int32 id /* IN: dataset ID */)
{
    NC       *handle = NULL;
    NC_var   *var = NULL;
    NC_dim   *dim = NULL;
    int32     dimindex;
    intn      ret_value = TRUE;

#ifdef SDDEBUG
    fprintf(stderr, "SDiscoordvar: I've been called\n");
#endif
    

    handle = SDIhandle_from_id(id, SDSTYPE);
    if(handle == NULL)
      {
        ret_value = FALSE;
        goto done;
      }

    if(handle->vars == NULL)
      {
        ret_value = FALSE;
        goto done;
      }

    var = SDIget_var(handle, id);
    if(var == NULL)
      {
        ret_value = FALSE;
        goto done;
      }

    if(var->assoc->count != 1)
      {
        ret_value = FALSE;
        goto done;
      }

    dimindex = var->assoc->values[0];

    dim = SDIget_dim(handle, dimindex);
    if(dim == NULL)
      {
        ret_value = FALSE;
        goto done;
      }

    if(var->name->len != dim->name->len)
      {
        ret_value = FALSE;
        goto done;
      }

    if(HDstrcmp(var->name->values, dim->name->values))
      {
        ret_value = FALSE;
        goto done;
      }

    ret_value = TRUE;

done:
    if (ret_value == FALSE)
      { /* FALSE cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDiscoordvar */


#if 0
/* ---------------------------- RAGGED ARRAYS ----------------------------- */
/*

  Ragged arrays are a cross between datasets and index structures.  The
  basic idea is that all but one of the dimensions is constant.  The
  other dimension can vary over the course of the dataset.  This is
  useful for storing equalarea grids and for making alogrithms much
  more complex.  

  Ragged arrays can be multi-dimensional and, eventually, record variables
  too.  A 2-dimensional ragged array would look like:

  **********
  ********
  *********    <---------- This is a line
  *****
  ***
  *********
  *******

  The above ragged array has 7 "lines" the "line length" of the fifth line is 
  three.  It is not necessary to set all of the line lengths at the same time
  nor retreive them all at the same time.   However, to specify the line
  length for line X, the length must be specified for all Y < X (is this
  really necessary?)

  Internally, the above ragged array would be stored as a one-dimensional
  dataset.  In addition, there will be a rag_fill array that contains the
  line lengths.  This rag_fill array will get written to a separate
  structure in the file (tag DFTAG_SDRAG).

*/
/* ------------------------------------------------------------------------ */


/******************************************************************************
  Set the lengths of the lines of a ragged array.

  Currently, these lines must be specified in increasing order (i.e. can't
  use hyperslab type locations to set them).  This should probably be made
  nicer once everything else works.

******************************************************************************/
int32
SDsetrag(int32 sdsid, 
         int32 low, 
         int32 count, 
         int32 *sizes)
{
    NC       *handle = NULL;
    NC_var   *var = NULL;
    int32     ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDsetrag: I've been called\n");
#endif
    

    /* get the variable */
    handle = SDIhandle_from_id(sdsid, SDSTYPE);
    if(handle == NULL || handle->file_type != HDF_FILE) 
      {
          ret_value = FAIL;
          goto done;
      }

    if(handle->vars == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    var = SDIget_var(handle, sdsid);
    if((var == NULL) || (var->is_ragged == FALSE))
      {
          ret_value = FAIL;
          goto done;
      }

    /* verify writing to a valid area */
    if(var->rag_fill != low) 
      {
        printf("var->rag_fill %d    low %d\n", var->rag_fill, low); 
        ret_value = FAIL;
        goto done;
      }

    /* allocate some space for the ragged dimension if needed */
    /* BUG: will need to be changed for ragged records */
    if(var->rag_list == NULL) 
      {
        var->rag_list = (int32 *) HDmalloc(sizeof(int32) * var->dsizes[0]);
        if(var->rag_list == NULL) 
          {
              ret_value = FAIL;
              goto done;
          }
      }

    /* copy over the new values */
    HDmemcpy(&(var->rag_list[low]), sizes, sizeof(int32) * count);
    
    /* update count */
    var->rag_fill += count;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDsetrag */

#endif /* 0 */


/******************************************************************************
 NAME
	SDsetaccesstype -- set the I/O access type of an SD

 DESCRIPTION
    Set the type of I/O (serial, parallel, ...) for accessing the
	data of the SD.  Access types can be DFACC_SERIAL, DFACC_PARALLEL,
	DFACC_DEFAULT.

 RETURNS
    Return SUCCEED if the SD data can be accessed via accesstype.
	Otherwise return FAIL.

******************************************************************************/	
intn 
SDsetaccesstype(int32 id,         /* IN: dataset ID */
                uintn accesstype  /* IN: access type */)
{
    NC       *handle = NULL;
    NC_var   *var = NULL;
    intn      ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDsetaccesstype: I've been called\n");
#endif


    switch (accesstype)
      {
        case DFACC_DEFAULT:
        case DFACC_SERIAL:
        case DFACC_PARALLEL:
            break;
        default:
            ret_value = FAIL;
            goto done;
      }

    handle = SDIhandle_from_id(id, SDSTYPE);
    if(handle == NULL || handle->file_type != HDF_FILE)
      {
          ret_value = FAIL;
          goto done;
      }

    if(handle->vars == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    var = SDIget_var(handle, id);
    if(var == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* if aid is not valid yet, there is no access_rec setup yet. */
    /* Go ahead and try set it up. */
    if(var->aid == FAIL && hdf_get_vp_aid(handle, var) == FAIL)
        ret_value = FAIL;
    else 
        ret_value = (intn)Hsetaccesstype(var->aid, accesstype);

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDsetaccesstype */


/******************************************************************************
 NAME
	SDsetblocksize -- set the size of the linked blocks created.

 DESCRIPTION
    Set the size of the blocks used for storing the data for unlimited
	dimension datasets.  This is used when creating new datasets only,
	it does not have any affect on existing datasets.  The block_size 
	should probably be set to a multiple of the "slice" size.

 RETURNS
    SUCCEED/FAIL

******************************************************************************/
intn
SDsetblocksize(int32 sdsid,      /* IN: dataset ID */
               int32 block_size  /* IN: size of the block in bytes */)
{
    NC      *handle = NULL;
    NC_var  *var = NULL;
    intn     ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDsetblocksize: I've been called\n");
#endif


    /* get the handle */
    handle = SDIhandle_from_id(sdsid, SDSTYPE);
    if(handle == NULL) 
      {
          ret_value = FAIL;
          goto done;
      }

    /* get the variable */
    var = SDIget_var(handle, sdsid);
    if(var == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* set the block size */
    var->block_size = block_size;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDsetblocksize */

/******************************************************************************
 NAME
   SDsetfillmode -- set fill mode as fill or nofill

 DESCRIPTION
   Calls ncsetfill().

 RETURNS
   The current fill mode of the file, or FAIL for error.

******************************************************************************/
intn
SDsetfillmode(int32 sd_id,  /* IN: HDF file ID, returned from SDstart */
              intn fillmode /* IN: Desired fill mode for the file,
                                   either SD_FILL or SD_NOFILL.
                                   SD_FILL is the default mode. */)
{
    NC     *handle = NULL;
    intn    cdfid;
    intn    ret_value = FAIL;

#ifdef SDDEBUG
    fprintf(stderr, "SDsetfillmode: I've been called\n");
#endif


    /* get the handle */
    handle = SDIhandle_from_id(sd_id, CDFTYPE);
    if(handle == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    cdfid = (intn)sd_id & 0xffff;
    ret_value = ncsetfill(cdfid, fillmode);

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDsetfillmode() */

/******************************************************************************
 NAME
    SDsetdimval_comp -- set dimval backward compatibility

 DESCRIPTION

 RETURNS
    SUCCEED/FAIL

******************************************************************************/
intn
SDsetdimval_comp(int32 dimid,    /* IN: dimension ID, returned from SDgetdimid */
                 intn comp_mode  /* IN: backward compatibility:
                                    SD_DIMVAL_BW_COMP -- compatible
                                    SD_DIMVAL_BW_INCOMP -- incompatible.
                                    (defined in mfhdf.h ) */)
{
    NC      *handle = NULL;
    NC_dim  *dim = NULL;
    intn     ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDsetdimval_comp: I've been called\n");
#endif


    /* get the handle */
    handle = SDIhandle_from_id(dimid, DIMTYPE);
    if(handle == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* get the dimension structure */
    dim = SDIget_dim(handle, dimid);
    if(dim == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

/*    if (dim->size != SD_UNLIMITED  
        && dim->dim00_compat != comp_mode )  
*/
    if (dim->dim00_compat != comp_mode)
      {
        dim->dim00_compat = comp_mode;

        /* make sure it gets reflected in the file */
        handle->flags |= NC_HDIRTY;
      }

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDsetdimval_comp */


/******************************************************************************
 NAME
   SDisdimval_bwcomp -- get dimval backward compatibility

 DESCRIPTION

 RETURNS
    SD_DIMVAL_BW_COMP if dimval is backward compatible;
    SD_DIMVAL_BW_INCOMP for not compatible; FAIL for error. 

******************************************************************************/
intn
SDisdimval_bwcomp(int32 dimid /* IN: dimension ID, returned from SDgetdimid */)
{
    NC      *handle = NULL;
    NC_dim  *dim = NULL;
    intn    ret_value = FAIL;

#ifdef SDDEBUG
    fprintf(stderr, "SDisdimval_bwcomp: I've been called\n");
#endif


    /* get the handle */
    handle = SDIhandle_from_id(dimid, DIMTYPE);
    if(handle == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* get the dimension structure */
    dim = SDIget_dim(handle, dimid);
    if(dim == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

/* Default is incompatible. Return dim->dim00_compat.
    if (dim->size == SD_UNLIMITED) 
        ret_value = SD_DIMVAL_BW_COMP;
    else 
*/
    ret_value = dim->dim00_compat; 

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;    
} /* SDisdimval_bwcomp */

/*====================== Chunking Routines ================================*/

/******************************************************************************
 NAME
      SDsetchunk  -- make SDS a chunked SDS

 DESCRIPTION
      This routine makes the SDS a chunked SDS according to the chunk
      definiton passed in.

      The dataset currently cannot be special already.  i.e. NBIT,
      COMPRESSED, or EXTERNAL. This is an Error.

      The defintion of the HDF_CHUNK_DEF union with relvant fields is:

      typedef union hdf_chunk_def_u
      {
         int32   chunk_lengths[MAX_VAR_DIMS];  Chunk lengths along each dimension

         struct 
          {   
            int32     chunk_lengths[MAX_VAR_DIMS]; Chunk lengths along each dimension
            int32     comp_type;                   Compression type 
            comp_info cinfo;                       Compression info struct 
          }comp;

      } HDF_CHUNK_DEF

      The variable agruement 'flags' is a bit-or'd value which can currently be
      'HDF_CHUNK' or 'HDF_CHUNK | HDF_COMP'.

      The simplist is the 'chunk_lengths' array specifiying chunk 
      lengths for each dimension where the 'flags' argument set to 
      'HDF_CHUNK';

      COMPRESSION is set by using the 'HDF_CHUNK_DEF' union to set the
      appropriate compression information along with the required chunk lengths
      for each dimension. The compression information is the same as 
      that set in 'SDsetcompress()'. The bit-or'd 'flags' argument' is set to 
      'HDF_CHUNK | HDF_COMP'.

      See the example in pseudo-C below for further usage.

      The maximum number of Chunks in an HDF file is 65,535.

      The dataset currently cannot have an UNLIMITED dimension.

      The performance of the SDxxx interface with chunking is greatly
      affected by the users access pattern over the dataset and by
      the maximum number of chunks set in the chunk cache. The cache contains 
      the Least Recently Used(LRU cache replacment policy) chunks. See the
      routine SDsetchunkcache() for further info on the chunk cache and how 
      to set the maximum number of chunks in the chunk cache. A default chunk 
      cache is always created.

      The following example shows the organization of chunks for a 2D array.
      e.g. 4x4 array with 2x2 chunks. The array shows the layout of
           chunks in the chunk array.

            4 ---------------------                                           
              |         |         |                                                 
        Y     |  (0,1)  |  (1,1)  |                                       
        ^     |         |         |                                      
        |   2 ---------------------                                       
        |     |         |         |                                               
        |     |  (0,0)  |  (1,0)  |                                      
        |     |         |         |                                           
        |     ---------------------                                         
        |     0         2         4                                       
        ---------------> X                                                       

        --Without compression--:
        {                                                                    
        HDF_CHUNK_DEF chunk_def;
                                                                            
        .......                                                                    
        -- Set chunk lengths --                                                    
        chunk_def.chunk_lengths[0]= 2;                                                     
        chunk_def.chunk_lengths[1]= 2; 

        -- Set Chunking -- 
        SDsetchunk(sdsid, chunk_def, HDF_CHUNK);                      
         ......                                                                  
        }                                                                           

        --With compression--:
        {                                                                    
        HDF_CHUNK_DEF chunk_def;
                                                                            
        .......                
        -- Set chunk lengths first --                                                    
        chunk_def.chunk_lengths[0]= 2;                                                     
        chunk_def.chunk_lengths[1]= 2;

        -- Set compression --
        chunk_def.comp.cinfo.deflate.level = 9;
        chunk_def.comp.comp_type = COMP_CODE_DEFLATE;

        -- Set Chunking with Compression --
        SDsetchunk(sdsid, chunk_def, HDF_CHUNK | HDF_COMP);                      
         ......                                                                  
        }                                                                           

        NOTE:
           This routine directly calls a Special Chunked Element fcn HMCxxx.

 RETURNS
        SUCCEED/FAIL

 AUTHOR 
        -GeorgeV
******************************************************************************/
intn 
SDsetchunk(int32         sdsid,     /* IN: sds access id */
           HDF_CHUNK_DEF chunk_def, /* IN: chunk definition */
           int32         flags      /* IN: flags */)
{
    CONSTR(FUNC, "SDsetchunk");    /* for HGOTO_ERROR */
    NC        *handle = NULL;      /* file handle */
    NC_var    *var    = NULL;      /* SDS variable */
    NC_attr  **fill_attr = NULL;   /* fill value attribute */
    HCHUNK_DEF chunk[1];           /* H-level chunk defintion */
    HDF_CHUNK_DEF *cdef   = NULL;  /* SD Chunk definition */
    model_info minfo;              /* dummy model info struct */
    comp_info  cinfo;              /* compression info - NBIT */
uint32 comp_config;
    int32     *cdims    = NULL;    /* array of chunk lengths */
    int32      fill_val_len = 0;   /* fill value length */
    void      *fill_val    = NULL; /* fill value */
    int32      ndims    = 0;       /* # dimensions i.e. rank */
    uint8      nlevels  = 1;       /* default # levels is 1 */
    int8       platntsubclass;     /* the machine type of the current platform */
    int8       outntsubclass;      /* the data's machine type */
    uintn      convert;            /* whether to convert or not */
    static     int32 tBuf_size = 0;/* statc conversion buffer size */
    static     void  *tBuf = NULL; /* static buffer used for conversion */
    intn       i;                  /* loop variable */
    intn       ret_value = SUCCEED;   /* return value */

#ifdef CHK_DEBUG
    fprintf(stderr,"SDsetchunk: called  \n");
#endif


     /* make sure this is cleared */
     memset(chunk,0,sizeof(chunk[0]));
    /* Check some args */

    /* get file handle and verify it is an HDF file 
       we only handle dealing with SDS only not coordinate variables */
    handle = SDIhandle_from_id(sdsid, SDSTYPE);
    if(handle == NULL || handle->file_type != HDF_FILE || handle->vars == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    /* get variable from id */
    var = SDIget_var(handle, sdsid);
    if(var == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    /* disallow setting chunk for SDS with rank = 0 - BMR, bug #1045 */
    if(var->shape == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    /* Decide type of defintion passed in  */
    switch (flags)
      {
      case HDF_CHUNK: /* case where chunk_def only has chunk lengths */
          cdef  = (HDF_CHUNK_DEF *)&chunk_def;
          cdims = cdef->chunk_lengths;
          chunk[0].chunk_flag = 0;  /* nothing set for this now */
          chunk[0].comp_type = COMP_CODE_NONE; /* nothing set */
          chunk[0].model_type = COMP_MODEL_STDIO; /* Default */
          chunk[0].cinfo = &cinfo; /* dummy */
          chunk[0].minfo = &minfo; /* dummy */
          break;
      case (HDF_CHUNK | HDF_COMP):
      /*  EIP 9/11/03 
       *  We have to take special care if SZIP library is not available;
       *  Fow all other compression types do
       */
          cdef  = (HDF_CHUNK_DEF *)&chunk_def;

    HCget_config_info( (comp_coder_t)cdef->comp.comp_type , &comp_config);
    if ((comp_config & COMP_DECODER_ENABLED|COMP_ENCODER_ENABLED) == 0) {
	/* coder not present?? */
	    HGOTO_ERROR(DFE_NOENCODER, FAIL);
    }
    if ((comp_config & COMP_ENCODER_ENABLED) == 0) {
	/* encoder not present?? */
	HGOTO_ERROR(DFE_BADCODER, FAIL);
    }
      if ((comp_coder_t)cdef->comp.comp_type != COMP_CODE_SZIP) {
          cdims = cdef->comp.chunk_lengths;
          chunk[0].chunk_flag = SPECIAL_COMP;  /* Compression */
          chunk[0].comp_type  = (comp_coder_t)cdef->comp.comp_type; 
          chunk[0].model_type = COMP_MODEL_STDIO; /* Default */
          chunk[0].cinfo = &cdef->comp.cinfo; 
          chunk[0].minfo = &minfo; /* dummy */
       }
       else /* requested compression is SZIP */

#ifdef H4_HAVE_LIBSZ          /* we have the library */
          {
           cdims = cdef->comp.chunk_lengths;
           chunk[0].chunk_flag = SPECIAL_COMP;  /* Compression */
           chunk[0].comp_type  = (comp_coder_t)cdef->comp.comp_type; 
           chunk[0].model_type = COMP_MODEL_STDIO; /* Default */
           chunk[0].minfo = &minfo; /* dummy */
    	   HDmemcpy(&cinfo,&(cdef->comp.cinfo),sizeof(comp_info));
	   if ( SDsetup_szip_parms( sdsid, handle, &cinfo, cdims) == FAIL ) {
	       HGOTO_ERROR(DFE_INTERNAL, FAIL);
	   }
              chunk[0].cinfo = &cinfo; 
          }
#else                         /* we do not have the SZIP library */
          {
		HGOTO_ERROR(DFE_BADCODER, FAIL);
          }
#endif /* H4_HAVE_LIBSZ */

          break;
      case (HDF_CHUNK | HDF_NBIT):
          cdef  = (HDF_CHUNK_DEF *)&chunk_def;
          cdims = cdef->nbit.chunk_lengths;
          chunk[0].chunk_flag = SPECIAL_COMP;  /* NBIT is a type of compression */
          chunk[0].comp_type  = COMP_CODE_NBIT;   /* Nbit compression? */
          chunk[0].model_type = COMP_MODEL_STDIO; /* Default */
          /* set up n-bit parameters */
          cinfo.nbit.nt        = var->HDFtype;
          cinfo.nbit.sign_ext  = cdef->nbit.sign_ext;
          cinfo.nbit.fill_one  = cdef->nbit.fill_one;
          cinfo.nbit.start_bit = cdef->nbit.start_bit;
          cinfo.nbit.bit_len   = cdef->nbit.bit_len;
          chunk[0].cinfo = &cinfo; 
          chunk[0].minfo = &minfo; /* dummy */
          break;
      default:
          ret_value = FAIL;
          goto done;
      }

#ifdef CHK_DEBUG
    fprintf(stderr,"SDsetchunk: does data ref exist?  \n");
#endif
    /* Does data exist yet */
    if(!var->data_ref) 
      {   /* doesn't exist */
#ifdef CHK_DEBUG
    fprintf(stderr,"SDsetchunk: data ref does not exist  \n");
#endif
          /* element doesn't exist so we need a reference number */
          var->data_ref=Hnewref(handle->hdf_file);
          if(var->data_ref == 0)
            {
#ifdef CHK_DEBUG
    fprintf(stderr,"SDsetchunk: failed to get data ref  \n");
#endif
              ret_value = FAIL;
              goto done;
            }
      } 
    else /* data ref exists, Error since can't convert existing SDS to chunked */
      {
          ret_value = FAIL;
          goto done;
      }

    /* Now start setting chunk info */
    ndims = var->assoc->count; /* set number of dims i.e. rank */
#ifdef CHK_DEBUG
    fprintf(stderr,"SDsetchunk: got data ref, ndims =%d  \n",ndims);
#endif

    /* allocate space for chunk dimensions */
    if ((chunk[0].pdims = (DIM_DEF *)HDmalloc(ndims*sizeof(DIM_DEF))) == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* initialize datset/chunk sizes using CHUNK defintion structure */
    chunk[0].chunk_size = 1;
    chunk[0].num_dims = ndims;
    for (i = 0; i < ndims; i++)
      {   /* get dimension length from shape arrays */
          /* check if dimension in unlimited since we don't 
             handle that yet */
          if (var->shape[i] != SD_UNLIMITED)
              chunk[0].pdims[i].dim_length = (int32) var->shape[i];
          else
            { /* UNLIMITED dimension case */
#ifdef CHK_DEBUG
    fprintf(stderr,"SDsetchunk: unlimited dimension case  \n");
    fflush(stderr);
#endif
                ret_value = FAIL;
                goto done;
            }

#ifdef CHK_DEBUG
    fprintf(stderr,"SDsetchunk: (int32) var->shape[%d]=%d\n",i,(int32) var->shape[i]);
    fflush(stderr);
#endif
          /* set chunk lengths */
          if (cdims[i] >= 1)
              chunk[0].pdims[i].chunk_length = cdims[i];
          else
            { /* chunk length is less than 1 */
#ifdef CHK_DEBUG
    fprintf(stderr,"SDsetchunk: chunk length less than 1, cdims[%d]=%d \n",i,cdims[i]);
    fflush(stderr);
#endif
                ret_value = FAIL;
                goto done;
            }
#ifdef CHK_DEBUG
    fprintf(stderr,"SDsetchunk: cdims[%d]=%d \n",i,cdims[i]);
    fflush(stderr);
#endif          
          /* Data distribution along dimensions 
          *  Check dimension length agains chunk length */
          if (cdims[i] == (int32)var->shape[i])
              chunk[0].pdims[i].distrib_type = 0;     /* NONE */
          else
              chunk[0].pdims[i].distrib_type = 1;     /* BLOCK */

          /* compute chunk size */
          chunk[0].chunk_size *= cdims[i];
      } /* end for ndims */

    /* Set number type size i.e. size of data type */
    chunk[0].nt_size = var->HDFsize;

#ifdef CHK_DEBUG
    fprintf(stderr,"SDsetchunk: var->HDFsize=%d\n",var->HDFsize);
    fflush(stderr);
#endif
    /* allocate space for fill value whose number type is the same as
       the dataset */
    fill_val_len = var->HDFsize;
    if ((fill_val = (void *)HDmalloc(fill_val_len)) == NULL)
      {
          ret_value = FAIL;
          goto done;
      }

    /* get fill value if one is set for this Dataset.
       The number type is the same as that for the dataset. */
    fill_attr = (NC_attr **) NC_findattr(&(var->attrs), _FillValue);
    if(fill_attr != NULL)
      {
          NC_copy_arrayvals((char *)fill_val, (*fill_attr)->data) ;    
      }
    else /* copy standard default fill value for now */
      {
        void *p = fill_val;

        switch(var->HDFtype & 0xff) 
          {
          case DFNT_CHAR:
          case DFNT_UCHAR:
              *((uint8 *)p) = FILL_CHAR;
              break;
          case DFNT_INT8:
          case DFNT_UINT8:
              *((int8 *)p) = FILL_BYTE;
              break;
          case DFNT_INT16:
          case DFNT_UINT16:
              *((int16 *)p) = FILL_SHORT;
              break;
          case DFNT_INT32:
          case DFNT_UINT32:
              *((int32 *)p) = FILL_LONG;
              break;
          case DFNT_FLOAT32:
              *((float32 *)p) = FILL_FLOAT;
              break;
          case DFNT_FLOAT64:
              *((float64 *)p) = FILL_DOUBLE;
              break;
        default:
            ret_value = FAIL;
            goto done;
        }
      }

    /* figure out if fill value has to be converted */
    if (FAIL == (platntsubclass = DFKgetPNSC(var->HDFtype, DF_MT)))
      {
          ret_value = FAIL;
          goto done;
      }
 
    if (DFKisnativeNT(var->HDFtype))
      {
        if (FAIL == (outntsubclass = DFKgetPNSC(var->HDFtype, DF_MT)))
          {
              ret_value = FAIL;
              goto done;
          }
       }
    else
      {
          outntsubclass = DFKislitendNT(var->HDFtype) ? DFNTF_PC : DFNTF_HDFDEFAULT;
      }
            
    convert = (uintn)(platntsubclass != outntsubclass);

    /* make sure our tmp buffer is big enough to hold fill value */
    if(convert && tBuf_size < fill_val_len) 
      {
          if(tBuf != NULL) 
              HDfree(tBuf);
          tBuf_size = fill_val_len;
          tBuf      = HDmalloc(tBuf_size);
          if(tBuf == NULL) 
            {
                tBuf_size = 0;
                ret_value    = FAIL;
                goto done;
            } /* end if */
      } /* end if */

#ifdef CHK_DEBUG
    fprintf(stderr,"SDsetchunk: get ready to create, convert=%d\n",convert);
#endif
    if (convert)
      { /* convert fill value */
          if (FAIL == DFKconvert(fill_val, tBuf, var->HDFtype,
                                (uint32) (fill_val_len/var->HDFsize), DFACC_WRITE, 0, 0))
            {
                ret_value    = FAIL;
                goto done;
            } 

        /* check to see already special.
           Error if already special since doubly special elements are
           not yet handled. HMCcreate should catch this....*/
        /* Create SDS as chunked element  */
        ret_value = HMCcreate(handle->hdf_file,       /* HDF file handle */
                           (uint16)DATA_TAG,       /* Data tag */
                           (uint16) var->data_ref, /* Data ref */
                           nlevels,                /* nlevels */
                           fill_val_len,           /* fill value length */
                           tBuf,           /* fill value */
                           (HCHUNK_DEF *)chunk      /* chunk definition */);
      }
    else /* no need to convert fill value */
      {
          /* check to see already special.
             Error if already special since doubly special elements are
             not yet handled. HMCcreate should catch this....*/
          /* Create SDS as chunked element  */
          ret_value = HMCcreate(handle->hdf_file,       /* HDF file handle */
                             (uint16)DATA_TAG,       /* Data tag */
                             (uint16) var->data_ref, /* Data ref */
                             nlevels,                /* nlevels */
                             fill_val_len,           /* fill value length */
                             fill_val,       /* fill value */
                             (HCHUNK_DEF *)chunk      /* chunk definition */);
      }

#ifdef CHK_DEBUG
    fprintf(stderr,"SDsetchunk: ret_value =%d \n", ret_value);
#endif
    /* check return */
    if(ret_value != FAIL) 
      { /* close old aid and set new one
         ..hmm......maybe this is for the doubly specail hack since
         this code framework came from SDsetcompress()....*/
          if((var->aid != 0) && (var->aid != FAIL))
            {
              if (Hendaccess(var->aid) == FAIL)
                  {
                      ret_value = FAIL;
                      goto done;
                  }
            }

          var->aid = ret_value;
          ret_value = SUCCEED; /* re-set to successful */
      } /* end if */

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */

    /* free fill value */
    if (fill_val != NULL)
        HDfree(fill_val);

    /* free chunk dims */
    if (chunk[0].pdims != NULL)
        HDfree(chunk[0].pdims);


    return ret_value;
} /* SDsetchunk */

/******************************************************************************
 NAME
     SDgetchunkinfo -- get Info on SDS

 DESCRIPTION
     This routine gets any special information on the SDS. If its chunked,
     chunked and compressed or just a regular SDS. Currently it will only
     fill the array of chunk lengths for each dimension as specified in
     the 'HDF_CHUNK_DEF' union. It does not tell you the type of compression
     or the compression parameters used. You can pass in a NULL for 'chunk_def'
     if don't want the chunk lengths for each dimension.
     If successfull it will return a bit-or'd value in 'flags' indicating 
     if the SDS is  chunked(HDF_CHUNK), chunked and compressed(HDF_CHUNK | HDF_COMP) 
     or non-chunked(HDF_NONE).
 
     e.g. 4x4 array - Pseudo-C
     {
     HDF_CHUNK_DEF rchunk_def;
     int32   cflags;
     ...
     SDgetchunkinfo(sdsid, &rchunk_def, &cflags);
     ...
     }

 RETURNS
        SUCCEED/FAIL

 AUTHOR 
        -GeorgeV
******************************************************************************/
intn 
SDgetchunkinfo(int32          sdsid,      /* IN: sds access id */
               HDF_CHUNK_DEF *chunk_def,  /* IN/OUT: chunk definition */
               int32         *flags       /* IN/OUT: flags */)
{
    NC       *handle = NULL;       /* file handle */
    NC_var   *var    = NULL;       /* SDS variable */
    sp_info_block_t info_block;    /* special info block */
    int16     special;             /* Special code */
    intn      i;                   /* loop variable */
    intn      ret_value = SUCCEED; /* return value */


    /* Check args */

    /* get file handle and verify it is an HDF file 
       we only handle dealing with SDS only not coordinate variables */
    handle = SDIhandle_from_id(sdsid, SDSTYPE);
    if(handle == NULL || handle->file_type != HDF_FILE || handle->vars == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    /* get variable from id */
    var = SDIget_var(handle, sdsid);
    if(var == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    /* Check to see if data aid exists? i.e. may need to create a ref for SDS */
    if(var->aid == FAIL && hdf_get_vp_aid(handle, var) == FAIL) 
      {
        ret_value = FAIL;
        goto done;
      }

    /* inquire about element */
    ret_value = Hinquire(var->aid, NULL, NULL, NULL, NULL, NULL, NULL, NULL, &special);
    if (ret_value != FAIL)
      {   /* make sure it is chunked element */
          if (special == SPECIAL_CHUNKED)
            { /* get info about chunked element */
             if ((ret_value = HDget_special_info(var->aid, &info_block)) != FAIL)
               {   /* Does user want chunk lengths back? */
                   if (chunk_def != NULL)
                     {
                         /* we assume user has allocat space for chunk lengths */
                         /* copy chunk lengths over */
                         for (i = 0; i < info_block.ndims; i++)
                           {
                               chunk_def->chunk_lengths[i] = info_block.cdims[i];
                           }
                     }
                   /* dont forget to free up info is special info block 
                      This space was allocated by the library */
                   HDfree(info_block.cdims);

                   /* Check to see if compressed.
                      Currently we don't fill in the 'comp' structure 
                      because currently only the information about the 
                      compression type is available in get compression
                      info code and not the parameters that went along. */
                   switch(info_block.comp_type)
                     {
                     case COMP_CODE_NONE:
                         *flags = HDF_CHUNK;
                         break;
                     case COMP_CODE_NBIT:
                         *flags = (HDF_CHUNK | HDF_NBIT);
                         break;
                     default:
                         *flags = (HDF_CHUNK | HDF_COMP);
                         break;
                     }
               }
            }
          else /* not special chunked element */
            {
              *flags = HDF_NONE; /* regular SDS */
            }
      }

  done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */


    return ret_value;
} /* SDgetchunkinfo() */


/******************************************************************************
 NAME
     SDwritechunk   -- write the specified chunk to the SDS

 DESCRIPTION
     This routine writes a whole chunk of data to the chunked SDS 
     specified by chunk 'origin' for the given SDS and can be used
     instead of SDwritedata() when this information is known. This
     routine has less overhead and is much faster than using SDwritedata().

     Origin specifies the co-ordinates of the chunk according to the chunk
     position in the overall chunk array.

     'datap' must point to a whole chunk of data.

     See SDsetchunk() for a description of the organization of chunks in an SDS.

     NOTE:
           This routine directly calls a Special Chunked Element fcn HMCxxx.

 RETURNS
        SUCCEED/FAIL

 AUTHOR 
       -GeorgeV
******************************************************************************/
intn 
SDwritechunk(int32       sdsid, /* IN: access aid to SDS */
             int32      *origin,/* IN: origin of chunk to write */
             const void *datap  /* IN: buffer for data */)
{
    CONSTR(FUNC, "SDwritechunk");    /* for HGOTO_ERROR */
    NC        *handle = NULL;   /* file handle */
    NC_var    *var    = NULL;   /* SDS variable */
    int16      special;         /* Special code */
    int32      csize;           /* phsical chunk size */
    uint32     byte_count;      /* bytes to write */
    int8       platntsubclass;  /* the machine type of the current platform */
    int8       outntsubclass;   /* the data's machine type */
    uintn      convert;         /* whether to convert or not */
    comp_coder_t comp_type;
    comp_info c_info;
    uint32  comp_config;
    int32 status;
    intn       i;
    sp_info_block_t info_block; /* special info block */
    static uint32 tBuf_size = 0; /* statc conversion buffer size */
    static void  *tBuf = NULL;   /* static buffer used for conversion */
    intn       ret_value = SUCCEED;


    info_block.cdims = NULL;

    /* Check args */
    if (origin == NULL || datap == NULL)
        HGOTO_ERROR(DFE_ARGS, FAIL);

    /* get file handle and verify it is an HDF file 
       we only handle writinng to SDS only not coordinate variables */
    handle = SDIhandle_from_id(sdsid, SDSTYPE);
    if(handle == NULL || handle->file_type != HDF_FILE || handle->vars == NULL)
      {
        ret_value = FAIL;
        goto done;
      }


    /* get variable from id */
    var = SDIget_var(handle, sdsid);
    if(var == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    /* Check to see if data aid exists? i.e. may need to create a ref for SDS */
    if(var->aid == FAIL && hdf_get_vp_aid(handle, var) == FAIL) 
      {
        ret_value = FAIL;
        goto done;
      }

    /* Check compression method is enabled */
    status = HCPgetcompress(handle->hdf_file, var->data_tag, var->data_ref, 
		&comp_type, &c_info);

    if (status != FAIL) {
	    HCget_config_info( comp_type , &comp_config);
	    if ((comp_config & COMP_DECODER_ENABLED|COMP_ENCODER_ENABLED) == 0) {
		/* coder not present?? */
		    HGOTO_ERROR(DFE_NOENCODER, FAIL);
	    }
	    if ((comp_config & COMP_ENCODER_ENABLED) == 0) {
		/* encoder not present?? */
		HGOTO_ERROR(DFE_BADCODER, FAIL);
	    }
    }

    /* inquire about element */
    ret_value = Hinquire(var->aid, NULL, NULL, NULL, NULL, NULL, NULL, NULL, &special);
    if (ret_value != FAIL)
      {
          if (special == SPECIAL_CHUNKED)
            {  /* yes */

                /* get ready to write */
                handle->xdrs->x_op = XDR_ENCODE;

                /* get info about chunked element */
                if ((ret_value = HDget_special_info(var->aid, &info_block)) != FAIL)
                  {   
                      /* calcualte chunk size  */
                      csize = 1;
                      for (i = 0; i < info_block.ndims; i++)
                          csize *= info_block.cdims[i];

                      /* adjust for number type size */
                      csize *= var->HDFsize;

                      /* figure out if data needs to be converted */
                      byte_count = csize;

                      if (FAIL == (platntsubclass = DFKgetPNSC(var->HDFtype, DF_MT)))
                        {
                            ret_value = FAIL;
                            goto done;
                        }

                      if (DFKisnativeNT(var->HDFtype))
                        {
                            if (FAIL == (outntsubclass = DFKgetPNSC(var->HDFtype, DF_MT)))
                              {
                                  ret_value = FAIL;
                                  goto done;
                              }
                        }
                      else
                        {
                            outntsubclass = DFKislitendNT(var->HDFtype) ? DFNTF_PC : DFNTF_HDFDEFAULT;
                        }

                      convert = (uintn)(platntsubclass != outntsubclass);

                      /* make sure our tmp buffer is big enough to hold everything */
                      if(convert && tBuf_size < byte_count) 
                        {
                            if(tBuf != NULL) 
                                HDfree(tBuf);
                            tBuf_size = byte_count;
                            tBuf      = HDmalloc(tBuf_size);
                            if(tBuf == NULL) 
                              {
                                  tBuf_size = 0;
                                  ret_value    = FAIL;
                                  goto done;
                              } /* end if */
                        } /* end if */

                      /* Write chunk out, */
                      if(convert) 
                        {
#ifdef CHK_DEBUG
        fprintf(stderr,"SDwritechunk: convert, var->HDFsize=%d, var->HDFtype=%d \n",
                var->HDFsize, var->HDFtype);
#endif
                            /* convert it */
                            if (FAIL == DFKconvert((VOIDP)datap, tBuf, var->HDFtype,
                                                  (byte_count/var->HDFsize), DFACC_WRITE, 0, 0))
                              {
                                  ret_value    = FAIL;
                                  goto done;
                              } 

                            /* write it out now */
                            if ((ret_value = HMCwriteChunk(var->aid, origin, tBuf)) 
                                != FAIL)
                              {
                                  ret_value = SUCCEED;
                              }

                            goto done; /* done */
                        } /* end if */
                      else 
                        {
                          if ((ret_value = HMCwriteChunk(var->aid, origin, datap)) 
                              != FAIL)
                            {
                                ret_value = SUCCEED;
                            }

                          goto done; /* done */
                        }
                  } /* end if get special info block */
            }
          else /* not special CHUNKED */
              ret_value = FAIL;
      } /* end if Hinquire */

  done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */
    /* dont forget to free up info is special info block 
       This space was allocated by the library */
    if (info_block.cdims != NULL)
        HDfree(info_block.cdims);


    return ret_value;
} /* SDwritechunk() */

/******************************************************************************
 NAME
     SDreadchunk   -- read the specified chunk to the SDS

 DESCRIPTION
     This routine reads a whole chunk of data from the chunked SDS
     specified by chunk 'origin' for the given SDS and can be used
     instead of SDreaddata() when this information is known. This
     routine has less overhead and is much faster than using SDreaddata().

     Origin specifies the co-ordinates of the chunk according to the chunk
     position in the overall chunk array.

     'datap' must point to a whole chunk of data.

     See SDsetchunk() for a description of the organization of chunks in an SDS.

     NOTE:
         This routine directly calls a Special Chunked Element fcn HMCxxx.

 RETURNS
        SUCCEED/FAIL

 AUTHOR 
       -GeorgeV
******************************************************************************/
intn 
SDreadchunk(int32  sdsid,  /* IN: access aid to SDS */
            int32 *origin, /* IN: origin of chunk to write */
            void  *datap   /* IN/OUT: buffer for data */)
{
    CONSTR(FUNC, "SDreadchunk");    /* for HGOTO_ERROR */
    NC        *handle = NULL;   /* file handle */
    NC_var    *var    = NULL;   /* SDS variable */
    int16      special;         /* Special code */
    int32      csize;           /* phsical chunk size */
    uint32     byte_count;      /* bytes to read */
    int8       platntsubclass;  /* the machine type of the current platform */
    int8       outntsubclass;   /* the data's machine type */
    uintn      convert;         /* whether to convert or not */
    comp_coder_t comp_type;
    comp_info c_info;
    uint32  comp_config;
    int32 status;
    intn       i;
    sp_info_block_t info_block; /* special info block */
    static uint32 tBuf_size = 0; /* statc conversion buffer size */
    static void  *tBuf = NULL; /* static buffer used for conversion */
    intn       ret_value = SUCCEED;


    info_block.cdims = NULL;

    /* Check args */
    if (origin == NULL || datap == NULL)
        HGOTO_ERROR(DFE_ARGS, FAIL);

    /* get file handle and verify it is an HDF file 
       we only handle reading from SDS only not coordinate variables */
    handle = SDIhandle_from_id(sdsid, SDSTYPE);
    if(handle == NULL || handle->file_type != HDF_FILE || handle->vars == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    /* get variable from id */
    var = SDIget_var(handle, sdsid);
    if(var == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    /* Check to see if data aid exists? i.e. may need to create a ref for SDS */
    if(var->aid == FAIL && hdf_get_vp_aid(handle, var) == FAIL) 
      {
        ret_value = FAIL;
        goto done;
      }

    /* Check compression method is enabled */
    status = HCPgetcompress(handle->hdf_file, var->data_tag, var->data_ref, 
		&comp_type, &c_info);

    if (status != FAIL) {
	    HCget_config_info( comp_type , &comp_config);
	    if ((comp_config & COMP_DECODER_ENABLED|COMP_ENCODER_ENABLED) == 0) {
		/* coder not present?? */
		    HGOTO_ERROR(DFE_NOENCODER, FAIL);
	    }
	    if ((comp_config & COMP_DECODER_ENABLED) == 0) {
		/* decoder not present?? */
		HGOTO_ERROR(DFE_BADCODER, FAIL);
	    }
    }

    /* inquire about element */
    ret_value = Hinquire(var->aid, NULL, NULL, NULL, NULL, NULL, NULL, NULL, &special);
    if (ret_value != FAIL)
      {
          if (special == SPECIAL_CHUNKED)
            {  /* yes */
                /* get ready to read */
                handle->xdrs->x_op = XDR_DECODE;

                /* get info about chunked element */
                if ((ret_value = HDget_special_info(var->aid, &info_block)) != FAIL)
                  {   
                      /* calcualte chunk size  */
                      csize = 1;
                      for (i = 0; i < info_block.ndims; i++)
                          csize *= info_block.cdims[i];

                      /* adjust for number type size */
                      csize *= var->HDFsize;

                      /* figure out if data needs to be converted */
                      byte_count = csize;

                      if (FAIL == (platntsubclass = DFKgetPNSC(var->HDFtype, DF_MT)))
                        {
                            ret_value = FAIL;
                            goto done;
                        }

                      if (DFKisnativeNT(var->HDFtype))
                        {
                            if (FAIL == (outntsubclass = DFKgetPNSC(var->HDFtype, DF_MT)))
                              {
                                  ret_value = FAIL;
                                  goto done;
                              }
                        }
                      else
                        {
                            outntsubclass = DFKislitendNT(var->HDFtype) ? DFNTF_PC : DFNTF_HDFDEFAULT;
                        }

                      convert = (uintn)(platntsubclass != outntsubclass);

                      /* make sure our tmp buffer is big enough to hold everything */
                      if(convert && tBuf_size < byte_count) 
                        {
                            if(tBuf != NULL) 
                                HDfree(tBuf);
                            tBuf_size = byte_count;
                            tBuf      = HDmalloc(tBuf_size);
                            if(tBuf == NULL) 
                              {
                                  tBuf_size = 0;
                                  ret_value = FAIL;
                                  goto done;
                              } /* end if */
                        } /* end if */

                      /* read chunk in */
                      if(convert) 
                        {
#ifdef CHK_DEBUG
        fprintf(stderr,"SDreadchunk: convert, var->HDFsize=%d, var->HDFtype=%d \n",
                var->HDFsize, var->HDFtype);
#endif
                            /* read it in */
                            if ((ret_value = HMCreadChunk(var->aid, origin, tBuf)) 
                                != FAIL)
                                {
                                    /* convert chunk */
                                    if (FAIL == DFKconvert(tBuf, datap, var->HDFtype,
                                                         (byte_count/var->HDFsize), DFACC_READ, 0, 0))
                                      {
                                          ret_value = FAIL;
                                          goto done;
                                      }

                                    ret_value = SUCCEED;
                                }

                            goto done; /* done */
                        } /* end if */
                      else 
                        {
                          if ((ret_value = HMCreadChunk(var->aid, origin, datap))
                              != FAIL)
                            {
                              ret_value = SUCCEED;
                            }

                          goto done; /* done */
                        }
                  } /* end if get special info block */
            }
          else /* not special CHUNKED */
              ret_value = FAIL;
      } /* end if Hinquire */

  done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */
    /* dont forget to free up info is special info block 
       This space was allocated by the library */
    if (info_block.cdims != NULL)
        HDfree(info_block.cdims);


    return ret_value;
} /* SDreadchunk() */

/******************************************************************************
NAME
     SDsetchunkcache - maximum number of chunks to cache 

DESCRIPTION
     Set the maximum number of chunks to cache.

     The cache contains the Least Recently Used(LRU cache replacment policy) 
     chunks. This routine allows the setting of maximum number of chunks that 
     can be cached, 'maxcache'.

     The performance of the SDxxx interface with chunking is greatly
     affected by the users access pattern over the dataset and by
     the maximum number of chunks set in the chunk cache. The number chunks 
     that can be set in the cache is process memory limited. It is a good 
     idea to always set the maximum number of chunks in the cache as the 
     default heuristic does not take into account the memory available for 
     the application.

     By default when the SDS is promoted to a chunked element the 
     maximum number of chunks in the cache 'maxcache' is set to the number of
     chunks along the last dimension.

     The values set here affects the current SDS object's caching behaviour.

     If the chunk cache is full and 'maxcache' is greater then the
     current 'maxcache' value, then the chunk cache is reset to the new
     'maxcache' value, else the chunk cache remains at the current
     'maxcache' value.

     If the chunk cache is not full, then the chunk cache is set to the
     new 'maxcache' value only if the new 'maxcache' value is greater than the
     current number of chunks in the cache.

     Use flags argument of 'HDF_CACHEALL' if the whole object is to be cached 
     in memory, otherwise pass in zero(0). Currently you can only
     pass in zero.

     See SDsetchunk() for a description of the organization of chunks in an SDS.
     
     NOTE:
          This routine directly calls a Special Chunked Element fcn HMCxxx.

RETURNS
     Returns the 'maxcache' value for the chunk cache if successful 
     and FAIL otherwise

AUTHOR 
      -GeorgeV
******************************************************************************/
intn
SDsetchunkcache(int32 sdsid,     /* IN: access aid to mess with */
                int32 maxcache,  /* IN: max number of chunks to cache */
                int32 flags      /* IN: flags = 0, HDF_CACHEALL */)
{
    NC       *handle = NULL;        /* file handle */
    NC_var   *var    = NULL;        /* SDS variable */
    int16     special;              /* Special code */
    intn      ret_value = SUCCEED;


    /* Check args */
    if (maxcache < 1 )
      {
        ret_value = FAIL;
        goto done;
      }

    if (flags != 0 && flags != HDF_CACHEALL)
      {
        ret_value = FAIL;
        goto done;
      }

    /* get file handle and verify it is an HDF file 
       we only handle dealing with SDS only not coordinate variables */
    handle = SDIhandle_from_id(sdsid, SDSTYPE);
    if(handle == NULL || handle->file_type != HDF_FILE || handle->vars == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    /* get variable from id */
    var = SDIget_var(handle, sdsid);
    if(var == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    /* Check to see if data aid exists? i.e. may need to create a ref for SDS */
    if(var->aid == FAIL && hdf_get_vp_aid(handle, var) == FAIL) 
      {
        ret_value = FAIL;
        goto done;
      }

    /* inquire about element */
    ret_value = Hinquire(var->aid, NULL, NULL, NULL, NULL, NULL, NULL, NULL, &special);
    if (ret_value != FAIL)
      {
          if (special == SPECIAL_CHUNKED)
              ret_value = HMCsetMaxcache(var->aid, maxcache, flags); /* set cache*/
          else
              ret_value = FAIL;
      }

  done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */

    return ret_value;
} /* SDsetchunkcache() */


/******************************************************************************
 NAME
	SDcheckempty -- checks whether an SDS is empty

 DESCRIPTION
    Given an sdsid, set the second parameter, emptySDS, to TRUE if the 
    SDS has not been written with data, and FALSE, otherwise. 

 RETURNS
    SUCCEED/FAIL

 AUTHOR
    bmribler - 9-01-98
        
 MODIFICATION
    bmribler - 9/29/2004
        When the SDS is not a special element, we only need to check
        its data ref# to decide whether it has data written, but
        when the SDS is a special element, it still has a valid
        data ref# even though it doesn't have data, we'll then need
        to perform a more detailed check.  Added more detailed checks.

******************************************************************************/
int32
SDcheckempty(int32 sdsid,  /* IN: dataset ID */
	     intn  *emptySDS /* TRUE if SDS is empty */)
{
    CONSTR(FUNC, "SDcheckempty");	/* for HGOTO_ERROR */
    NC     *handle = NULL;		/* file record struct */
    NC_var *var = NULL;			/* variable record struct */
    int32   ret_value = SUCCEED;

#ifdef SDDEBUG
    fprintf(stderr, "SDcheckempty: I've been called\n");
#endif

    /* get the handle */
    handle = SDIhandle_from_id(sdsid, SDSTYPE);
    if(handle == NULL) 
      {
        ret_value = FAIL;
        goto done;
      }

    /* get the variable */
    var = SDIget_var(handle, sdsid);
    if(var == NULL)
      {
        ret_value = FAIL;
        goto done;
      }

    /* assume that the SDS is not empty until proving otherwise */
    *emptySDS = FALSE;

    /* if the data ref# of the SDS is 0, it indicates that the SDS has 
	not been written with data because no storage is created 
	for the SDS data */
    if (var->data_ref == 0)
    {
        *emptySDS = TRUE;
    }
    else
    { /* data_ref is not 0, so must check on special SDSs to determine if
	 the SDS is empty */
	ret_value = HDcheck_empty(handle->hdf_file, var->data_tag, 
				  var->data_ref, emptySDS);
	if (ret_value == FAIL) HGOTO_ERROR(DFE_INTERNAL, FAIL);
    } /* var->data_ref != 0 */

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */
    return ret_value;
} /* SDcheckempty */

/******************************************************************************
 NAME
	SDidtype -- returns the type of an id

 DESCRIPTION
    Given an id, return its type, which is either an SD id, an SDS id, or
    a dimension id, or indicate that it is not a valid SD API id.

 RETURNS
    A value of type hdf_idtype_t, which can be either of the following:
    SD_ID, SDS_ID, DIM_ID, NOT_SDAPI_ID.

 AUTHOR
    bmribler - 1-19-2005
        
 MODIFICATION

******************************************************************************/
hdf_idtype_t SDidtype(int32 an_id)
{
    NC     *handle = NULL;	/* file record struct */
    hdf_idtype_t ret_value = NOT_SDAPI_ID;

#ifdef SDDEBUG
    fprintf(stderr, "SDidtype: I've been called\n");
#endif

    /* Assuming that the id is an SD id, get and check the handle */
    handle = SDIhandle_from_id(an_id, CDFTYPE);

    /* If it is, indicate so */
    if(handle != NULL)
	ret_value = SD_ID;

    /* otherwise, check further... */
    else
    {
	/* Assuming that it is an SDS id, get and check the handle */
	handle = SDIhandle_from_id(an_id, SDSTYPE);

	/* If it is, indicate so */
	if(handle != NULL)
            ret_value = SDS_ID;

	/* otherwise, check if it is a dimension id, or just not valid */
	else
	{
            handle = SDIhandle_from_id(an_id, DIMTYPE);
            if(handle != NULL)
		ret_value = DIM_ID;
	    else
		ret_value = NOT_SDAPI_ID;
        }
    }
done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */
    return ret_value;
} /* SDidtype */

#endif /* HDF */
