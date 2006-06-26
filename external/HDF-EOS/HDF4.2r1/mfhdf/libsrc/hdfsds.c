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
static char RcsId[] = "@(#)$Revision: 1.44 $";
#endif

/* $Id: hdfsds.c,v 1.44 1998/02/02 21:50:51 smitchel Exp $ */

/***************************************************************************
 *
 * This file contains the code to read old SDS objects out of HDF files
 *  an pretend that they are netCDF / mutli-file SDS data objects
 *
 **************************************************************************/

/***************************************************************************
  General Thoughts


  Will need to add a 'type' field to the variable stucture so that we can
  how it was stored (basically will want to just store the tag of the object
  as well as the ref (which is already stored)).
  

  We can use the LUF record to get names of dimensions.  If no info stored
  int the LUF then we will need to fake it.

  Will prolly be easiest to define a coordinate variable for every dimension 
  even if it doesn't need any attributes or data values

  Scales are a problem.  In HDF scales, are used to define the values for
  the coordinate variables.  The problem is that all of the values for the
  scales in HDF are jammed into the same tag's storage.  So, we will need
  to add an 'offset' field to the variable record so that we know where to 
  start reading to get the data.  If the offset == 0 then we should start
  reading from the beginning.  If the offset == -1 then there is *NO* data
  (since a scale may not have data specified for it).

 LOCAL ROUTINES
 ==============
 hdf_query_seen_sdg
 hdf_register_seen_sdg
 hdf_read_ndgs

 PUBLIC ROUTINES
 ===============
 hdf_read_sds_ndgs


 NOTE: This file could use the current comments updated and more
       comments in parts of the code. I tried my best. -GV 9/10/97

 **************************************************************************/

#if (defined macintosh) || (defined MPW)
#define HDF  /* Need this for the Macintosh, avoid putting on compile line */
#endif

#ifdef HDF
#include        "local_nc.h"

#define SDG_MAX_INITIAL 100

/* local variables */
PRIVATE intn       sdgCurrent;
PRIVATE intn       sdgMax;
PRIVATE uint16     *sdgTable=NULL;
PRIVATE uint8      *ptbuf = NULL;

/* Local routines */
PRIVATE intn hdf_query_seen_sdg
    (uint16 ndgRef);

PRIVATE intn hdf_register_seen_sdg
    (uint16 ndgRef);

PRIVATE intn hdf_read_ndgs
    (NC *handle);

/******************************************************************************
 NAME
   hdf_query_seen_sdg

 DESCRIPTION
   The SDG with the given ref number might be part of an SDG-NDG combo
   if so, we return TRUE else FALSE.
 
   Attempt to discover if we've seen it in the most inefficient manner
   possible

 RETURNS
   TRUE / FALSE

******************************************************************************/
PRIVATE intn 
hdf_query_seen_sdg(uint16 ndgRef)
{
    intn i;

    if(!sdgTable) 
        return FALSE;

    for(i = 0; i < sdgCurrent; i++)
      {
        if(sdgTable[i] == ndgRef) 
            return TRUE;
      }

    return FALSE;
} /* hdf_query_seen_sdg */


/******************************************************************************
 NAME
   hdf_register_seen_sdg

 DESCRIPTION
   The SDG with the given ref is in an SDG-NDG combo.  Add it to the seen table
   so we don't read it twice

 RETURNS
   SUCCEED / FAIL

******************************************************************************/
PRIVATE intn 
hdf_register_seen_sdg(uint16 sdgRef)
{
    intn ret_value = SUCCEED;

    /* check if table is allocated */
    if(!sdgTable) 
      {
          sdgMax = SDG_MAX_INITIAL;
          sdgTable = (uint16 *) HDmalloc(sdgMax * sizeof(uint16));
          if (sdgTable == NULL)
            {
                ret_value = FAIL;
                goto done;
            }
          sdgCurrent = 0;
      }
    
    /* add ref to table */
    sdgTable[sdgCurrent++] = sdgRef;
    
    /* check if we need to increase size of table */
    if(sdgCurrent == sdgMax) 
      {
          sdgMax *= 2;
          sdgTable = (uint16 *) HDrealloc((VOIDP) sdgTable, sdgMax * sizeof(uint16));
          if (sdgTable == NULL)
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
} /* hdf_register_seen_sdg */


/******************************************************************************
 NAME
   hdf_read_ndgs
 
 DESCRIPTION
   Loop through all of the NDGs in the file and create data structures for 
   them

 RETURNS
   SUCCEED / FAIL
 
******************************************************************************/
PRIVATE intn 
hdf_read_ndgs(NC *handle)
{
    static const char *FUNC = "hdf_read_ndg_dims";
    char     tmpname[80] = "";
    uint8    ntstring[4] = "";
    intn     dimcount;
    intn     dimattrcnt;
    /* info about NDG structure */
    int32    GroupID;
    int32    aid;
    int32    aid1;
    uint16   ndgTag;
    uint16   ndgRef;
    uint16   sddRef;
    uint16   lRef;
    uint16   uRef;
    uint16   fRef;
    uint16   sRef;
    uint16   sdRef;
    uint16   tmpTag;
    uint16   tmpRef;
    int16    rank=0;
    nc_type  type;
    int32   *dimsizes = NULL;
    int32   *scaletypes = NULL;
    int32    HDFtype;
    intn     dim;
    intn     max_thangs;
    intn     current_dim;
    intn     current_var;
    intn     current_attr;
    intn    *vardims = NULL;
    /* info about netCDF structures */
    NC_dim  **dims = NULL;   /* hold list of dimensions as we create it */
    NC_var  **vars = NULL;   /* hold variable list as we create it      */
    NC_attr **attrs = NULL;  /* hold attribute list as we create it     */
    NC_attr  *dimattrs[10];  /* for LUF and anno_label, anno_desc, 10 is enough */
    uint8    *labelbuf = NULL; /* label should not be used as var name due to non-uniqueness  */
    uint8    *scalebuf = NULL; /* buffer to store scale info */
    uint8    *unitbuf = NULL;  /* buffer to store unit info */
    uint8    *formatbuf = NULL; /* buffer to store format info */
    uint8    *coordbuf = NULL; /* buffer to store coord system info */
    intn      new_dim;   /* == new dim so create coord variable     */
    /* random book-keeping */
    intn      i;
    intn      status;
    intn      tag_index;
    uint8    *p = NULL;
    uint8     tBuf[128] = "";
    intn      scale_offset;   /* current offset into the scales record for the
                            current dimension's values */
    intn      ret_value = SUCCEED;

    /*
     *  Allocate the array to store the dimensions
     */
    max_thangs  = 100; /* what is this limit ? */

    dims = (NC_dim **) HDmalloc(sizeof(NC_dim *) * max_thangs);
    if(NULL == dims) 
      {
          HERROR(DFE_NOSPACE);
          ret_value = FAIL;
          goto done;
      }
    
    vars = (NC_var **) HDmalloc(sizeof(NC_var *) * max_thangs);
    if(NULL == vars) 
      {
          HERROR(DFE_NOSPACE);
          ret_value = FAIL;
          goto done;
      }

    attrs = (NC_attr **) HDmalloc(sizeof(NC_attr *) * max_thangs);
    if(NULL == attrs) 
      {
          HERROR(DFE_NOSPACE);
          ret_value = FAIL;
          goto done;
      }

    /* Check if temproray buffer has been allocated */
    if (ptbuf == NULL)
      {
          ptbuf = (uint8 *)HDmalloc(TBUF_SZ * sizeof(uint8));
          if (ptbuf == NULL)
            {
                HERROR(DFE_NOSPACE);
                ret_value = FAIL;
                goto done;
            }
      }

    /* no dimensions or variables yet */
    current_dim = 0;
    current_var = 0;
    dimcount = 0;

    for(tag_index = 0; tag_index < 2; tag_index++) 
      {

          if(tag_index == 0)
              ndgTag = DFTAG_NDG;
          else
              ndgTag = DFTAG_SDG;

          /*
           * Start from the beginning and look though
           */
          aid = Hstartread(handle->hdf_file, ndgTag, DFREF_WILDCARD);
          if(aid == FAIL) 
              continue; /* hmm if we fail we continue the loop? */
        
          /*
           * Loop through all of the NDGs in the file
           */
          status = SUCCEED;
          while (status == SUCCEED) 
            {
                uint16 ntTag;
                uint16 ntRef;
            
                if(HQuerytagref(aid, &ndgTag, &ndgRef) == FAIL) 
                  {
#ifdef DEBUG
                      fprintf(stderr, "Call to Hinquire failed\n");
#endif
                      ret_value = FAIL;
                      goto done;
                  }

                /* Test if its an SDG-NDG which we've processed already */
                if((ndgTag == DFTAG_SDG) && (hdf_query_seen_sdg(ndgRef))) 
                  {
                      status = Hnextread(aid, ndgTag, DFREF_WILDCARD, DF_CURRENT);  
                      continue; /* go to next element */
                  }
                
                /* OK, now we need to get the relevant dimension structure */
#ifdef DEBUG
                printf("Found NDG at %d %d\n", ndgTag, ndgRef);
#endif
            
                /* read the group into memory */
                if ((GroupID = DFdiread(handle->hdf_file, ndgTag, ndgRef)) < 0) 
                  {
                      ret_value = FAIL;
                      goto done;
                  }
            
                sddRef = lRef = uRef = fRef = sRef = sdRef = 0;
            
                /* default number type is Float32 */
                type    = NC_FLOAT; 
                HDFtype = DFNT_FLOAT32;
            
                /* no attributes found yet */
                current_attr = 0;
            
                /* no meta-data seen yet */
                labelbuf = unitbuf = scalebuf = formatbuf = NULL;
            
                /*
                 * Loop through the members of the group looking for stuff
                 *
                 * NOTE:  Only generate attributes for meta-data which does
                 *        not depend on the rank of the data since we can not
                 *        be sure that we get the rank first.  
                 *        If the meta-data depends on the rank, just remember the
                 *        ref number and read the element once this while loop
                 *        is finished.
                 */
                while (!DFdiget(GroupID, &tmpTag, &tmpRef)) 
                  {
                      switch(tmpTag) 
                        {
                        case DFTAG_SDD:
                            aid1 = Hstartread(handle->hdf_file, tmpTag, tmpRef);
                            if (aid1 == FAIL)
                              {
                                  ret_value = FAIL;
                                  goto done;
                              }

                            /* read rank */
                            if (Hread(aid1, (int32) 2, ptbuf) == FAIL) 
                              {
                                  ret_value = FAIL;
                                  goto done;
                              }

                            p = ptbuf;
                            INT16DECODE(p, rank);
                    
                            /* get space for dimensions */
                            dimsizes = (int32 *) HDmalloc((uint32) rank * sizeof(int32));
                            if (dimsizes == NULL) 
                              {
                                  ret_value = FAIL;
                                  goto done;
                              }

                            vardims = (intn *) HDmalloc((uint32) rank * sizeof(intn));
                            if (vardims == NULL) 
                              {
                                  ret_value = FAIL;
                                  goto done;
                              }

                            scaletypes = (int32 *) HDmalloc((uint32) rank * sizeof(int32));
                            if (scaletypes == NULL) 
                              {
                                  ret_value = FAIL;
                                  goto done;
                              }
                    
                            /* read dimension record */
                            if (Hread(aid1, (int32) 4 * rank, ptbuf) == FAIL) 
                              {
                                  ret_value = FAIL;
                                  goto done;
                              }

                            p = ptbuf;
                            for (i = 0; i < rank; i++)
                                INT32DECODE(p, dimsizes[i]);
                    
                          /* read tag/ref of NT */
                            if (Hread(aid1,(int32) 4,  ptbuf) == FAIL) 
                              {
                                  ret_value = FAIL;
                                  goto done;
                              }
                            p = ptbuf;
                            UINT16DECODE(p, ntTag);
                            UINT16DECODE(p, ntRef);
                    
                            /* read actual NT */
                            if (Hgetelement(handle->hdf_file, ntTag, ntRef, ntstring) == FAIL)
                              {
                                  ret_value = FAIL;
                                  goto done;
                              }
                    
                            HDFtype = ntstring[1];
                            if ((type = hdf_unmap_type(HDFtype)) == FAIL)
                              {
#ifdef DEBUG
                                  /* replace it with NCAdvice or HERROR? */
                                  fprintf(stderr "hdf_read_ndgs: hdf_unmap_type failed for %d\n", HDFtype);
#endif
                                  ret_value = FAIL;
                                  goto done;
                              }

                            /* test if data was stored in native format of different 
                             machine or in the LITEND format, and make sure the
                             numbertype version numbers are the same */
                            if ((ntstring[0] != DFNT_VERSION) 
                                || ((ntstring[3] != DFNTF_NONE) 
                                    && (ntstring[3] != DFNTF_IEEE)))  
                              {
                                  if (ntstring[3] == DFNTF_PC)  /* Little Endian */
                                      HDFtype |= DFNT_LITEND;
                                  else  
                                    {   /* same machine type?  */
                                        if (ntstring[3] == DFKgetPNSC(HDFtype, DF_MT))  
                                            HDFtype |= DFNT_NATIVE;
                                        else  /* different machine */
                                          {
                                              ret_value = FAIL;
                                              goto done;
                                          }
                                    }  /* machine type */
                              }   /* Little Endian */

                            /* read in scale NTs */
                            for(i = 0; i < rank; i++) 
                              {
                                  if (Hread(aid1,(int32) 4,  ptbuf) == FAIL) 
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }

                                  p = ptbuf;
                                  UINT16DECODE(p, ntTag);
                                  UINT16DECODE(p, ntRef);
                        
                                  /* read NT of this scale (dimension) */
                                  if (Hgetelement(handle->hdf_file, ntTag, ntRef, ntstring) == FAIL)
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }
                        
                                  scaletypes[i] = ntstring[1];
                                  /* check native format and LITEND */
                                  if ((ntstring[0] != DFNT_VERSION) 
                                      || ((ntstring[3] != DFNTF_NONE) 
                                          && (ntstring[3] != DFNTF_IEEE)))  
                                    {
                                        if (ntstring[3] == DFNTF_PC)  /* Little Endian */
                                            scaletypes[i] |= DFNT_LITEND;
                                        else  
                                          {   /* same machine type?  */
                                              if (ntstring[3] == DFKgetPNSC(HDFtype, DF_MT))
                                                  scaletypes[i] |= DFNT_NATIVE;
                                              else  /* different machine */
                                                {
                                                    ret_value = FAIL;
                                                    goto done;
                                                }
                                          }  /* scale machine type */
                                    }    /* Little Endian */
                              }
                    
                            sddRef = tmpRef;    /* prepare for a new dim var */
                            if (Hendaccess(aid1) == FAIL)
                              {
                                  ret_value = FAIL;
                                  goto done;
                              }
                    
                            break;
                    
                        case DFTAG_SDL:       /* label */
                            lRef = tmpRef;
                            break;
                    
                        case DFTAG_SDU:       /* unit */
                            uRef = tmpRef;
                            break;
                    
                        case DFTAG_SDF:       /* format */
                            fRef = tmpRef;
                            break;

                        case DFTAG_SDC:       /* coord  */
                            /*
                             * DFTAG_SDC => 'cordsys'
                             */
                        { 
                            int len;

                            len = Hlength(handle->hdf_file, DFTAG_SDC, tmpRef);
                            if (len == FAIL) 
                              {
                                  ret_value = FAIL;
                                  goto done;
                              }

                            coordbuf = (uint8 *) HDmalloc((uint32) len + 1);
                            if (NULL == coordbuf) 
                              {
                                  HERROR(DFE_NOSPACE);
                                  ret_value = FAIL;
                                  goto done;
                              }

                            if (Hgetelement(handle->hdf_file, DFTAG_SDC, tmpRef, coordbuf) == FAIL)
                              {
                                  HDfreespace((VOIDP)coordbuf);
                                  ret_value = FAIL;
                                  goto done;
                              }

                            coordbuf[len] = '\0';
                            if (coordbuf[0] != '\0')  
                              {
                                  attrs[current_attr] =
                                      (NC_attr *) NC_new_attr(_HDF_CoordSys,
                                                              NC_CHAR, HDstrlen(coordbuf), coordbuf);
                                  if (NULL == attrs[current_attr])
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }
                                  else
                                      attrs[current_attr++]->HDFtype = DFNT_CHAR;
                              }

                            HDfreespace((VOIDP)coordbuf);
                        }
                        break;

                        case DFTAG_SDS:       /* scales */
                            sRef = tmpRef;
                            break;
                    
                        case DFTAG_SD:        /* actual data */
                            sdRef = tmpRef;
                            break;
                    
                        case DFTAG_CAL:        /* calibration info */
                            /* 
                           * DFTAG_CAL => 'scale_factor', 'add_offset', 'scale_factor_err', 
                           *              'add_offset_err'
                           */
                            if (Hgetelement(handle->hdf_file, tmpTag, tmpRef, ptbuf) == FAIL)
                              {
                                  ret_value = FAIL;
                                  goto done;
                              }
                    
                            if (Hlength(handle->hdf_file, tmpTag, tmpRef) == 36) 
                              {
                                  /* DFNT_FLOAT64 based calibration */
                        
                                  if (FAIL == DFKconvert((VOIDP)ptbuf, 
                                             (VOIDP) tBuf, 
                                             DFNT_FLOAT64, 4, DFACC_READ, 0, 0))
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }
                        
                                  attrs[current_attr] = 
                                      (NC_attr *) NC_new_attr(_HDF_ScaleFactor, 
                                                              NC_DOUBLE, 
                                                              1, 
                                                              (Void *) &(tBuf[0]));

                                  if (NULL == attrs[current_attr])
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }
                                  else
                                      attrs[current_attr++]->HDFtype = DFNT_FLOAT64; 

                                  attrs[current_attr] = 
                                      (NC_attr *) NC_new_attr(_HDF_ScaleFactorErr, 
                                                              NC_DOUBLE, 
                                                              1, 
                                                              (Void *) &(tBuf[8]));    

                                  if (NULL == attrs[current_attr])
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }
                                  else
                                      attrs[current_attr++]->HDFtype = DFNT_FLOAT64;
 
                                  attrs[current_attr] = 
                                      (NC_attr *) NC_new_attr(_HDF_AddOffset, 
                                                              NC_DOUBLE, 
                                                              1, 
                                                              (Void *) &(tBuf[16]));

                                  if (NULL == attrs[current_attr])
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }
                                  else
                                      attrs[current_attr++]->HDFtype = DFNT_FLOAT64;

                                  attrs[current_attr] = 
                                      (NC_attr *) NC_new_attr(_HDF_AddOffsetErr, 
                                                              NC_DOUBLE, 
                                                              1, 
                                                              (Void *) &(tBuf[24]));

                                  if (NULL == attrs[current_attr])
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }
                                  else
                                      attrs[current_attr++]->HDFtype = DFNT_FLOAT64;

                                /* don't forget number_type  */
                                  if (FAIL == DFKconvert((VOIDP)(ptbuf + 32),
                                             (VOIDP) tBuf,
                                             DFNT_INT32, 1, DFACC_READ, 0,0))
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }


                                  attrs[current_attr] =
                                      (NC_attr *) NC_new_attr(_HDF_CalibratedNt,
                                                              NC_LONG,
                                                              1,
                                                              (Void *) &(tBuf[0]));

                                  if (NULL == attrs[current_attr])
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }
                                  else
                                      attrs[current_attr++]->HDFtype = DFNT_INT32;

                              } 
                            else 
                              {
                                  /* DFNT_FLOAT32 based calibration */

                                  if (FAIL == DFKconvert((VOIDP)ptbuf, 
                                             (VOIDP)tBuf, 
                                             DFNT_FLOAT32, 4, DFACC_READ, 0, 0))
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }

                        
                                  attrs[current_attr] = 
                                      (NC_attr *) NC_new_attr(_HDF_ScaleFactor, 
                                                              NC_FLOAT, 
                                                              1, 
                                                              (Void *) &(tBuf[0]));

                                  if (NULL == attrs[current_attr])
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }
                                  else
                                      attrs[current_attr++]->HDFtype = DFNT_FLOAT32;
 
                                  attrs[current_attr] = 
                                      (NC_attr *) NC_new_attr(_HDF_ScaleFactorErr, 
                                                              NC_FLOAT, 
                                                              1, 
                                                              (Void *) &(tBuf[4]));    

                                  if (NULL == attrs[current_attr])
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }
                                  else
                                      attrs[current_attr++]->HDFtype = DFNT_FLOAT32;

                                  attrs[current_attr] = 
                                      (NC_attr *) NC_new_attr(_HDF_AddOffset, 
                                                              NC_FLOAT, 
                                                              1, 
                                                              (Void *) &(tBuf[8]));

                                  if (NULL == attrs[current_attr])
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }
                                  else
                                      attrs[current_attr++]->HDFtype = DFNT_FLOAT32;

                                  attrs[current_attr] = 
                                      (NC_attr *) NC_new_attr(_HDF_AddOffsetErr, 
                                                              NC_FLOAT, 
                                                              1, 
                                                              (Void *) &(tBuf[12]));

                                  if (NULL == attrs[current_attr])
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }
                                  else
                                      attrs[current_attr++]->HDFtype = DFNT_FLOAT32;

                                  /* don't forget number_type  */
                                  if (FAIL == DFKconvert((VOIDP)(ptbuf + 16),
                                             (VOIDP) tBuf,
                                             DFNT_INT16, 1, DFACC_READ, 0,0))
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }


                                  attrs[current_attr] =
                                      (NC_attr *) NC_new_attr(_HDF_CalibratedNt,
                                                              NC_SHORT,
                                                              1,
                                                              (Void *) &(tBuf[0]));

                                  if (NULL == attrs[current_attr])
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }
                                  else
                                      attrs[current_attr++]->HDFtype = DFNT_INT16;
                              }
                    
                            break;
                    
                        case DFTAG_SDM:        /* valid range info */
                    
                            if (Hgetelement(handle->hdf_file, tmpTag, tmpRef, ptbuf) == FAIL)
                              {
                                  ret_value = FAIL;
                                  goto done;
                              }
                    
                            if (FAIL == DFKconvert((VOIDP)ptbuf, 
                                       (VOIDP)tBuf, 
                                       HDFtype, 2, DFACC_READ, 0, 0))
                              {
                                  ret_value = FAIL;
                                  goto done;
                              }

                    
                            attrs[current_attr] = 
                                (NC_attr *) NC_new_attr(_HDF_ValidMax, 
                                                        type, 
                                                        1, 
                                                        (Void *) tBuf);

                            if (NULL == attrs[current_attr])
                              {
                                  ret_value = FAIL;
                                  goto done;
                              }
                            else
                                attrs[current_attr++]->HDFtype = HDFtype;
                    
                            attrs[current_attr] = 
                                (NC_attr *) NC_new_attr(_HDF_ValidMin, 
                                                        type, 
                                                        1, 
                                                        (Void *) &(tBuf[DFKNTsize(HDFtype|DFNT_NATIVE)]));

                            if (NULL == attrs[current_attr])
                              {
                                  ret_value = FAIL;
                                  goto done;
                              }
                            else
                                attrs[current_attr++]->HDFtype = HDFtype;
                            break;

                        case DFTAG_SDLNK:
                            if(ndgTag == DFTAG_SDG) 
                                continue; /* continue processing? */

                            if (Hgetelement(handle->hdf_file, tmpTag, tmpRef, ptbuf) == FAIL) 
                              {
                                  ret_value = FAIL;
                                  goto done;
                              }
                            else 
                              {
                                  uint16 sdgTag, sdgRef;

                                  p = ptbuf;
                        
                                  /* the first two are for the NDG tag/ref */
                                  UINT16DECODE(p, sdgTag);
                                  UINT16DECODE(p, sdgRef);
                        
                                  /* now the ones we're interested in */
                                  UINT16DECODE(p, sdgTag);
                                  UINT16DECODE(p, sdgRef);

                                  if (hdf_register_seen_sdg(sdgRef) == FAIL)
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }
                              }

                            break;
                    
                        default:
                            break;
                        } /* end switch 'tmpTag */
                  }     /* end while 'DFdiget()'*/
            
                if(lRef) 
                  {
                      int len;
                
                      /*
                       *  Add three NULLS to the end to account for a bug in HDF 3.2r1-3
                       */

                      len = Hlength(handle->hdf_file, DFTAG_SDL, lRef);
                      if(len == FAIL) 
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                
                      labelbuf = (uint8 *) HDmalloc((uint32) len + 3);
                      if(NULL == labelbuf) 
                        {
                            HERROR(DFE_NOSPACE);
                            ret_value = FAIL;
                            goto done;
                        }

                      if(Hgetelement(handle->hdf_file, DFTAG_SDL, lRef, labelbuf) == FAIL)
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                
                      labelbuf[len + 2] = '\0';
                      labelbuf[len + 1] = '\0';
                      labelbuf[len + 0] = '\0';
                
                  } 
                else 
                    labelbuf = NULL;
            
                if(uRef) 
                  {
                      int len;
                
                      len = Hlength(handle->hdf_file, DFTAG_SDU, uRef);
                      if(len == FAIL) 
                        {
                            ret_value = FAIL;
                            goto done;
                        }

                      unitbuf = (uint8 *) HDmalloc((uint32) len+3);
                      if(NULL == unitbuf) 
                        {
                            HERROR(DFE_NOSPACE);
                            ret_value = FAIL;
                            goto done;
                        }
                
                      if(Hgetelement(handle->hdf_file, DFTAG_SDU, uRef, unitbuf) == FAIL)
                        {
                            ret_value = FAIL;
                            goto done;
                        }

                      unitbuf[len + 2] = '\0';
                      unitbuf[len + 1] = '\0';
                      unitbuf[len + 0] = '\0';
                  } 
                else 
                    unitbuf = NULL;
           
                if(fRef) 
                  {
                      int len;

                      len = Hlength(handle->hdf_file, DFTAG_SDF, fRef);
                      if(len == FAIL) 
                        {
                            ret_value = FAIL;
                            goto done;
                        }

                      formatbuf = (uint8 *) HDmalloc((uint32) len+3);
                      if(NULL == formatbuf) 
                        {
                            HERROR(DFE_NOSPACE);
                            ret_value = FAIL;
                            goto done;
                        }

                      if(Hgetelement(handle->hdf_file, DFTAG_SDF, fRef, formatbuf) == FAIL)
                        {
                            ret_value = FAIL;
                            goto done;
                        }

                      formatbuf[len + 2] = '\0';
                      formatbuf[len + 1] = '\0';
                      formatbuf[len + 0] = '\0';
                  } 
                else
                    formatbuf = NULL;
 
                if(sRef) 
                  {
                      int len;
                
                      len = Hlength(handle->hdf_file, DFTAG_SDS, sRef);
                      if(len == FAIL) 
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                
                      scalebuf = (uint8 *) HDmalloc((uint32) len);
                      if(NULL == scalebuf) 
                        {
                            HERROR(DFE_NOSPACE);
                            ret_value = FAIL;
                            goto done;
                        }
                
                      if(Hgetelement(handle->hdf_file, DFTAG_SDS, sRef, scalebuf) == FAIL)
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                
                  } 
                else 
                    scalebuf = NULL;
            
                /* skip over the garbage at the beginning */
                scale_offset = rank * sizeof(uint8);
            
                for (dim = 0; dim < rank; dim++) 
                  {
                      intn this_dim     = FAIL;
                      char *labelvalue  = NULL;
                      char *unitvalue = NULL;
                      char *formatvalue = NULL;

                      /* now loop though each dimension
                         - get the size from dimsize[i]
                         - lref will give the ref of the label descriptor to see if 
                         has a real name else fake one based on the label of the NDG
                         - look at uref for units information and fref for formating 
                         info store both of these as attributes of the coordinate 
                         variable
                         - Promote the dimension to a variable if any of the LUF or
                         scales is assigned to this dimentsion. 
                         - The dimension variable name is the same as the dimension 
                         record name, fakeDim<dim count>.
                         */
               
                      new_dim = FAIL; 
                      tmpname[0] = '\0';
                
                      if(lRef) 
                        {
                            labelvalue = (char *) labelbuf;
                            for(i = 0; i < dim + 1; i++) 
                                labelvalue += HDstrlen(labelvalue) + 1;
                            if (labelvalue[0] != '\0') 
                                new_dim = SUCCEED;
                        }
                
                      if(uRef) 
                        {
                            unitvalue = (char *) unitbuf;
                            for(i = 0; i < dim + 1; i++) 
                                unitvalue += HDstrlen(unitvalue) + 1;
                            if (unitvalue[0] != '\0') 
                                new_dim = SUCCEED;
                        }

                      if(fRef) 
                        {
                            formatvalue = (char *) formatbuf;
                            for(i = 0; i < dim + 1; i++) 
                                formatvalue += HDstrlen(formatvalue) + 1;
                            if (formatvalue[0] != '\0') 
                                new_dim = SUCCEED;
                        }

                      sprintf(tmpname, "fakeDim%d", dimcount++);
                
                      this_dim = current_dim++;
                      if(current_dim == max_thangs) 
                        {
                            /* need to allocate more space */    
                            max_thangs *= 2;
                            dims = (NC_dim **) HDrealloc((VOIDP) dims, sizeof(NC_dim *) * max_thangs);
                            if(NULL == dims) 
                              {
                                  HERROR(DFE_NOSPACE);
                                  ret_value = FAIL;
                                  goto done;
                              }

                            vars = (NC_var **) HDrealloc((VOIDP) vars, sizeof(NC_var *) * max_thangs);
                            if(NULL == vars) 
                              {
                                  HERROR(DFE_NOSPACE);
                                  ret_value = FAIL;
                                  goto done;
                              }
                        }

                      /*
                       * Create a new dimension record for each dimension. 
                       * Each NC_dim takes 8 bytes in memory. 
                       */
                      dims[this_dim] = NC_new_dim(tmpname, dimsizes[dim]);
                      if (NULL == dims[this_dim])
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                
                      /* 
                       * It looks like were gonna have to do the variable define
                       *    here too cuz we need to remember the indicies of where
                       *    we put the dimensions
                       */
                
                      vardims[dim] = (intn) this_dim;
                
                
                      /*
                       * Look at the scale NTs since the scales may have different number 
                       *   types
                       * Promote the dimension to a variable, but only if it has meta-data
                       *   stored with it.  
                       */
                      if(new_dim || (scalebuf && scalebuf[dim])) 
                        {
                            nc_type stype;

                            if ((stype = hdf_unmap_type(scaletypes[dim])) == FAIL)
                              {
#ifdef DEBUG
                                  /* replace it with NCAdvice or HERROR? */
                                  fprintf(stderr "hdf_read_ndgs: hdf_unmap_type failed for %d\n", scaletypes[dim]);
#endif
                                  ret_value = FAIL;
                                  goto done;
                              }

                            vars[current_var] = NC_new_var(tmpname, 
                                                           stype,
                                                           1, 
                                                           &this_dim);
                            if (NULL == vars[current_var])
                              {
                                  ret_value = FAIL;
                                  goto done;
                              }

                            vars[current_var]->data_tag = DFTAG_SDS;  /* not normal data */
                            vars[current_var]->data_ref = sRef;
                            vars[current_var]->HDFtype  = scaletypes[dim];
#ifdef NOT_YET
                            vars[current_var]->ndg_ref  = Htagnewref(handle->hdf_file,DFTAG_NDG);
#else /* NOT_YET */
                            vars[current_var]->ndg_ref  = Hnewref(handle->hdf_file);
#endif /* NOT_YET */

                            /*
                             * See if a scales record has been stored and if there have
                             *   been scale values provided
                             */
                            if((scalebuf) && (scalebuf[dim])) 
                              {
                                  vars[current_var]->numrecs = dimsizes[dim];
                                  vars[current_var]->data_offset = scale_offset;
                                  scale_offset += dimsizes[dim] * DFKNTsize(scaletypes[dim]);
                              } 
                            else 
                              {
                                  vars[current_var]->data_offset = -1;  /* no values */
                              }
                            /*
                             * Convert dimstrs into attributes  
                             * label -- "long_name" (cuz SDsetdimstrs() assigns "long_name" to label)
                             * unit  -- "units"
                             * format -- "format"
                             */

                            /* label => "long_name"  */
                            dimattrcnt = 0;
                            if (labelvalue && HDstrlen((char *)labelvalue) > 0) 
                              {
                                  dimattrs[dimattrcnt] =
                                      (NC_attr *) NC_new_attr(_HDF_LongName, NC_CHAR,
                                                              HDstrlen((char *)labelvalue),
                                                              (Void *) labelvalue);

                                  if (NULL == dimattrs[dimattrcnt])
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }
                                  else
                                      dimattrs[dimattrcnt++]->HDFtype = DFNT_CHAR;
                              }

                            /* Units => 'units' */
                            if(unitvalue && HDstrlen((char *)unitvalue) > 0) 
                              {
                                  dimattrs[dimattrcnt] =
                                      (NC_attr *) NC_new_attr(_HDF_Units, NC_CHAR,
                                                              HDstrlen((char *)unitvalue),
                                                              (Void *) unitvalue);

                                  if (NULL == dimattrs[dimattrcnt])
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }
                                  else
                                      dimattrs[dimattrcnt++]->HDFtype = DFNT_CHAR;
                              }

                            /* Fomrat => 'format' */
                            if(formatvalue && HDstrlen((char *)formatvalue) > 0) 
                              {
                                  dimattrs[dimattrcnt] =
                                      (NC_attr *) NC_new_attr(_HDF_Format, NC_CHAR,
                                                              HDstrlen((char *)formatvalue),
                                                              (Void *) formatvalue);

                                  if (NULL == dimattrs[dimattrcnt])
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }
                                  else
                                      dimattrs[dimattrcnt++]->HDFtype = DFNT_CHAR;
                              }

                            /*
                             * Add the attributes to the variable
                             */
                            if(dimattrcnt)
                              {
                                vars[current_var]->attrs = NC_new_array(NC_ATTRIBUTE,
                                                                        dimattrcnt,
                                                                        (Void *) dimattrs);
                                  if (NULL == vars[current_var]->attrs)
                                    {
                                        ret_value = FAIL;
                                        goto done;
                                    }
                              }
                            else
                                vars[current_var]->attrs = NULL;

                            current_var++;  

                            if(current_var == max_thangs) 
                              {
                                  /* need to allocate more space */    
                                  max_thangs *= 2;

                                  dims = (NC_dim **) HDrealloc((VOIDP)dims, sizeof(NC_dim *) * max_thangs);
                                  if(NULL == dims) 
                                    {
                                        HERROR(DFE_NOSPACE);
                                        ret_value = FAIL;
                                        goto done;
                                    }
                        
                                  vars = (NC_var **) HDrealloc((VOIDP)vars, sizeof(NC_var *) * max_thangs);
                                  if(NULL == vars) 
                                    {
                                        HERROR(DFE_NOSPACE);
                                        ret_value = FAIL;
                                        goto done;
                                    }
                              }
                        } /* end if 'new_dim' */
                  } /* end for 'dim' */
            
                /*
                 * Should the LUF-label be mapped as attr of "longname", to be consistent
                 *   with the dim vars? 8/18/94
                 * Should the annotation-label mapped to attr "anno-label", if "longname"
                 *   has been taken by LUF-label?  8/18/94.
                 *   
                 * (If there is a data label use that as the variable name else) 
                 * Use the reference number of the NDG as part of
                 *    a made up name (Label is mapped as attr "longname" 9/2/94).
                 *
                 * Convert spaces in the name to underscores (yuck) otherwise
                 *    ncgen will barf on ncdumped files)
                 */
#if 0
                /* if(labelbuf && (labelbuf[0] != '\0')) 
                   {
                   char *c;
                   for(c = (char *)labelbuf; *c; c++)
                   if((*c) == ' ') (*c) = '_';

                   vars[current_var] = NC_new_var((char *) labelbuf, type, (int) rank, vardims);
                   } 
                   else 
                   { */
#endif

                sprintf(tmpname, "Data-Set-%d", ndgRef); 
                vars[current_var] = NC_new_var(tmpname, type, (int) rank, vardims);
                if (NULL == vars[current_var])
                  {
                      ret_value = FAIL;
                      goto done;
                  }

#if 0
                /*   } */
#endif 
                /*
                 * Fill in extra information so it will be easier to backtrack
                 *    when the user wants to lift data out
                 */
                vars[current_var]->data_tag = DATA_TAG;
                vars[current_var]->data_ref = sdRef;
                vars[current_var]->HDFtype  = HDFtype;

                /*
                 * NOTE:  If the user changes the file and saves setting this
                 *   to ndgRef will blow away the old ndgs (but they will get
                 *   rewritten).  Otherwise calls to SDidtoref() will return
                 *   bogus values
                 */
                vars[current_var]->ndg_ref  = ndgRef; 

                /*
                 * --------------------------------------------
                 * Convert HDF meta-data into netCDF attributes
                 * --------------------------------------------
                 */
            
                /*
                 * If there is an annotation put in 'remarks'
                 */
            
                {
                    /* Re-vamped desc annotation handling to use new ANxxx interface 
                     *  -georgev 6/11/97 */
                    int32  an_handle   = FAIL;
                    int32  *ddescs    = NULL;
                    char   *ann_desc  = NULL;
                    int32  ann_len;
                    intn   num_ddescs;
                    char   hremark[30] = ""; /* should be big enough for new attribute */

                    /* start Annotation inteface */
                    if ((an_handle = ANstart(handle->hdf_file)) == FAIL)
                      {
                          ret_value = FAIL;
                          goto done_adesc; 
                      }

                    /* Get number of data descs with this tag/ref */
                    num_ddescs = ANnumann(an_handle, AN_DATA_DESC, ndgTag, ndgRef);
#ifdef AN_DEBUG
                    fprintf(stderr,"SDS has %d descs \n", num_ddescs);
#endif
                    if (num_ddescs != 0)
                      {
                          /* allocate space for list of desc annotation id's with this tag/ref */
                          if ((ddescs = (int32 *)HDmalloc(num_ddescs * sizeof(int32))) == NULL)
                            {
#ifdef AN_DEBUG
                                fprintf(stderr,"failed to allocate space for %d descs \n", num_ddescs);
#endif
                                ret_value = FAIL;
                                goto done_adesc;
                            }

                          /* get list of desc annotations id's with this tag/ref */
                          if (ANannlist(an_handle, AN_DATA_DESC, ndgTag, ndgRef, ddescs) != num_ddescs)
                            {
#ifdef AN_DEBUG
                                fprintf(stderr,"failed to get %d descs list \n", num_ddescs);
#endif
                                ret_value = FAIL;
                                goto done_adesc;
                            }

                          /* loop through desc list. */
                          for (i = 0; i < num_ddescs; i++)
                            {
                                if ((ann_len = ANannlen(ddescs[i])) == FAIL)
                                  {
#ifdef AN_DEBUG
                                      fprintf(stderr,"failed to get %d desc  length \n", i);
#endif
                                      ret_value = FAIL;
                                      goto done_adesc;
                                  }
        
                                /* allocate space for desc */
                                if (ann_desc == NULL)
                                  {
                                      if ((ann_desc = (char *)HDmalloc((ann_len+1)*sizeof(char))) == NULL)
                                        {
#ifdef AN_DEBUG
                                            fprintf(stderr,"failed to allocate space for desc %d \n", i);
#endif
                                            ret_value = FAIL;
                                            goto done_adesc;
                                        }
                                      HDmemset(ann_desc,'\0', ann_len+1);
                                  }
      
                                /* read desc */
                                if (ANreadann(ddescs[i], ann_desc, ann_len+1) == FAIL)
                                  {
#ifdef AN_DEBUG
                                      fprintf(stderr,"failed to read %d desc \n", i);
#endif
                                      ret_value = FAIL;
                                      goto done_adesc;
                                  }

                                /* make unique attribute */
                                sprintf(hremark,"%s-%d",_HDF_Remarks,i+1);
                            
                                /* add it as a attribute */
                                attrs[current_attr] = 
                                    (NC_attr *) NC_new_attr(hremark, 
                                                            NC_CHAR, 
                                                            HDstrlen(ann_desc), 
                                                            ann_desc);

                                if (NULL == attrs[current_attr])
                                  {
                                      ret_value = FAIL;
                                      goto done_adesc;
                                  }
                                else
                                    attrs[current_attr++]->HDFtype = DFNT_CHAR;

                                /* end access */
                                ANendaccess(ddescs[i]);

                                /* free buffer */
                                if(ann_desc != NULL)
                                  {
                                      HDfree(ann_desc);
                                      ann_desc = NULL;
                                  }
                            }

                      } /* end if descs */

                  done_adesc: /* GOTO Label */
                    /* cleanup */
                    if(ddescs != NULL)
                        HDfree(ddescs);

                    if(an_handle != FAIL)
                        ANend(an_handle);

                    /* check for error during ANxxx processing */
                    if (ret_value == FAIL)
                        goto done; /* error so return */

                } /* end annotation description conversion */
            
                /*
                 * If there is a label put in attr 'anno_label' (note: NOT 'long_name' 9/2/94)
                 */
                {
                    /* Re-vamped label annotation handling to use new ANxxx interface 
                     *  -georgev 6/11/97 */
                    int32  an_handle   = FAIL;
                    int32  *dlabels    = NULL;
                    char   *ann_label  = NULL;
                    int32  ann_len;
                    intn   num_dlabels;
                    char   hlabel[30] = ""; /* should be big enough for new attribute */

                    /* start Annotation inteface */
                    if ((an_handle = ANstart(handle->hdf_file)) == FAIL)
                      {
                          ret_value = FAIL;
                          goto done_alabel;
                      }

                    /* Get number of data labels with this tag/ref */
                    num_dlabels = ANnumann(an_handle, AN_DATA_LABEL, ndgTag, ndgRef);
#ifdef AN_DEBUG
                    fprintf(stderr,"SDS has %d labels \n", num_dlabels);
#endif

                    if (num_dlabels != 0)
                      {
                          /* allocate space for list of label annotation id's with this tag/ref */
                          if ((dlabels = (int32 *)HDmalloc(num_dlabels * sizeof(int32))) == NULL)
                            {
#ifdef AN_DEBUG
                                fprintf(stderr,"failed to allocate space for %d labels \n", num_dlabels);
#endif
                                ret_value = FAIL;
                                goto done_alabel;
                            }

                          /* get list of label annotations id's with this tag/ref */
                          if (ANannlist(an_handle, AN_DATA_LABEL, ndgTag, ndgRef, dlabels) != num_dlabels)
                            {
#ifdef AN_DEBUG
                                fprintf(stderr,"failed to get %d label list \n", num_dlabels);
#endif
                                ret_value = FAIL;
                                goto done_alabel;
                            }

                          /* loop through label list */
                          for (i = 0; i < num_dlabels; i++)
                            {
                                if ((ann_len = ANannlen(dlabels[i])) == FAIL)
                                  {
#ifdef AN_DEBUG
                                      fprintf(stderr,"failed to get %d label  length \n", i);
#endif
                                      ret_value = FAIL;
                                      goto done_alabel;
                                  }
        
                                /* allocate space for label */
                                if (ann_label == NULL)
                                  {
                                      if ((ann_label = (char *)HDmalloc((ann_len+1)*sizeof(char))) == NULL)
                                        {
#ifdef AN_DEBUG
                                            fprintf(stderr,"failed to allocate space for label %d \n", i);
#endif
                                            ret_value = FAIL;
                                            goto done_alabel;
                                        }
                                      HDmemset(ann_label,'\0', ann_len+1);
                                  }
      
                                /* read label */
                                if (ANreadann(dlabels[i], ann_label, ann_len+1) == FAIL)
                                  {
#ifdef AN_DEBUG
                                      fprintf(stderr,"failed to read %d label \n", i);
#endif
                                      ret_value = FAIL;
                                      goto done_alabel;
                                  }

                                /* make unique attribute */
                                sprintf(hlabel,"%s-%d",_HDF_AnnoLabel,i+1);

                                /* add as atriburte */
                                attrs[current_attr] = 
                                    (NC_attr *) NC_new_attr(hlabel, 
                                                            NC_CHAR, 
                                                            HDstrlen(ann_label), 
                                                            ann_label);

                                if (NULL == attrs[current_attr])
                                  {
                                      ret_value = FAIL;
                                      goto done_alabel;
                                  }
                                else
                                    attrs[current_attr++]->HDFtype = DFNT_CHAR;

                                /* end access */
                                ANendaccess(dlabels[i]);

                                /* free buffer */
                                if(ann_label != NULL)
                                  {
                                      HDfree(ann_label);
                                      ann_label = NULL;
                                  }
                            }

                      } /* end if labels */

                  done_alabel: /* GOTO Label */
                    /* cleanup */
                    if(dlabels != NULL)
                        HDfree(dlabels);

                    if(an_handle != FAIL)
                        ANend(an_handle);

                    /* check for error during ANxxx processing */
                    if (ret_value == FAIL)
                        goto done; /* error so return */
                } /* end annotation label processing */
            
                /* 
                 * Label => 'long_name'
                 */
                if(labelbuf && (labelbuf[0] != '\0')) 
                  {
                      attrs[current_attr] =
                          (NC_attr *) NC_new_attr(_HDF_LongName,
                                                  NC_CHAR,
                                                  HDstrlen((char *)labelbuf),
                                                  (Void *) labelbuf);

                      if (NULL == attrs[current_attr])
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                      else
                          attrs[current_attr++]->HDFtype = DFNT_CHAR;
                  }
 
                /*
                 * Units => 'units'
                 */
                if(unitbuf && (unitbuf[0] != '\0')) 
                  {
                      attrs[current_attr] = 
                          (NC_attr *) NC_new_attr(_HDF_Units, 
                                                  NC_CHAR, 
                                                  HDstrlen((char *)unitbuf), 
                                                  (Void *) unitbuf);

                      if (NULL == attrs[current_attr])
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                      else
                          attrs[current_attr++]->HDFtype = DFNT_CHAR;
                  }
            
                /*
                 * (Don't do format cuz HDF doesn't distinguish between C and Fortran
                 * Actually, it seems HDF Format == netCDF Fortran Format)
                 * Don't use 'C_format' or 'FORTRAN_format'
                 * Format => 'format'
                 */
                if(formatbuf && (formatbuf[0] != '\0')) 
                  {
                      attrs[current_attr] =
                          (NC_attr *) NC_new_attr(_HDF_Format,
                                                  NC_CHAR,
                                                  HDstrlen((char *)formatbuf),
                                                  (Void *) formatbuf);

                      if (NULL == attrs[current_attr])
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                      else
                          attrs[current_attr++]->HDFtype = DFNT_CHAR;
                  }

            
                /*
                 * Add the attributes to the variable
                 */ 
                if(current_attr)
                  {
                    vars[current_var]->attrs = NC_new_array(NC_ATTRIBUTE, 
                                                            current_attr,
                                                            (Void *) attrs);

                      if (NULL == vars[current_var]->attrs)
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                  }
                else
                    vars[current_var]->attrs = NULL;
            
            
                current_var++;

                if(current_var == max_thangs) 
                  {
                      /* need to allocate more space */    
                      max_thangs *= 2;
                
                      dims = (NC_dim **) HDrealloc((VOIDP) dims, sizeof(NC_dim *) * max_thangs);
                      if(NULL == dims) 
                        {
                            HERROR(DFE_NOSPACE);
                            ret_value = FAIL;
                            goto done;
                        }
                
                      vars = (NC_var **) HDrealloc((VOIDP) vars, sizeof(NC_var *) * max_thangs);
                      if(NULL == vars) 
                        {
                            HERROR(DFE_NOSPACE);
                            ret_value = FAIL;
                            goto done;
                        }
                
                  }
            
                /*
                 * De-allocate temporary storage
                 */
                if(labelbuf != NULL)  
                    HDfree((VOIDP)labelbuf);
                if(scalebuf != NULL)
                    HDfree((VOIDP)scalebuf);
                if(unitbuf != NULL)  
                    HDfree((VOIDP)unitbuf);
                if(formatbuf != NULL)  
                    HDfree((VOIDP)formatbuf);
                if (dimsizes != NULL)
                    HDfree((VOIDP)dimsizes);
                if (vardims != NULL)
                    HDfree((VOIDP)vardims);
                if (scaletypes != NULL)
                    HDfree((VOIDP)scaletypes);
            
                /*
                 * Look for the next DataSet
                 */
                status = Hnextread(aid, ndgTag, DFREF_WILDCARD, DF_CURRENT);
            
            } /* while (more NDGs) */
        
          if (Hendaccess(aid) == FAIL)
            {
                ret_value = FAIL;
                goto done;
            }
        
          /*
           * Set up the structures in the proper form
           */
          if(current_dim)
            {
              handle->dims = NC_new_array(NC_DIMENSION, current_dim, (Void *) dims);
              if (NULL == handle->dims)
                {
                    ret_value = FAIL;
                    goto done;
                }
            }
          else
              handle->dims = NULL;
        
          if(current_var)
            {
              handle->vars = NC_new_array(NC_VARIABLE, current_var, (Void *) vars);
              if (NULL == handle->vars)
                {
                    ret_value = FAIL;
                    goto done;
                }
            }
          else
              handle->vars = NULL;
        
      } /* outermost for loop to loop between NDGs and SDGs */


done:
    if (ret_value == FAIL)
      { /* FAIL cleanup? */
          if(labelbuf != NULL)  
              HDfree((VOIDP)labelbuf);
          if(scalebuf != NULL)
              HDfree((VOIDP)scalebuf);
          if(unitbuf != NULL)  
              HDfree((VOIDP)unitbuf);
          if(formatbuf != NULL)  
              HDfree((VOIDP)formatbuf);
          if (dimsizes != NULL)
              HDfree((VOIDP)dimsizes);
          if (vardims != NULL)
              HDfree((VOIDP)vardims);
          if (scaletypes != NULL)
              HDfree((VOIDP)scaletypes);

      }
    /* Normal cleanup */
    if (dims != NULL)
        HDfree((VOIDP)dims);
    if (vars != NULL)
        HDfree((VOIDP)vars);
    if (attrs != NULL)
        HDfree((VOIDP)attrs);

    return ret_value;
} /* hdf_read_ndgs */


/******************************************************************************
 NAME
   hdf_read_sds_cdf
 
 DESCRIPTION
   Read SDSs out of an HDF file an pretend like they are netCDF objects
   We can already assume that the file is open and that it is in fact
   an HDF file

 RETURNS
   SUCCEED / FAIL
 
******************************************************************************/
int
hdf_read_sds_cdf(XDR *xdrs, 
                 NC **handlep)
{
    int32  status;
    NC     *handle = NULL;
    intn  ret_value = SUCCEED;
    
    /* 
     * go through and treat each SDS as a separate varibiable 
     */

    /* 
     * should we try to create an unlimited dimension somehow ???
     */
    
    /* we haven't seen any SDG-NDG combos yet */
    HDfreenclear(sdgTable);

    handle = (*handlep);
    if(NULL == handle) 
      {
        ret_value = FAIL;
        goto done;
      }

    status = hdf_read_ndgs(handle);
    if(status == FAIL) 
      {
        ret_value = FAIL;
        goto done;
      }

    /* deallocate SDG-NDG space */
    if(sdgTable != NULL) 
        HDfree((VOIDP)sdgTable);

    sdgTable = NULL;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */

      }
    /* Normal cleanup */

    return ret_value;
} /* hdf_read_sds_cdf */

#endif /* HDF */
