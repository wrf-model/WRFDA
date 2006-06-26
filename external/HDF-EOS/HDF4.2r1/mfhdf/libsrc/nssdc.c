/*

  This file contains the code to read CDF V2.x files created with the
  CDF library as written by the NSSDC group at NASA Goddard

  The code in this file was written by the HDF Group at the National
  Center for Supercomputing Applications and is governed by the
  standard HDF copyright statements.

*/

/* #define DEBUG 1 */
#include "local_nc.h"
#ifdef HDF
#include "hfile.h"

/* constants/macros pulled out of the CDF library source */
#define V2_CDR_OFFSET     8
#define VDRzVDR_RFUe_LEN  128
#define CDF_VAR_NAME_LEN  64
#define CDF_ATTR_NAME_LEN 64
#define CDF_FORMAT_BIT    1       /* format: set = single file,
                                           clear = multi file */

#define rVAR_RECVARY_BIT        0       /* record variance:
                                             set = VARY,
                                             clear = NOVARY */
#define rVAR_PADVALUE_BIT       1       /* pad value:
                                             set = specified,
                                             clear = not specified (use
                                                     default) */
#define zVAR_RECVARY_BIT        0       /* record variance:
                                             set = VARY,
                                             clear = NOVARY */
#define zVAR_PADVALUE_BIT       1       /* pad value:
                                             set = specified,
                                             clear = not specified (use
                                                     default) */

#define bitset(a,bit)     (a & (1 << bit))
#define CDF_INT1     (long) 1
#define CDF_INT2     (long) 2
#define CDF_INT4     (long) 4

#define CDF_UINT1    (long) 11
#define CDF_UINT2    (long) 12
#define CDF_UINT4    (long) 14

#define CDF_REAL4    (long) 21
#define CDF_REAL8    (long) 22

#define CDF_EPOCH    (long) 31                  /* NSSDC standard */

#define CDF_BYTE     (long) 41                  /* same as CDF_INT1 (signed) */
#define CDF_FLOAT    (long) 44                  /* same as CDF_REAL4 */
#define CDF_DOUBLE   (long) 45                  /* same as CDF_REAL8 */

#define CDF_CHAR     (long) 51                  /* a "string" data type */
#define CDF_UCHAR    (long) 52                  /* a "string" data type */

#define NETWORK_ENCODING        1L
#define SUN_ENCODING            2L
#define VAX_ENCODING            3L
#define DECSTATION_ENCODING     4L
#define SGi_ENCODING            5L
#define IBMPC_ENCODING          6L
#define IBMRS_ENCODING          7L
#define HOST_ENCODING           8L
#define HP_ENCODING             11L
#define NeXT_ENCODING           12L
#define ALPHAOSF1_ENCODING      13L
#define ALPHAVMSd_ENCODING      14L
#define ALPHAVMSg_ENCODING      15L


/* ---------------------------- cdf_unmap_type ---------------------------- */
/*
  UnMap a data type.  I.e. go from a CDF type to an NC_<type>
*/
nc_type
  cdf_unmap_type(type)
int type;
{

    switch(type & 0xff) {
    case CDF_CHAR        :
    case CDF_UCHAR       :
        return NC_CHAR;
    case CDF_BYTE        :
    case CDF_INT1        :
    case CDF_UINT1       :
        return NC_BYTE;
    case CDF_INT2        :
    case CDF_UINT2       :
        return NC_SHORT;
    case CDF_INT4        :
    case CDF_UINT4       :
        return NC_LONG;
    case CDF_REAL4       : 
    case CDF_FLOAT       :
        return NC_FLOAT; 
    case CDF_REAL8       : 
    case CDF_DOUBLE      :
    case CDF_EPOCH       :
        return NC_DOUBLE;
    default:
        return (nc_type) FAIL;	/* need to return a better (legal) value */
  }

} /* cdf_unmap_type */

/* ---------------------------- nssdc_read_cdf ---------------------------- */
/*
  Read a NC structure out of a CDF file
*/
bool_t
nssdc_read_cdf(xdrs, handlep)
     XDR *  xdrs;
     NC  ** handlep;
{

    NC     * handle;
    uint8    buffer[1000];
    uint8  * b;
    int32    dummy;
    static const char   * FUNC = "nssdc_read_cdf";
    hdf_file_t fp;
    intn     i, j;
    int32    rank, current_var, current_dim, hdftype;
    nc_type  nctype;
    intn     dims[MAX_VAR_DIMS];
    int32    dim_sz[MAX_VAR_DIMS];
    NC_dim * dim_rec[MAX_VAR_DIMS];
    NC_var * vars[MAX_NC_VARS];
    NC_var * var = NULL;    /* shorthand for vars[current_var] */
    vix_t  * end;

    /* interesting stuff in CDR record */
    int32   gdrOffset, vers, release, encoding, flags, inc;

    /* interesting stuff in GDR record */
    int32   varNext, zVarNext, adrNext, numRVars, numAttrs, maxRec;
    int32   numDims, numZVars;

    /* interesting stuff in VDR record */
    int32   nt, vMaxRec, vxrNext, VXRtail, vFlags, numElem, num;
    char    name[CDF_VAR_NAME_LEN + 1];

    /* interesting stuff in ADR record */
    int32   scope, aedrNext, aedzNext;

    /* interesting stuff in AEDR record */
    int32   count;

#if DEBUG
    fprintf(stderr, "nssdc_read_cdf i've been called\n");
#endif

    handle = (*handlep);
    fp = handle->cdf_fp;
    current_var = 0;

    /* 
     * pull in the CDR and see what we want out of it 
     */
    if (HI_SEEK(fp, V2_CDR_OFFSET) == FAIL)
        HRETURN_ERROR(DFE_SEEKERROR,FALSE);
    
    if (HI_READ(fp, buffer, 12 * sizeof(int32)) == FAIL)
       HRETURN_ERROR(DFE_READERROR,FALSE);

    b = buffer;
    INT32DECODE(b, dummy);      /* record size */
    INT32DECODE(b, dummy);      /* record type */
    INT32DECODE(b, gdrOffset);  /* offset of GDR record */
    INT32DECODE(b, vers);       /* version number */
    INT32DECODE(b, release);    /* release number */
    INT32DECODE(b, encoding);   /* type of encoding */
    INT32DECODE(b, flags);      /* special flags (uint32????) */
    INT32DECODE(b, dummy);      /* rfuA */
    INT32DECODE(b, dummy);      /* rfuB */
    INT32DECODE(b, inc);        /* increment - do we care?? */
    INT32DECODE(b, dummy);      /* rfuD */
    INT32DECODE(b, dummy);      /* rfuE */

    /*
     * Only handle single file implementations for now
     */
    if(!bitset(flags, CDF_FORMAT_BIT)) {
#ifdef DEBUG
        fprintf(stderr, "We are only able to handle single-file CDFs.  Sorry.\n");
#endif
        return (FALSE);
    }

    /* Check the encoding */
    if((encoding != NETWORK_ENCODING) &&
       (encoding != SUN_ENCODING)     && 
       (encoding != SGi_ENCODING)     &&      
       (encoding != IBMRS_ENCODING)   && 
       (encoding != HP_ENCODING)) {
#ifdef DEBUG
        fprintf(stderr, "We are only able to handle IEEE encoded files.  Sorry.\n");
#endif
        return (FALSE);
    }
       
    /* 
     * pull in the GDR and see what we want out of it 
     */
    if (HI_SEEK(fp, gdrOffset) == FAIL)
        HRETURN_ERROR(DFE_SEEKERROR,FALSE);
    
    if (HI_READ(fp, buffer, 15 * sizeof(int32)) == FAIL)
       HRETURN_ERROR(DFE_READERROR,FALSE);

    b = buffer;
    INT32DECODE(b, dummy);      /* record size */
    INT32DECODE(b, dummy);      /* record type */
    INT32DECODE(b, varNext);    /* start of next R variable */
    INT32DECODE(b, zVarNext);   /* start of Zvariables */
    INT32DECODE(b, adrNext);    /* start of attributes */
    INT32DECODE(b, dummy);      /* eof */
    INT32DECODE(b, numRVars);   /* number of R variables */
    INT32DECODE(b, numAttrs);   /* number of attributes */
    INT32DECODE(b, maxRec);     /* dunno */
    INT32DECODE(b, numDims);    /* number of dimensions */
    INT32DECODE(b, numZVars);   /* number of Z variables */
    INT32DECODE(b, dummy);      /* rfuB */
    INT32DECODE(b, dummy);      /* rfuC */
    INT32DECODE(b, dummy);      /* rfuD */
    INT32DECODE(b, dummy);      /* rfuE */

    /* 
     * dimension sizes are here 
     */
    if(numDims > MAX_VAR_DIMS)
        return FALSE;

    if (HI_READ(fp, buffer, numDims * sizeof(int32)) == FAIL)
       HRETURN_ERROR(DFE_READERROR,FALSE);

#ifdef DEBUG
    fprintf(stderr,"File version %d.%d\n", (int)vers, (int)release);
    fprintf(stderr," rVars %d, Attrs %d, zVars %d\n", (int)numRVars, (int)numAttrs, (int)numZVars);
    fprintf(stderr," Dims %d, maxRec %d\n", (int)numDims, (int)maxRec);
#endif

    /*
     * For each rVariable dimension size create a new dimension object
     */
    b = buffer;
    current_dim = 0;
    for(i = 0; i < numDims; i++) {
        char tmpname[80];
        INT32DECODE(b, dim_sz[i]);
        sprintf(tmpname, "CDFdim%d_%d", i, (int)dim_sz[i]);
        dim_rec[current_dim++] = NC_new_dim(tmpname, dim_sz[i]);

#ifdef DEBUG
        fprintf(stderr,"Created dimension %s\n", tmpname);
#endif
    }

    /* 
     * Loop over Rvariables and read them in
     */
    while(varNext != 0) {

#ifdef DEBUG
        fprintf(stderr,"Variable %d seeking to %d\n", (int)current_var, (int)varNext);
#endif

        if (HI_SEEK(fp, varNext) == FAIL)
            HRETURN_ERROR(DFE_SEEKERROR,FALSE);

        if (HI_READ(fp, buffer, 4) == FAIL)
            HRETURN_ERROR(DFE_READERROR,FALSE);

        b = buffer;
        INT32DECODE(b, dummy);      /* record size */
        dummy -= 4;
            
        if (HI_READ(fp, buffer, dummy) == FAIL)
            HRETURN_ERROR(DFE_READERROR,FALSE);
        
        b = buffer;
        INT32DECODE(b, dummy);      /* record type */
        INT32DECODE(b, varNext);    /* start of next R variable */
        INT32DECODE(b, nt);         /* number type */
        INT32DECODE(b, vMaxRec);    /* number of records for this variable */
        INT32DECODE(b, vxrNext);    /* start of VXR recrods */
        INT32DECODE(b, VXRtail);    /* ??? */
        INT32DECODE(b, vFlags);     /* variable flags <---- record variance in here */
        INT32DECODE(b, dummy);      /* rfuA */
        INT32DECODE(b, dummy);      /* rfuB */
        INT32DECODE(b, dummy);      /* rfuC */
        INT32DECODE(b, dummy);      /* rfuF */
        b += VDRzVDR_RFUe_LEN;      /* reserved field */
        INT32DECODE(b, numElem);    /* == 1 unless string variable */
        INT32DECODE(b, num);        /* */
        INT32DECODE(b, dummy);      /* rfuD */
        INT32DECODE(b, dummy);      /* nextEndRecs */

        HDmemcpy(name, b, CDF_VAR_NAME_LEN);
        name[CDF_VAR_NAME_LEN] = '\0';
        b += CDF_VAR_NAME_LEN;

        j = 0;
        while((name[j] != ' ') && (name[j] != '\0')) j++;
        name[j] = '\0';

#ifdef DEBUG
        fprintf(stderr,"\tName %s nt %d vMaxRec %d\n", name, (int)nt, (int)vMaxRec);
        fprintf(stderr,"\tNext var at %d\n", (int)varNext);
#endif

        /*
         *  Figure out which dimensions are meaningful
         *  If we vary in the record dimension it is reflected in the variable's
         *    flags
         *  Variance in the other dimensions is specified singly as int32 
         *    quantities in the file
         *  Only define the variable in terms of dimensions in which it varies
         */
        rank = 0;

        if(bitset(vFlags, rVAR_RECVARY_BIT)) {
            char dimname[1000];
            sprintf(dimname, "%s_REC", name);
            if(numElem > 1)
                dim_rec[current_dim] = NC_new_dim(dimname, numElem);
            else
                if(maxRec > 0)
                    dim_rec[current_dim] = NC_new_dim(dimname, maxRec);
                else
                    dim_rec[current_dim] = NC_new_dim(dimname, NC_UNLIMITED);

            dims[rank++] = current_dim++;
        }
        
        for(j = 0; j < numDims; j++) {
            INT32DECODE(b, dummy);
            if(dummy) 
                dims[rank++] = j;
        }

        /* map the CDF type into a netCDF type */
        nctype = cdf_unmap_type(nt);

        /* define the variable */
        var = vars[current_var] = NC_new_var((char *) name, nctype, (int) rank, dims);
        if(var == NULL)
            HRETURN_ERROR(DFE_NOSPACE, FALSE);

#ifdef DEBUG
        fprintf(stderr,"Was able to call NC_new_var()\n");
#endif
        
        /* if it is unsigned at least set the HDFtype to reflect it */
        switch(nt) {
        case CDF_UINT1:
            var->HDFtype = DFNT_UINT8;    break;
        case CDF_UINT2:
            var->HDFtype = DFNT_UINT16;   break;  
        case CDF_UINT4:
            var->HDFtype = DFNT_UINT32;   break;
        default:
            break;
        }

        /*
         *  Read the pad value if applicable
         */
        if(bitset(vFlags, rVAR_PADVALUE_BIT)) {
            /* need to pull the pad value out of the file at current location */
            /* make into _Fillvalue attribute */
        }
        
        /*
         *  Mess with setting up VXR records
         */

        var->vixHead = end = NULL;
        while(vxrNext != 0) {
            vix_t * vix;

            vix = (vix_t *)HDmalloc(sizeof(vix_t));
            if(vix == NULL) 
                HRETURN_ERROR(DFE_NOSPACE,FALSE);
            
            /* stick vix at the end of our list and update end pointer */
            if(end == NULL)
                var->vixHead = end = vix;
            else
                end = end->next = vix;

            vix->next = NULL;

#ifdef DEBUG
            fprintf(stderr, "Reading a VXR record at %d\n", (int)vxrNext);
#endif
            /*
             * Read the next record out of the file 
             */
            if (HI_SEEK(fp, vxrNext) == FAIL)
                HRETURN_ERROR(DFE_SEEKERROR,FALSE);
            
            if (HI_READ(fp, buffer, 4) == FAIL)
            HRETURN_ERROR(DFE_READERROR,FALSE);
            
            b = buffer;
            INT32DECODE(b, dummy);      /* record size */
            dummy -= 4;
            
            if (HI_READ(fp, buffer, dummy) == FAIL)
                HRETURN_ERROR(DFE_READERROR,FALSE);
            
            b = buffer;
            INT32DECODE(b, dummy);         /* record type */
            INT32DECODE(b, vxrNext);       /* next VXR record */
            INT32DECODE(b, vix->nEntries); /* number of entries */
            INT32DECODE(b, vix->nUsed);    /* number of used entries */

            for(i = 0; i < vix->nEntries; i++)
                INT32DECODE(b, vix->firstRec[i]);
            for(i = 0; i < vix->nEntries; i++)
                INT32DECODE(b, vix->lastRec[i]);
            for(i = 0; i < vix->nEntries; i++)
                INT32DECODE(b, vix->offset[i]);

#ifdef DEBUG
            for(i = 0; i < vix->nEntries; i++)
                fprintf(stderr, "\t%d %d %d\n", (int)vix->firstRec[i], (int)vix->lastRec[i], (int)vix->offset[i]);
            fprintf(stderr, "Next record at %d\n", (int)vxrNext);
#endif

        }

        current_var++;

    }

    /* 
     * Loop over Zvariables and read them in
     */
    while(zVarNext != 0) {

#ifdef DEBUG
        fprintf(stderr,"zVariable %d seeking to %d\n", i, (int)zVarNext);
#endif

        if (HI_SEEK(fp, zVarNext) == FAIL)
            HRETURN_ERROR(DFE_SEEKERROR,FALSE);

        if (HI_READ(fp, buffer, 4) == FAIL)
            HRETURN_ERROR(DFE_READERROR,FALSE);

        b = buffer;
        INT32DECODE(b, dummy);      /* record size */
        dummy -= 4;
            
        if (HI_READ(fp, buffer, dummy) == FAIL)
            HRETURN_ERROR(DFE_READERROR,FALSE);
        
        b = buffer;
        INT32DECODE(b, dummy);      /* record type */
        INT32DECODE(b, zVarNext);    /* start of next R variable */
        INT32DECODE(b, nt);         /* number type */
        INT32DECODE(b, vMaxRec);    /* dunno */
        INT32DECODE(b, vxrNext);    /* start of VXR records */
        INT32DECODE(b, VXRtail);    /* ??? */
        INT32DECODE(b, vFlags);     /* variable flags <---- record variance in here */
        INT32DECODE(b, dummy);      /* rfuA */
        INT32DECODE(b, dummy);      /* rfuB */
        INT32DECODE(b, dummy);      /* rfuC */
        INT32DECODE(b, dummy);      /* rfuF */
        b += VDRzVDR_RFUe_LEN;      /* reserved field */
        INT32DECODE(b, numElem);    /* */
        INT32DECODE(b, num);        /* */
        INT32DECODE(b, dummy);      /* rfuD */
        INT32DECODE(b, dummy);      /* nextEndRecs */

        HDmemcpy(name, b, CDF_VAR_NAME_LEN);
        name[CDF_VAR_NAME_LEN] = '\0';
        b += CDF_VAR_NAME_LEN;

        j = 0;
        while((name[j] != ' ') && (name[j] != '\0')) j++;
        name[j] = '\0';

        /* MORE STUFF */
#ifdef DEBUG
        fprintf(stderr,"\tName %s nt %d vMaxRec %d\n", name, (int)nt, (int)vMaxRec);
#endif

        /*
         * Get the number of dimensions defined with this dataset
         * This is not necessarily the rank since we may not vary in
         *   some of them.  In that case, do not create dimension 
         *   objects for them.
         */
        INT32DECODE(b, numDims);        /* number of dimensions */

        /* 
         * read dimension sizes
         * figure out which ones are actually used
         * create dimension objects for the dimensions that are used
         */
        for(j = 0; j < numDims; j++)
            INT32DECODE(b, dim_sz[j]);

        rank = 0;
        for(j = 0; j < numDims; j++) {
            INT32DECODE(b, dummy);
            if(dummy) {
                char dimname[1000];
                sprintf(dimname, "%s%d_%d", name, (int)rank, (int)dim_sz[j]);
                dim_rec[current_dim] = NC_new_dim(dimname, dim_sz[j]);
                dims[rank++] = current_dim++;
            }
        }

#ifdef DEBUG
        fprintf(stderr,"\trank %d numDims %d\n", (int)rank, (int)numDims);
#endif

        /* map the CDF type into a netCDF type */
        nctype = cdf_unmap_type(nt);

        /* define the variable */
        var = vars[current_var] = NC_new_var((char *) name, nctype, (int) rank, dims);
        if(var == NULL)
            return (FALSE);
        
        /* if it is unsigned at least set the HDFtype to reflect it */
        switch(nt) {
        case CDF_UINT1:
            var->HDFtype = DFNT_UINT8;            break;
        case CDF_UINT2:
            var->HDFtype = DFNT_UINT16;           break;  
        case CDF_UINT4:
            var->HDFtype = DFNT_UINT32;           break;
        default:
            break;
        } 


        /* what's up with PadValues ??? */


        /*
         *  Mess with setting up VXR records
         */
        var->vixHead = end = NULL;
        while(vxrNext != 0) {
            vix_t * vix;

            vix = (vix_t *)HDmalloc(sizeof(vix_t));
            if(vix == NULL) 
                HRETURN_ERROR(DFE_NOSPACE,FALSE);
            
            /* stick vix at the end of our list and update end pointer */
            if(end == NULL)
                var->vixHead = end = vix;
            else
                end = end->next = vix;

            vix->next = NULL;

#ifdef DEBUG
            fprintf(stderr, "Reading a VXR record at %d\n", (int)vxrNext);
#endif
            /*
             * Read the next record out of the file 
             */
            if (HI_SEEK(fp, vxrNext) == FAIL)
                HRETURN_ERROR(DFE_SEEKERROR,FALSE);
            
            if (HI_READ(fp, buffer, 4) == FAIL)
            HRETURN_ERROR(DFE_READERROR,FALSE);
            
            b = buffer;
            INT32DECODE(b, dummy);      /* record size */
            dummy -= 4;
            
            if (HI_READ(fp, buffer, dummy) == FAIL)
                HRETURN_ERROR(DFE_READERROR,FALSE);
            
            b = buffer;
            INT32DECODE(b, dummy);         /* record type */
            INT32DECODE(b, vxrNext);       /* next VXR record */
            INT32DECODE(b, vix->nEntries); /* number of entries */
            INT32DECODE(b, vix->nUsed);    /* number of used entries */

            for(i = 0; i < vix->nEntries; i++)
                INT32DECODE(b, vix->firstRec[i]);
            for(i = 0; i < vix->nEntries; i++)
                INT32DECODE(b, vix->lastRec[i]);
            for(i = 0; i < vix->nEntries; i++)
                INT32DECODE(b, vix->offset[i]);

#ifdef DEBUG
            for(i = 0; i < vix->nEntries; i++)
                fprintf(stderr, "\t%d %d %d\n", (int)vix->firstRec[i], (int)vix->lastRec[i], (int)vix->offset[i]);
            fprintf(stderr, "Next record at %d\n", (int)vxrNext);
#endif

        }

        current_var++;

    } /* loop over zVariabes */

    /* 
     * Loop over Attributes and read them in
     */
    for(i = 0; i < numAttrs; i++) {

#ifdef DEBUG
        fprintf(stderr,"Attribute %d seeking to %d\n", i, (int)adrNext);
#endif
        
        if (HI_SEEK(fp, adrNext) == FAIL)
            HRETURN_ERROR(DFE_SEEKERROR,FALSE);

        if (HI_READ(fp, buffer, 4) == FAIL)
            HRETURN_ERROR(DFE_READERROR,FALSE);

        b = buffer;
        INT32DECODE(b, dummy);      /* record size */
        dummy -= 4;
            
        if (HI_READ(fp, buffer, dummy) == FAIL)
            HRETURN_ERROR(DFE_READERROR,FALSE);

        b = buffer;
        INT32DECODE(b, dummy);      /* record type */
        INT32DECODE(b, adrNext);    /* start of next ADR record */
        INT32DECODE(b, aedrNext);   /* start of next AEDR object */
        INT32DECODE(b, scope);      /* scope 1==global 2==local */
        INT32DECODE(b, dummy);      /* num */
        INT32DECODE(b, dummy);      /* numR */
        INT32DECODE(b, dummy);      /* maxR */
        INT32DECODE(b, dummy);      /* rfuA */
        INT32DECODE(b, aedzNext);   /* AzEDRhread */
        INT32DECODE(b, dummy);      /* NumZ */
        INT32DECODE(b, dummy);      /* MaxZ */
        INT32DECODE(b, dummy);      /* rfuE */

        HDmemcpy(name, b, CDF_ATTR_NAME_LEN);
        name[CDF_ATTR_NAME_LEN] = '\0';

#ifdef DEBUG
        fprintf(stderr,"\tname %s (%s) data at %d\n", name, (scope == 1 ? "global" : "local"), (int)aedrNext);
        fprintf(stderr,"\taedrNext %d    aedzNext %d\n", (int)aedrNext, (int)aedzNext);
        
#endif

        /*
         * Read in the AEDR records now and add them to the appropriate object
         */
        while(aedrNext != 0) {
            NC_array ** ap;
            NC_attr  *  attr[1];
            char     *  tBuf;
            int32       bsize;

            if (HI_SEEK(fp, aedrNext) == FAIL)
                HRETURN_ERROR(DFE_SEEKERROR,FALSE);
            
            if (HI_READ(fp, buffer, 4) == FAIL)
                HRETURN_ERROR(DFE_READERROR,FALSE);
            
            b = buffer;
            INT32DECODE(b, dummy);      /* record size */
            dummy -= 4;
            
            if (HI_READ(fp, buffer, dummy) == FAIL)
                HRETURN_ERROR(DFE_READERROR,FALSE);

            b = buffer;
            INT32DECODE(b, dummy);      /* record type */
            INT32DECODE(b, aedrNext);   /* start of next AEDR record */
            INT32DECODE(b, num);        /* attr (?) number */
            INT32DECODE(b, nt);         /* number type */
            INT32DECODE(b, num);        /* var (?) number */
            INT32DECODE(b, count);      /* number of elements */
            INT32DECODE(b, dummy);      /* rfuA */
            INT32DECODE(b, dummy);      /* rfuB */
            INT32DECODE(b, dummy);      /* rfuC */
            INT32DECODE(b, dummy);      /* rfuD */
            INT32DECODE(b, dummy);      /* rfuE */

            /* map the CDF type into a netCDF type */
            nctype  = cdf_unmap_type(nt);
            hdftype = hdf_map_type(nctype);

            bsize = nctypelen(nctype) * count;
            tBuf = (char *)HDmalloc((uint32) bsize);

            /* convert attrbute values and create attr object */
            DFKconvert((VOIDP) b, (VOIDP) tBuf, hdftype, count, DFACC_READ, 0, 0);
            attr[0] = NC_new_attr(name, nctype, count, tBuf);
            HDfree(tBuf);

            /* make sure we got a vaild attribute */
            if(attr[0] == NULL)
                return (FALSE);

            /* find the appropriate attribute list */
            if(scope == 1) {
                /* global attribute */
                ap = &(handle->attrs);
            } else {
                /* local --- find the appropriate variable */
                ap = &(vars[num]->attrs);
#ifdef DEBUG
        fprintf(stderr,"\tAdding %s (%s) to var %d \n", name, (scope == 1 ? "global" : "local"), (int)num);
#endif
            }

            /* add the attribute to the list */
            if(*ap == NULL) { 
                /* first time */
                (*ap) = NC_new_array(NC_ATTRIBUTE,(unsigned)1, (Void*)attr);
                if((*ap) == NULL)
                    return (FALSE);
            } else {
                if(NC_incr_array((*ap), (Void *)attr) == NULL)
                    return (FALSE);
            }
        } /* AEDR loop */

        /*
         * Read in the AEDZ records now and add them to the appropriate object
         * It is not clear to me how these are different from aedr records 
         *   except for that they are for Zvariables rather than Rvariables.
         *   Any other reasons?????
         */
        while(aedzNext != 0) {
            NC_array ** ap;
            NC_attr  *  attr[1];
            char     *  tBuf;
            int32       bsize;

#ifdef DEBUG
        fprintf(stderr,"\tReading aedz from %d\n", (int)aedzNext);
#endif

            if (HI_SEEK(fp, aedzNext) == FAIL)
                HRETURN_ERROR(DFE_SEEKERROR,FALSE);
            
            if (HI_READ(fp, buffer, 4) == FAIL)
                HRETURN_ERROR(DFE_READERROR,FALSE);
            
            b = buffer;
            INT32DECODE(b, dummy);      /* record size */
            dummy -= 4;
            
            if (HI_READ(fp, buffer, dummy) == FAIL)
                HRETURN_ERROR(DFE_READERROR,FALSE);

            b = buffer;
            INT32DECODE(b, dummy);      /* record type */
            INT32DECODE(b, aedzNext);   /* start of next AEDR record */
            INT32DECODE(b, num);        /* attr (?) number */
            INT32DECODE(b, nt);         /* number type */
            INT32DECODE(b, num);        /* var (?) number */
            INT32DECODE(b, count);      /* number of elements */
            INT32DECODE(b, dummy);      /* rfuA */
            INT32DECODE(b, dummy);      /* rfuB */
            INT32DECODE(b, dummy);      /* rfuC */
            INT32DECODE(b, dummy);      /* rfuD */
            INT32DECODE(b, dummy);      /* rfuE */

            /* map the CDF type into a netCDF type */
            nctype  = cdf_unmap_type(nt);
            hdftype = hdf_map_type(nctype);

            bsize = nctypelen(nctype) * count;
            tBuf = (char *)HDmalloc((uint32) bsize);

            /* convert attrbute values and create attr object */
            DFKconvert((VOIDP) b, (VOIDP) tBuf, hdftype, count, DFACC_READ, 0, 0);
            attr[0] = NC_new_attr(name, nctype, count, tBuf);
            HDfree(tBuf);

            /* make sure we got a vaild attribute */
            if(attr[0] == NULL)
                return (FALSE);

            /* find the appropriate attribute list */
            if(scope == 1) {
                /* global attribute */
                ap = &(handle->attrs);
            } else {
                /* local --- find the appropriate variable */
                ap = &(vars[num]->attrs);
#ifdef DEBUG
        fprintf(stderr,"\tAdding %s (%s) to Zvar %d \n", name, (scope == 1 ? "global" : "local"), (int)num);
#endif
            }

            /* add the attribute to the list */
            if(*ap == NULL) { 
                /* first time */
                (*ap) = NC_new_array(NC_ATTRIBUTE,(unsigned)1, (Void*)attr);
                if((*ap) == NULL)
                    return (FALSE);
            } else {
                if(NC_incr_array((*ap), (Void *)attr) == NULL)
                    return (FALSE);
            }
        } /* AEDZ loop */


    } /* ADR loop */

    /*
     * Set up the dimension list
     */
    if(current_dim)
        handle->dims = NC_new_array(NC_DIMENSION, current_dim, (Void *) dim_rec);
    else
        handle->dims = NULL;

    /* 
     * Set up the variable list define the variables 
     */
    if(current_var)
        handle->vars = NC_new_array(NC_VARIABLE, current_var, (Void *) vars);
    else
        handle->vars = NULL;

    return (TRUE);
        
} /* nssdc_read_cdf */


/* ---------------------------- nssdc_write_cdf ---------------------------- */
/*
  Write a NC structure out to a CDF file
*/
bool_t
nssdc_write_cdf(xdrs, handlep)
     XDR *  xdrs;
     NC  ** handlep;
{
#if DEBUG
    fprintf(stderr, "nssdc_write_cdf i've been called\n");
#endif

#ifdef DEBUG
    fprintf(stderr, "We are sorry, we currently do not support writing to\n");
    fprintf(stderr, "CDF files.  If you would like to see this capabilities\n");
    fprintf(stderr, "please contact the HDF group at NCSA.\n");
#endif

    return (FALSE);
        
} /* nssdc_write_cdf */


/* ---------------------------- nssdc_xdr_cdf ----------------------------- */
/*
  Handle basic I/O of the NC structure.
  CDF analogue of hdf_xdr_cdf and NC_xdr_cdf
*/
bool_t
nssdc_xdr_cdf(xdrs, handlep)
     XDR *  xdrs;
     NC  ** handlep;
{

    int status;

#if DEBUG
    fprintf(stderr, "nssdc_xdr_cdf i've been called op = %d \n", xdrs->x_op);
#endif
    
    switch(xdrs->x_op) {
    case XDR_ENCODE :
        status = nssdc_write_cdf(xdrs, handlep);
        break;
    case XDR_DECODE :
        status = nssdc_read_cdf(xdrs, handlep);
        break;
    case XDR_FREE   :
        NC_free_cdf((*handlep));
        status = TRUE;
        break;
    default:
        status = TRUE;
    }
    
    return (status);
  
} /* nssdc_xdr_cdf */

#endif /* HDF */
