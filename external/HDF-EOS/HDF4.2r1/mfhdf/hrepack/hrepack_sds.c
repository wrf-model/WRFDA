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



#include <assert.h>
#include "hdf.h"
#include "mfhdf.h"

#include "hrepack_sdutil.h"
#include "hrepack_sds.h"
#include "hrepack_an.h"
#include "hrepack_utils.h"
#include "hrepack_parse.h"
#include "hrepack_opttable.h"



/*-------------------------------------------------------------------------
 * Function: copy_sds
 *
 * Purpose: copy an SDS from input file to output file and compress it 
 *  using options
 *
 * Return: 0, -1 for error 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 10, 2003
 *
 *-------------------------------------------------------------------------
 */

int copy_sds(int32 sd_in,
             int32 sd_out,
             int32 tag,               /* tag of input SDS */
             int32 ref,               /* ref of input SDS */
             int32 vgroup_id_out_par, /* output parent group ID */
             char*path_name,          /* absolute path for input group name */
             options_t *options,
             table_t *table,
             int32 infile_id,
             int32 outfile_id)
{
 int32 sds_id,                /* data set identifier */
       sds_out,               /* data set identifier */
       sds_index,             /* index number of the data set */
       dtype,                 /* SDS data type */
       dimsizes[MAX_VAR_DIMS],/* dimensional size of SDS */
       start[MAX_VAR_DIMS],   /* read start */
       edges[MAX_VAR_DIMS],   /* read edges */
       nattrs,                /* number of SDS attributes */
       rank,                  /* rank of SDS */
       sds_ref,               /* reference number of the output data set */
       numtype,               /* number type */
       eltsz,                 /* element size */
       nelms,                 /* number of elements */
       dim_size,              /* dimension size */
       dim_id,                /* dimension ID */
       dim_out;               /* dimension ID */
 char             sds_name[MAX_NC_NAME]; 
 char             dim_name[MAX_NC_NAME];
 char             *path=NULL;
 VOIDP            buf=NULL;
 VOIDP            dim_buf=NULL;
 int              i, j, ret=1,stat;
 int              info;           /* temporary int compression information */
 comp_coder_t     comp_type;      /* compression type requested  */
 comp_coder_t     comp_type_in;   /* compression type original  */
 comp_info        c_info;         /* compression information requested  */
 comp_info        c_info_in;      /* compression information original  */
 HDF_CHUNK_DEF    chunk_def;      /* chunk definition */
 HDF_CHUNK_DEF    chunk_def_in;   /* chunk definition original */
 int32            chunk_flags;    /* chunk flags */ 
 int32            chunk_flags_in; /* chunk flags original*/ 
 int              szip_mode;      /* szip mode, EC, NN */
 intn             empty_sds;
 int              have_info=0;

 sds_out=FAIL;

 sds_index = SDreftoindex(sd_in,ref);
 sds_id    = SDselect(sd_in,sds_index);
 
 /*obtain name,rank,dimsizes,datatype and num of attributes of sds */
 if (SDgetinfo(sds_id,sds_name,&rank,dimsizes,&dtype,&nattrs)==FAIL){
  printf( "Could not get information for SDS\n");
  SDendaccess(sds_id);
  return -1;
 }

 /* check if the given SDS is a dimension scale, return 0 for no table add */
 if ( SDiscoordvar(sds_id) ) {
  SDendaccess(sds_id);
  return 0;
 }
 
 /* initialize path */
 path=get_path(path_name,sds_name);
 
 /* add object to table */
 table_add(table,tag,ref,path);

#if defined(HZIP_DEBUG)
 printf ("\t%s %d\n", path, ref); 
#endif
 
/*-------------------------------------------------------------------------
 * get the original compression/chunk information from the object 
 *-------------------------------------------------------------------------
 */
 
 comp_type_in = COMP_CODE_NONE;  /* reset variables before retrieving information */
 HDmemset(&c_info_in, 0, sizeof(comp_info)) ;
 stat=SDgetcompress(sds_id, &comp_type_in, &c_info_in);
 if (stat==FAIL && comp_type_in>0){
  printf( "Could not get compression information for SDS <%s>\n",path);
  SDendaccess(sds_id);
  return -1;
 }

 /* get chunk lengths */
 stat=SDgetchunkinfo(sds_id, &chunk_def_in, &chunk_flags_in);
 if (stat==FAIL){
  printf( "Could not get chunking information for SDS <%s>\n",path);
  SDendaccess(sds_id);
  return -1;
 }

 /* retrieve the compress information if so */
 if ( (HDF_CHUNK | HDF_COMP) == chunk_flags_in )
 {
  chunk_def_in.comp.comp_type=comp_type_in;
  switch (comp_type_in)
  {
  case COMP_CODE_RLE:
   chunk_def_in.comp.comp_type              = COMP_CODE_RLE;
   break;
  case COMP_CODE_SKPHUFF:
   chunk_def_in.comp.comp_type              = COMP_CODE_SKPHUFF;
   chunk_def_in.comp.cinfo.skphuff          = c_info_in.skphuff;
   break;
  case COMP_CODE_DEFLATE:
   chunk_def_in.comp.comp_type              = COMP_CODE_DEFLATE;
   chunk_def_in.comp.cinfo.deflate          = c_info_in.deflate;
   break;
  case COMP_CODE_SZIP:
#ifdef H4_HAVE_LIBSZ
   chunk_def_in.comp.comp_type              = COMP_CODE_SZIP;
   chunk_def_in.comp.cinfo.szip             = c_info_in.szip;
#else
   printf("Error: SZIP compression is not available <%s>\n",path);
  SDendaccess(sds_id);
  return -1;
#endif
   break;
  default:
   printf("Error: Unrecognized compression code in %d <%s>\n",comp_type_in,path);
  };
 }

/*-------------------------------------------------------------------------
 * set the default values to the ones read from the object
 *-------------------------------------------------------------------------
 */

 comp_type   = comp_type_in;
 switch (comp_type_in)
  {

  case COMP_CODE_NBIT:
   printf("Nbit compression not supported in this version <%s>\n",path);
   break;
  case COMP_CODE_NONE:
   break;
  case COMP_CODE_RLE:
   break;
  case COMP_CODE_SZIP:
#ifdef H4_HAVE_LIBSZ
   info      = c_info_in.szip.pixels_per_block;
 if (c_info_in.szip.options_mask & SZ_EC_OPTION_MASK) {
  szip_mode = EC_MODE;
 } else if (c_info_in.szip.options_mask & SZ_NN_OPTION_MASK) {
  szip_mode = NN_MODE;
 }
#else
   printf("SZIP compression not supported in this version <%s>\n",path);
#endif
   break;
  case COMP_CODE_SKPHUFF:
   info  = c_info_in.skphuff.skp_size;
   break;
  case COMP_CODE_DEFLATE:
   info  = c_info_in.deflate.level;
   break;
  default:
   printf("Error: Unrecognized compression code in %d <%s>\n",comp_type,path);
   break;
  };
 chunk_flags = chunk_flags_in;
 if ( (HDF_CHUNK) == chunk_flags )
 {
  for (i = 0; i < rank; i++) 
   chunk_def.chunk_lengths[i]      = chunk_def_in.chunk_lengths[i];
 }
 else if ( (HDF_CHUNK | HDF_COMP) == chunk_flags )
 {
  for (i = 0; i < rank; i++) {
   chunk_def.chunk_lengths[i]      = chunk_def_in.chunk_lengths[i];
   chunk_def.comp.chunk_lengths[i] = chunk_def_in.chunk_lengths[i];
  }
  chunk_def.comp.comp_type=comp_type_in;
  switch (comp_type_in)
  {
  case COMP_CODE_RLE:
   chunk_def.comp.comp_type              = COMP_CODE_RLE;
   break;
  case COMP_CODE_SKPHUFF:
   chunk_def.comp.comp_type              = COMP_CODE_SKPHUFF;
   chunk_def.comp.cinfo.skphuff          = c_info_in.skphuff;
   break;
  case COMP_CODE_DEFLATE:
   chunk_def.comp.comp_type              = COMP_CODE_DEFLATE;
   chunk_def.comp.cinfo.deflate         = c_info_in.deflate;
   break;
  case COMP_CODE_SZIP:
#ifdef H4_HAVE_LIBSZ
   chunk_def.comp.comp_type              = COMP_CODE_SZIP;
   chunk_def.comp.cinfo.szip             = c_info_in.szip;
#else
   printf("Error: SZIP compression not available in %d <%s>\n",comp_type_in,path);
#endif
   break;
  default:
   printf("Error: Unrecognized compression code in %d <%s>\n",comp_type_in,path);
  };
 }

/*-------------------------------------------------------------------------
 * get the compression/chunk information of this object from the table
 * translate to usable information
 * this is done ONLY for the second trip inspection 
 *-------------------------------------------------------------------------
 */
 
 /* check inspection mode */
 if ( options->trip>0 ) 
 {
  have_info = 
  options_get_info(options,      /* global options */
                   &chunk_flags, /* chunk flags OUT */
                   &chunk_def,   /* chunk definition OUT */
                   &info,        /* compression information OUT */
                   &szip_mode,   /* compression information OUT */
                   &comp_type,   /* compression type OUT  */
                   rank,         /* rank of object IN */
                   path,         /* path of object IN */
                   1,            /* number of GR image planes (for SZIP), IN */
                   dimsizes,     /* dimensions (for SZIP), IN */
                   dtype         /* numeric type ( for SZIP), IN */
                    );
  if (have_info==FAIL)
   comp_type=COMP_CODE_NONE;
 } /* check inspection mode */


/*-------------------------------------------------------------------------
 * get size before printing
 *-------------------------------------------------------------------------
 */

 /* compute the number of the bytes for each value. */
 numtype = dtype & DFNT_MASK;
 eltsz = DFKNTsize(numtype | DFNT_NATIVE);

 /* set edges of SDS */
 nelms=1;
 for (j = 0; j < rank; j++) {
  nelms   *= dimsizes[j];
  edges[j] = dimsizes[j];
  start[j] = 0;
 }

/*-------------------------------------------------------------------------
 * check for maximum number of chunks treshold
 *-------------------------------------------------------------------------
 */
 if ( options->trip>0 ) 
 {
  int count=1, nchunks;
  int maxchunk=INT_MAX;
  if ( (chunk_flags == HDF_CHUNK) || (chunk_flags == (HDF_CHUNK | HDF_COMP)) )
  {
   for (j = 0; j < rank; j++) {
    count   *= chunk_def.chunk_lengths[j];
   }
   nchunks=nelms/count;
   if (nchunks>maxchunk){
    printf("Warning: number of chunks is %d (greater than %d). Not chunking <%s>\n",
    nchunks,maxchunk,path);
    chunk_flags=HDF_NONE;
   }
  }
 }


/*-------------------------------------------------------------------------
 * check for objects too small
 *-------------------------------------------------------------------------
 */
 if ( have_info==1 && options->trip>0  && nelms*eltsz<options->threshold )
 {
  /* reset to the original values . we don't want to uncompress if it was */
  chunk_flags=chunk_flags_in;
  comp_type=comp_type_in;
  if (options->verbose) {
   printf("Warning: object size smaller than %d bytes. Not compressing <%s>\n",
    options->threshold,path);
  }
 }

/*-------------------------------------------------------------------------
 * print the PATH, COMP and CHUNK information
 *-------------------------------------------------------------------------
 */ 
 
 if (options->verbose)
 {
  int pr_comp_type=0;
  if (comp_type>0)
  {
   pr_comp_type=comp_type;
  }
  else
  {
   if (chunk_flags== (HDF_CHUNK | HDF_COMP))
   {
    pr_comp_type=chunk_def.comp.comp_type;
   }
  }
  printf(PFORMAT,
   (chunk_flags>0)?"chunk":"",                    /*chunk information*/
   (pr_comp_type>0)?get_scomp(pr_comp_type):"",   /*compression information*/
   path);                                         /*name*/
 }

/*-------------------------------------------------------------------------
 * check if the requested compression is valid
 * SDSs do not support JPEG
 *-------------------------------------------------------------------------
 */
 
 /* check inspection mode */
 if ( options->trip>0 ) 
 {
  switch(comp_type)
  {
  case COMP_CODE_NONE:
  case COMP_CODE_RLE:
  case COMP_CODE_SKPHUFF:
  case COMP_CODE_DEFLATE:
  case COMP_CODE_SZIP:
  case COMP_CODE_NBIT:
   break;
  case COMP_CODE_JPEG:
   printf("Error: JPEG compression is not available for <%s>\n",path);
   ret=FAIL;
   goto out;
   break;
  default:
   printf("Error: Unrecognized compression code %d in <%s>\n",comp_type_in,path);
   ret=FAIL;
   goto out;
  }
 } /* check inspection mode */



 

/*-------------------------------------------------------------------------
 * if we are in first trip inspection mode, exit, after printing the information
 *-------------------------------------------------------------------------
 */ 
 
 /* check inspection mode */
 if ( options->trip==0 ) {
  SDendaccess(sds_id);
  if (path) free(path);
  return 0;
 }

/*-------------------------------------------------------------------------
 * create new SDS
 *-------------------------------------------------------------------------
 */

 if ((sds_out = SDcreate(sd_out,sds_name,dtype,rank,dimsizes)) == FAIL) {
  printf( "Failed to create new SDS <%s>\n", path);
  ret=-1;
  goto out;
 }


/*-------------------------------------------------------------------------
 * set chunk 
 *
 * Chunked                  -> flags = HDF_CHUNK
 * Chunked and compressed   -> flags = HDF_CHUNK | HDF_COMP 
 * Non-chunked              -> flags = HDF_NONE
 *-------------------------------------------------------------------------
 */

 /* set chunk */
 if ( (chunk_flags == HDF_CHUNK) || (chunk_flags == (HDF_CHUNK | HDF_COMP)) )
 {
  if (SDsetchunk (sds_out, chunk_def, chunk_flags)==FAIL)
  {
   printf( "Error: Failed to set chunk dimensions for <%s>\n", path);
   ret=-1;
   goto out;
  }

 }

/*-------------------------------------------------------------------------
 * set compression
 *
 * COMP_CODE_RLE       -> simple RLE encoding
 * COMP_CODE_SKPHUFF   -> Skipping huffman encoding
 * COMP_CODE_DEFLATE   -> gzip 'deflate' encoding
 *-------------------------------------------------------------------------
 */
   
 /* use compress without chunk-in */
 else if ( chunk_flags==HDF_NONE && comp_type>COMP_CODE_NONE)  
 {
 if ( nelms*eltsz<options->threshold )
 {
  /* reset to the original values . we don't want to uncompress if it was */
    comp_type=COMP_CODE_NONE;
  if (options->verbose) {
   printf("Warning: object size smaller than %d bytes. Not compressing <%s>\n",
    options->threshold,path);
  } 
  } else  {

  /* setup compression factors */
  switch(comp_type) 
  {
  case COMP_CODE_SZIP:
   if (set_szip (info,szip_mode,&c_info)==FAIL)
   {
    comp_type=COMP_CODE_NONE;
   }
   break;
  case COMP_CODE_RLE:         
   break;
  case COMP_CODE_SKPHUFF:     
   c_info.skphuff.skp_size = info;
   break;
  case COMP_CODE_DEFLATE:
   c_info.deflate.level = info;
   break;
  case COMP_CODE_NBIT:
   comp_type = COMP_CODE_NONE;  /* not supported in this version */
   break;
  default:
   printf( "Error: Unrecognized compression code %d\n", comp_type);
  }

  if (SDsetcompress (sds_out, comp_type, &c_info)==FAIL)
  {
   printf( "Error: Failed to set compression for <%s>\n", path);
   ret=-1;
   goto out;
  }
  }
 }


/*-------------------------------------------------------------------------
 * check if the input SDS is empty. if so , do not read its data and write to new one
 *-------------------------------------------------------------------------
 */ 
 if (SDcheckempty( sds_id, &empty_sds ) == FAIL) {
  printf( "Failed to check empty SDS <%s>\n", path);
  ret=-1;
  goto out;
 }
/*-------------------------------------------------------------------------
 * read sds and write new one
 *-------------------------------------------------------------------------
 */
 if (empty_sds==0 )
 {
  /* alloc */
  if ((buf = (VOIDP) HDmalloc(nelms * eltsz)) == NULL) {
   printf( "Failed to allocate %d elements of size %d\n", nelms, eltsz);
   ret=-1;
   goto out;
  }
  
  /* read data */
  if (SDreaddata (sds_id, start, NULL, edges, buf) == FAIL) {
   printf( "Could not read SDS <%s>\n", path);
   ret=-1;
   goto out;
  }
  
  /* write the data */
  if (SDwritedata(sds_out, start, NULL, edges, buf) == FAIL) {
   printf( "Failed to write to new SDS <%s>\n", path);
   ret=-1;
   goto out;
  }
 } /* empty_sds */

/*-------------------------------------------------------------------------
 * copy attributes
 *-------------------------------------------------------------------------
 */ 
 
 if( copy_sds_attrs(sds_id,sds_out,nattrs,options)==FAIL) {
  ret=-1;
  goto out;
 }
 
/*-------------------------------------------------------------------------
 * copy dimension scales
 *-------------------------------------------------------------------------
 */ 
 
 /* loop through each dimension up to rank of SDS */
 for (i = 0; i < rank; i++) 
 {
  /* get dimension handle for input dimension */
  if ((dim_id = SDgetdimid(sds_id, i)) == FAIL) {
   printf( "Failed to get dimension %d of SDS <%s>\n", i, path);
   ret=-1;
   goto out;
  }
  /* get dimension handle for output dimension */
  if ((dim_out = SDgetdimid(sds_out, i)) == FAIL) {
   printf( "Failed to get dim_id for dimension %d of SDS <%s>\n", i, path);
   ret=-1;
   goto out;
  }
  /* get dimension information for input dimension */
  if (SDdiminfo(dim_id, dim_name, &dim_size, &dtype, &nattrs) == FAIL) {
   printf( "Failed to get information for dimension %d of SDS <%s>\n", i, path);
   ret=-1;
   goto out;
  }
  /* set output dimension name */
  if (SDsetdimname(dim_out, dim_name) == FAIL) {
   printf( "Failed to set dimension name %d of SDS <%s>\n", i, path);
   ret=-1;
   goto out;
  }
  /* copy attributes */
  if (nattrs && copy_sds_attrs(dim_id, dim_out, nattrs, options) == FAIL) {
   printf( "Failed to copy attributes for dimension %d of of SDS <%s>\n", i, path);
   ret=-1;
   goto out;
  }
  /* copy scale information over */
  if (dtype != 0) 
  {
   intn okdim;

   /* compute the number of the bytes for each value. */
   numtype = dtype & DFNT_MASK;
   eltsz = DFKNTsize(numtype | DFNT_NATIVE);

   if ((dim_buf = (VOIDP) HDmalloc(dim_size * eltsz)) == NULL) {
    printf( "Failed to alloc %d for dimension scale\n", dim_size);
    ret=-1;
    goto out;
   }
   if ((okdim=SDgetdimscale(dim_id, dim_buf)) == FAIL) {
    printf( "Warning: Failed to get scale information for %s\n", dim_name);
    /* SDgetdimscale incorrectly returns FAIL when
       the associated SDS has the same name as the dimension.
       we avoid returning FAIL, allowing for the rest of the file
       to be processed */
   }
   if (okdim!=FAIL && SDsetdimscale(dim_out, dim_size, dtype, dim_buf) == FAIL) {
    printf( "Failed to set scale information for %s\n", dim_name);
    ret=-1;
    goto out;
   }
   free(dim_buf);
  }
 }

 /* obtain the reference number of the new SDS using its identifier */
 if ((sds_ref = SDidtoref (sds_out)) == FAIL) {
  printf( "Failed to get new SDS reference in <%s>\n", path);
  ret=-1;
  goto out;
 }

/*-------------------------------------------------------------------------
 * add SDS to group
 *-------------------------------------------------------------------------
 */ 
 
 /* add it to group, if present */
 if (vgroup_id_out_par) 
 {
  /* add the SDS to the vgroup. the tag DFTAG_NDG is used */
  if (Vaddtagref (vgroup_id_out_par, TAG_GRP_DSET, sds_ref)==FAIL) {
   printf( "Failed to add new SDS to group <%s>\n", path);
   ret=-1;
   goto out;
  }
 }

/*-------------------------------------------------------------------------
 * copy ANs
 *-------------------------------------------------------------------------
 */ 
 
 if (copy_an(infile_id,outfile_id,
  ref,tag,sds_ref,tag, 
  path,options)<0) {
  ret=-1;
  goto out;
 }

out:
 /* terminate access to the SDSs */
 if (SDendaccess(sds_id)== FAIL )
  printf( "Failed to close SDS <%s>\n", path);
 if (sds_out!=FAIL) {
  if (SDendaccess (sds_out)== FAIL )
   printf( "Failed to close SDS <%s>\n", path);
 }
   
 if (path)
  free(path);
 if (buf)
  free(buf);

 return ret;
 
}


/*-------------------------------------------------------------------------
 * Function: copy_sds_attrs
 *
 * Purpose: copy SD attributes from input file to output file 
 *   used for global, dataset and dimension attributes
 *
 * Return: 1, for success, -1 for error 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 30, 2003
 *
 *-------------------------------------------------------------------------
 */

int copy_sds_attrs(int32 id_in,
                   int32 id_out,
                   int32 nattrs,          
                   options_t *options)
{
 int32 dtype,                 /* SDS data type */
       numtype,               /* number type */
       eltsz,                 /* element size */
       nelms;                 /* number of elements */
 char  attr_name[MAX_NC_NAME];
 VOIDP attr_buf=NULL;
 int   i;

 /* loop through attributes in input SDS */
 for (i = 0; i < nattrs; i++) 
 {
  if (SDattrinfo (id_in, i, attr_name, &dtype, &nelms) == FAIL) {
   printf( "Cannot get information for attribute number %d\n", i);
   return-1;
  }
  /* compute the number of the bytes for each value. */
  numtype = dtype & DFNT_MASK;
  eltsz   = DFKNTsize(numtype | DFNT_NATIVE);
  if ((attr_buf = (VOIDP) HDmalloc(nelms * eltsz)) == NULL) {
   printf( "Error allocating %d values of size %d for attribute %s",
    nelms, numtype, attr_name);
   return-1;
  }
  /* read attributes from input SDS */
  if (SDreadattr(id_in, i, attr_buf) == FAIL) {
   printf( "Cannot read attribute %s\n", attr_name);
   return-1;
  }
  /* put attributes into output SDS */
  if (SDsetattr(id_out, attr_name, dtype, nelms, attr_buf) == FAIL) {
   printf( "Cannot write attribute %s\n", attr_name);
   return-1;
  }

  if (attr_buf)
   free(attr_buf);
 }

 return 1;
}

