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


#include "hdf.h"
#include "mfhdf.h"
#include "hcomp.h"
#include "test_hrepack_verify.h"

/*-------------------------------------------------------------------------
 * Function: sds_verifiy_comp
 *
 * Purpose: utility function to verify compression for SDS_NAME
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 3, 2003
 *
 *-------------------------------------------------------------------------
 */

int sds_verifiy_comp(const char *sds_name, 
                     int32 in_comp_type, 
                     int32 in_comp_info)
{
 comp_coder_t  comp_type;    /* to retrieve compression type into */
 comp_info     comp_info;    /* compression structure */ 
 int32         sd_id,
               sds_id, 
               sds_index;   

 /* get chunk and comp */
 sd_id     = SDstart (FILENAME_OUT, DFACC_RDONLY);
 sds_index = SDnametoindex(sd_id, sds_name);
 if ((sds_id = SDselect(sd_id, sds_index))==FAIL) {
  printf("Error: Cannot open sds <%s>", sds_name);
  SDend (sd_id);
  return -1;
 }

/*-------------------------------------------------------------------------
 * retrieve and verify the compression info
 *-------------------------------------------------------------------------
 */
 
 comp_type = COMP_CODE_NONE;  /* reset variables before retrieving info */
 HDmemset(&comp_info, 0, sizeof(comp_info)) ;
 SDgetcompress(sds_id, &comp_type, &comp_info);
 if ( comp_type != in_comp_type )
 {
  printf("Error: Compression type does not match ");
  SDendaccess (sds_id);
  SDend (sd_id);
  return -1;
 }
 if (in_comp_info) {
  if ( comp_info.skphuff.skp_size != in_comp_info )
  {
   printf("Error: compresion information does not match ");
   SDendaccess (sds_id);
   SDend (sd_id);
   return -1;
  }
 }
 
 /* terminate access to the sds */
 SDendaccess (sds_id);
 
 /* terminate access to the sd interface */
 SDend (sd_id);
 
 return 0;
 
}

/*-------------------------------------------------------------------------
 * Function: sds_verifiy_comp_all
 *
 * Purpose: utility function to verify compression for all SDSs
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 3, 2003
 *
 *-------------------------------------------------------------------------
 */

int sds_verifiy_comp_all(int32 in_comp_type, 
                         int32 in_comp_info)
{
 comp_coder_t  comp_type;    /* to retrieve compression type into */
 comp_info     comp_info;    /* compression structure */ 
 int32         sd_id,
               sds_id, 
               sds_index,
               n_datasets,   /* number of datasets in the file */
               n_file_attrs, /* number of file attributes */
               data_type,              /* number type  */
               rrank,                  /* read rank */
               n_attrs,                /* number of attributes */
               dim_sizes[MAX_VAR_DIMS];/* dimensions of an image */
 char          name[MAX_GR_NAME];      /* name of dataset */
 int           info;
 int status;

 /* initialize the sd interface */
 sd_id  = SDstart (FILENAME_OUT, DFACC_READ);
 
 /* determine the number of data sets in the file */
 if (SDfileinfo (sd_id, &n_datasets, &n_file_attrs)==FAIL) {
  printf("Error: Cannot get file information");
  SDend (sd_id);
  return -1;
 }
 
 for (sds_index = 0; sds_index < n_datasets; sds_index++)
 {
  sds_id   = SDselect (sd_id, sds_index);
  
  /* skip dimension scales */
  if ( SDiscoordvar(sds_id) ) {
   SDendaccess(sds_id);
   continue;
  }

  name[0] = '\0';
  status=SDgetinfo(sds_id, name, &rrank, dim_sizes, &data_type, &n_attrs);
  if (status < 0) {
   printf("Error: can't read info for SDS <%s>",name);
   SDendaccess (sds_id);
   SDend (sd_id);
   return -1;
  }
 
 /*-------------------------------------------------------------------------
  * retrieve and verify the compression info
  *-------------------------------------------------------------------------
  */
  
  comp_type = COMP_CODE_NONE;  /* reset variables before retrieving info */
  HDmemset(&comp_info, 0, sizeof(comp_info)) ;
  
  status = SDgetcompress(sds_id, &comp_type, &comp_info);
  if (status < 0) {
   printf("Warning: can't read compression for SDS <%s>",name);
  } else {
  if ( comp_type != in_comp_type )
  {
   printf("Error: compression type does not match <%s>",name);
   SDendaccess (sds_id);
   SDend (sd_id);
   return -1;
  }
  if (in_comp_type) 
  {
   switch (in_comp_type)
   {
   case COMP_CODE_NONE:
    break;
   case COMP_CODE_RLE:
    break;
   case COMP_CODE_SZIP:
    break;
   case COMP_CODE_SKPHUFF:
    info  = comp_info.skphuff.skp_size;
    break;
   case COMP_CODE_DEFLATE:
    info  = comp_info.deflate.level;
    break;
   default:
    printf("Error: Unrecognized compression code %d\n",in_comp_type);
    break;
   };
   
   if ( info != in_comp_info )
   {
    printf("Error: compresion information does not match for <%s>",name);
    SDendaccess (sds_id);
    SDend (sd_id);
    return -1;
   }
  }
  }
  
  /* terminate access to the current dataset */
  SDendaccess (sds_id);
 }
 
 /* terminate access to the sd interface */
 SDend (sd_id);
 
 return 0;
}

/*-------------------------------------------------------------------------
 * Function: sds_verifiy_chunk
 *
 * Purpose: utility function to verify chunking for  SDS_NAME
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 3, 2003
 *
 *-------------------------------------------------------------------------
 */

int sds_verifiy_chunk(const char *sds_name, 
                      int32 in_chunk_flags, 
                      int rank, 
                      int32 *in_chunk_lengths)
{
 HDF_CHUNK_DEF chunk_def;    /* chunk defintion read */ 
 int32         chunk_flags;  /* chunking flag */ 
 int32         sd_id,
               sds_id, 
               sds_index;   
 int           i;

 /* get chunk and comp */
 sd_id     = SDstart (FILENAME_OUT, DFACC_RDONLY);
 sds_index = SDnametoindex(sd_id, sds_name);
 if ((sds_id = SDselect(sd_id, sds_index))==FAIL) {
  printf("Error: cannot open sds <%s>", sds_name);
  SDend (sd_id);
  return -1;
 }
 SDgetchunkinfo (sds_id, &chunk_def, &chunk_flags);

/*-------------------------------------------------------------------------
 * retrieve and verify the chunk info
 *-------------------------------------------------------------------------
 */
 if ( chunk_flags != (in_chunk_flags) )
 {
  printf("Error: chunk flags do not match");
  SDendaccess (sds_id);
  SDend (sd_id);
  return -1;
 }
 for (i = 0; i < rank; i++)
 {
  if (chunk_def.chunk_lengths[i] != in_chunk_lengths[i] )
  {
   printf("Error: chunk lengths do not match ");
   SDendaccess (sds_id);
   SDend (sd_id);
   return -1;
  }
 }

 /* terminate access to the sds */
 SDendaccess (sds_id);
 
 /* terminate access to the sd interface */
 SDend (sd_id);
 
 return 0;
 
}

/*-------------------------------------------------------------------------
 * Function: sds_verifiy_chunk_all
 *
 * Purpose: utility function to verify chunking for all SDSs
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 3, 2003
 *
 *-------------------------------------------------------------------------
 */

int sds_verifiy_chunk_all(int32 in_chunk_flags, 
                          int rank, 
                          int32 *in_chunk_lengths,
                          const char *sds_exclude)
{
 HDF_CHUNK_DEF chunk_def;    /* chunk defintion read */ 
 int32         chunk_flags;  /* chunking flag */ 
 int32         sd_id,
               sds_id, 
               sds_index,
               n_datasets,   /* number of datasets in the file */
               n_file_attrs, /* number of file attributes */
               data_type,              /* number type  */
               rrank,                  /* read rank */
               n_attrs,                /* number of attributes */
               dim_sizes[MAX_VAR_DIMS];/* dimensions of an image */
 char          name[MAX_GR_NAME];      /* name of dataset */
 int           i;

 /* initialize the sd interface */
 sd_id  = SDstart (FILENAME_OUT, DFACC_READ);
 
 /* determine the number of data sets in the file */
 if (SDfileinfo (sd_id, &n_datasets, &n_file_attrs)==FAIL) {
  printf("Error: cannot get file information");
  SDend (sd_id);
  return -1;
 }
 
 for (sds_index = 0; sds_index < n_datasets; sds_index++)
 {
  sds_id   = SDselect (sd_id, sds_index);
  
  /* skip dimension scales */
  if ( SDiscoordvar(sds_id) ) {
   SDendaccess(sds_id);
   continue;
  }

  SDgetinfo(sds_id, name, &rrank, dim_sizes, &data_type, &n_attrs);
  SDgetchunkinfo (sds_id, &chunk_def, &chunk_flags);

  /* do not compare this one */
  if (strcmp(name,sds_exclude)==0)
  {
   SDendaccess(sds_id);
   SDend (sd_id);
   return 0;
  }
  
 /*-------------------------------------------------------------------------
  * retrieve and verify the chunk info
  *-------------------------------------------------------------------------
  */
  if ( chunk_flags != (in_chunk_flags) )
  {
   printf("Error: chunk flags do not match");
   SDendaccess (sds_id);
   SDend (sd_id);
   return -1;
  }
  for (i = 0; i < rank; i++)
  {
   if (chunk_def.chunk_lengths[i] != in_chunk_lengths[i] )
   {
    printf("Error: chunk lengths do not match ");
    SDendaccess (sds_id);
    SDend (sd_id);
    return -1;
   }
  }
  
  /* terminate access to the current dataset */
  SDendaccess (sds_id);
 }
 
 /* terminate access to the sd interface */
 SDend (sd_id);
 
 return 0;
 
}

