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

static void usage();
static int sds_get_compck(char *fname, char *sds_name);
static int sds_get_all(char *fname);
static const char* get_schunk(int32 flags);
static const char* get_scomp(comp_coder_t code);


int main(int argc, char **argv)
{
 if (argc==3)
  sds_get_compck(argv[1], argv[2]);
 else if (argc==2)
  sds_get_all(argv[1]);
 else {
  usage();
  return 1;
 }
 
 return 0;
}


/*-------------------------------------------------------------------------
 * Function: sds_get_compck
 *
 * Purpose: utility function to verify chunking and compressing for  SDS_NAME
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 3, 2003
 *
 *-------------------------------------------------------------------------
 */

static
int sds_get_compck(char *fname, char *sds_name)
{
 HDF_CHUNK_DEF chunk_def;    /* chunk defintion read */ 
 comp_coder_t  comp_type;    /* to retrieve compression type into */
 comp_info     comp_info;    /* compression structure */ 
 int32         chunk_flags;  /* chunking flag */ 
 int32         sd_id,
               sds_id, 
               sds_index,
               dimsizes[MAX_VAR_DIMS],/* dimensional size of SDS */
               nattrs,                /* number of SDS attributes */
               dtype,                 /* SDS data type */
               rank;                  /* rank of SDS */
 int           i;

 if ((sd_id = SDstart (fname, DFACC_RDONLY))==FAIL) {
  printf("Error: cannot open file <%s>\n", fname);
  return -1;
 }
 sds_index = SDnametoindex(sd_id, sds_name);
 if ((sds_id = SDselect(sd_id, sds_index))==FAIL) {
  printf("Error: cannot open sds <%s>\n", sds_name);
  SDend (sd_id);
  return -1;
 }
 SDgetchunkinfo (sds_id, &chunk_def, &chunk_flags);

 /*obtain name,rank,dimsizes,datatype and num of attributes of sds */
 SDgetinfo(sds_id,sds_name,&rank,dimsizes,&dtype,&nattrs);

/*-------------------------------------------------------------------------
 * print the dimensions
 *-------------------------------------------------------------------------
 */
 
 printf("dimensions:  [");
 for (i = 0; i < rank; i++)
 {
  printf("%d ", dimsizes[i]);
 }
 printf("]\n");

/*-------------------------------------------------------------------------
 * print the chunk info
 *-------------------------------------------------------------------------
 */

 printf("chunk flags:  %s \n", get_schunk(chunk_flags));
 if (HDF_NONE != chunk_flags )
 {
  printf("chunk dimension:  [");
  for (i = 0; i < rank; i++)
  {
   printf("%d ", chunk_def.chunk_lengths[i]);
  }
  printf("]\n");
 }


/*-------------------------------------------------------------------------
 * retrieve the compression info
 *-------------------------------------------------------------------------
 */
 
 comp_type = COMP_CODE_NONE;  /* reset variables before retrieving info */
 HDmemset(&comp_info, 0, sizeof(comp_info)) ;
 SDgetcompress(sds_id, &comp_type, &comp_info);
 
 printf("compression type:  %s \n", get_scomp(comp_type));
 if (COMP_CODE_NONE != comp_type )
 {
  switch (comp_type)
  {
  default:
   break;
  case COMP_CODE_RLE:
   break;
  case COMP_CODE_SKPHUFF:
   printf("skipping factor:  %d \n", comp_info.skphuff.skp_size);
   break;
  case COMP_CODE_DEFLATE:
   printf("level:  %d \n", comp_info.deflate.level);
   break;
  case COMP_CODE_JPEG:
   printf("quality factor:  %d \n", comp_info.jpeg.quality);
   break;
  case COMP_CODE_SZIP:
   printf("pixels per block:  %d \n", comp_info.szip.pixels_per_block);
   break;
  };
 }

 /* terminate access to the sds */
 SDendaccess (sds_id);
 
 /* terminate access to the sd interface */
 SDend (sd_id);
 
 return 0;
 
}

/*-------------------------------------------------------------------------
 * Function: sds_get_all
 *
 * Purpose: utility function to ptint all SDSs names
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 3, 2003
 *
 *-------------------------------------------------------------------------
 */

static
int sds_get_all(char *fname)
{
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

 /* initialize the sd interface */
  if ((sd_id = SDstart (fname, DFACC_RDONLY))==FAIL) {
  printf("Error: cannot open file <%s>\n", fname);
  return -1;
 }
 
 /* determine the number of data sets in the file */
 if (SDfileinfo (sd_id, &n_datasets, &n_file_attrs)==FAIL) {
  printf("Error: Cannot get file information\n");
  SDend (sd_id);
  return -1;
 }
 
 printf("List of sds:\n");
 for (sds_index = 0; sds_index < n_datasets; sds_index++)
 {
  sds_id   = SDselect (sd_id, sds_index);
  
  /* skip dimension scales */
  if ( SDiscoordvar(sds_id) ) {
   SDendaccess(sds_id);
   continue;
  }

  SDgetinfo(sds_id, name, &rrank, dim_sizes, &data_type, &n_attrs);
 
  printf("    %s\n", name);
 
  /* terminate access to the current dataset */
  SDendaccess (sds_id);
 }
 
 /* terminate access to the sd interface */
 SDend (sd_id);
 
 return 0;
}


/*-------------------------------------------------------------------------
 * Function: get_scomp
 *
 * Purpose: return the compression type as a string
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */

static
const char* get_scomp(comp_coder_t code)
{
 if (code==COMP_CODE_RLE)
  return "RLE";
 else if (code==COMP_CODE_SKPHUFF)
  return "HUFF";
 else if (code==COMP_CODE_DEFLATE)
  return "GZIP";
 else if (code==COMP_CODE_JPEG)
  return "JPEG";
 if (code==COMP_CODE_SZIP)
  return "SZIP";
 else if (code==COMP_CODE_NONE)
  return "NONE";
 else {
  printf("Input Error in compression type\n");
  exit(1);
 }
 /* not reached */
 return NULL;
} 


/*-------------------------------------------------------------------------
 * Function: get_schunk
 *
 * Purpose: return the chunking flags as a string
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */

static
const char* get_schunk(int32 flags)
{
 if (flags==(HDF_CHUNK | HDF_COMP))
  return "HDF_CHUNK | HDF_COMP";
 else if (flags==(HDF_CHUNK))
  return "HDF_CHUNK";
 else if (flags==(HDF_COMP))
  return "HDF_COMP";
 else if (flags==(HDF_NONE))
  return "HDF_NONE";
 else
  return "Invalid chunk flags";
} 


/*-------------------------------------------------------------------------
 * Function: usage
 *
 * Purpose: print usage
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */

static 
void usage()
{
 printf("hrepack_check file_name <sds_name>\n");
 printf("    file_name   HDF File\n");
 printf("    sds_name    SDS name (if no name, a list of all names is printed)\n");
}
