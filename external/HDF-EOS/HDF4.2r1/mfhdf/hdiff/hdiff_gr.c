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


#include "hdiff.h"
#include "hdiff_list.h"
#include "hdiff_mattbl.h"


/*-------------------------------------------------------------------------
 * Function: diff_gr
 *
 * Purpose: diff for GR
 *
 * Return: Number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 27, 2003
 *
 *-------------------------------------------------------------------------
 */

int diff_gr( int32 file1_id,
             int32 file2_id,
             int32 gr1_id,              
             int32 gr2_id,
             int32 ref1,              
             int32 ref2,
             diff_opt_t *opt)  
{
 int32 ri1_id,                 /* data set identifier */
       ri1_index,              /* index number of the data set */
       dtype1,                 /* GR data type */
       dimsizes1[MAX_VAR_DIMS],/* dimensional size */
       nattrs1,                /* number of attributes */
       ncomps1,                /* number of components */
       interlace_mode1,        /* interlace mode of an image */ 
       ri2_id,                 /* data set identifier */
       ri2_index,              /* index number of the data set */
       dtype2,                 /* GR data type */
       dimsizes2[MAX_VAR_DIMS],/* dimensional size */
       nattrs2,                /* number of attributes */
       ncomps2,                /* number of components */
       interlace_mode2,        /* interlace mode of an image */ 
       start[MAX_VAR_DIMS],    /* read start */
       edges[MAX_VAR_DIMS],    /* read edges */
       numtype,                /* number type */
       eltsz,                  /* element size */
       nelms,                  /* number of elements */
       data_size;
 char  gr1_name[MAX_NC_NAME]; 
 char  gr2_name[MAX_NC_NAME]; 
 int   nfound=0;
 int   dim_diff=0;             /* dimensions are different */
 VOIDP buf1=NULL;
 VOIDP buf2=NULL;
 uint32 max_err_cnt;
 int   i, cmp;


/*-------------------------------------------------------------------------
 * object 1
 *-------------------------------------------------------------------------
 */
 
 ri1_index = GRreftoindex(gr1_id,(uint16)ref1);
 ri1_id    = GRselect(gr1_id,ri1_index);
   
 /*obtain name,rank,dimsizes,datatype and num of attributes of gr */
 if (GRgetiminfo(ri1_id,gr1_name,&ncomps1,&dtype1,&interlace_mode1,dimsizes1,&nattrs1)==FAIL) {
   printf( "Failed to get info for SDS ref <%d>\n",ref1);
   GRendaccess(ri1_id);
   return FAIL;
  }


/*-------------------------------------------------------------------------
 * object 2
 *-------------------------------------------------------------------------
 */
 
 ri2_index = GRreftoindex(gr2_id,(uint16)ref2);
 ri2_id    = GRselect(gr2_id,ri2_index);
   
 /*obtain name,rank,dimsizes,datatype and num of attributes of gr */
 if (GRgetiminfo(ri2_id,gr2_name,&ncomps2,&dtype2,&interlace_mode2,dimsizes2,&nattrs2)==FAIL) {
   printf( "Failed to get info for SDS ref <%d>\n",ref2);
   GRendaccess(ri2_id);
   return FAIL;
  }


/*-------------------------------------------------------------------------
 * check for different type
 *-------------------------------------------------------------------------
 */
 
 if (dtype1 != dtype2) 
 {
  printf("Comparison not supported\n");
  printf("<%s> has datatype %d, <%s> has datatype %d ",gr1_name,dtype1,gr2_name,dtype2);
  goto out;
 }

/*-------------------------------------------------------------------------
 * check for the same rank
 *-------------------------------------------------------------------------
 */
 
 if ( ncomps1 != ncomps2 )
 {
  printf("Comparison not supported\n");
  printf("<%s> has %d components\n", gr1_name, ncomps1);
  printf("\n" );
  printf("<%s> has %d components\n", gr2_name, ncomps2);
  goto out;
 }

/*-------------------------------------------------------------------------
 * check for different dimensions
 *-------------------------------------------------------------------------
 */
 
 for ( i=0; i<2; i++) 
 {
  if ( dimsizes1[i] != dimsizes2[i] )
   dim_diff=1;
 }

/*-------------------------------------------------------------------------
 * dimensions
 *-------------------------------------------------------------------------
 */

 if (dim_diff==1)
 {
  printf("Comparison not supported\n");
  printf("<%s> has dimensions ", gr1_name);
  print_dims(2,dimsizes1);
  printf("\n" );
  printf("<%s> has dimensions ", gr2_name);
  print_dims(2,dimsizes2);
  goto out;
 }

/*-------------------------------------------------------------------------
 * match interlace 
 * NOTE: GR images are always stored as pixel_interlace (0) on disk
 *       that does not happen with images saved with the 
 *       DF24 - Single-file 24-Bit Raster Image Interface,
 *       where the interlace mode on disk can be 0, 1 or 2
 *-------------------------------------------------------------------------
 */
 if ( interlace_mode1 != interlace_mode2 )
 {
  if (opt->verbose)
  printf("Warning: different interlace mode: <%d> and <%d>\n", 
   interlace_mode1,interlace_mode2);
  interlace_mode1=interlace_mode2;
 }


/*-------------------------------------------------------------------------
 * get size 
 *-------------------------------------------------------------------------
 */

 /* compute the number of the bytes for each value. */
 numtype = dtype1 & DFNT_MASK;
 eltsz = DFKNTsize(numtype | DFNT_NATIVE);

 /* set edges of SDS */
 nelms=1;
 for (i = 0; i < 2; i++) {
  nelms   *= dimsizes1[i];
  edges[i] = dimsizes1[i];
  start[i] = 0;
 }

 data_size = dimsizes1[0]*dimsizes1[1]*ncomps1*eltsz;

/*-------------------------------------------------------------------------
 * Read image 1
 *-------------------------------------------------------------------------
 */
 
 /* alloc */
 if ((buf1 = (VOIDP) HDmalloc(data_size)) == NULL) {
  printf( "Failed to allocate %d elements of size %d\n", nelms, eltsz);
  goto out;
 }
 
 /* set the interlace for reading  */
 if ( GRreqimageil(ri1_id, interlace_mode1) == FAIL ){
  printf( "Could not set interlace for GR <%s>\n", gr1_name);
  goto out;
 }
 
 /* read data */
 if (GRreadimage (ri1_id, start, NULL, edges, buf1) == FAIL) {
  printf( "Could not read GR <%s>\n", gr1_name);
  goto out;
 }

/*-------------------------------------------------------------------------
 * Read image 2
 *-------------------------------------------------------------------------
 */

 /* alloc */
 if ((buf2 = (VOIDP) HDmalloc(data_size)) == NULL) {
  printf( "Failed to allocate %d elements of size %d\n", nelms, eltsz);
  goto out;
 }
 
 /* set the interlace for reading  */
 if ( GRreqimageil(ri2_id, interlace_mode2) == FAIL ){
  printf( "Could not set interlace for GR <%s>\n", gr2_name);
  goto out;
 }
 
 /* read data */
 if (GRreadimage (ri2_id, start, NULL, edges, buf2) == FAIL) {
  printf( "Could not read GR <%s>\n", gr2_name);
  goto out;
 }


/*-------------------------------------------------------------------------
 * Comparing
 *-------------------------------------------------------------------------
 */
 

 if (opt->verbose)
 printf("Comparing <%s>\n",gr1_name); 

 cmp = HDmemcmp(buf1,buf2,data_size);
 if (cmp!=0)
 {
  
 /* 
  If max_err_cnt is set (i.e. not its default -1), use it otherwise set it
  to tot_err_cnt so it doesn't trip  
  */
  max_err_cnt = (opt->max_err_cnt >= 0) ? opt->max_err_cnt : nelms;
  nfound=array_diff(
   buf1, 
   buf2, 
   nelms, 
   gr1_name,
   gr2_name,
   2,
   dimsizes1,
   dtype1, 
   opt->err_limit, 
   max_err_cnt, 
   opt->statistics, 
   0, 
   0);
 }
  
/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */

out:
 if (GRendaccess(ri1_id)<0)
  printf("GRendaccess returned -1");
 if (GRendaccess(ri2_id)<0)
  printf("GRendaccess returned -1");
 if (buf1) free(buf1);
 if (buf2) free(buf2);

 return nfound;
}

