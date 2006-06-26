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

#include "hrepack_an.h"

/*-------------------------------------------------------------------------
 * Function: copy_vg_an
 *
 * Purpose: copy Vgroup ANs
 *
 * Return: ok, 1, -1 not ok 
 *
 *-------------------------------------------------------------------------
 */

int copy_vg_an(int32 infile_id,
               int32 outfile_id,
               int32 vgroup_id,
               int32 vgroup_id_out, 
               char *path,
               options_t *options) 
{
 int32 ref_in,
       tag_in,
       ref_out,
       tag_out;

 if ( options->trip==0 ) 
 {
  return 1;
 }

 if ((ref_in = VQueryref(vgroup_id))==FAIL){
  printf( "Failed to get ref for <%s>\n", path);
  return-1;
 }
 if ((tag_in = VQuerytag(vgroup_id))==FAIL){
  printf( "Failed to get tag for <%s>\n", path);
  return-1;
 }
 if ((ref_out = VQueryref(vgroup_id_out))==FAIL){
  printf( "Failed to get ref for <%s>\n", path);
  return-1;
 }
 if ((tag_out = VQuerytag(vgroup_id_out))==FAIL){
  printf( "Failed to get tag for <%s>\n", path);
  return-1;
 }

 if (copy_an(infile_id,
  outfile_id,
  ref_in,
  tag_in,
  ref_out,
  tag_out, 
  path,
  options)<0)
   return FAIL;

 return SUCCESS;
}


/*-------------------------------------------------------------------------
 * Function: copy_vs_an
 *
 * Purpose: copy Vdata ANs
 *
 * Return: ok, 1, -1 not ok 
 *
 *-------------------------------------------------------------------------
 */

int copy_vs_an(int32 infile_id,
               int32 outfile_id,
               int32 vdata_id,
               int32 vdata_id_out, 
               char *path,
               options_t *options) 
{
 int32 ref_in,
       tag_in,
       ref_out,
       tag_out;

 if ( options->trip==0 ) 
 {
  return 1;
 }

 if ((ref_in = VSQueryref(vdata_id))==FAIL){
  printf( "Failed to get ref for <%s>\n", path);
  return-1;
 }
 if ((tag_in = VSQuerytag(vdata_id))==FAIL){
  printf( "Failed to get tag for <%s>\n", path);
  return-1;
 }
 if ((ref_out = VSQueryref(vdata_id_out))==FAIL){
  printf( "Failed to get ref for <%s>\n", path);
  return-1;
 }
 if ((tag_out = VSQuerytag(vdata_id_out))==FAIL){
  printf( "Failed to get tag for <%s>\n", path);
  return-1;
 }

 if (copy_an(infile_id,
  outfile_id,
  ref_in,
  tag_in,
  ref_out,
  tag_out, 
  path,
  options)<0)
   return FAIL;

 return 1;
}


/*-------------------------------------------------------------------------
 * Function: copy_an_data
 *
 * Purpose: copy DATA ANs
 *
 * Return: ok, 1, -1 not ok 
 *
 *-------------------------------------------------------------------------
 */

int copy_an_data(int32 infile_id,
                 int32 outfile_id,
                 int32 ref_in, 
                 int32 tag_in,
                 int32 ref_out, 
                 int32 tag_out,
                 ann_type type, 
                 char *path, 
                 options_t *options) 
{
 int32 an_id,         /* AN interface identifier */
       an_out,        /* AN interface identifier */
       ann_id,        /* an annotation identifier */
       ann_out,       /* an annotation identifier */
       i,             /* position of an annotation */
       ann_length,    /* length of the text in an annotation */
       n_anno;
       
 char *buf;           /* buffer to hold the read annotation */
 int  is_label= (type==AN_DATA_LABEL)?1:0;
 int  ret=0;

 if ( options->trip==0 ) 
 {
  return 1;
 }
 
 /* Initialize the AN interface  */
 an_id  = ANstart (infile_id);
 an_out = ANstart (outfile_id);

 /* Get the number of ANs in this object  */
 if((n_anno = ANnumann(an_id,type,(uint16)tag_in,(uint16)ref_in))==FAIL) {
  printf( "Failed to get annotations for <%s>\n", path);
  return-1;
 }
 
 for (i = 0; i < n_anno; i++) 
 {
 /*-------------------------------------------------------------------------
  * read
  *-------------------------------------------------------------------------
  */ 
  if((ann_id = ANselect(an_id,i,type))==FAIL) {
   printf( "Failed to select AN %d of <%s>\n", i, path);
   continue;
  }
  if((ann_length = ANannlen(ann_id))==FAIL) {
   printf( "Failed to get AN %d lenght of <%s>\n", i, path);
   continue;
  }
  
 /*
  * Read the data label.  Note that the size of the buffer,
  * i.e., the third parameter, is 1 character more than the length of
  * the data label; that is for the null character.  It is not the case
  * when a description is retrieved because the description does not 
  * necessarily end with a null character.
  * 
  */
  if (is_label)
   ann_length++;

  if ((buf = (char *)malloc((ann_length)*sizeof(int8)))==NULL ) {
   printf( "Failed to get memory for AN %d of <%s>\n", i, path);
   continue;
  }
  if(ANreadann(ann_id,buf,ann_length)==FAIL){
   printf( "Failed to read AN %d of <%s>\n", i, path);
   if (buf) free(buf);
   continue;
  }
  if(ANendaccess(ann_id)==FAIL){
   printf( "Failed to end AN %d of <%s>\n", i, path);
   if (buf) free(buf);
   continue;
  }
 /*-------------------------------------------------------------------------
  * write
  *-------------------------------------------------------------------------
  */  
 /* Create the data label for the vgroup identified by its tag and ref number */
  if((ann_out = ANcreate(an_out,(uint16)tag_out,(uint16)ref_out,type))==FAIL) {
   printf( "Failed to create AN %d of <%s>\n", i, path);
   continue;
  }
  /* Write the annotation  */
  if (ANwriteann (ann_out,buf,ann_length)==FAIL){
   printf( "Failed to write AN %d of <%s>\n", i, path);
  }
  if(ANendaccess(ann_out)==FAIL){
   printf( "Failed to end AN %d of <%s>\n", i, path);
   if (buf) free(buf);
   continue;
  }
  if (buf) free(buf);
 }
 
 /* Terminate access to the AN interface */
 if (ANend (an_id)==FAIL){
  printf( "Failed close AN for <%s>\n", path);
  ret=-1;
 }
 if (ANend (an_out)==FAIL){
  printf( "Failed close AN for <%s>\n", path);
  ret=-1;
 }
 return ret;
}


/*-------------------------------------------------------------------------
 * Function: copy_an
 *
 * Purpose: copy DATA ANs (AN_DATA_LABEL and AN_DATA_DESC)
 *
 * Return: ok, 1, -1 not ok 
 *
 *-------------------------------------------------------------------------
 */

int copy_an(int32 infile_id,
            int32 outfile_id,
            int32 ref_in, 
            int32 tag_in,
            int32 ref_out, 
            int32 tag_out,
            char *path, 
            options_t *options) 
{
 
 if (copy_an_data(infile_id,outfile_id,
  ref_in,tag_in,ref_out,tag_out, 
  AN_DATA_LABEL,path,options)<0)
  return FAIL;
 if (copy_an_data(infile_id,outfile_id,
  ref_in,tag_in,ref_out,tag_out,
  AN_DATA_DESC,path,options)<0)
  return FAIL;
 
 return SUCCESS;
}

