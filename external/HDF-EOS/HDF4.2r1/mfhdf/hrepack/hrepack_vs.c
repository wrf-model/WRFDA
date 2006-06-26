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

#include "hrepack_vs.h"
#include "hrepack_utils.h"
#include "hrepack_an.h"


/*-------------------------------------------------------------------------
 * Function: copy_vs
 *
 * Purpose: copy a VS from input file to output file
 *
 * Return: 0, -1 for error 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 11, 2003
 *
 *-------------------------------------------------------------------------
 */

int copy_vs( int32 infile_id,
             int32 outfile_id,
             int32 tag,               /* tag of input VS */
             int32 ref,               /* ref of input VS */
             int32 vgroup_id_out_par, /* output parent group ID */
             char*path_name,          /* absolute path for input group name */
             options_t *options,
             table_t *table,
             int   is_lone)
{
 int32 vdata_id,              /* vdata identifier */
       vdata_out,             /* vdata identifier */
       vdata_ref,             /* reference number of the vdata */
       n_records,             /* number of records */
       vdata_size, 
       interlace_mode,
       field_type, 
       field_order;
 int   n_fields, n_attrs;
 char  vdata_name [VSNAMELENMAX], vdata_class[VSNAMELENMAX];
 char  fieldname_list[VSFIELDMAX*FIELDNAMELENMAX];
 char  *path=NULL;
 char  *field_name;
 uint8 *buf=NULL;
 int   i, j, ret=1;

/*-------------------------------------------------------------------------
 * attach the vdata, gets its name and class
 *-------------------------------------------------------------------------
 */ 

 if ((vdata_id  = VSattach (infile_id, ref, "r")) == FAIL ){
  printf( "Failed to attach vdata ref %d\n", ref);
  return-1;
 }
 if (VSgetname  (vdata_id, vdata_name)==FAIL){
  printf( "Failed to name for vdata ref %d\n", ref);
  return-1;
 }
 if (VSgetclass (vdata_id, vdata_class)==FAIL){
  printf( "Failed to name for vdata ref %d\n", ref);
  return-1;
 }
 
 /* ignore reserved HDF groups/vdatas; they are lone ones */
 if( is_lone==1 && vdata_class != NULL) {
  if( is_reserved(vdata_class)){
   if (VSdetach (vdata_id)== FAIL )
    printf( "Failed to detach vdata <%s>\n", path_name);
   return 0;
  }
 }
 
 /* initialize path */
 path=get_path(path_name,vdata_name);
 
 /* add object to table */
 table_add(table,tag,ref,path);
 
#if defined(HZIP_DEBUG)
 printf ("\t%s %d\n", path, ref); 
#endif
 
 if (options->verbose)
 {
  printf(PFORMAT,"","",path);    
 }
 
 /* check inspection mode */
 if ( options->trip==0 ) {
  if (VSdetach (vdata_id)==FAIL)
   printf( "Failed to detach vdata <%s>\n", path_name);
  if (path) free(path);
  return 0;
 }
 
 
/*-------------------------------------------------------------------------
 * get vdata info
 *-------------------------------------------------------------------------
 */ 
 
 if (VSinquire(vdata_id, &n_records, &interlace_mode, fieldname_list, 
  &vdata_size, vdata_name) == FAIL) {
  printf( "Failed to get info for vdata ref %d\n", ref);
  if (path) free(path);
  return-1;
 }
 
#if defined( HZIP_DEBUG)
 printf( 
  "Transferring vdata %s: class=%s, %d recs, interlace=%d, size=%d\n\tfields='%s'\n",
  vdata_name, vdata_class, n_records, interlace_mode, vdata_size, 
  fieldname_list);
#endif
 
 
 /*-------------------------------------------------------------------------
  * create the VS in the output file.  the vdata reference number is set
  * to -1 for creating and the access mode is "w" for writing 
  *-------------------------------------------------------------------------
  */ 
 
 if ((vdata_out  = VSattach (outfile_id, -1, "w")) == FAIL) {
  printf( "Failed to create new VS <%s>\n", path);
  VSdetach (vdata_id);
  if (path) free(path);
  return -1;
 }
 if (VSsetname (vdata_out, vdata_name)== FAIL) {
  printf( "Failed to set name for new VS <%s>\n", path);
  ret=-1;
  goto out;
 }
 if (VSsetclass(vdata_out, vdata_class)== FAIL) {
  printf( "Failed to set class for new VS <%s>\n", path);
  ret=-1;
  goto out;
 }
 if (VSsetinterlace(vdata_out, interlace_mode) == FAIL) {
  printf( "Failed to set interlace mode for output vdata\n");
  ret=-1;
  goto out;
 }


/*-------------------------------------------------------------------------
 * define the fields for vdata_out
 *-------------------------------------------------------------------------
 */ 
 
 if ((n_fields = VFnfields(vdata_id)) == FAIL ){
  printf( "Failed getting fields for VS <%s>\n", path);
  ret=-1;
  goto out;
 }
 
 for (i = 0; i < n_fields; i++) {
  field_name = VFfieldname(vdata_id, i);
  field_type = VFfieldtype(vdata_id, i);
  field_order = VFfieldorder(vdata_id, i);
  if (VSfdefine(vdata_out, field_name, field_type, field_order) == FAIL) {
   printf( "Error: cannot define fields for VS <%s>\n", path);
   ret=-1;
   goto out;
  }
 }
 
 /* Set fields */
 if (VSsetfields(vdata_out, fieldname_list)==FAIL) {
  printf( "Error: cannot define fields for VS <%s>\n", path);
  ret=-1;
  goto out;
 }

 
/*-------------------------------------------------------------------------
 * read, write vdata
 *-------------------------------------------------------------------------
 */ 

 /* Set fields for reading */
 if (VSsetfields(vdata_id, fieldname_list)== FAIL) {
   printf( "Error: cannot define fields for VS <%s>\n", path);
   ret=-1;
   goto out;
  }
 if (n_records>0)
 {
  if ((buf = (uint8 *)malloc( (size_t)(n_records * vdata_size))) == NULL ){
   printf( "Failed to get memory for new VS <%s>\n", path);
   ret=-1;
   goto out;
  }
  if (VSread(vdata_id, buf, n_records, interlace_mode) == FAIL) {
   printf( "Error reading vdata <%s>\n", path);
   ret=-1;
   goto out;
  }
  if (VSwrite(vdata_out, buf, n_records, interlace_mode) == FAIL) {
   printf( "Error writing vdata <%s>\n", path);
   ret=-1;
   goto out;
  }
 }
 

/*-------------------------------------------------------------------------
 * read, write attributes
 *-------------------------------------------------------------------------
 */ 
 
 if ((n_attrs = VSfnattrs( vdata_id, -1 )) == FAIL ){
  printf( "Failed getting attributes for VS <%s>\n", path);
  ret=-1;
  goto out;
 }
 for (i = 0; i < n_attrs; i++) {
  copy_vdata_attribute(vdata_id, vdata_out, -1, i);
 }
 
/*-------------------------------------------------------------------------
 * read, write field attributes
 *-------------------------------------------------------------------------
 */ 
  
 for (i = 0; i < n_fields; i++) {
  if ((n_attrs = VSfnattrs(vdata_id, i)) == FAIL ){
   printf( "Failed getting fields for VS <%s>\n", path);
   ret=-1;
   goto out;
  }
  for (j = 0; j < n_attrs; j++) {
   copy_vdata_attribute(vdata_id, vdata_out, i, j);
  }
 }
  
/*-------------------------------------------------------------------------
 * add VS to group, if needed
 *-------------------------------------------------------------------------
 */ 
 if (vgroup_id_out_par) 
 {
  /* obtain the reference number of the new VS  */
  if ((vdata_ref=VSQueryref(vdata_out)) == 0) {
   printf( "Failed to get new VS reference in <%s>\n", path);
  }
  
  /* add the VS to the vgroup. the INPUT TAG is used */
  if (Vaddtagref (vgroup_id_out_par, tag, vdata_ref)== FAIL) {
   printf( "Failed to add new VS to group <%s>\n", path);
  }
 }

/*-------------------------------------------------------------------------
 * copy ANs
 *-------------------------------------------------------------------------
 */ 

 if (copy_vs_an(infile_id,outfile_id,vdata_id,vdata_out,path,options)<0) {
  ret=-1;
  goto out;
 }
 
out:
 /* terminate access to the VSs */
 if (VSdetach (vdata_id)==FAIL||
     VSdetach (vdata_out)==FAIL){
  printf( "Could not detach VG in <%s>\n", path);
 }
 
 if (path)
  free(path);
 if (buf)
  free(buf);
 
 return ret;
}


/*-------------------------------------------------------------------------
 * Function: copy_vdata_attribute
 *
 * Purpose: copy VS attributes from input file to output file 
 *
 * Return: 1, for success, -1 for error 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 28, 2003
 *
 *-------------------------------------------------------------------------
 */

int copy_vdata_attribute(int32 in, int32 out, int32 findex, intn attrindex)
{
 char   attr_name[MAX_NC_NAME];
 int32  n_values, attr_size, attr_type;
 VOIDP  *values=NULL;

 /* Get attribute information */
 VSattrinfo(in, findex, attrindex, attr_name, &attr_type, &n_values, &attr_size);

 /* Allocate space for attribute values */
 if ((values = (VOIDP)malloc((size_t)(attr_size * n_values))) == NULL) {
  printf( "Cannot allocate %d values of size %d for attribute %s",
   n_values, attr_size, attr_name);
  return-1;
 }

 /* Read attribute from input object */
 if (VSgetattr(in, findex, attrindex, values) == FAIL) {
  printf( "Cannot read attribute %s\n", attr_name);
  if (values) free(values);
  return-1;
 }

 /* Write attribute to output object */
 if (VSsetattr(out, findex, attr_name, attr_type, n_values, values) == FAIL) {
  printf( "Cannot write attribute %s\n", attr_name);
  if (values) free(values);
  return-1;
 }

 if (values) free(values);

 return 1;
}



