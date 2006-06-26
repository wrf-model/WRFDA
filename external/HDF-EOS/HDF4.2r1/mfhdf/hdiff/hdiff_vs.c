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
 * Function: diff_vs
 *
 * Purpose: diff for VS 
 *
 * Return: Number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 26, 2003
 *
 *-------------------------------------------------------------------------
 */

int diff_vs( int32 file1_id,
             int32 file2_id,
             int32 ref1,              
             int32 ref2,
             diff_opt_t * opt)          
{
 int32 vdata1_id,             /* vdata identifier */
       n_records1,            /* number of records */
       vdata1_size, 
       interlace1_mode,
       vdata2_id,             /* vdata identifier */
       n_records2,            /* number of records */
       vdata2_size, 
       interlace2_mode;
 char  vdata1_name [VSNAMELENMAX];
 char  vdata1_class[VSNAMELENMAX];
 char  fieldname1_list[VSFIELDMAX*FIELDNAMELENMAX];
 char  vdata2_name [VSNAMELENMAX];
 char  vdata2_class[VSNAMELENMAX];
 char  fieldname2_list[VSFIELDMAX*FIELDNAMELENMAX];
 int   ret=0;


 
/*-------------------------------------------------------------------------
 * object 1
 *-------------------------------------------------------------------------
 */

 if (Vstart (file1_id)==FAIL) {
  printf("Error: Could not start VS interface in VS ref %d\n", ref1);
  return FAIL;
 }

 if ((vdata1_id  = VSattach (file1_id, ref1, "r")) == FAIL ){
  printf( "Failed to attach VS ref %d\n", ref1);
  return FAIL;
 }
 if (VSgetname  (vdata1_id, vdata1_name) == FAIL ){
  printf( "Failed to name for VS ref %d\n", ref1);
  ret=-1;
  goto out;
 }
 if (VSgetclass (vdata1_id, vdata1_class) == FAIL ){
  printf( "Failed to name for VS ref %d\n", ref1);
  ret=-1;
  goto out;
 }
 
 if (VSinquire(vdata1_id, &n_records1, &interlace1_mode, fieldname1_list, 
  &vdata1_size, vdata1_name) == FAIL) {
  printf( "Failed to get info for VS ref %d\n", ref1);
  ret=-1;
  goto out;
 }
 
 if (VFnfields(vdata1_id)== FAIL ){
  printf( "Failed getting fields forVS ref %d\n", ref1);
  ret=-1;
  goto out;
 }
 

/*-------------------------------------------------------------------------
 * object 2
 *-------------------------------------------------------------------------
 */

 if (Vstart (file2_id)==FAIL) {
  printf("Error: Could not start VS interface in VS ref %d\n", ref1);
  return FAIL;
 }

 if ((vdata2_id  = VSattach (file2_id, ref2, "r")) == FAIL ){
  printf( "Failed to attach VS ref %d\n", ref2);
  ret=-2;
  goto out;
 }
 if (VSgetname  (vdata2_id, vdata2_name) == FAIL ){
  printf( "Failed to name for VS ref %d\n", ref2);
  ret=-2;
  goto out;
 }
 if (VSgetclass (vdata2_id, vdata2_class) == FAIL ){
  printf( "Failed to name for VS ref %d\n", ref2);
  ret=-2;
  goto out;
 }
 
 if (VSinquire(vdata2_id, &n_records2, &interlace2_mode, fieldname2_list, 
  &vdata2_size, vdata2_name) == FAIL) {
  printf( "Failed to get info for VS ref %d\n", ref2);
  ret=-2;
  goto out;
 }
 
 if (VFnfields(vdata2_id)== FAIL ){
  printf( "Failed getting fields forVS ref %d\n", ref2);
  ret=-2;
  goto out;
 }

/*-------------------------------------------------------------------------
 * check for input VSs
 *-------------------------------------------------------------------------
 */
 
 if (opt->nuvars > 0)   /* if specified vdata is selected */
 {
  int imatch = 0, j;
  for (j = 0; j < opt->nuvars; j++)
  {
   if (strcmp(vdata1_name, opt->uvars[j]) == 0)
   {
    imatch = 1;
    break;
   }
  }
  if (imatch == 0)
  {
   goto out;
  }
 }   


/*-------------------------------------------------------------------------
 * Comparing
 *-------------------------------------------------------------------------
 */

 if (opt->verbose)
 printf("Comparing <%s>\n",vdata1_name);  

 ret=vdata_cmp(vdata1_id,vdata2_id,vdata1_name,vdata1_class,opt->max_err_cnt);

out:
 /* terminate access to the VSs */
 if (VSdetach (vdata1_id)==FAIL) {
  printf( "Failed to dettach VS ref %d\n", ref1);
 }
 if (VSdetach (vdata2_id)==FAIL) {
  printf( "Failed to dettach VS ref %d\n", ref2);
 }
 
 return ret;
}





