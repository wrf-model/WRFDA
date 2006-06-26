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
#include "hrepack_utils.h"


/*-------------------------------------------------------------------------
 * Function: is_reserved
 *
 * Purpose: check for reserved Vgroup/Vdata class/names
 *
 * Return: 1 if reserved, 0 if not
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 22, 2003
 *
 *-------------------------------------------------------------------------
 */

int is_reserved(char*vgroup_class)
{
 int ret=0;
 
 /* ignore reserved HDF groups/vdatas */
 if(vgroup_class != NULL) {
  if( (strcmp(vgroup_class,_HDF_ATTRIBUTE)==0) ||
   (strcmp(vgroup_class,_HDF_VARIABLE) ==0) || 
   (strcmp(vgroup_class,_HDF_DIMENSION)==0) ||
   (strcmp(vgroup_class,_HDF_UDIMENSION)==0) ||
   (strcmp(vgroup_class,DIM_VALS)==0) ||
   (strcmp(vgroup_class,DIM_VALS01)==0) ||
   (strcmp(vgroup_class,_HDF_CDF)==0) ||
   (strcmp(vgroup_class,GR_NAME)==0) ||
   (strcmp(vgroup_class,RI_NAME)==0) || 
   (strcmp(vgroup_class,RIGATTRNAME)==0) ||
   (strcmp(vgroup_class,RIGATTRCLASS)==0) ){
   ret=1;
  }

  /* class and name(partial) for chunk table i.e. Vdata */
  if( (strncmp(vgroup_class,"_HDF_CHK_TBL_",13)==0)){
   ret=1;
  }

 }
 
 return ret;
}



/*-------------------------------------------------------------------------
 * Function: get_path
 *
 * Purpose: return absolute path for an object
 *
 * Return: path
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 11, 2003
 *
 *-------------------------------------------------------------------------
 */

char *get_path(char*path_name, char*obj_name) 
{
 char *path=NULL;
 /* initialize path */
 if (path_name!=NULL) 
 {
  path = (char*) malloc(strlen(path_name) + strlen(obj_name) + 2);
  strcpy( path, path_name );
  strcat( path, "/" );
  strcat( path, obj_name ); 
 }
 else
 {
  path = (char*) malloc(strlen(obj_name) + 1);
  strcpy( path, obj_name ); 
 }
 return path;
}

