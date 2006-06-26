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

#include <stdio.h>
#include <stdlib.h>

#include "hdiff_table.h"

/*-------------------------------------------------------------------------
 * Function: dtable_search
 *
 * Purpose: search the table for tag and ref
 *
 * Return: index on success, -1 on failure
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 3, 2003
 *
 *-------------------------------------------------------------------------
 */

int dtable_search(dtable_t *table, int32 tag, int32 ref )
{
 int i;
 
 for (i = 0; i < table->nobjs; i++)
  if (table->objs[i].tag == tag && table->objs[i].ref == ref)
   return i;
  
  return -1;
}


/*-------------------------------------------------------------------------
 * Function: dtable_add
 *
 * Purpose: add pair tag/ref and object path to table
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 3, 2003
 *
 *-------------------------------------------------------------------------
 */

void dtable_add(dtable_t *table, int32 tag, int32 ref, char* path)
{
 int i;
 
 if (table->nobjs == table->size) {
  table->size *= 2;
  table->objs = (dobj_info_t*)realloc(table->objs, table->size * sizeof(dobj_info_t));
  
  for (i = table->nobjs; i < table->size; i++) {
   table->objs[i].tag = table->objs[i].ref = -1;
   table->objs[i].flags[0] = table->objs[i].flags[1] = -1;
  }
 }
 
 i = table->nobjs++;
 table->objs[i].tag = tag;
 table->objs[i].ref = ref;
 strcpy(table->objs[i].obj_name,path);
 table->objs[i].flags[0] = table->objs[i].flags[1] = -1;
}



/*-------------------------------------------------------------------------
 * Function: dtable_init
 *
 * Purpose: initialize table
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 3, 2003
 *
 *-------------------------------------------------------------------------
 */

void dtable_init( dtable_t **tbl )
{
 int i;
 dtable_t* table = (dtable_t*) malloc(sizeof(dtable_t));
 
 table->size = 20;
 table->nobjs = 0;
 table->objs = (dobj_info_t*) malloc(table->size * sizeof(dobj_info_t));
 
 for (i = 0; i < table->size; i++) {
  table->objs[i].tag = table->objs[i].ref = -1;
  table->objs[i].flags[0] = table->objs[i].flags[1] = -1;
 }
 
 *tbl = table;
}

/*-------------------------------------------------------------------------
 * Function: dtable_free
 *
 * Purpose: free table memory
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 3, 2003
 *
 *-------------------------------------------------------------------------
 */

void dtable_free( dtable_t *table )
{
 free(table->objs);
 free(table);
}


/*-------------------------------------------------------------------------
 * Function: dtable_check
 *
 * Purpose: search the table for valid objects
 *
 * Return: error string or NULL for success
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 3, 2003
 *
 *-------------------------------------------------------------------------
 */

char* dtable_check(dtable_t *table, char*obj_name)
{
 int   i;
 int32 tag;
 
 for (i = 0; i < table->nobjs; i++)
 {
  if (strcmp(table->objs[i].obj_name,obj_name)==0)
  {
   /* found the name; check if it is an SDS or Image */
   tag=table->objs[i].tag;
   if (tag==DFTAG_SD  ||
       tag==DFTAG_SDG ||
       tag==DFTAG_NDG ||
       tag==DFTAG_RI  ||
       tag==DFTAG_CI  ||
       tag==DFTAG_RIG ||
       tag==DFTAG_RI8 ||
       tag==DFTAG_CI8 ||
       tag==DFTAG_II8 ) return NULL; 
   else return "not compressible/chunk object";
  }
 }
  
  return "not found";
}


/*-------------------------------------------------------------------------
 * Function: dtable_print
 *
 * Purpose: print object list
 *
 * Return: void
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 25, 2003
 *
 *-------------------------------------------------------------------------
 */

void dtable_print(dtable_t *table)
{
 int i;

 printf("---------------------------------------\n");
 printf("%5s %6s    %-15s\n", "Tag", "Ref", "Name");
 printf("---------------------------------------\n");

 for (i = 0; i < table->nobjs; i++)
 {
  printf("%5d %6d    %-15s\n", 
   table->objs[i].tag, 
   table->objs[i].ref, 
   table->objs[i].obj_name);
 }

}
