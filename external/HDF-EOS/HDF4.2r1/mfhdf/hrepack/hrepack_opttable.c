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


#include <string.h>
#include <stdio.h>

#include "hrepack_opttable.h"



/*-------------------------------------------------------------------------
 * Function: options_table_init
 *
 * Purpose: init options table
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */

void options_table_init( options_table_t **tbl )
{
 int i;
 options_table_t* table = (options_table_t*) malloc(sizeof(options_table_t));
 
 table->size   = 3;
 table->nelems = 0;
 table->objs   = (pack_info_t*) malloc(table->size * sizeof(pack_info_t));
 
 for (i = 0; i < table->size; i++) {
   strcpy(table->objs[i].path,"\0");
   table->objs[i].comp.info  = -1;
   table->objs[i].comp.type  = COMP_CODE_NONE;
   table->objs[i].chunk.rank = -1;
  }
 
 *tbl = table;
}

/*-------------------------------------------------------------------------
 * Function: options_table_free
 *
 * Purpose: free table memory
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */

void options_table_free( options_table_t *table )
{
 free(table->objs);
 free(table);
}

/*-------------------------------------------------------------------------
 * Function: options_add_chunk
 *
 * Purpose: add a chunking -c option to the option list
 *
 * Return: 
 *
 *-------------------------------------------------------------------------
 */

int options_add_chunk(obj_list_t *obj_list,int n_objs,int32 *chunk_lengths,
                      int chunk_rank,options_table_t *table )
{
 int i, j, k, I, added=0, found=0;
 
 if (table->nelems+n_objs >= table->size) {
  table->size += n_objs;
  table->objs = (pack_info_t*)realloc(table->objs, table->size * sizeof(pack_info_t));
  for (i = table->nelems; i < table->size; i++) {
   strcpy(table->objs[i].path,"\0");
   table->objs[i].comp.info  = -1;
   table->objs[i].comp.type  = COMP_CODE_NONE;
   table->objs[i].chunk.rank = -1;
  }
 }
 
 /* search if this object is already in the table; "path" is the key */
 if (table->nelems>0)
 {
  /* go tru the supplied list of names */
  for (j = 0; j < n_objs; j++) 
  {
   /* linear table search */
   for (i = 0; i < table->nelems; i++) 
   {
    /*already on the table */
    if (strcmp(obj_list[j].obj,table->objs[i].path)==0)
    {
     /* already chunk info inserted for this one; exit */
     if (table->objs[i].chunk.rank>0)
     {
      printf("Input Error: chunk information already inserted for <%s>\n",obj_list[j].obj);
      exit(1);
     }
     /* insert the chunk info */
     else
     {
      table->objs[i].chunk.rank = chunk_rank;
      for (k = 0; k < chunk_rank; k++) 
       table->objs[i].chunk.chunk_lengths[k] = chunk_lengths[k];
      found=1;
      break;
     }
    } /* if */
   } /* i */
   
   if (found==0)
   {
    /* keep the grow in a temp var */
    I = table->nelems + added;  
    added++;
    strcpy(table->objs[I].path,obj_list[j].obj);
    table->objs[I].chunk.rank = chunk_rank;
    for (k = 0; k < chunk_rank; k++) 
     table->objs[I].chunk.chunk_lengths[k] = chunk_lengths[k];
   }
  } /* j */ 
 }
 
 /* first time insertion */
 else
 {
  /* go tru the supplied list of names */
  for (j = 0; j < n_objs; j++) 
  {
   I = table->nelems + added;  
   added++;
   strcpy(table->objs[I].path,obj_list[j].obj);
   table->objs[I].chunk.rank = chunk_rank;
   for (k = 0; k < chunk_rank; k++) 
    table->objs[I].chunk.chunk_lengths[k] = chunk_lengths[k];
  }
 }
 
 table->nelems+= added;
 
 return 0;
}



/*-------------------------------------------------------------------------
 * Function: options_add_comp
 *
 * Purpose: add a compression -t option to the option list
 *
 * Return: 
 *
 *-------------------------------------------------------------------------
 */

int options_add_comp(obj_list_t *obj_list,int n_objs,comp_info_t comp,
                     options_table_t *table )
{
 
 int i, j, I, added=0, found=0;
 
 if (table->nelems+n_objs >= table->size) {
  table->size += n_objs;
  table->objs = (pack_info_t*)realloc(table->objs, table->size * sizeof(pack_info_t));
  for (i = table->nelems; i < table->size; i++) {
   strcpy(table->objs[i].path,"\0");
   table->objs[i].comp.info  = -1;
   table->objs[i].comp.type  = COMP_CODE_NONE;
   table->objs[i].chunk.rank = -1;
  }
 }
 
 /* search if this object is already in the table; "path" is the key */
 if (table->nelems>0)
 {
  /* go tru the supplied list of names */
  for (j = 0; j < n_objs; j++) 
  {
   /* linear table search */
   for (i = 0; i < table->nelems; i++) 
   {
    /*already on the table */
    if (strcmp(obj_list[j].obj,table->objs[i].path)==0)
    {
     /* already COMP info inserted for this one; exit */
     if (table->objs[i].comp.type>0)
     {
      printf("Input Error: compression information already inserted for <%s>\n",obj_list[j].obj);
      exit(1);
     }
     /* insert the comp info */
     else
     {
      table->objs[i].comp = comp;
      found=1;
      break;
     }
    } /* if */
   } /* i */
   
   if (found==0)
   {
    /* keep the grow in a temp var */
    I = table->nelems + added;  
    added++;
    strcpy(table->objs[I].path,obj_list[j].obj);
    table->objs[I].comp = comp;
   }
  } /* j */ 
 }
 
 /* first time insertion */
 else
 {
  /* go tru the supplied list of names */
  for (j = 0; j < n_objs; j++) 
  {
   I = table->nelems + added;  
   added++;
   strcpy(table->objs[I].path,obj_list[j].obj);
   table->objs[I].comp = comp;
  }
 }
 
 table->nelems+= added;
 
 return 0;
}

/*-------------------------------------------------------------------------
 * Function: options_get_object
 *
 * Purpose: get object from table; "path" is the key
 *
 * Return: 
 *
 *-------------------------------------------------------------------------
 */

pack_info_t* options_get_object(char *path,options_table_t *table)
{
 int i;
 
 for ( i = 0; i < table->nelems; i++) 
 {
  /* found it */
  if (strcmp(table->objs[i].path,path)==0)
  {
   return (&table->objs[i]);
  }
 }
 
 return NULL;
}




