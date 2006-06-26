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


#ifndef REPACK_LSTTABLE_H_
#define REPACK_LSTTABLE_H_


#include "hdf.h"
#include "mfhdf.h"

#define PFORMAT  "  %-7s %-7s %-7s\n" /*chunk info, compression info, name*/
#define PFORMAT1 "  %-7s %-7s %-7s"   /*chunk info, compression info, name*/

#ifdef __cplusplus
extern "C" {
#endif


/*struct to store the tag/ref and path of an object 
 the pair tag/ref uniquely identifies an HDF object */
typedef struct obj_info_t {
 int   tag;
 int   ref;
 char  obj_name[MAX_NC_NAME];
 int   flags[2];     
 /*flags that store matching object information
   between the 2 files 
   object exists in file = 1
   does not exist        = 0
 */
} obj_info_t;

/*struct that stores all objects */
typedef struct table_t {
 int        size;
 int        nobjs;
 obj_info_t *objs;
} table_t;


/* table methods */
void  table_init(table_t **table);
void  table_free(table_t *table);
int   table_search(table_t *table, int tag, int ref );
void  table_add(table_t *table, int tag, int ref, char* obj_name);
const char* table_check(table_t *table, char*obj_name);
void  table_print(table_t *table);



#ifdef __cplusplus
}
#endif


#endif  /* REPACK_LSTTABLE_H_ */
