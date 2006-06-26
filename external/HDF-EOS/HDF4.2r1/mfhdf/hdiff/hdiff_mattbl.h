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


#ifndef HDF_HDIFF_MATCH_TABLE__
#define HDF_HDIFF_MATCH_TABLE__


#include "hdf.h"
#include "mfhdf.h"

#ifdef __cplusplus
extern "C" {
#endif

/* match objects in 2 HDF files */ 
typedef struct match_info_t {
 int32   tag1;
 int32   ref1;
 int32   tag2;
 int32   ref2;
 char  obj_name[MAX_NC_NAME];      /* same name for file1 and 2 */
 int   flags[2];                   /* object exists in file=1, no=0 */  
} match_info_t;

/* table to store the match info */
typedef struct match_table_t {
 int          size;
 int          nobjs;
 match_info_t *objs;
} match_table_t;


/* table methods */
void match_table_init(match_table_t **table);
void match_table_free(match_table_t *table);
void match_table_add (match_table_t *table, 
                      unsigned *flags, 
                      char* path, 
                      int32 tag1, 
                      int32 ref1,
                      int32 tag2, 
                      int32 ref2 );




#ifdef __cplusplus
}
#endif


#endif  /* HDF_HDIFF_MATCH_TABLE__ */
