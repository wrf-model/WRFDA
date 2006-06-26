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


#ifndef REPACK_OPTTABLE_H_
#define REPACK_OPTTABLE_H_

#include "hdf.h"
#include "mfhdf.h"

#include "hrepack.h"


#ifdef __cplusplus
extern "C" {
#endif


void options_table_init( options_table_t **tbl );
void options_table_free( options_table_t *table );
int options_add_chunk(obj_list_t *obj_list,int n_objs,int32 *chunk_lengths,
                      int chunk_rank,options_table_t *table );
int options_add_comp(obj_list_t *obj_list,int n_objs,comp_info_t comp,
                     options_table_t *table );
pack_info_t* options_get_object(char *path,options_table_t *table);


#ifdef __cplusplus
}
#endif


#endif  /* REPACK_OPTTABLE_H_ */
