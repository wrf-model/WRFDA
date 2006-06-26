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


#ifndef REPACK_VS_H_
#define REPACK_VS_H_

#include "hrepack.h"
#include "hrepack_lsttable.h"


#ifdef __cplusplus
extern "C" {
#endif


int  copy_vdata_attribute(int32 in, int32 out, int32 findex, intn attrindex);


int  copy_vs( int32 infile_id,
              int32 outfile_id,
              int32 tag,
              int32 ref,               /* ref of input VS */
              int32 vgroup_id_out_par, /* output parent group ID */
              char*path_name,          /* absolute path for input group name */
              options_t *options,
              table_t *table,
              int   is_lone);

int copy_vgroup_attrs(int32 vg_in, 
                      int32 vg_out, 
                      char *path,
                      options_t *options);



#ifdef __cplusplus
}
#endif


#endif  /* REPACK_VS_H_ */
