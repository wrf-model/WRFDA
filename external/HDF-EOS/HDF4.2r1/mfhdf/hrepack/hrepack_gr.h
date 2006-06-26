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


#ifndef REPACK_GR_H_
#define REPACK_GR_H_

#include "hrepack.h"
#include "hrepack_lsttable.h"


#ifdef __cplusplus
extern "C" {
#endif

int  copy_gr(int32 infile_id,
             int32 outfile_id,
             int32 gr_in,
             int32 gr_out,
             int32 tag,               /* tag of input GR */
             int32 ref,               /* ref of input GR */
             int32 vgroup_id_out_par, /* output parent group ID */
             char*path_name,          /* absolute path for input group name */
             options_t *options,
             table_t *table);


int copy_gr_attrs(int32 ri_id,
                  int32 ri_out,
                  int32 nattrs,          
                  options_t *options);


#ifdef __cplusplus
}
#endif


#endif  /* REPACK_GR_H_ */
