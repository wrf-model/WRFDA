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


#ifndef REPACK_SDS_H_
#define REPACK_SDS_H_

#include "hrepack.h"
#include "hrepack_lsttable.h"


#ifdef __cplusplus
extern "C" {
#endif


int  copy_sds(int32 sd_in,
              int32 sd_out,
              int32 tag,
              int32 ref,
              int32 vgroup_id_out_par,
              char*group_name,
              options_t *options,
              table_t *table,
              int32 infile_id,
              int32 outfile_id);



int copy_sds_attrs(int32 sds_id,
                   int32 sds_out,
                   int32 nattrs,          
                   options_t *options);



#ifdef __cplusplus
}
#endif


#endif  /* REPACK_SDS_H_ */
