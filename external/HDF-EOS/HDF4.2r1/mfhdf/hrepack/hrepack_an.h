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


#ifndef REPACK_AN_H_
#define REPACK_AN_H_

#include "hrepack.h"


#ifdef __cplusplus
extern "C" {
#endif


int copy_an(int32 infile_id,int32 outfile_id,
            int32 ref_in, int32 tag_in,
            int32 ref_out, int32 tag_out,
            char *path, options_t *options);


int copy_vg_an(int32 infile_id,
               int32 outfile_id,
               int32 vgroup_id,
               int32 vgroup_id_out, 
               char *path,
               options_t *options);

int copy_vs_an(int32 infile_id,
               int32 outfile_id,
               int32 vdata_id,
               int32 vdata_id_out, 
               char *path,
               options_t *options);

int copy_an_data(int32 infile_id,
                 int32 outfile_id,
                 int32 ref_in, 
                 int32 tag_in,
                 int32 ref_out, 
                 int32 tag_out,
                 ann_type type, 
                 char *path, 
                 options_t *options);



#ifdef __cplusplus
}
#endif


#endif  /* REPACK_AN_H_ */
