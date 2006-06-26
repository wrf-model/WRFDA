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


#ifndef REPACK_SDUTIL_H_
#define REPACK_SDUTIL_H_

#include "hrepack.h"
#include "hrepack_parse.h"


#ifdef __cplusplus
extern "C" {
#endif

int  options_get_info(options_t      *options,     /* global options */
                      int32          *chunk_flags, /* chunk flags OUT */
                      HDF_CHUNK_DEF  *chunk_def,   /* chunk definition OUT */
                      int            *info,        /* compression info OUT */
                      int            *szip_mode,   /* compression information OUT */
                      comp_coder_t   *comp_type,   /* compression type OUT  */
                      int            rank,         /* rank of object IN */
                      char           *path,        /* path of object IN */
                      int            ncomps,       /* number of GR image planes (for SZIP), IN */
                      int32          *dimsizes,    /* dimensions (for SZIP), IN */
                      int32          dtype         /* numeric type (for SZIP), IN */
                      );

int set_szip( int   pixels_per_block, /*in */
             int   compression_mode, /* in */
             comp_info *c_info/*out*/);


#ifdef __cplusplus
}
#endif


#endif  /* REPACK_SDUTIL_H_ */
