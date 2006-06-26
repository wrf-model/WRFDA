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


#ifndef REPACK_PAL_H_
#define REPACK_PAL_H_

#include "hrepack.h"
#include "hrepack_lsttable.h"



#ifdef __cplusplus
extern "C" {
#endif


int copy_pal(char* infname,char* outfname,int32 infile_id,int32 outfile_id,
              table_t *table,options_t *options);



#ifdef __cplusplus
}
#endif


#endif  /* REPACK_PAL_H_ */
