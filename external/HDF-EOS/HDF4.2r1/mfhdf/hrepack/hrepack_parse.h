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


#ifndef REPACK_PARSE_H_
#define REPACK_PARSE_H_

#include "hdf.h"
#include "mfhdf.h"
#include "hrepack.h"


#ifdef __cplusplus
extern "C" {
#endif

int parse_number(char *str);


/* compression */

obj_list_t* parse_comp(const char *str, int *n_objs, comp_info_t *comp);
const char*       get_scomp(comp_coder_t code);

/* chunking */

obj_list_t* parse_chunk(const char *str, int *n_objs, int32 *chunk_lengths, int *chunk_rank);


#ifdef __cplusplus
}
#endif

#define NN_MODE 0
#define EC_MODE 1

#endif  /* REPACK_PARSE_H_ */
