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


#ifndef REPACK_UTILS_H_
#define REPACK_UTILS_H_



#ifdef __cplusplus
extern "C" {
#endif


int is_reserved(char*vgroup_class);
char *get_path(char*path_name, char*obj_name);


#ifdef __cplusplus
}
#endif


#endif  /* REPACK_UTILS_H_ */
