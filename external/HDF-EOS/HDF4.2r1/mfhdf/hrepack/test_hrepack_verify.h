
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


#ifndef REPACK_VERIFY_H
#define REPACK_VERIFY_H



#ifdef __cplusplus
extern "C" {
#endif

 
#define FILENAME         "hziptst.hdf"
#define FILENAME_OUT     "hziptst_out.hdf"


int sds_verifiy_comp(const char *sds_name, int32 in_comp_type, int32 in_comp_info);
int sds_verifiy_comp_all(int32 in_comp_type, int32 in_comp_info);
int sds_verifiy_chunk(const char *sds_name, int32 in_chunk_flags, int rank, 
                      int32 *in_chunk_lengths);
int sds_verifiy_chunk_all(int32 in_chunk_flags, int rank, 
                          int32 *in_chunk_lengths,const char *sds_exclude);


#ifdef __cplusplus
}
#endif


#endif  /* REPACK_VERIFY_H */
