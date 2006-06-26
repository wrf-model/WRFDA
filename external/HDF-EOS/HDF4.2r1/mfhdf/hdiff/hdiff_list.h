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


#ifndef HDF_HDIFF_LIST__
#define HDF_HDIFF_LIST__



#ifdef __cplusplus
extern "C" {
#endif

#include "hdiff_table.h"


/* get the list of HDF objects in the file */
int Hgetlist (const char* fname, dtable_t *list);

int hdiff_list_vg (const char* fname,int32 file_id,int32 sd_id,int32 gr_id,dtable_t *table);
int hdiff_list_gr (const char* fname,int32 file_id,int32 gr_id,dtable_t *table);
int hdiff_list_sds(const char* fname,int32 file_id,int32 sd_id,dtable_t *table);
int hdiff_list_vs (const char* fname,int32 file_id,dtable_t *table);
int hdiff_list_glb(const char* fname,int32 file_id,int32 sd_id,int32 gr_id,dtable_t *table);
int hdiff_list_an (const char* fname,int32 file_id,dtable_t *table);


int insert_vg_attrs(int32 vgroup_id,char *path);
int insert_vg_an(int32 file_id,int32 vgroup_id,char *path);


int insert_vg(const char* fname,
              int32 file_id,
              int32 sd_id,             /* SD interface identifier */
              int32 gr_id,             /* GR interface identifier */
              char*path_name,          /* absolute path for input group name */          
              int32* in_tags,          /* tag list for parent group */
              int32* in_refs,          /* ref list for parent group */
              int npairs,              /* number tag/ref pairs for parent group */
              dtable_t *table);

int  insert_sds(int32 file_id,
                int32 sd_id,
                int32 tag,
                int32 ref,
                char *path_name,
                dtable_t *table);

int  insert_gr(int32 file_id,
               int32 gr_in,
               int32 tag,               /* tag of input GR */
               int32 ref,               /* ref of input GR */
               char*path_name,          /* absolute path for input group name */
               dtable_t *table);

int  insert_vs( int32 file_id,
                int32 tag,
                int32 ref,               /* ref of input VS */
                char*path_name,          /* absolute path for input group name */
                dtable_t *table,
                int is_lone);


int insert_sds_attrs(int32 sds_id,
                     int32 nattrs);

int insert_gr_attrs(int32 ri_id,
                    int32 nattrs);


int insert_an(int32 file_id,
              int32 ref_in, 
              int32 tag_in,
              char *path);

int insert_vs_an(int32 file_id,
                 int32 vdata_id,
                 char *path);

int  insert_vs_attrs(int32 in, int32 findex, intn attrindex);




#ifdef __cplusplus
}
#endif


#endif  /* HDF_HDIFF_LIST__ */
