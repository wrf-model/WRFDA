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

#include <assert.h>


#include "hdf.h"
#include "mfhdf.h"
#include "hdiff_list.h"

static
int is_reserved(char*vgroup_class);
static
char *get_path(char*path_name, char*obj_name);
static 
int insert_an_data(int32 file_id,
                   int32 ref_in, 
                   int32 tag_in,
                   ann_type type, 
                   char *path);




/*-------------------------------------------------------------------------
 * Function: Hgetlist
 *
 * Purpose: locate all HDF objects in the file and return a list of them
 *
 * Return: number of objects in the file, ok, -1 not ok
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 21, 2003
 *
 * Description:
 *
 * A main loop is used to locate all the objects in the file. This loop preserves the 
 * hierarchy of the file. The algorithm used is 
 * 1) Obtain the number of lone VGroups in the HDF file. 
 * 2) Do a loop for each one of these groups. In each iteration a table is updated 
 *    with the tag/reference pair of an object. 
 *    2.1) Obtain the pairs of tag/references for the group 
 *    2.2) Switch between the tag of the current object. Four cases are possible: 
 *         1) Object is a group: recursively repeat the process (obtain the pairs of
 *            tag/references for this group and do another tag switch). 
 *            Add the object to the table. 
 *         2) Object is a dataset: Add the object to the table.
 *         3) Object is an image: Add the object to the table.  
 *         4) Object is a vdata: Add the object to the table. 
 * 3) Read all the HDF interfaces (SDS, GR and VS), checking for objects that are 
 *    already in the table (meaning they belong to a previous inspected group, 
 *    and should not be added).  These objects belong to a root group. 
 * 4) Read all global attributes and annotations. 
 *
 *-------------------------------------------------------------------------
 */


int Hgetlist (const char* fname, dtable_t *table)
{
 int32    file_id, 
          sd_id, 
          gr_id;
 int      n_objs=0;

 /* open the file for read */
 if ((file_id  = Hopen (fname,DFACC_READ,(int16)0))==FAIL)
 {
  printf("Cannot open file <%s>\n",fname);
  return FAIL;
 }

 /* initialize the SD interface */
 if ((sd_id  = SDstart (fname, DFACC_READ))==FAIL){
  printf( "Could not start SD for <%s>\n",fname);
  return FAIL;
 }

 /* initialize the GR interface */
 if ((gr_id  = GRstart (file_id))==FAIL){
  printf( "Could not start GR for <%s>\n",fname);
  return FAIL;
 }

 /* iterate tru HDF interfaces */
 hdiff_list_vg (fname,file_id,sd_id,gr_id,table);
 hdiff_list_gr (fname,file_id,gr_id,table);
 hdiff_list_sds(fname,file_id,sd_id,table);
 hdiff_list_vs (fname,file_id,table);
 hdiff_list_glb(fname,file_id,sd_id,gr_id,table);
 hdiff_list_an (fname,file_id,table);

 /* close */
 if (GRend (gr_id)==FAIL) {
  printf( "Failed to close GR interface <%s>\n", fname);
  return FAIL;
 }
 if (SDend (sd_id)==FAIL) {
  printf( "Failed to close SD interface <%s>\n", fname);
  return FAIL;
 }
 if (Hclose (file_id) == FAIL ) {
  printf( "Failed to close file <%s>\n", fname);
  return FAIL;
 }
 
 n_objs=table->nobjs;
 return n_objs;
}



/*-------------------------------------------------------------------------
 * Function: hdiff_list_vg
 *
 * Purpose: locate all lone Vgroups in the file
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */


int hdiff_list_vg(const char* fname,
                  int32 file_id,
                  int32 sd_id,             /* SD interface identifier */
                  int32 gr_id,             /* GR interface identifier */
                  dtable_t *table)
{
 int32 vgroup_id,      /* vgroup identifier */
       nlones = 0,     /* number of lone vgroups */
       ntagrefs,       /* number of tag/ref pairs in a vgroup */
       *ref_array=NULL,/* buffer to hold the ref numbers of lone vgroups   */
       *tags,          /* buffer to hold the tag numbers of vgroups   */
       *refs,          /* buffer to hold the ref numbers of vgroups   */
       tag_vg,
       ref_vg;
 char  vgroup_name[VGNAMELENMAX], vgroup_class[VGNAMELENMAX];
 int   i;

 /* initialize the V interface */
 if (Vstart (file_id)==FAIL) {
  printf("Error: Could not start group interface in <%s>\n", fname);
  return FAIL;
 }

/*
 * get the names and class names of all the lone vgroups.
 * first, call Vlone with nlones set to 0 to get the number of
 * lone vgroups in the file, but not to get their reference numbers.
 */
 nlones = Vlone (file_id, NULL, nlones );

 if (nlones > 0)
 {
 /*
  * use the nlones returned to allocate sufficient space for the
  * buffer ref_array to hold the reference numbers of all lone vgroups,
  */
  ref_array = (int32 *) malloc(sizeof(int32) * nlones);
  
 /*
  * and call Vlone again to retrieve the reference numbers into 
  * the buffer ref_array.
  */
  nlones = Vlone (file_id, ref_array, nlones);
  
 /*
  * iterate tru each lone vgroup.
  */
  for (i = 0; i < nlones; i++)
  {
  /*
   * attach to the current vgroup then get its
   * name and class. note: the current vgroup must be detached before
   * moving to the next.
   */
   if ((vgroup_id = Vattach (file_id, ref_array[i], "r"))==FAIL){
    printf("Error: Could not attach group with ref <%d>\n", ref_array[i]);
   }
   if (Vgetname (vgroup_id, vgroup_name)==FAIL){
    printf("Error: Could not get name for group with ref <%d>\n", ref_array[i]);
   }
   if (Vgetclass (vgroup_id, vgroup_class)==FAIL){
    printf("Error: Could not get class for group with ref <%d>\n", ref_array[i]);
   }
   
   /* ignore reserved HDF groups/vdatas */
   if( is_reserved(vgroup_class)){
    if (Vdetach (vgroup_id)==FAIL){
     printf("Error: Could not detach group <%s>\n", vgroup_class);
    }
    continue;
   }
   if(vgroup_name != NULL) 
    if(strcmp(vgroup_name,GR_NAME)==0) {
     if (Vdetach (vgroup_id)==FAIL){
      printf("Error: Could not detach group <%s>\n", vgroup_class);
     }
     continue;
    }

    if ((ref_vg = VQueryref(vgroup_id))==FAIL){
     printf( "Failed to get ref for <%s>\n", vgroup_name);
    }
    if ((tag_vg = VQuerytag(vgroup_id))==FAIL){
     printf( "Failed to get tag for <%s>\n", vgroup_name);
    }

     /* add object to table */
    dtable_add(table,tag_vg,ref_vg,vgroup_name);

#if defined (HDIFF_DEBUG)
    printf("%s\n",vgroup_name); 
#endif
  
    insert_vg_attrs(vgroup_id,vgroup_name);
    insert_vg_an(file_id,vgroup_id,vgroup_name);

   /* insert objects for this group */
    ntagrefs = Vntagrefs(vgroup_id);
    if ( ntagrefs > 0 )
    {
     tags = (int32 *) malloc(sizeof(int32) * ntagrefs);
     refs = (int32 *) malloc(sizeof(int32) * ntagrefs);
     Vgettagrefs(vgroup_id, tags, refs, ntagrefs);
     
     insert_vg(fname,file_id,sd_id,gr_id,vgroup_name,tags,refs,ntagrefs,table);
     
     if (tags ) free (tags);
     if (refs) free (refs);
    }
    
    if(Vdetach (vgroup_id)==FAIL){
     printf("Error: Could not detach group <%s>\n", vgroup_name);
    }
  
  } /* for */
  
  
  /* free the space allocated */
  if (ref_array) 
   free (ref_array);
 } /* if */
 

 /* terminate access to the V interface */
 if (Vend (file_id)==FAIL) {
  printf("Error: Could not end group interface in <%s>\n", vgroup_name);
 }
 
 return 0;
}

/*-------------------------------------------------------------------------
 * Function: insert_vg
 *
 * Purpose: recursive function to locate objects in lone Vgroups
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */

int insert_vg(const char* fname,
              int32 file_id,
              int32 sd_id,             /* SD interface identifier */
              int32 gr_id,             /* GR interface identifier */
              char*path_name,          /* absolute path for input group name */          
              int32* in_tags,          /* tag list for parent group */
              int32* in_refs,          /* ref list for parent group */
              int npairs,              /* number tag/ref pairs for parent group */
              dtable_t *table)
{
 int32 vgroup_id,             /* vgroup identifier */
       ntagrefs,              /* number of tag/ref pairs in a vgroup */
       tag,                   /* temporary tag */
       ref,                   /* temporary ref */
       *tags,                 /* buffer to hold the tag numbers of vgroups   */
       *refs;                 /* buffer to hold the ref numbers of vgroups   */
 char  vgroup_name[VGNAMELENMAX], vgroup_class[VGNAMELENMAX];
 char  *path=NULL;
 int   i;
 
 for ( i = 0; i < npairs; i++ ) 
 {
  tag = in_tags[i];
  ref = in_refs[i];
  
  switch(tag) 
  {
/*-------------------------------------------------------------------------
 * VG
 *-------------------------------------------------------------------------
 */
  case DFTAG_VG: 
   
   vgroup_id = Vattach (file_id, ref, "r");
   Vgetname (vgroup_id, vgroup_name);
   Vgetclass (vgroup_id, vgroup_class);
   
   /* ignore reserved HDF groups/vdatas */
   if( is_reserved(vgroup_class)){
    Vdetach (vgroup_id);
    break;
   }
   if(vgroup_name != NULL) 
    if(strcmp(vgroup_name,GR_NAME)==0) {
     Vdetach (vgroup_id);
     break;
    }

   /* initialize path */
   path=get_path(path_name,vgroup_name);

   /* add object to table */
   dtable_add(table,tag,ref,path);

#if defined (HDIFF_DEBUG)
   printf("%s\n",path); 
#endif
  
   insert_vg_attrs(vgroup_id,path);
   insert_vg_an(file_id,vgroup_id,path);
  
   /* get objects for this group */
   ntagrefs  = Vntagrefs(vgroup_id);
   if ( ntagrefs > 0 )
   {
    tags = (int32 *) malloc(sizeof(int32) * ntagrefs);
    refs = (int32 *) malloc(sizeof(int32) * ntagrefs);
    Vgettagrefs(vgroup_id, tags, refs, ntagrefs);
    /* recurse */
    insert_vg(fname,file_id,sd_id,gr_id,path,tags,refs,ntagrefs,table);
    free (tags);
    free (refs);
   }
   if(Vdetach (vgroup_id)==FAIL){
    printf("Error: Could not detach group <%s>\n", vgroup_name);
   }
   if (path)
    free(path);

   break;
   

/*-------------------------------------------------------------------------
 * SDS
 *-------------------------------------------------------------------------
 */   
   
  case DFTAG_SD:  /* Scientific Data */
  case DFTAG_SDG: /* Scientific Data Group */
  case DFTAG_NDG: /* Numeric Data Group */
   insert_sds(file_id,sd_id,tag,ref,path_name,table);
   break;
   
/*-------------------------------------------------------------------------
 * Image
 *-------------------------------------------------------------------------
 */   
  case DFTAG_RI:  /* Raster Image */
  case DFTAG_CI:  /* Compressed Image */
  case DFTAG_RIG: /* Raster Image Group */
  case DFTAG_RI8:  /* Raster-8 image */
  case DFTAG_CI8:  /* RLE compressed 8-bit image */
  case DFTAG_II8:  /* IMCOMP compressed 8-bit image */
   insert_gr(file_id,gr_id,tag,ref,path_name,table);
   break;

/*-------------------------------------------------------------------------
 * Vdata
 *-------------------------------------------------------------------------
 */   
  case DFTAG_VH:  /* Vdata Header */
   insert_vs(file_id,tag,ref,path_name,table,0);
   break;
   
  }
 }

 return 0;
}


/*-------------------------------------------------------------------------
 * Function: hdiff_list_gr
 *
 * Purpose: get top level GR images
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */

int hdiff_list_gr(const char* fname,
                  int32 file_id,
                  int32 gr_id,             /* GR interface identifier */
                  dtable_t *table)
{
 int32 ri_id,             /* raster image identifier */
       n_rimages,         /* number of raster images in the file */
       n_file_attrs,      /* number of file attributes */
       ri_index,          /* index of a image */
       gr_ref,            /* reference number of the GR image */
       dim_sizes[2],      /* dimensions of an image */
       n_comps,           /* number of components an image contains */
       interlace_mode,    /* interlace mode of an image */ 
       data_type,         /* number type of an image */
       n_attrs;           /* number of attributes belong to an image */
 char  name[MAX_GR_NAME]; /* name of an image */
 
 /* determine the contents of the file */
 if (GRfileinfo (gr_id, &n_rimages, &n_file_attrs)<0)
 {
  return -1;
 }
 
 for (ri_index = 0; ri_index < n_rimages; ri_index++)
 {
  ri_id = GRselect (gr_id, ri_index);
  GRgetiminfo (ri_id, name, &n_comps, &data_type, &interlace_mode, 
   dim_sizes, &n_attrs);

  gr_ref = GRidtoref(ri_id);

  /* check if already inserted in Vgroup; search all image tags */
  if ( dtable_search(table,DFTAG_RI,gr_ref)>=0 ||
       dtable_search(table,DFTAG_CI,gr_ref)>=0 ||
       dtable_search(table,DFTAG_RIG,gr_ref)>=0 ||
       dtable_search(table,DFTAG_RI8,gr_ref)>=0 ||
       dtable_search(table,DFTAG_CI8,gr_ref)>=0 ||
       dtable_search(table,DFTAG_II8,gr_ref)>=0 )
  {
   GRendaccess (ri_id);
   continue;
  }

  /* insert GR  */
  insert_gr(file_id,gr_id,DFTAG_RI,gr_ref,0,table);

  /* terminate access to the current raster image */
  GRendaccess (ri_id);
 }
 return 0;
}


/*-------------------------------------------------------------------------
 * Function: hdiff_list_sds
 *
 * Purpose: get top level SDS
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */

int hdiff_list_sds(const char* fname,
                   int32 file_id,
                   int32 sd_id,                  /* SD interface identifier */
                   dtable_t *table)
{
 int32 sds_id,                 /* dataset identifier */
       n_datasets,             /* number of datasets in the file */
       n_file_attrs,           /* number of file attributes */
       index,                  /* index of a dataset */
       sds_ref,                /* reference number */
       dim_sizes[MAX_VAR_DIMS],/* dimensions of an image */
       data_type,              /* number type  */
       rank,                   /* rank */
       n_attrs;                /* number of attributes */
 char  name[MAX_GR_NAME];      /* name of dataset */

 /* determine the number of data sets in the file and the number of file attributes */
 if (SDfileinfo (sd_id, &n_datasets, &n_file_attrs)<0)
 {
  return -1;
 }

 for (index = 0; index < n_datasets; index++)
 {
  sds_id  = SDselect (sd_id, index);
  SDgetinfo(sds_id, name, &rank, dim_sizes, &data_type, &n_attrs);
  sds_ref = SDidtoref(sds_id);

  /* check if already inserted in Vgroup; search all SDS tags */
  if ( dtable_search(table,DFTAG_SD,sds_ref)>=0 ||
       dtable_search(table,DFTAG_SDG,sds_ref)>=0 ||
       dtable_search(table,DFTAG_NDG,sds_ref)>=0 )
  {
   SDendaccess (sds_id);
   continue;
  }

  /* insert SDS  */
  insert_sds(file_id,sd_id,DFTAG_NDG,sds_ref,0,table);
     
  /* terminate access to the current dataset */
  SDendaccess (sds_id);
 }

 return 0;
}

/*-------------------------------------------------------------------------
 * Function: hdiff_list_vs
 *
 * Purpose: get top level VS
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */


int hdiff_list_vs(const char* fname,
                  int32 file_id,
                  dtable_t *table)
{
 int32 nlones = 0,   /* number of lone vdatas */
       *ref_array,   /* buffer to hold the ref numbers of lone vdatas   */
       ref;          /* temporary ref number  */
 int   i;

 /* initialize the VS interface */
 Vstart (file_id);
/*
 * get and print the names and class names of all the lone vdatas.
 * first, call Vlone with nlones set to 0 to get the number of
 * lone vdatas in the file, but not to get their reference numbers.
 */
 nlones = VSlone (file_id, NULL, nlones );

 if (nlones > 0)
 {
 /*
  * use the nlones returned to allocate sufficient space for the
  * buffer ref_array to hold the reference numbers of all lone vgroups,
  */
  ref_array = (int32 *) malloc(sizeof(int32) * nlones);
  
 /*
  * and call VSlone again to retrieve the reference numbers into 
  * the buffer ref_array.
  */
  nlones = VSlone (file_id, ref_array, nlones);

 /*
  * iterate tru each lone vdata.
  */
  for (i = 0; i < nlones; i++)
  {
  /*
   * attach to the current vdata then get its
   * name and class. note: the current vdata must be detached before
   * moving to the next.
   */
   ref = ref_array[i];

   /* check if already inserted in Vgroup*/
   if ( dtable_search(table,DFTAG_VH,ref)>=0 ) {
    continue;
   }

   /* insert VS */
   insert_vs(file_id,DFTAG_VH,ref,0,table,1);
 
  } /* for */

  
  /* free the space allocated */
  if (ref_array) free (ref_array);
 } /* if */

 /* terminate access to the VS interface */
 Vend (file_id);
 return 0;
}


/*-------------------------------------------------------------------------
 * Function: insert_vg_attrs
 *
 * Purpose: insert VG attributes
 *
 * Return: 1 
 *
 *-------------------------------------------------------------------------
 */

int insert_vg_attrs(int32 vg_in,char *path) 
{
 int    n_attrs;
 int32  data_type, size,  n_values;
 char   attr_name[MAX_NC_NAME];
 int    i;

 /* Get the number of attributes attached to this vgroup.  */
 if((n_attrs = Vnattrs (vg_in))==FAIL) {
  printf( "Failed to get attributes for <%s>\n", path);
  return-1;
 }
 
 for (i = 0; i < n_attrs; i++) 
 {
  if((Vattrinfo (vg_in, i, attr_name, &data_type, &n_values, &size))==FAIL) {
   printf( "Failed to get attribute %d of <%s>\n", i, path);
   continue;
  }
 
 }
 return 1;
}

/*-------------------------------------------------------------------------
 * Function: hdiff_list_glb
 *
 * Purpose: list/insert global SDS attributes, global GR atrributes
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */

int hdiff_list_glb(const char* fname,
                   int32 file_id,
                   int32 sd_id,                  /* SD interface identifier */
                   int32 gr_id,                  /* GR interface identifier */
                   dtable_t *table)
{
 int32 n_datasets,             /* number of datasets in the file */
       n_file_attrs;           /* number of file attributes */
     
/*-------------------------------------------------------------------------
 * insert SDS global attributes
 *-------------------------------------------------------------------------
 */ 
 /* determine the number of data sets in the file and the number of file attributes */
 SDfileinfo (sd_id, &n_datasets, &n_file_attrs);
 
 insert_sds_attrs(sd_id,n_file_attrs);

/*-------------------------------------------------------------------------
 * insert GR global attributes
 *-------------------------------------------------------------------------
 */ 
 /* determine the number of data sets in the file and the number of file attributes */
 GRfileinfo (gr_id, &n_datasets, &n_file_attrs);
 
 insert_gr_attrs(gr_id,n_file_attrs);
 return 0;
}


/*-------------------------------------------------------------------------
 * Function: hdiff_list_an
 *
 * Purpose: list/insert AN FILE objects
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */


int hdiff_list_an(const char* fname,
                  int32 file_id,
                  dtable_t *table)
{
 int32 an_id,         /* AN interface identifier */
       ann_id,        /* an annotation identifier */
       i,             /* position of an annotation in all of the same type*/
       n_file_labels, n_file_descs, n_data_labels, n_data_descs;

 /* Initialize the AN interface  */
 an_id  = ANstart (file_id);

/*
 * Get the annotation information, e.g., the numbers of file labels, file
 * descriptions, data labels, and data descriptions.
 */
 ANfileinfo (an_id, &n_file_labels, &n_file_descs, &n_data_labels, 
  &n_data_descs);
 

/*-------------------------------------------------------------------------
 * AN_FILE_LABEL
 *-------------------------------------------------------------------------
 */ 


 for (i = 0; i < n_file_labels; i++)
 {
 /* Get the identifier of the current data label */
  ann_id = ANselect (an_id, i, AN_FILE_LABEL);
  
   /* Terminate access to the current data label */
  ANendaccess (ann_id);
 
 }

/*-------------------------------------------------------------------------
 * AN_FILE_DESC
 *-------------------------------------------------------------------------
 */ 

 for (i = 0; i < n_file_descs; i++)
 {
 /* Get the identifier of the current data label */
  ann_id = ANselect (an_id, i, AN_FILE_DESC);
  
  /* Terminate access to the current data label */
  ANendaccess (ann_id);
 }
 
 /* Terminate access to the AN interface */
 ANend (an_id);
 
 return 0;
}


/*-------------------------------------------------------------------------
 * Function: insert_vg_an
 *
 * Purpose: insert Vgroup ANs
 *
 * Return: ok, 1, -1 not ok 
 *
 *-------------------------------------------------------------------------
 */

int insert_vg_an(int32 file_id,
                 int32 vgroup_id,
                 char *path) 
{
 int32 ref_in,
       tag_in;

 if ((ref_in = VQueryref(vgroup_id))==FAIL){
  printf( "Failed to get ref for <%s>\n", path);
  return-1;
 }
 if ((tag_in = VQuerytag(vgroup_id))==FAIL){
  printf( "Failed to get tag for <%s>\n", path);
  return-1;
 }
 insert_an(file_id,ref_in,tag_in,path);

 return 1;
}


/*-------------------------------------------------------------------------
 * Function: insert_vs_an
 *
 * Purpose: insert Vdata ANs
 *
 * Return: ok, 1, -1 not ok 
 *
 *-------------------------------------------------------------------------
 */

int insert_vs_an(int32 file_id,
                 int32 vdata_id,
                 char *path)
{
 int32 ref_in,
       tag_in;

 if ((ref_in = VSQueryref(vdata_id))==FAIL){
  printf( "Failed to get ref for <%s>\n", path);
  return-1;
 }
 if ((tag_in = VSQuerytag(vdata_id))==FAIL){
  printf( "Failed to get tag for <%s>\n", path);
  return-1;
 }
 
 insert_an(file_id,ref_in,tag_in,path);

 return 1;
}


/*-------------------------------------------------------------------------
 * Function: insert_an_data
 *
 * Purpose: insert DATA ANs
 *
 * Return: ok, 1, -1 not ok 
 *
 *-------------------------------------------------------------------------
 */

static
int insert_an_data(int32 file_id,
                   int32 ref_in, 
                   int32 tag_in,
                   ann_type type, 
                   char *path) 
{
 int32 an_id,         /* AN interface identifier */
       ann_id,        /* an annotation identifier */
       i,             /* position of an annotation */
       n_anno;
       
 /* Initialize the AN interface  */
 an_id  = ANstart (file_id);

 /* Get the number of ANs in this object  */
 if((n_anno = ANnumann(an_id,type,(uint16)tag_in,(uint16)ref_in))==FAIL) {
  printf( "Failed to get annotations for <%s>\n", path);
  return-1;
 }
 
 for (i = 0; i < n_anno; i++) 
 {
  if((ann_id = ANselect(an_id,i,type))==FAIL) {
   printf( "Failed to select AN %d of <%s>\n", i, path);
   continue;
  }
 
  if(ANendaccess(ann_id)==FAIL){
   printf( "Failed to end AN %d of <%s>\n", i, path);
   continue;
  }

 }

  /* Terminate access to the AN interface */
 ANend (an_id);
 
 return 1;
}


/*-------------------------------------------------------------------------
 * Function: insert_an
 *
 * Purpose: insert DATA ANs (AN_DATA_LABEL and AN_DATA_DESC)
 *
 * Return: ok, 1, -1 not ok 
 *
 *-------------------------------------------------------------------------
 */


int insert_an(int32 file_id,
            int32 ref_in, 
            int32 tag_in,
            char *path) 
{

 insert_an_data(file_id,ref_in,tag_in, 
                AN_DATA_LABEL,path);
 insert_an_data(file_id,ref_in,tag_in,
                AN_DATA_DESC,path);

 return 1;
}



/*-------------------------------------------------------------------------
 * Function: insert_sds
 *
 * Purpose: insert an SDS into file object list
 *
 * Return: 0, -1 for error 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 22, 2003
 *
 *-------------------------------------------------------------------------
 */

int  insert_sds(int32 file_id,
                int32 sd_id,
                int32 tag,            /* tag of input SDS */
                int32 ref,            /* ref of input SDS */
                char *path_name,      /* absolute path for input group name */
                dtable_t *table)
{
 int32 sds_id,                /* data set identifier */
       sds_index,             /* index number of the data set */
       dtype,                 /* SDS data type */
       dimsizes[MAX_VAR_DIMS],/* dimensional size of SDS */
       nattrs,                /* number of SDS attributes */
       rank,                  /* rank of SDS */
       dim_size,              /* dimension size */
       dim_id;                /* dimension ID */
 char             sds_name[MAX_NC_NAME]; 
 char             dim_name[MAX_NC_NAME];
 char             *path=NULL;
 int              i;
 
 sds_index = SDreftoindex(sd_id,ref);
 sds_id    = SDselect(sd_id,sds_index);
 
 /*obtain name,rank,dimsizes,datatype and num of attributes of sds */
 SDgetinfo(sds_id,sds_name,&rank,dimsizes,&dtype,&nattrs);

 /* check if the given SDS is a dimension scale, return 0 for no table add */
 if ( SDiscoordvar(sds_id) ) {
  SDendaccess(sds_id);
  return 0;
 }
 
 /* initialize path */
 path=get_path(path_name,sds_name);
 
 /* add object to table */
 dtable_add(table,tag,ref,path);

#if defined (HDIFF_DEBUG)
   printf("%s\n",path); 
#endif

/*-------------------------------------------------------------------------
 * insert attributes
 *-------------------------------------------------------------------------
 */ 
 
 insert_sds_attrs(sds_id,nattrs);
 
/*-------------------------------------------------------------------------
 * dimension scales
 *-------------------------------------------------------------------------
 */ 
 
 /* loop through each dimension up to rank of SDS */
 for (i = 0; i < rank; i++) 
 {
  /* get dimension handle for input dimension */
  if ((dim_id = SDgetdimid(sds_id, i)) == FAIL) {
   printf( "Failed to get dimension %d of SDS <%s>\n", i, path);
   continue;
  }
  /* get dimension information for input dimension */
  if (SDdiminfo(dim_id, dim_name, &dim_size, &dtype, &nattrs) == FAIL) {
   printf( "Failed to get info for dimension %d of SDS <%s>\n", i, path);
   continue;
  }
  /* attributes */
  if (nattrs && insert_sds_attrs(dim_id, nattrs) == FAIL) {
   printf( "Failed to copy attributes for dimension %d of of SDS <%s>\n", i, path);
   continue;
  }
  
 }

/*-------------------------------------------------------------------------
 * insert ANs
 *-------------------------------------------------------------------------
 */ 
 
 insert_an(file_id,ref,tag,path);

/*-------------------------------------------------------------------------
 * terminate access to the SDSs
 *-------------------------------------------------------------------------
 */ 

 SDendaccess(sds_id);
   
 if (path)
  free(path);
 
 return 0;
 
}


/*-------------------------------------------------------------------------
 * Function: insert_sds_attrs
 *
 * Purpose: insert SDS attributes 
 *   used for global, dataset and dimension attributes
 *
 * Return: 1, for success
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 22, 2003
 *
 *-------------------------------------------------------------------------
 */

int insert_sds_attrs(int32 id_in,int32 nattrs)
{
 int32 dtype,                 /* SDS data type */
       nelms;                 /* number of elements */
 char  attr_name[MAX_NC_NAME];
 int   i;

 /* loop through attributes in input SDS */
 for (i = 0; i < nattrs; i++) 
 {
  if (SDattrinfo (id_in, i, attr_name, &dtype, &nelms) == FAIL) {
   printf( "Cannot get info for attribute number %d\n", i);
   continue;
  }
 
 }

 return 1;
}

/*-------------------------------------------------------------------------
 * Function: insert_gr_attrs
 *
 * Purpose: insert GR attributes 
 *
 * Return: 1, for success 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 22, 2003
 *
 *-------------------------------------------------------------------------
 */

int insert_gr_attrs(int32 ri_id,int32 nattrs)
{
 int32 dtype,                 /* SDS data type */
       nelms;                 /* number of elements */
 char  attr_name[MAX_NC_NAME];
 int   i;

 /* loop through attributes in input GR */
 for (i = 0; i < nattrs; i++) 
 {
  if (GRattrinfo (ri_id, i, attr_name, &dtype, &nelms) == FAIL) {
   printf( "Cannot get info for attribute number %d\n", i);
   continue;
  }
 
 }

 return 1;
}

/*-------------------------------------------------------------------------
 * Function: insert_vs_attrs
 *
 * Purpose: insert VS attributes 
 *
 * Return: 1, for success, -1 for error 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 22, 2003
 *
 *-------------------------------------------------------------------------
 */

int insert_vs_attrs(int32 in, int32 findex, intn attrindex)
{
 char   attr_name[MAX_NC_NAME];
 int32  n_values, attr_size, attr_type;

 /* Get attribute information */
 VSattrinfo(in, findex, attrindex, attr_name, &attr_type, &n_values, &attr_size);

 return 1;
}


/*-------------------------------------------------------------------------
 * Function: insert_gr
 *
 * Purpose: insert a GR 
 *
 * Return: 1
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 22, 2003
 *
 *-------------------------------------------------------------------------
 */

int  insert_gr(int32 file_id,
               int32 gr_in,
               int32 tag,               /* tag of input GR */
               int32 ref,               /* ref of input GR */
               char*path_name,          /* absolute path for input group name */
               dtable_t *table)
{
 int32         ri_id,         /* raster image identifier */
               ri_index,      /* index of a image */
               dimsizes[2],   /* dimensions of an image */
               n_comps,       /* number of components an image contains */
               interlace_mode,/* interlace mode of an image */ 
               dtype,         /* number type of an image */
               n_attrs;       /* number of attributes belong to an image */
               
 int32         pal_id,        /* palette identifier */
               r_num_entries, 
               r_data_type, 
               r_ncomp, 
               r_interlace_mode; 
 char          gr_name[MAX_GR_NAME]; 
 char          *path=NULL;
 int           has_pal = 0;

 ri_index = GRreftoindex(gr_in,(uint16)ref);
 ri_id    = GRselect(gr_in,ri_index);
   
 GRgetiminfo(ri_id,gr_name,&n_comps,&dtype,&interlace_mode,dimsizes,&n_attrs);
 
 /* initialize path */
 path=get_path(path_name,gr_name);

 /* add object to table */
 dtable_add(table,tag,ref,path);

#if defined (HDIFF_DEBUG)
   printf("%s\n",path); 
#endif  
 
/*-------------------------------------------------------------------------
 * insert attributes
 *-------------------------------------------------------------------------
 */ 
 
 insert_gr_attrs(ri_id,n_attrs);

/*-------------------------------------------------------------------------
 * check for palette
 *-------------------------------------------------------------------------
 */ 

 pal_id = GRgetlutid(ri_id, 0);
 GRgetlutinfo(pal_id,&r_ncomp,&r_data_type,&r_interlace_mode,&r_num_entries);

 /*check if there is palette data */
 has_pal=((r_ncomp == 0) || (r_interlace_mode < 0) || (r_num_entries == 0))?0:1;

 if ( has_pal==1 )
 {
 
 } /* has_pal==1 */

 
/*-------------------------------------------------------------------------
 * insert ANs
 *-------------------------------------------------------------------------
 */ 

 insert_an(file_id,ref,DFTAG_RIG,path);
 insert_an(file_id,ref,DFTAG_RI,path);
 
/*-------------------------------------------------------------------------
 * terminate access to the GR
 *-------------------------------------------------------------------------
 */ 

 /* terminate access to the GRs */
 GRendaccess(ri_id);
    
 if (path)
  free(path);
 
 return 0;
 
}



/*-------------------------------------------------------------------------
 * Function: insert_vs
 *
 * Purpose: insert a VS 
 *
 * Return: 0, -1 for error 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 22, 2003
 *
 *-------------------------------------------------------------------------
 */

int  insert_vs( int32 file_id,
                int32 tag,
                int32 ref,               /* ref of input VS */
                char*path_name,          /* absolute path for input group name */
                dtable_t *table,
                int is_lone)
{
 int32 vdata_id,              /* vdata identifier */
       tag_vs,
       ref_vs;
 int   n_fields, n_attrs;
 char  vdata_name [VSNAMELENMAX], vdata_class[VSNAMELENMAX];
 char  *path=NULL;
 int   i, j, ret=1;

/*-------------------------------------------------------------------------
 * attach the vdata, gets its name and class
 *-------------------------------------------------------------------------
 */ 

 if ((vdata_id  = VSattach (file_id, ref, "r")) == FAIL ){
  printf( "Failed to attach vdata ref %d\n", ref);
  return-1;
 }
 if (VSgetname  (vdata_id, vdata_name) == FAIL ){
  printf( "Failed to name for vdata ref %d\n", ref);
  return-1;
 }
 if (VSgetclass (vdata_id, vdata_class) == FAIL ){
  printf( "Failed to name for vdata ref %d\n", ref);
  return-1;
 }
 
 /* ignore reserved HDF groups/vdatas; they are lone ones */
 if( is_lone==1 && vdata_class != NULL) {
  if( is_reserved(vdata_class)){
   if (VSdetach (vdata_id) == FAIL )
    printf( "Failed to detach vdata <%s>\n", path_name);
   return 0;
  }
 }

 
 if ((ref_vs = VSQueryref(vdata_id))==FAIL){
  printf( "Failed to get ref for <%s>\n", vdata_name);
 }
 if ((tag_vs = VSQuerytag(vdata_id))==FAIL){
  printf( "Failed to get tag for <%s>\n", vdata_name);
 }


 /* initialize path */
 path=get_path(path_name,vdata_name);
 
 /* add object to table */
 dtable_add(table,tag_vs,ref_vs,path);

#if defined (HDIFF_DEBUG)
  printf("%s\n",path); 
  assert(tag==tag_vs);
  assert(ref==ref_vs);
#endif

/*-------------------------------------------------------------------------
 * fields 
 *-------------------------------------------------------------------------
 */ 
 
 if ((n_fields = VFnfields(vdata_id)) == FAIL ){
  printf( "Failed getting fields for VS <%s>\n", path);
  ret=-1;
  goto out;
 }
 



/*-------------------------------------------------------------------------
 * insert attributes
 *-------------------------------------------------------------------------
 */ 
 
 if ((n_attrs = VSfnattrs( vdata_id, -1 )) == FAIL ){
  printf( "Failed getting attributes for VS <%s>\n", path);
  ret=-1;
  goto out;
 }
 for (i = 0; i < n_attrs; i++) {
  insert_vs_attrs(vdata_id, -1, i);
 }
 
/*-------------------------------------------------------------------------
 * insert field attributes
 *-------------------------------------------------------------------------
 */ 
  
 for (i = 0; i < n_fields; i++) {
  if ((n_attrs = VSfnattrs(vdata_id, i)) == FAIL ){
   printf( "Failed getting fields for VS <%s>\n", path);
   ret=-1;
   goto out;
  }
  for (j = 0; j < n_attrs; j++) {
   insert_vs_attrs(vdata_id, i, j);
  }
 }
  

/*-------------------------------------------------------------------------
 * insert ANs
 *-------------------------------------------------------------------------
 */ 

 insert_vs_an(file_id,vdata_id,path);

/*-------------------------------------------------------------------------
 * terminate access to the VSs
 *-------------------------------------------------------------------------
 */ 
 
out:
 VSdetach (vdata_id);
 
 if (path)
  free(path);
 
 return ret;
}






/*-------------------------------------------------------------------------
 * Function: is_reserved
 *
 * Purpose: check for reserved Vgroup/Vdata class/names
 *
 * Return: 1 if reserved, 0 if not
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 22, 2003
 *
 *-------------------------------------------------------------------------
 */

static
int is_reserved(char*vgroup_class)
{
 int ret=0;
 
 /* ignore reserved HDF groups/vdatas */
 if(vgroup_class != NULL) {
  if( (strcmp(vgroup_class,_HDF_ATTRIBUTE)==0) ||
   (strcmp(vgroup_class,_HDF_VARIABLE) ==0) || 
   (strcmp(vgroup_class,_HDF_DIMENSION)==0) ||
   (strcmp(vgroup_class,_HDF_UDIMENSION)==0) ||
   (strcmp(vgroup_class,DIM_VALS)==0) ||
   (strcmp(vgroup_class,DIM_VALS01)==0) ||
   (strcmp(vgroup_class,_HDF_CDF)==0) ||
   (strcmp(vgroup_class,GR_NAME)==0) ||
   (strcmp(vgroup_class,RI_NAME)==0) || 
   (strcmp(vgroup_class,RIGATTRNAME)==0) ||
   (strcmp(vgroup_class,RIGATTRCLASS)==0) ){
   ret=1;
  }

  /* class and name(partial) for chunk table i.e. Vdata */
  if( (strncmp(vgroup_class,"_HDF_CHK_TBL_",13)==0)){
   ret=1;
  }

 }
 
 return ret;
}



/*-------------------------------------------------------------------------
 * Function: get_path
 *
 * Purpose: return absolute path for an object
 *
 * Return: path
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 11, 2003
 *
 *-------------------------------------------------------------------------
 */

static
char *get_path(char*path_name, char*obj_name) 
{
 char *path=NULL;
 /* initialize path */
 if (path_name!=NULL) 
 {
  path = (char*) malloc(strlen(path_name) + strlen(obj_name) + 2);
  strcpy( path, path_name );
  strcat( path, "/" );
  strcat( path, obj_name ); 
 }
 else
 {
  path = (char*) malloc(strlen(obj_name) + 1);
  strcpy( path, obj_name ); 
 }
 return path;
}




