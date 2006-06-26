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


#include "hdf.h"
#include "mfhdf.h"
#include "hrepack.h"


int cmp_grs(char* file1_name,char* file2_name);
int cmp_gr(int32 ri1_id, int32 ri2_id);



/*-------------------------------------------------------------------------
 * Function: cmp_grs
 *
 * Purpose: compare all GR images in 2 files, assumed to be identical
 *
 * Return: same as memcmp
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 03, 2003
 *
 *-------------------------------------------------------------------------
 */

int cmp_grs(char* file1_name,char* file2_name)
{
 int32 file1_id,          /* file identifier */
       file2_id,          /* file identifier */
       gr1_id,            /* GR interface identifier */
       gr2_id,            /* GR interface identifier */
       ri1_id,            /* raster image identifier */
       ri2_id,            /* raster image identifier */
       n_rimages,         /* number of raster images in the file */
       n_file_attrs,      /* number of file attributes */
       ri_index;          /* index of a image */
 int   cmp=-1;

 /* open the files for read  */
 file1_id = Hopen (file1_name,DFACC_READ,0);
 file2_id = Hopen (file2_name,DFACC_READ,0);
 
 /* initialize the GR interface */
 gr1_id = GRstart (file1_id);
 gr2_id = GRstart (file2_id);
 
 /* determine the contents of the file */
 if (GRfileinfo (gr1_id, &n_rimages, &n_file_attrs)==FAIL){
  printf("Error: Cannot get GR info\n");
  goto out;
 }
  
 for (ri_index = 0; ri_index < n_rimages; ri_index++)
 {
  ri1_id = GRselect (gr1_id, ri_index);
  ri2_id = GRselect (gr2_id, ri_index);
  
  /* compare GR  */
  cmp = cmp_gr(ri1_id,ri2_id);

  /* terminate access to the current raster image */
  GRendaccess (ri1_id);
  GRendaccess (ri2_id);
 }
 
out:
 /* terminate access to the GR interface */
 GRend (gr2_id);
 GRend (gr2_id);
 /* close the HDF files */
 if (Hclose (file1_id)== FAIL )
  printf( "Failed to close file <%s>\n", file1_name);
 if (Hclose (file2_id)== FAIL )
  printf( "Failed to close file <%s>\n", file2_name);

 return cmp;

}


/*-------------------------------------------------------------------------
 * Function: cmp_gr
 *
 * Purpose: compare 2 GR images in 2 files, assumed to be identical
 *
 * Return: same as memcmp
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 03, 2003
 *
 *-------------------------------------------------------------------------
 */

int  cmp_gr(int32 ri1_id, int32 ri2_id)
{
 int32         dimsizes[2],   /* dimensions of an image */
               n_comps,       /* number of components an image contains */
               interlace_mode1,/* interlace mode of an image */ 
               dtype,         /* number type of an image */
               n_attrs;       /* number of attributes belong to an image */
 int32         interlace_mode2;        
 char          gr_name[MAX_GR_NAME]; 
 int           j, rank=2;
 int32         start[2],       /* read start */
               edges[2],       /* read edges */
               numtype,        /* number type */
               eltsz,          /* element size */
               nelms,          /* number of elements */
               data_size;
 VOIDP         buf1=NULL, buf2=NULL;
 int           cmp=-1;

 GRgetiminfo(ri1_id,gr_name,&n_comps,&dtype,&interlace_mode1,dimsizes,&n_attrs);
 GRgetiminfo(ri2_id,gr_name,&n_comps,&dtype,&interlace_mode2,dimsizes,&n_attrs);

 printf( "Comparing GR <%s>: ", gr_name);

 
/*-------------------------------------------------------------------------
 * match interlace 
 * NOTE: GR images are always stored as pixel_interlace (0) on disk
 *       that does not happen with images saved with the 
 *       DF24 - Single-file 24-Bit Raster Image Interface,
 *       where the interlace mode on disk can be 0, 1 or 2
 *-------------------------------------------------------------------------
 */
 if ( interlace_mode1 != interlace_mode2 )
 {
  printf("Warning: different interlace mode: <%d> and <%d>", 
   interlace_mode1,interlace_mode2);
  interlace_mode1=interlace_mode2;
 }

/*-------------------------------------------------------------------------
 * check for data size before printing
 *-------------------------------------------------------------------------
 */

 /* compute the number of the bytes for each value. */
 numtype = dtype & DFNT_MASK;
 eltsz = DFKNTsize(numtype | DFNT_NATIVE);

 /* set edges of GR */
 nelms=1;
 for (j = 0; j < rank; j++) {
  nelms   *= dimsizes[j];
  edges[j] = dimsizes[j];
  start[j] = 0;
 }

 data_size = dimsizes[0]*dimsizes[1]*n_comps*eltsz;

/*-------------------------------------------------------------------------
 * read gr 1
 *-------------------------------------------------------------------------
 */

 /* alloc */
 if ((buf1 = (VOIDP) HDmalloc(data_size)) == NULL) {
  printf( "Failed to allocate %d elements of size %d\n", nelms, eltsz);
  goto out;
 }


 /* set the interlace for reading  */
 if ( GRreqimageil(ri1_id, interlace_mode1) == FAIL ){
  printf( "Could not set interlace for GR\n");
  goto out;
 }
 
 /* read data */
 if (GRreadimage (ri1_id, start, NULL, edges, buf1) == FAIL) {
  printf( "Could not read GR\n");
  goto out;
 }

/*-------------------------------------------------------------------------
 * read gr 2
 *-------------------------------------------------------------------------
 */

 /* alloc */
 if ((buf2 = (VOIDP) HDmalloc(data_size)) == NULL) {
  printf( "Failed to allocate %d elements of size %d\n", nelms, eltsz);
  goto out;
 }

 /* set the interlace for reading  */
 if ( GRreqimageil(ri2_id, interlace_mode2 /*interlace_mode1*/) == FAIL ){
  printf( "Could not set interlace for GR\n");
  goto out;
 }
 
 /* read data */
 if (GRreadimage (ri2_id, start, NULL, edges, buf2) == FAIL) {
  printf( "Could not read GR\n");
  goto out;
 }
 
 cmp = HDmemcmp(buf1,buf2,data_size);
 if (cmp!=0)
  printf("Differences found\n");
 else
  printf("\n");

out:
 /* terminate access to the GRs */
 GRendaccess(ri1_id);
 GRendaccess(ri2_id);
 if (buf1)
  free(buf1);
 if (buf2)
  free(buf2);
 return cmp;
 
}

