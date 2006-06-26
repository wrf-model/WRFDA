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
#include "hrepack_pal.h"



/*-------------------------------------------------------------------------
 * Function: copy_pal
 *
 * Purpose: list/copy lone palettes
 *
 *-------------------------------------------------------------------------
 */

int copy_pal(char* infname,char* outfname,int32 infile_id,int32 outfile_id,
              table_t *table,options_t *options)
{
 uint8  palette_data[256*3];
 intn   nPals, j;
 uint16 ref;
 
 if ( options->trip==0 ) 
 {
  return SUCCESS;
 }

 DFPrestart();
 
 if((nPals = DFPnpals (infname))==FAIL ) {
  printf( "Failed to get palettes in <%s>\n", infname);
  return FAIL;
 }
 
 for ( j = 0; j < nPals; j++) 
 {
  if (DFPgetpal(infname, (VOIDP)palette_data)==FAIL ) {
   printf( "Failed to read palette <%d> in <%s>\n", j, infname);
   continue;
  }
  
  ref=DFPlastref();
  
  /* check if already inserted in image */
  if ( table_search(table,DFTAG_IP8,ref)>=0 ){
   continue;
  }
  
  if (DFPaddpal(outfname,palette_data)==FAIL){
   printf( "Failed to write palette in <%s>\n", outfname);
   return FAIL;
  }
  
 }
 
 return SUCCESS;
}


