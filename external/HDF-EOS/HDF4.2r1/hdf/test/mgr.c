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

#ifdef RCSID
static char RcsId[] = "$Revision: 1.39 $";
#endif

/* $Id: mgr.c,v 1.39 2005/02/14 00:08:13 bmribler Exp $ */

/***********************************************************
*
* Test program:  mgr
*
* Test the multi-file raster image interface
*
*************************************************************/

#define TESTFILE "tmgr.hdf"
#define TESTFILE2 "tmgrchk.hdf"
#define DATAFILE "tmgr.dat"

#define MFGR_TESTER
#include "tproto.h"
#include "mfgr.h"

/* Local pre-processor macros */
#define XDIM    0
#define YDIM    1
#define MAX_IMG_NAME    64  /* Maximum length of image names for this test */

/* Local Data to verify image information in datafile */
const struct {
    const char *name;
    int32 ncomp;
    int32 nt;
    int32 il;
    int32 dimsizes[2];
    int32 n_attr;
} datafile_info[]=
  { /* This information applies to the "tmgr.dat" file */
    {"Raster Image #0", 3, DFNT_UCHAR8, MFGR_INTERLACE_PIXEL, {13,15}, 2},
    {"Raster Image #1", 3, DFNT_UCHAR8, MFGR_INTERLACE_LINE, {13,15}, 2},
    {"Raster Image #2", 3, DFNT_UCHAR8, MFGR_INTERLACE_COMPONENT, {13,15}, 2},
    {"Test Image #1", 4, DFNT_UINT16, MFGR_INTERLACE_PIXEL, {21,23}, 3},
    {"Test Image #2", 2, DFNT_FLOAT64, MFGR_INTERLACE_PIXEL, {17,19}, 3}
  };

const uint8 image00[15][13][3]={
{{0 ,0 ,0 },{1 ,1 ,1 },{2 ,2 ,2 },{3 ,3 ,3 },{4 ,4 ,4 },{5 ,5 ,5 },{6 ,6 ,6 },{7 ,7 ,7 },{8 ,8 ,8 },{9 ,9 ,9 },{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 }},
{{1 ,1 ,1 },{2 ,2 ,2 },{3 ,3 ,3 },{4 ,4 ,4 },{5 ,5 ,5 },{6 ,6 ,6 },{7 ,7 ,7 },{8 ,8 ,8 },{9 ,9 ,9 },{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 }},
{{2 ,2 ,2 },{3 ,3 ,3 },{4 ,4 ,4 },{5 ,5 ,5 },{6 ,6 ,6 },{7 ,7 ,7 },{8 ,8 ,8 },{9 ,9 ,9 },{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 }},
{{3 ,3 ,3 },{4 ,4 ,4 },{5 ,5 ,5 },{6 ,6 ,6 },{7 ,7 ,7 },{8 ,8 ,8 },{9 ,9 ,9 },{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 }},
{{4 ,4 ,4 },{5 ,5 ,5 },{6 ,6 ,6 },{7 ,7 ,7 },{8 ,8 ,8 },{9 ,9 ,9 },{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 }},
{{5 ,5 ,5 },{6 ,6 ,6 },{7 ,7 ,7 },{8 ,8 ,8 },{9 ,9 ,9 },{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 },{17 ,17 ,17 }},
{{6 ,6 ,6 },{7 ,7 ,7 },{8 ,8 ,8 },{9 ,9 ,9 },{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 },{17 ,17 ,17 },{18 ,18 ,18 }},
{{7 ,7 ,7 },{8 ,8 ,8 },{9 ,9 ,9 },{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 },{17 ,17 ,17 },{18 ,18 ,18 },{19 ,19 ,19 }},
{{8 ,8 ,8 },{9 ,9 ,9 },{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 },{17 ,17 ,17 },{18 ,18 ,18 },{19 ,19 ,19 },{20 ,20 ,20 }},
{{9 ,9 ,9 },{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 },{17 ,17 ,17 },{18 ,18 ,18 },{19 ,19 ,19 },{20 ,20 ,20 },{21 ,21 ,21 }},
{{10 ,10 ,10 },{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 },{17 ,17 ,17 },{18 ,18 ,18 },{19 ,19 ,19 },{20 ,20 ,20 },{21 ,21 ,21 },{22 ,22 ,22 }},
{{11 ,11 ,11 },{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 },{17 ,17 ,17 },{18 ,18 ,18 },{19 ,19 ,19 },{20 ,20 ,20 },{21 ,21 ,21 },{22 ,22 ,22 },{23 ,23 ,23 }},
{{12 ,12 ,12 },{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 },{17 ,17 ,17 },{18 ,18 ,18 },{19 ,19 ,19 },{20 ,20 ,20 },{21 ,21 ,21 },{22 ,22 ,22 },{23 ,23 ,23 },{24 ,24 ,24 }},
{{13 ,13 ,13 },{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 },{17 ,17 ,17 },{18 ,18 ,18 },{19 ,19 ,19 },{20 ,20 ,20 },{21 ,21 ,21 },{22 ,22 ,22 },{23 ,23 ,23 },{24 ,24 ,24 },{25 ,25 ,25 }},
{{14 ,14 ,14 },{15 ,15 ,15 },{16 ,16 ,16 },{17 ,17 ,17 },{18 ,18 ,18 },{19 ,19 ,19 },{20 ,20 ,20 },{21 ,21 ,21 },{22 ,22 ,22 },{23 ,23 ,23 },{24 ,24 ,24 },{25 ,25 ,25 },{26 ,26 ,26 }}
};
const uint8 image1[15][13][3]={
{{0 ,1 ,2 },{3 ,4 ,5 },{6 ,7 ,8 },{9 ,10 ,11 },{12 ,0 ,1 },{2 ,3 ,4 },{5 ,6 ,7 },{8 ,9 ,10 },{11 ,12 ,0 },{1 ,2 ,3 },{4 ,5 ,6 },{7 ,8 ,9 },{10 ,11 ,12 }},
{{1 ,1 ,3 },{3 ,5 ,5 },{7 ,7 ,9 },{9 ,11 ,11 },{13 ,1 ,1 },{3 ,3 ,5 },{5 ,7 ,7 },{9 ,9 ,11 },{11 ,13 ,1 },{1 ,3 ,3 },{5 ,5 ,7 },{7 ,9 ,9 },{11 ,11 ,13 }},
{{2 ,3 ,2 },{3 ,6 ,7 },{6 ,7 ,10 },{11 ,10 ,11 },{14 ,2 ,3 },{2 ,3 ,6 },{7 ,6 ,7 },{10 ,11 ,10 },{11 ,14 ,2 },{3 ,2 ,3 },{6 ,7 ,6 },{7 ,10 ,11 },{10 ,11 ,14 }},
{{3 ,3 ,3 },{3 ,7 ,7 },{7 ,7 ,11 },{11 ,11 ,11 },{15 ,3 ,3 },{3 ,3 ,7 },{7 ,7 ,7 },{11 ,11 ,11 },{11 ,15 ,3 },{3 ,3 ,3 },{7 ,7 ,7 },{7 ,11 ,11 },{11 ,11 ,15 }},
{{4 ,5 ,6 },{7 ,4 ,5 },{6 ,7 ,12 },{13 ,14 ,15 },{12 ,4 ,5 },{6 ,7 ,4 },{5 ,6 ,7 },{12 ,13 ,14 },{15 ,12 ,4 },{5 ,6 ,7 },{4 ,5 ,6 },{7 ,12 ,13 },{14 ,15 ,12 }},
{{5 ,5 ,7 },{7 ,5 ,5 },{7 ,7 ,13 },{13 ,15 ,15 },{13 ,5 ,5 },{7 ,7 ,5 },{5 ,7 ,7 },{13 ,13 ,15 },{15 ,13 ,5 },{5 ,7 ,7 },{5 ,5 ,7 },{7 ,13 ,13 },{15 ,15 ,13 }},
{{6 ,7 ,6 },{7 ,6 ,7 },{6 ,7 ,14 },{15 ,14 ,15 },{14 ,6 ,7 },{6 ,7 ,6 },{7 ,6 ,7 },{14 ,15 ,14 },{15 ,14 ,6 },{7 ,6 ,7 },{6 ,7 ,6 },{7 ,14 ,15 },{14 ,15 ,14 }},
{{7 ,7 ,7 },{7 ,7 ,7 },{7 ,7 ,15 },{15 ,15 ,15 },{15 ,7 ,7 },{7 ,7 ,7 },{7 ,7 ,7 },{15 ,15 ,15 },{15 ,15 ,7 },{7 ,7 ,7 },{7 ,7 ,7 },{7 ,15 ,15 },{15 ,15 ,15 }},
{{8 ,9 ,10 },{11 ,12 ,13 },{14 ,15 ,8 },{9 ,10 ,11 },{12 ,8 ,9 },{10 ,11 ,12 },{13 ,14 ,15 },{8 ,9 ,10 },{11 ,12 ,8 },{9 ,10 ,11 },{12 ,13 ,14 },{15 ,8 ,9 },{10 ,11 ,12 }},
{{9 ,9 ,11 },{11 ,13 ,13 },{15 ,15 ,9 },{9 ,11 ,11 },{13 ,9 ,9 },{11 ,11 ,13 },{13 ,15 ,15 },{9 ,9 ,11 },{11 ,13 ,9 },{9 ,11 ,11 },{13 ,13 ,15 },{15 ,9 ,9 },{11 ,11 ,13 }},
{{10 ,11 ,10 },{11 ,14 ,15 },{14 ,15 ,10 },{11 ,10 ,11 },{14 ,10 ,11 },{10 ,11 ,14 },{15 ,14 ,15 },{10 ,11 ,10 },{11 ,14 ,10 },{11 ,10 ,11 },{14 ,15 ,14 },{15 ,10 ,11 },{10 ,11 ,14 }},
{{11 ,11 ,11 },{11 ,15 ,15 },{15 ,15 ,11 },{11 ,11 ,11 },{15 ,11 ,11 },{11 ,11 ,15 },{15 ,15 ,15 },{11 ,11 ,11 },{11 ,15 ,11 },{11 ,11 ,11 },{15 ,15 ,15 },{15 ,11 ,11 },{11 ,11 ,15 }},
{{12 ,13 ,14 },{15 ,12 ,13 },{14 ,15 ,12 },{13 ,14 ,15 },{12 ,12 ,13 },{14 ,15 ,12 },{13 ,14 ,15 },{12 ,13 ,14 },{15 ,12 ,12 },{13 ,14 ,15 },{12 ,13 ,14 },{15 ,12 ,13 },{14 ,15 ,12 }},
{{13 ,13 ,15 },{15 ,13 ,13 },{15 ,15 ,13 },{13 ,15 ,15 },{13 ,13 ,13 },{15 ,15 ,13 },{13 ,15 ,15 },{13 ,13 ,15 },{15 ,13 ,13 },{13 ,15 ,15 },{13 ,13 ,15 },{15 ,13 ,13 },{15 ,15 ,13 }},
{{14 ,15 ,14 },{15 ,14 ,15 },{14 ,15 ,14 },{15 ,14 ,15 },{14 ,14 ,15 },{14 ,15 ,14 },{15 ,14 ,15 },{14 ,15 ,14 },{15 ,14 ,14 },{15 ,14 ,15 },{14 ,15 ,14 },{15 ,14 ,15 },{14 ,15 ,14 }}
};
const uint8 image2[15][13][3]={
{{0 ,1 ,2 },{3 ,4 ,5 },{6 ,7 ,8 },{9 ,10 ,11 },{12 ,1 ,0 },{3 ,2 ,5 },{4 ,7 ,6 },{9 ,8 ,11 },{10 ,13 ,2 },{3 ,0 ,1 },{6 ,7 ,4 },{5 ,10 ,11 },{8 ,9 ,14 }},
{{3 ,2 ,1 },{0 ,7 ,6 },{5 ,4 ,11 },{10 ,9 ,8 },{15 ,4 ,5 },{6 ,7 ,0 },{1 ,2 ,3 },{12 ,13 ,14 },{15 ,8 ,5 },{4 ,7 ,6 },{1 ,0 ,3 },{2 ,13 ,12 },{15 ,14 ,9 }},
{{6 ,7 ,4 },{5 ,2 ,3 },{0 ,1 ,14 },{15 ,12 ,13 },{10 ,7 ,6 },{5 ,4 ,3 },{2 ,1 ,0 },{15 ,14 ,13 },{12 ,11 ,8 },{9 ,10 ,11 },{12 ,13 ,14 },{15 ,0 ,1 },{2 ,3 ,4 }},
{{9 ,8 ,11 },{10 ,13 ,12 },{15 ,14 ,1 },{0 ,3 ,2 },{5 ,10 ,11 },{8 ,9 ,14 },{15 ,12 ,13 },{2 ,3 ,0 },{1 ,6 ,11 },{10 ,9 ,8 },{15 ,14 ,13 },{12 ,3 ,2 },{1 ,0 ,7 }},
{{12 ,13 ,14 },{15 ,8 ,9 },{10 ,11 ,4 },{5 ,6 ,7 },{0 ,13 ,12 },{15 ,14 ,9 },{8 ,11 ,10 },{5 ,4 ,7 },{6 ,1 ,14 },{15 ,12 ,13 },{10 ,11 ,8 },{9 ,6 ,7 },{4 ,5 ,2 }},
{{0 ,1 ,2 },{3 ,4 ,5 },{6 ,7 ,8 },{9 ,10 ,11 },{12 ,1 ,0 },{3 ,2 ,5 },{4 ,7 ,6 },{9 ,8 ,11 },{10 ,13 ,2 },{3 ,0 ,1 },{6 ,7 ,4 },{5 ,10 ,11 },{8 ,9 ,14 }},
{{3 ,2 ,1 },{0 ,7 ,6 },{5 ,4 ,11 },{10 ,9 ,8 },{15 ,4 ,5 },{6 ,7 ,0 },{1 ,2 ,3 },{12 ,13 ,14 },{15 ,8 ,5 },{4 ,7 ,6 },{1 ,0 ,3 },{2 ,13 ,12 },{15 ,14 ,9 }},
{{6 ,7 ,4 },{5 ,2 ,3 },{0 ,1 ,14 },{15 ,12 ,13 },{10 ,7 ,6 },{5 ,4 ,3 },{2 ,1 ,0 },{15 ,14 ,13 },{12 ,11 ,8 },{9 ,10 ,11 },{12 ,13 ,14 },{15 ,0 ,1 },{2 ,3 ,4 }},
{{9 ,8 ,11 },{10 ,13 ,12 },{15 ,14 ,1 },{0 ,3 ,2 },{5 ,10 ,11 },{8 ,9 ,14 },{15 ,12 ,13 },{2 ,3 ,0 },{1 ,6 ,11 },{10 ,9 ,8 },{15 ,14 ,13 },{12 ,3 ,2 },{1 ,0 ,7 }},
{{12 ,13 ,14 },{15 ,8 ,9 },{10 ,11 ,4 },{5 ,6 ,7 },{0 ,13 ,12 },{15 ,14 ,9 },{8 ,11 ,10 },{5 ,4 ,7 },{6 ,1 ,14 },{15 ,12 ,13 },{10 ,11 ,8 },{9 ,6 ,7 },{4 ,5 ,2 }},
{{0 ,1 ,2 },{3 ,4 ,5 },{6 ,7 ,8 },{9 ,10 ,11 },{12 ,1 ,0 },{3 ,2 ,5 },{4 ,7 ,6 },{9 ,8 ,11 },{10 ,13 ,2 },{3 ,0 ,1 },{6 ,7 ,4 },{5 ,10 ,11 },{8 ,9 ,14 }},
{{3 ,2 ,1 },{0 ,7 ,6 },{5 ,4 ,11 },{10 ,9 ,8 },{15 ,4 ,5 },{6 ,7 ,0 },{1 ,2 ,3 },{12 ,13 ,14 },{15 ,8 ,5 },{4 ,7 ,6 },{1 ,0 ,3 },{2 ,13 ,12 },{15 ,14 ,9 }},
{{6 ,7 ,4 },{5 ,2 ,3 },{0 ,1 ,14 },{15 ,12 ,13 },{10 ,7 ,6 },{5 ,4 ,3 },{2 ,1 ,0 },{15 ,14 ,13 },{12 ,11 ,8 },{9 ,10 ,11 },{12 ,13 ,14 },{15 ,0 ,1 },{2 ,3 ,4 }},
{{9 ,8 ,11 },{10 ,13 ,12 },{15 ,14 ,1 },{0 ,3 ,2 },{5 ,10 ,11 },{8 ,9 ,14 },{15 ,12 ,13 },{2 ,3 ,0 },{1 ,6 ,11 },{10 ,9 ,8 },{15 ,14 ,13 },{12 ,3 ,2 },{1 ,0 ,7 }},
{{12 ,13 ,14 },{15 ,8 ,9 },{10 ,11 ,4 },{5 ,6 ,7 },{0 ,13 ,12 },{15 ,14 ,9 },{8 ,11 ,10 },{5 ,4 ,7 },{6 ,1 ,14 },{15 ,12 ,13 },{10 ,11 ,8 },{9 ,6 ,7 },{4 ,5 ,2 }}
};
const uint16 image3[23][21][4]={
{{0 ,1 ,2 ,3 },{1 ,2 ,3 ,4 },{2 ,3 ,4 ,5 },{3 ,4 ,5 ,6 },{4 ,5 ,6 ,7 },{5 ,6 ,7 ,8 },{6 ,7 ,8 ,9 },{7 ,8 ,9 ,10 },{8 ,9 ,10 ,11 },{9 ,10 ,11 ,12 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{10 ,11 ,12 ,13 },{2 ,2 ,2 ,2 },{12 ,13 ,14 ,15 },{2 ,2 ,2 ,2 },{14 ,15 ,16 ,17 },{2 ,2 ,2 ,2 },{16 ,17 ,18 ,19 },{2 ,2 ,2 ,2 },{18 ,19 ,20 ,21 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{20 ,21 ,22 ,23 },{2 ,2 ,2 ,2 },{1 ,2 ,3 ,4 },{2 ,2 ,2 ,2 },{3 ,4 ,5 ,6 },{2 ,2 ,2 ,2 },{5 ,6 ,7 ,8 },{2 ,2 ,2 ,2 },{7 ,8 ,9 ,10 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{9 ,10 ,11 ,12 },{2 ,2 ,2 ,2 },{11 ,12 ,13 ,14 },{2 ,2 ,2 ,2 },{13 ,14 ,15 ,16 },{2 ,2 ,2 ,2 },{15 ,16 ,17 ,18 },{2 ,2 ,2 ,2 },{17 ,18 ,19 ,20 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{19 ,20 ,21 ,22 },{2 ,2 ,2 ,2 },{0 ,1 ,2 ,3 },{2 ,2 ,2 ,2 },{2 ,3 ,4 ,5 },{2 ,2 ,2 ,2 },{4 ,5 ,6 ,7 },{2 ,2 ,2 ,2 },{6 ,7 ,8 ,9 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{8 ,9 ,10 ,11 },{2 ,2 ,2 ,2 },{10 ,11 ,12 ,13 },{2 ,2 ,2 ,2 },{12 ,13 ,14 ,15 },{2 ,2 ,2 ,2 },{14 ,15 ,16 ,17 },{2 ,2 ,2 ,2 },{16 ,17 ,18 ,19 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 },{2 ,2 ,2 ,2 },{0 ,0 ,0 ,0 }},
{{18 ,19 ,20 ,21 },{19 ,20 ,21 ,22 },{20 ,21 ,22 ,23 },{0 ,1 ,2 ,3 },{1 ,2 ,3 ,4 },{2 ,3 ,4 ,5 },{3 ,4 ,5 ,6 },{4 ,5 ,6 ,7 },{5 ,6 ,7 ,8 },{6 ,7 ,8 ,9 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{7 ,8 ,9 ,10 },{8 ,9 ,10 ,11 },{9 ,10 ,11 ,12 },{10 ,11 ,12 ,13 },{11 ,12 ,13 ,14 },{12 ,13 ,14 ,15 },{13 ,14 ,15 ,16 },{14 ,15 ,16 ,17 },{15 ,16 ,17 ,18 },{16 ,17 ,18 ,19 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{17 ,18 ,19 ,20 },{18 ,19 ,20 ,21 },{19 ,20 ,21 ,22 },{20 ,21 ,22 ,23 },{0 ,1 ,2 ,3 },{1 ,2 ,3 ,4 },{2 ,3 ,4 ,5 },{3 ,4 ,5 ,6 },{4 ,5 ,6 ,7 },{5 ,6 ,7 ,8 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{6 ,7 ,8 ,9 },{7 ,8 ,9 ,10 },{8 ,9 ,10 ,11 },{9 ,10 ,11 ,12 },{10 ,11 ,12 ,13 },{11 ,12 ,13 ,14 },{12 ,13 ,14 ,15 },{13 ,14 ,15 ,16 },{14 ,15 ,16 ,17 },{15 ,16 ,17 ,18 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{16 ,17 ,18 ,19 },{17 ,18 ,19 ,20 },{18 ,19 ,20 ,21 },{19 ,20 ,21 ,22 },{20 ,21 ,22 ,23 },{0 ,1 ,2 ,3 },{1 ,2 ,3 ,4 },{2 ,3 ,4 ,5 },{3 ,4 ,5 ,6 },{4 ,5 ,6 ,7 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }},
{{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 },{0 ,0 ,0 ,0 }}
};
const float64 image4[19][17][2]={
{{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0}},
{{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0}},
{{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0}},
{{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0}},
{{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0}},
{{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0}},
{{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0}},
{{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0}},
{{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{6.0,6.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0}},
{{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{0.0,0.0}},
{{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{0.0,0.0}},
{{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{0.0,0.0}},
{{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{0.0,0.0}},
{{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{0.0,0.0}},
{{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{0.0,0.0}},
{{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{0.0,0.0}},
{{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{0.0,0.0}},
{{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{9.0,9.0},{0.0,0.0}},
{{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0},{0.0,0.0}}
};

#ifdef QAK
static void dump_image(void *data, int32 xdim, int32 ydim, int32 ncomp, int32 nt);
#endif /* QAK */
static void test_mgr_init(void);
static void test_mgr_image_b1a(int flag);
static void test_mgr_image_b1b(int flag);
static void test_mgr_image_b2a1aa(int flag);
static void test_mgr_image_b2a1bb1(int flag);
static void test_mgr_image_b2a1bb2(int flag);
static void test_mgr_image_b2a1cc1(int flag);
static void test_mgr_image_b2a1cc2(int flag);
static void test_mgr_image_b2a2aa(int flag);
static void test_mgr_image_b2a2bb(int flag);
static void test_mgr_image_b2a2cc(int flag);
static void test_mgr_image_b2b1(int flag);
static void test_mgr_image_b2b2(int flag);
static void test_mgr_image_b2b3(int flag);
static void test_mgr_image(int flag);
static void test_mgr_index(int flag);
static void test_mgr_interlace(int flag);
static void test_mgr_lut(int flag);
static void test_mgr_special(int flag);
static void test_mgr_attr(int flag);

#define ABS(x)  ((int)(x)<0 ? (-x) : x)

static intn
fuzzy_memcmp(const void *s1, const void *s2, int32 len, intn fuzz_factor);

#ifdef DEC_ALPHA
#define JPEG_FUZZ 13
#else
#define JPEG_FUZZ 1
#endif

static intn
fuzzy_memcmp(const void *s1, const void *s2, int32 len, intn fuzz_factor)
{
    const uint8 *t1 = (const uint8 *) s1;
    const uint8 *t2 = (const uint8 *) s2;

    while (len > 0 && (int) ABS(*t2 - *t1) <= fuzz_factor)
      {
          t1++;
          t2++;
          len--;
      }     /* end while */
    if (len == 0)
        return (0);
    else
      {
          return ((intn) (*t1 - *t2));
      }
}   /* end fuzzy_memcmp() */


#ifdef QAK
static void dump_image(void *data, int32 xdim, int32 ydim, int32 ncomp, int32 nt)
{
    int32 nt_size=DFKNTsize(nt);
    int32 i,j,k;

    for(i=0; i<ydim; i++)
      {
#ifdef QAK
          printf("%ld:",(long)i);
#endif /* QAK */
          for(j=0; j<xdim; j++)
            {
                if(ncomp>1)
                    printf("{");
                for(k=0; k<ncomp; k++)
                  {
                    switch(nt)
                      {
                          case DFNT_CHAR8:
                          case DFNT_UCHAR8:
#ifdef QAK
                            {
                                char *ptr=(char *)data;
                                printf("%c",*ptr);
                            }
                            break;
#endif /* QAK */

                          case DFNT_UINT8:
                            {
                                unsigned char *ptr=(unsigned char *)data;
                                printf("%u",(unsigned)*ptr);
                            }
                            break;

                          case DFNT_INT8:
                            {
                                char *ptr=(char *)data;
                                printf("%d",(int)*ptr);
                            }
                            break;

                          case DFNT_UINT16:
                            {
                                uint16 *ptr=(uint16 *)data;
                                printf("%u",(unsigned)*ptr);
                            }
                            break;

                          case DFNT_INT16:
                            {
                                int16 *ptr=(int16 *)data;
                                printf("%d",(int)*ptr);
                            }
                            break;

                          case DFNT_UINT32:
                            {
                                uint32 *ptr=(uint32 *)data;
                                printf("%lu",(unsigned long)*ptr);
                            }
                            break;

                          case DFNT_INT32:
                            {
                                int32 *ptr=(int32 *)data;
                                printf("%ld",(long)*ptr);
                            }
                            break;

                          case DFNT_FLOAT32:
                            {
                                float32 *ptr=(float32 *)data;
                                printf("%f",(double)*ptr);
                            }
                            break;

                          case DFNT_FLOAT64:
                            {
                                float64 *ptr=(float64 *)data;
                                printf("%f",(double)*ptr);
                            }
                            break;

                          default:
                            printf("unknown NT: %ld\n",(long)nt);
                            break;

                      } /* end switch */
                    if(k<(ncomp-1))
                        printf(", ");
                    data=(void *)((char *)data+nt_size);
                  } /* end for */
                if(ncomp>1)
                    printf("}, ");
                else
                    printf(", ");

            } /* end for */
          printf("\n");
      } /* end for */
}   /* dump_image() */
#endif /* QAK */

/* Test outline:
    I. Interface Initialization
        A. GRstart
        B. GRend
        C. GRfileinfo
    II. Create Images
        A. GRcreate/GRselect/GRendaccess w/GRgetiminfo
        B. Write/Read images
            1. With no Data
                a. Default fill value
                b. user defined fill value
            2. With real Data
                a. New Image
                    1. With default fill value
                        aa. Whole image
                        bb. Sub-setted image
                        cc. Sub-sampled image
                    2. With user defined vill value
                        aa. Whole image
                        bb. Sub-setted image
                        cc. Sub-sampled image
                b. Existing Image
                    1. Whole image
                    2. Sub-setted image
                    3. Sub-sampled image
    III. ID/Ref/Index Functions
        A. GRidtoref
        B. GRreftoindex
    IV. Interlace Functions [Need to be implemented]
        A. GRreqlutil
        B. GRreqimageil
    V. Palette Functions
        A. GRgetlutid w/GRgetlutinfo
        B. Read/Write Palettes
            1. GRwritelut
            2. GRreadlut
        C. GRluttoref
    VI. Special Element Functions [Need to be implemented]
        A. GRsetexternalfile
        B. GRsetaccesstype
    VII. Atribute Functions
        A. GRattrinfo
        B. Read/Write Attributes
            1. GRsetattr
            2. GRgetattr
        C. GRfindattr
    VIII. Old-Style Raster Image Access
        A. Read data from RLE compressed image
        B. Create RLE compressed image & write to it (not implemented)
        C. Read data from 8-bit JPEG compressed image
        D. Create 8-bit JPEG compressed image & write to it
        E. Read data from 24-bit JPEG compressed image
        F. Create 24-bit JPEG compressed image & write to it
    IX. Compressed image Functions
    X.  Chunking write/read test

        
*/

/****************************************************************
**
**  test_mgr_init(): Multi-file Raster Initialization Test Routine
** 
**  I. Interface Initialization
**      A. GRstart
**      B. GRend
**      C. GRfileinfo
** 
****************************************************************/
static void
test_mgr_init(void)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 n_datasets;       /* number of datasets */
    int32 n_attrs;          /* number of attributes */
    int32 ret;              /* generic return value */
    char datafile[512] = "";
    char *srcdir = getenv("srcdir");

    /* Output message about test being performed */
    MESSAGE(6, printf("Testing Multi-file Raster Initialization routines\n"););

    MESSAGE(8, printf("Try creating a new file and checking it out\n"););

    /* Generate the correct name for the test file, by prepending the source path */
    if (srcdir && ((strlen(srcdir) + strlen(DATAFILE) + 1) < sizeof(datafile))) {
        strcpy(datafile, srcdir);
        strcat(datafile, "/");
    }
    strcat(datafile, DATAFILE);

    /* Ok, now create a new file */
    fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Try initializing the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    ret=(intn)GRfileinfo(grid,&n_datasets,&n_attrs);
    CHECK(ret,FAIL,"GRfileinfo");

    if(n_datasets!=0 || n_attrs!=0)
      {
          MESSAGE(3, printf("Error! Number of datasets/attributes in new file incorrect\n"););
          num_errs++;
      } /* end if */

    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");


    MESSAGE(8, printf("Try checking out an existing file\n"););

    /* Ok, now check an existing file */

    fid=Hopen(datafile,DFACC_READ,0);
    CHECK(fid,FAIL,"Hopen");

    /* Try initializing the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    ret=(intn)GRfileinfo(grid,&n_datasets,&n_attrs);
    CHECK(ret,FAIL,"GRfileinfo");

    if(n_datasets!=5 || n_attrs!=2)
      {
          MESSAGE(3, printf("Error! Number of datasets/attributes in existing file incorrect\n"););
          num_errs++;
      } /* end if */

    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
}   /* end test_mgr_init() */

/* Sub-tests for test_mgr_image() */
static void test_mgr_image_b1a(int flag)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */
    int32   cdims[2] = {1,1};    /* chunk dims */
    int32   *rcdims;             /* for SDgetchunkinfo() */
    HDF_CHUNK_DEF chunk_def;     /* Chunk defintion set */ 
    HDF_CHUNK_DEF rchunk_def;    /* Chunk defintion read */ 
    int32     cflags;            /* chunk flags */

/* B1a - Read/Write images - with no Data - Default Fill Value */
    MESSAGE(8, printf("Check out I/O on image with no data, using the default fill value\n"););

    /* Open up the existing datafile and get the image information from it */
    if (flag)
        fid=Hopen(TESTFILE2,DFACC_RDWR,0);
    else
        fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={4,5};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        float32 image[5][4][3]; /* space for the image data */
        float32 image0[5][4][3]; /* space for the image data */
        int32 start[2]; /* start of image data to grab */
        int32 stride[2];/* stride of image data to grab */

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Empty Image",3,DFNT_FLOAT32,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Check if creating chunked GR */
        if (flag)
          {
              /* Create chunked GR 
                 chunk is 2x2 which will create 6 chunks */
              cdims[0] = chunk_def.chunk_lengths[0] = 2;
              cdims[1] = chunk_def.chunk_lengths[1] = 2;
              ret = GRsetchunk(riid, chunk_def, HDF_CHUNK);
              CHECK(ret,FAIL,"GRsetchunk");

              /* Set Chunk cache to hold 3 chunks */
              ret = GRsetchunkcache(riid, 3, 0);
              CHECK(ret,FAIL,"GRsetchunkcache");
          }

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        HDmemset(image,255,(size_t)(dims[0]*dims[1]*3)*sizeof(float32));
        /* '0' is the default fill value */
        HDmemset(image0,0,(size_t)(dims[0]*dims[1]*3)*sizeof(float32));

        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,image0,sizeof(image0)))
          {
              MESSAGE(3, printf("Error reading data for image with default fill value\n"););
              num_errs++;
          } /* end if */

        /* check if we are doing chunked tests */
        if (flag)
          {
              /* Get chunk lengths */
              ret = GRgetchunkinfo(riid, &rchunk_def, &cflags);
              CHECK(ret,FAIL,"GRgetchunkinfo");

              rcdims = rchunk_def.chunk_lengths;

              /* check chunk lengths and to see if GR is chunked */
              if (cdims[0] != rcdims[0] || cdims[1] != rcdims[1] || cflags != HDF_CHUNK)
                {
                    fprintf(stderr, "Chunk Test. GRgetchunkinfo returned wrong values\n");
                    fprintf(stderr, "cdims[0]=%d,cdims[1]=%d \n",(int)cdims[0],(int)cdims[1]);
                    fprintf(stderr, "rcdims[0]=%d,rcdims[1]=%d \n",(int)rcdims[0],(int)rcdims[1]);
                    fprintf(stderr, "cflags =%d \n", (int)cflags );
                }

          }

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
} /* end test_mgr_image_b1a() */

static void test_mgr_image_b1b(int flag)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */
    int32   cdims[2] = {1,1};    /* chunk dims */
    int32   *rcdims;             /* for SDgetchunkinfo() */
    HDF_CHUNK_DEF chunk_def;     /* Chunk defintion set */ 
    HDF_CHUNK_DEF rchunk_def;    /* Chunk defintion read */ 
    int32     cflags;            /* chunk flags */

/* B1b - Read/Write images - with no Data - User-defined Fill Value */
    MESSAGE(8, printf("Check out I/O on image with no data, using User Defined fill-value\n"););

    /* Open up the existing datafile and get the image information from it */
    if (flag)
        fid=Hopen(TESTFILE2,DFACC_RDWR,0);
    else
        fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={5,7};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        float64 image[7][5][4]; /* space for the image data */
        float64 fill_pixel[4]={1.3,-2.4,1000.3,.25};   /* pixel with fill values */
        float64 image0[7][5][4]; /* space for the image data */
        int32 start[2]; /* start of image data to grab */
        int32 stride[2];/* stride of image data to grab */

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Empty Image2",4,DFNT_FLOAT64,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Set the fill-value */
        ret=GRsetattr(riid,FILL_ATTR,DFNT_FLOAT64,sizeof(fill_pixel)/sizeof(float64),fill_pixel);
        CHECK(ret,FAIL,"GRsetattr");

        /* Check if creating chunked GR */
        if (flag)
          {
              /* Create chunked GR 
                 chunk is 2x2 which will create 6 chunks */
              cdims[0] = chunk_def.chunk_lengths[0] = 2;
              cdims[1] = chunk_def.chunk_lengths[1] = 2;
              ret = GRsetchunk(riid, chunk_def, HDF_CHUNK);
              CHECK(ret,FAIL,"GRsetchunk");
          }

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        HDmemset(image,0,(size_t)(dims[0]*dims[1]*4)*sizeof(float64));
        /* fill the memory-only with the default pixel fill-value */
        HDmemfill(image0,fill_pixel,sizeof(fill_pixel),sizeof(image0)/sizeof(fill_pixel));

        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,image0,sizeof(image0)))
          {
              MESSAGE(3, printf("Error reading data for image with user defined fill-value\n"););
              num_errs++;
          } /* end if */

        /* check if we are doing chunked tests */
        if (flag)
          {
              /* Get chunk lengths */
              ret = GRgetchunkinfo(riid, &rchunk_def, &cflags);
              CHECK(ret,FAIL,"GRgetchunkinfo");

              rcdims = rchunk_def.chunk_lengths;

              /* check chunk lengths and to see if GR is chunked */
              if (cdims[0] != rcdims[0] || cdims[1] != rcdims[1] || cflags != HDF_CHUNK)
                {
                    fprintf(stderr, "Chunk Test. GRgetchunkinfo returned wrong values\n");
                    fprintf(stderr, "cdims[0]=%d,cdims[1]=%d \n",(int)cdims[0],(int)cdims[1]);
                    fprintf(stderr, "rcdims[0]=%d,rcdims[1]=%d \n",(int)rcdims[0],(int)rcdims[1]);
                    fprintf(stderr, "cflags =%d \n", (int)cflags );
                }

          }

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
} /* end test_mgr_image_b1b() */

static void test_mgr_image_b2a1aa(int flag)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */
    int32   cdims[2] = {1,1};    /* chunk dims */
    int32   *rcdims;             /* for SDgetchunkinfo() */
    HDF_CHUNK_DEF chunk_def;     /* Chunk defintion set */ 
    HDF_CHUNK_DEF rchunk_def;    /* Chunk defintion read */ 
    int32     cflags;            /* chunk flags */

/* B2a1aa - Read/Write images - with real Data - New Image - with Default Fill Value - Whole Image */
    MESSAGE(8, printf("Check out I/O on new image with real data, with Default fill-value, Whole Image\n"););

    /* Open up the existing datafile and get the image information from it */
    if (flag)
        fid=Hopen(TESTFILE2,DFACC_RDWR,0);
    else
        fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
#ifdef TEST_XDIM
#undef TEST_XDIM
#endif /* TEST_XDIM */
#define TEST_XDIM    3
#ifdef TEST_YDIM
#undef TEST_YDIM
#endif /* TEST_YDIM */
#define TEST_YDIM    8
#ifdef TEST_NCOMP
#undef TEST_NCOMP
#endif /* TEST_NCOMP */
#define TEST_NCOMP   2
#ifdef TEST_VARTYPE
#undef TEST_VARTYPE
#endif /* TEST_VARTYPE */
#define TEST_VARTYPE int32
#ifdef TEST_NT
#undef TEST_NT
#endif /* TEST_NT */
#define TEST_NT      DFNT_INT32

        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={TEST_XDIM,TEST_YDIM};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        TEST_VARTYPE image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE fill_pixel[TEST_NCOMP]={1,-2};   /* pixel with fill values */
        TEST_VARTYPE fill_pixel2[TEST_NCOMP]={-2,1};   /* pixel with fill values */
        TEST_VARTYPE image0[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        int32 start[2]; /* start of image data to grab */
        int32 stride[2];/* stride of image data to grab */
        intn i,j;       /* local counting variables */

        /* fill the memory-only with the default pixel fill-value */
        for(i=0; i<TEST_YDIM; i++)
          {
              for(j=0; j<TEST_XDIM; j++)
                {
                    if((j%2)==0)
                        HDmemcpy(&image0[i][j][0],fill_pixel,sizeof(fill_pixel));
                    else
                        HDmemcpy(&image0[i][j][0],fill_pixel2,sizeof(fill_pixel2));
                } /* end for */
          } /* end for */
        HDmemcpy(image,image0,sizeof(image0));

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Image B2a1aa",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Check if creating chunked GR */
        if (flag)
          {
              /* Create chunked GR 
                 chunk is 2x2 which will create 6 chunks */
              cdims[0] = chunk_def.chunk_lengths[0] = 2;
              cdims[1] = chunk_def.chunk_lengths[1] = 2;
              ret = GRsetchunk(riid, chunk_def, HDF_CHUNK);
              CHECK(ret,FAIL,"GRsetchunk");
          }

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRwriteimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,image0,sizeof(image0)))
          {
              MESSAGE(3, printf("%d:Error reading data for new image with default fill-value, whole image\n",__LINE__););
              num_errs++;
          } /* end if */

        /* check if we are doing chunked tests */
        if (flag)
          {
              /* Get chunk lengths */
              ret = GRgetchunkinfo(riid, &rchunk_def, &cflags);
              CHECK(ret,FAIL,"GRgetchunkinfo");

              rcdims = rchunk_def.chunk_lengths;

              /* check chunk lengths and to see if GR is chunked */
              if (cdims[0] != rcdims[0] || cdims[1] != rcdims[1] || cflags != HDF_CHUNK)
                {
                    fprintf(stderr, "Chunk Test. GRgetchunkinfo returned wrong values\n");
                    fprintf(stderr, "cdims[0]=%d,cdims[1]=%d \n",(int)cdims[0],(int)cdims[1]);
                    fprintf(stderr, "rcdims[0]=%d,rcdims[1]=%d \n",(int)rcdims[0],(int)rcdims[1]);
                    fprintf(stderr, "cflags =%d \n", (int)cflags );
                }

          }

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
} /* end test_mgr_image_b2a1aa() */

static void test_mgr_image_b2a1bb1(int flag)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */
    int32   cdims[2] = {1,1};    /* chunk dims */
    int32   *rcdims;             /* for SDgetchunkinfo() */
    HDF_CHUNK_DEF chunk_def;     /* Chunk defintion set */ 
    HDF_CHUNK_DEF rchunk_def;    /* Chunk defintion read */ 
    int32     cflags;            /* chunk flags */

/* B2a1bb - Read/Write images - with real Data - New Image - with Default Fill Value - Sub-setted Image */
    MESSAGE(8, printf("Check out I/O on new image with real data, with Default fill-value, Writing Sub-setted Image\n"););

    /* Open up the existing datafile and get the image information from it */
    if (flag)
        fid=Hopen(TESTFILE2,DFACC_RDWR,0);
    else
        fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
#ifdef TEST_XDIM
#undef TEST_XDIM
#endif /* TEST_XDIM */
#define TEST_XDIM    19
#ifdef TEST_YDIM
#undef TEST_YDIM
#endif /* TEST_YDIM */
#define TEST_YDIM    23
#ifdef TEST_NCOMP
#undef TEST_NCOMP
#endif /* TEST_NCOMP */
#define TEST_NCOMP   4
#ifdef TEST_VARTYPE
#undef TEST_VARTYPE
#endif /* TEST_VARTYPE */
#define TEST_VARTYPE uint16
#ifdef TEST_NT
#undef TEST_NT
#endif /* TEST_NT */
#define TEST_NT      DFNT_UINT16

        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={TEST_XDIM,TEST_YDIM};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        TEST_VARTYPE image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE fill_pixel[TEST_NCOMP]={40000,4800,3,1000};   /* pixel with fill values */
        TEST_VARTYPE fill_pixel2[TEST_NCOMP]={1230,1,65000,35000};   /* pixel with fill values */
        TEST_VARTYPE image0[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE sub_image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE *sub_ptr;
        int32 start[2]; /* start of image data to use */
        int32 stride[2];/* stride of image data to use */
        int32 count[2]; /* # of pixels of image data to use */
        intn i,j,k;     /* local counting variables */

        /* fill the memory-only with the default pixel fill-values */
        HDmemset(image0,0,sizeof(image0));
        sub_ptr=(TEST_VARTYPE *)sub_image;
        for(i=0; i<TEST_YDIM; i++)
          {
              for(j=0; j<TEST_XDIM; j++)
                {
                    if(((i>(TEST_YDIM/3)) && (i<(2*TEST_YDIM/3)))
                        && ((j>(TEST_XDIM/4)) && (j<(3*TEST_XDIM/4))))
                      {
                          if((j%2)==0)
                              HDmemcpy(&image0[i][j][0],fill_pixel,sizeof(fill_pixel));
                          else
                              HDmemcpy(&image0[i][j][0],fill_pixel2,sizeof(fill_pixel2));
                          HDmemcpy(sub_ptr,&image0[i][j][0],TEST_NCOMP*sizeof(TEST_VARTYPE));
                          sub_ptr+=TEST_NCOMP;
                      } /* end if */
                } /* end for */
          } /* end for */

        /* initialize the disk buffer */
        HDmemset(image,255,sizeof(image));

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Image B2a1bb",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Check if creating chunked GR */
        if (flag)
          {
              /* Create chunked GR 
                 chunk is 2x2 which will create 6 chunks */
              cdims[0] = chunk_def.chunk_lengths[0] = 2;
              cdims[1] = chunk_def.chunk_lengths[1] = 2;
              ret = GRsetchunk(riid, chunk_def, HDF_CHUNK);
              CHECK(ret,FAIL,"GRsetchunk");
          }

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        /* Create sub-setted window with only the filled pixels in it */
        start[XDIM]=(TEST_XDIM/4)+1;
        start[YDIM]=(TEST_YDIM/3)+1;
        count[XDIM]=((3*TEST_XDIM/4)-(TEST_XDIM/4))-1;
        count[YDIM]=((2*TEST_YDIM/3)-(TEST_YDIM/3))-1;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRwriteimage(riid,start,stride,count,sub_image);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        /* Get the whole image back */
        start[XDIM]=start[YDIM]=0;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,image0,sizeof(image0)))
          {
              MESSAGE(3, printf("%d:Error reading data for new image with default fill-value, sub-setted image\n",__LINE__););

              MESSAGE(8, for(i=0; i<TEST_YDIM; i++) \
                      for(j=0; j<TEST_XDIM; j++) \
                          for(k=0; k<TEST_NCOMP; k++) \
                              if(image[i][j][k]!=image0[i][j][k]) \
                                  printf("Location: [%d][%d][%d] image=%u, image0=%u \n",i,j,k,(unsigned)image[i][j][k],(unsigned)image0[i][j][k]); );
              num_errs++;
          } /* end if */

        /* check if we are doing chunked tests */
        if (flag)
          {
              /* Get chunk lengths */
              ret = GRgetchunkinfo(riid, &rchunk_def, &cflags);
              CHECK(ret,FAIL,"GRgetchunkinfo");

              rcdims = rchunk_def.chunk_lengths;

              /* check chunk lengths and to see if GR is chunked */
              if (cdims[0] != rcdims[0] || cdims[1] != rcdims[1] || cflags != HDF_CHUNK)
                {
                    fprintf(stderr, "Chunk Test. GRgetchunkinfo returned wrong values\n");
                    fprintf(stderr, "cdims[0]=%d,cdims[1]=%d \n",(int)cdims[0],(int)cdims[1]);
                    fprintf(stderr, "rcdims[0]=%d,rcdims[1]=%d \n",(int)rcdims[0],(int)rcdims[1]);
                    fprintf(stderr, "cflags =%d \n", (int)cflags );
                }

          }

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");

} /* end test_mgr_image_b2a1bb1() */

static void test_mgr_image_b2a1bb2(int flag)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */
    int32   cdims[2] = {1,1};    /* chunk dims */
    int32   *rcdims;             /* for SDgetchunkinfo() */
    HDF_CHUNK_DEF chunk_def;     /* Chunk defintion set */ 
    HDF_CHUNK_DEF rchunk_def;    /* Chunk defintion read */ 
    int32     cflags;            /* chunk flags */

    MESSAGE(8, printf("Check out I/O on new image with real data, with Default fill-value, Reading Sub-setted Image\n"););
    /* Open up the existing datafile and get the image information from it */
    if (flag)
        fid=Hopen(TESTFILE2,DFACC_RDWR,0);
    else
        fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
#ifdef TEST_XDIM
#undef TEST_XDIM
#endif /* TEST_XDIM */
#define TEST_XDIM    19
#ifdef TEST_YDIM
#undef TEST_YDIM
#endif /* TEST_YDIM */
#define TEST_YDIM    23
#ifdef TEST_NCOMP
#undef TEST_NCOMP
#endif /* TEST_NCOMP */
#define TEST_NCOMP   4
#ifdef TEST_VARTYPE
#undef TEST_VARTYPE
#endif /* TEST_VARTYPE */
#define TEST_VARTYPE uint16
#ifdef TEST_NT
#undef TEST_NT
#endif /* TEST_NT */
#define TEST_NT      DFNT_UINT16

        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={TEST_XDIM,TEST_YDIM};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        TEST_VARTYPE image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE fill_pixel[TEST_NCOMP]={40000,4800,3,1000};   /* pixel with fill values */
        TEST_VARTYPE fill_pixel2[TEST_NCOMP]={1230,1,65000,35000};   /* pixel with fill values */
        TEST_VARTYPE image0[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE sub_image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE *sub_ptr;
        int32 start[2]; /* start of image data to use */
        int32 stride[2];/* stride of image data to use */
        int32 count[2]; /* # of pixels of image data to use */
        intn i,j;       /* local counting variables */

        /* fill the memory-only with the default pixel fill-values */
        HDmemset(image0,0,sizeof(image0));
        sub_ptr=(TEST_VARTYPE *)sub_image;
        for(i=0; i<TEST_YDIM; i++)
          {
              for(j=0; j<TEST_XDIM; j++)
                {
                    if((j%2)==0)
                        HDmemcpy(&image0[i][j][0],fill_pixel,sizeof(fill_pixel));
                    else
                        HDmemcpy(&image0[i][j][0],fill_pixel2,sizeof(fill_pixel2));
                    if(((i>(TEST_YDIM/3)) && (i<(2*TEST_YDIM/3)))
                        && ((j>(TEST_XDIM/4)) && (j<(3*TEST_XDIM/4))))
                      {
                          HDmemcpy(sub_ptr,&image0[i][j][0],TEST_NCOMP*sizeof(TEST_VARTYPE));
                          sub_ptr+=TEST_NCOMP;
                      } /* end if */
                } /* end for */
          } /* end for */

        /* initialize the disk buffer */
        HDmemset(image,255,sizeof(image));

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Image B2a1bb2",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Check if creating chunked GR */
        if (flag)
          {
              /* Create chunked GR 
                 chunk is 2x2 which will create 6 chunks */
              cdims[0] = chunk_def.chunk_lengths[0] = 2;
              cdims[1] = chunk_def.chunk_lengths[1] = 2;
              ret = GRsetchunk(riid, chunk_def, HDF_CHUNK);
              CHECK(ret,FAIL,"GRsetchunk");

              /* Set Chunk cache to hold 3 chunks */
              ret = GRsetchunkcache(riid, 3, 0);
              CHECK(ret,FAIL,"GRsetchunkcache");
          }

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        /* Create whole image */
        start[XDIM]=start[YDIM]=0;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRwriteimage(riid,start,stride,dims,image0);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        /* Get the sub-set image back */
        start[XDIM]=(TEST_XDIM/4)+1;
        start[YDIM]=(TEST_YDIM/3)+1;
        count[XDIM]=((3*TEST_XDIM/4)-(TEST_XDIM/4))-1;
        count[YDIM]=((2*TEST_YDIM/3)-(TEST_YDIM/3))-1;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRreadimage(riid,start,stride,count,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,sub_image,(size_t)(count[XDIM]*count[YDIM])*sizeof(fill_pixel)))
          {
              MESSAGE(3, printf("%d:Error reading data for new image with default fill-value, sub-setted image\n",__LINE__););
              num_errs++;
          } /* end if */

        /* check if we are doing chunked tests */
        if (flag)
          {
              /* Get chunk lengths */
              ret = GRgetchunkinfo(riid, &rchunk_def, &cflags);
              CHECK(ret,FAIL,"GRgetchunkinfo");

              rcdims = rchunk_def.chunk_lengths;

              /* check chunk lengths and to see if GR is chunked */
              if (cdims[0] != rcdims[0] || cdims[1] != rcdims[1] || cflags != HDF_CHUNK)
                {
                    fprintf(stderr, "Chunk Test. GRgetchunkinfo returned wrong values\n");
                    fprintf(stderr, "cdims[0]=%d,cdims[1]=%d \n",(int)cdims[0],(int)cdims[1]);
                    fprintf(stderr, "rcdims[0]=%d,rcdims[1]=%d \n",(int)rcdims[0],(int)rcdims[1]);
                    fprintf(stderr, "cflags =%d \n", (int)cflags );
                }

          }

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");

} /* end test_mgr_image_b2a1bb2() */

static void test_mgr_image_b2a1cc1(int flag)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */
    int32   cdims[2] = {1,1};    /* chunk dims */
    int32   *rcdims;             /* for SDgetchunkinfo() */
    HDF_CHUNK_DEF chunk_def;     /* Chunk defintion set */ 
    HDF_CHUNK_DEF rchunk_def;    /* Chunk defintion read */ 
    int32     cflags;            /* chunk flags */

/* B2a1cc - Read/Write images - with real Data - New Image - with Default Fill Value - Sub-sampled Image */
    MESSAGE(8, printf("Check out I/O on new image with real data, with Default fill-value, Writing Sub-sampled Image\n"););

    /* Open up the existing datafile and get the image information from it */
    if (flag)
        fid=Hopen(TESTFILE2,DFACC_RDWR,0);
    else
        fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
#ifdef TEST_XDIM
#undef TEST_XDIM
#endif /* TEST_XDIM */
#define TEST_XDIM    19
#ifdef TEST_YDIM
#undef TEST_YDIM
#endif /* TEST_YDIM */
#define TEST_YDIM    23
#ifdef TEST_NCOMP
#undef TEST_NCOMP
#endif /* TEST_NCOMP */
#define TEST_NCOMP   5
#ifdef TEST_VARTYPE
#undef TEST_VARTYPE
#endif /* TEST_VARTYPE */
#define TEST_VARTYPE int16
#ifdef TEST_NT
#undef TEST_NT
#endif /* TEST_NT */
#define TEST_NT      DFNT_INT16

        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={TEST_XDIM,TEST_YDIM};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        TEST_VARTYPE image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE fill_pixel[TEST_NCOMP]={-20000,-1,4800,3,1000};   /* pixel with fill values */
        TEST_VARTYPE fill_pixel2[TEST_NCOMP]={45,1230,1,32000,-32000};   /* pixel with fill values */
        TEST_VARTYPE image0[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE sub_image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE *sub_ptr;
        int32 start[2]; /* start of image data to use */
        int32 stride[2];/* stride of image data to use */
        int32 count[2]; /* # of pixels of image data to use */
        intn i,j,k;     /* local counting variables */

        /* fill the memory-only with the default pixel fill-values */
        HDmemset(image0,0,sizeof(TEST_VARTYPE)*(size_t)(TEST_YDIM*TEST_XDIM*TEST_NCOMP));
        sub_ptr=(TEST_VARTYPE *)sub_image;
        for(i=0; i<TEST_YDIM; i++)
          {
              for(j=0; j<TEST_XDIM; j++)
                {
                    if((i%2)!=0 && (j%2)!=0)
                      {
                          if((j%3)==0)
                              HDmemcpy(&image0[i][j][0],fill_pixel,sizeof(TEST_VARTYPE)*TEST_NCOMP);
                          else
                              HDmemcpy(&image0[i][j][0],fill_pixel2,sizeof(TEST_VARTYPE)*TEST_NCOMP);
                          HDmemcpy(sub_ptr,&image0[i][j][0],TEST_NCOMP*sizeof(TEST_VARTYPE));
                          sub_ptr+=TEST_NCOMP;
                      } /* end if */
                } /* end for */
          } /* end for */

        /* initialize the disk buffer */
        HDmemset(image,255,sizeof(TEST_VARTYPE)*(size_t)(TEST_YDIM*TEST_XDIM*TEST_NCOMP));

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Image B2a1cc",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Check if creating chunked GR */
        if (flag)
          {
              /* Create chunked GR 
                 chunk is 2x2 which will create 6 chunks */
              cdims[0] = chunk_def.chunk_lengths[0] = 2;
              cdims[1] = chunk_def.chunk_lengths[1] = 2;
              ret = GRsetchunk(riid, chunk_def, HDF_CHUNK);
              CHECK(ret,FAIL,"GRsetchunk");
          }

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        /* Create sub-sampled window with only the filled pixels in it */
        start[XDIM]=1;
        start[YDIM]=1;
        count[XDIM]=TEST_XDIM/2;
        count[YDIM]=TEST_YDIM/2;
        stride[XDIM]=stride[YDIM]=2;
        ret=GRwriteimage(riid,start,stride,count,sub_image);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        /* Get the whole image back */
        start[XDIM]=start[YDIM]=0;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,image0,sizeof(TEST_VARTYPE)*TEST_YDIM*TEST_XDIM*TEST_NCOMP))
          {
              MESSAGE(3, printf("%d:Error reading data for new image with default fill-value, sub-sampled image\n",__LINE__););

              MESSAGE(8, for(i=0; i<TEST_YDIM; i++) \
                      for(j=0; j<TEST_XDIM; j++) \
                          for(k=0; k<TEST_NCOMP; k++) \
                            if(image[i][j][k]!=image0[i][j][k]) \
                                  printf("Location: [%d][%d][%d] image=%d, image0=%d \n",(int)i,(int)j,(int)k,(int)image[i][j][k],(int)image0[i][j][k]); );
              num_errs++;
          } /* end if */

        /* check if we are doing chunked tests */
        if (flag)
          {
              /* Get chunk lengths */
              ret = GRgetchunkinfo(riid, &rchunk_def, &cflags);
              CHECK(ret,FAIL,"GRgetchunkinfo");

              rcdims = rchunk_def.chunk_lengths;

              /* check chunk lengths and to see if GR is chunked */
              if (cdims[0] != rcdims[0] || cdims[1] != rcdims[1] || cflags != HDF_CHUNK)
                {
                    fprintf(stderr, "Chunk Test. GRgetchunkinfo returned wrong values\n");
                    fprintf(stderr, "cdims[0]=%d,cdims[1]=%d \n",(int)cdims[0],(int)cdims[1]);
                    fprintf(stderr, "rcdims[0]=%d,rcdims[1]=%d \n",(int)rcdims[0],(int)rcdims[1]);
                    fprintf(stderr, "cflags =%d \n", (int)cflags );
                }

          }

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");

}

static void test_mgr_image_b2a1cc2(int flag)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */
    int32   cdims[2] = {1,1};    /* chunk dims */
    int32   *rcdims;             /* for SDgetchunkinfo() */
    HDF_CHUNK_DEF chunk_def;     /* Chunk defintion set */ 
    HDF_CHUNK_DEF rchunk_def;    /* Chunk defintion read */ 
    int32     cflags;            /* chunk flags */

    MESSAGE(8, printf("Check out I/O on new image with real data, with Default fill-value, Reading Sub-sampled Image\n"););
    /* Open up the existing datafile and get the image information from it */
    if (flag)
        fid=Hopen(TESTFILE2,DFACC_RDWR,0);
    else
        fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
#ifdef TEST_XDIM
#undef TEST_XDIM
#endif /* TEST_XDIM */
#define TEST_XDIM    19
#ifdef TEST_YDIM
#undef TEST_YDIM
#endif /* TEST_YDIM */
#define TEST_YDIM    23
#ifdef TEST_NCOMP
#undef TEST_NCOMP
#endif /* TEST_NCOMP */
#define TEST_NCOMP   4
#ifdef TEST_VARTYPE
#undef TEST_VARTYPE
#endif /* TEST_VARTYPE */
#define TEST_VARTYPE uint32
#ifdef TEST_NT
#undef TEST_NT
#endif /* TEST_NT */
#define TEST_NT      DFNT_UINT32

        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={TEST_XDIM,TEST_YDIM};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        TEST_VARTYPE image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE fill_pixel[TEST_NCOMP]={4000000,4800,3,1000};   /* pixel with fill values */
        TEST_VARTYPE fill_pixel2[TEST_NCOMP]={1230,1,65000,350000};   /* pixel with fill values */
        TEST_VARTYPE image0[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE sub_image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE *sub_ptr;
        int32 start[2]; /* start of image data to use */
        int32 stride[2];/* stride of image data to use */
        int32 count[2]; /* # of pixels of image data to use */
        intn i,j;       /* local counting variables */

        /* fill the memory-only with the default pixel fill-values */
        HDmemset(image0,0,sizeof(image0));
        sub_ptr=(TEST_VARTYPE *)sub_image;
        for(i=0; i<TEST_YDIM; i++)
          {
              for(j=0; j<TEST_XDIM; j++)
                {
                    if((j%2)==0)
                        HDmemcpy(&image0[i][j][0],fill_pixel,sizeof(fill_pixel));
                    else
                        HDmemcpy(&image0[i][j][0],fill_pixel2,sizeof(fill_pixel2));
                    if((i%2) && (j%2))
                      {
                          HDmemcpy(sub_ptr,&image0[i][j][0],TEST_NCOMP*sizeof(TEST_VARTYPE));
                          sub_ptr+=TEST_NCOMP;
                      } /* end if */
                } /* end for */
          } /* end for */

        /* initialize the disk buffer */
        HDmemset(image,255,sizeof(image));

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Image B2a1cc2",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Check if creating chunked GR */
        if (flag)
          {
              /* Create chunked GR 
                 chunk is 2x2 which will create 6 chunks */
              cdims[0] = chunk_def.chunk_lengths[0] = 2;
              cdims[1] = chunk_def.chunk_lengths[1] = 2;
              ret = GRsetchunk(riid, chunk_def, HDF_CHUNK);
              CHECK(ret,FAIL,"GRsetchunk");
          }

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        /* Create whole image */
        start[XDIM]=start[YDIM]=0;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRwriteimage(riid,start,stride,dims,image0);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        /* Get the sub-sample image back */
        start[XDIM]=1;
        start[YDIM]=1;
        count[XDIM]=TEST_XDIM/2;
        count[YDIM]=TEST_YDIM/2;
        stride[XDIM]=stride[YDIM]=2;
        ret=GRreadimage(riid,start,stride,count,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,sub_image,(size_t)(count[XDIM]*count[YDIM])*sizeof(fill_pixel)))
          {
              MESSAGE(3, printf("%d:Error reading data for new image with default fill-value, sub-sampled image\n",__LINE__););
              num_errs++;
          } /* end if */

        /* check if we are doing chunked tests */
        if (flag)
          {
              /* Get chunk lengths */
              ret = GRgetchunkinfo(riid, &rchunk_def, &cflags);
              CHECK(ret,FAIL,"GRgetchunkinfo");

              rcdims = rchunk_def.chunk_lengths;

              /* check chunk lengths and to see if GR is chunked */
              if (cdims[0] != rcdims[0] || cdims[1] != rcdims[1] || cflags != HDF_CHUNK)
                {
                    fprintf(stderr, "Chunk Test. GRgetchunkinfo returned wrong values\n");
                    fprintf(stderr, "cdims[0]=%d,cdims[1]=%d \n",(int)cdims[0],(int)cdims[1]);
                    fprintf(stderr, "rcdims[0]=%d,rcdims[1]=%d \n",(int)rcdims[0],(int)rcdims[1]);
                    fprintf(stderr, "cflags =%d \n", (int)cflags );
                }

          }

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");

} /* end test_mgr_image_b2a1cc() */

static void test_mgr_image_b2a2aa(int flag)
{
#ifdef QAK
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */

/* B2a2aa - Read/Write images - with real Data - New Image - with User-Defined Fill Value - Whole Image */
/* The following test is unnecessary, fill-values only are important when writing out partial images */
    MESSAGE(8, printf("Check out I/O on new image with real data, with User-Defined fill-value, Whole Image\n"););

    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
#ifdef TEST_XDIM
#undef TEST_XDIM
#endif /* TEST_XDIM */
#define TEST_XDIM    3
#ifdef TEST_YDIM
#undef TEST_YDIM
#endif /* TEST_YDIM */
#define TEST_YDIM    8
#ifdef TEST_NCOMP
#undef TEST_NCOMP
#endif /* TEST_NCOMP */
#define TEST_NCOMP   2
#ifdef TEST_VARTYPE
#undef TEST_VARTYPE
#endif /* TEST_VARTYPE */
#define TEST_VARTYPE int32
#ifdef TEST_NT
#undef TEST_NT
#endif /* TEST_NT */
#define TEST_NT      DFNT_INT32

        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={TEST_XDIM,TEST_YDIM};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        TEST_VARTYPE image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE fill_pixel[TEST_NCOMP]={1,-2};   /* pixel with fill values */
        TEST_VARTYPE fill_pixel2[TEST_NCOMP]={-2,1};   /* pixel with fill values */
        TEST_VARTYPE image0[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        int32 start[2]; /* start of image data to grab */
        int32 stride[2];/* stride of image data to grab */
        intn i,j,k;     /* local counting variables */

        /* fill the memory-only with the default pixel fill-value */
        for(i=0; i<TEST_YDIM; i++)
          {
              for(j=0; j<TEST_XDIM; j++)
                {
                    if((j%2)==0)
                        HDmemcpy(&image0[i][j][0],fill_pixel,sizeof(fill_pixel));
                    else
                        HDmemcpy(&image0[i][j][0],fill_pixel2,sizeof(fill_pixel2));
                } /* end for */
          } /* end for */
        HDmemcpy(image,image0,sizeof(image0));

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Image B2a1aa",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRwriteimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,image0,sizeof(image0)))
          {
              MESSAGE(3, printf("%d:Error reading data for new image with default fill-value, whole image\n",__LINE__););
              num_errs++;
          } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
#endif /* QAK */

} /* end test_mgr_image_b2a2aa() */

static void test_mgr_image_b2a2bb(int flag)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */
    int32   cdims[2] = {1,1};    /* chunk dims */
    int32   *rcdims;             /* for SDgetchunkinfo() */
    HDF_CHUNK_DEF chunk_def;     /* Chunk defintion set */ 
    HDF_CHUNK_DEF rchunk_def;    /* Chunk defintion read */ 
    int32     cflags;            /* chunk flags */

/* B2a2bb - Read/Write images - with real Data - New Image - with User-Defined Fill Value - Sub-setted Image */
    MESSAGE(8, printf("Check out I/O on new image with real data, with User-Defined fill-value, Writing Sub-setted Image\n"););

    /* Open up the existing datafile and get the image information from it */
    if (flag)
        fid=Hopen(TESTFILE2,DFACC_RDWR,0);
    else
        fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
#ifdef TEST_XDIM
#undef TEST_XDIM
#endif /* TEST_XDIM */
#define TEST_XDIM    19
#ifdef TEST_YDIM
#undef TEST_YDIM
#endif /* TEST_YDIM */
#define TEST_YDIM    23
#ifdef TEST_NCOMP
#undef TEST_NCOMP
#endif /* TEST_NCOMP */
#define TEST_NCOMP   4
#ifdef TEST_VARTYPE
#undef TEST_VARTYPE
#endif /* TEST_VARTYPE */
#define TEST_VARTYPE float32
#ifdef TEST_NT
#undef TEST_NT
#endif /* TEST_NT */
#define TEST_NT      DFNT_FLOAT32

        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={TEST_XDIM,TEST_YDIM};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        TEST_VARTYPE image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE fill_pixel[TEST_NCOMP]={(TEST_VARTYPE)-3.75,(TEST_VARTYPE)4.5,(TEST_VARTYPE)-0.375,(TEST_VARTYPE)100.125};   /* pixel with fill values */
        TEST_VARTYPE pixel[TEST_NCOMP]={(TEST_VARTYPE)-20.00,(TEST_VARTYPE)4.875,(TEST_VARTYPE)0.125,(TEST_VARTYPE)1.0};   /* pixel with fill values */
        TEST_VARTYPE pixel2[TEST_NCOMP]={(TEST_VARTYPE)1.25,(TEST_VARTYPE)1.0,(TEST_VARTYPE)-6500.0,(TEST_VARTYPE)350.0};   /* pixel with fill values */
        TEST_VARTYPE image0[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE sub_image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE *sub_ptr;
        int32 start[2]; /* start of image data to use */
        int32 stride[2];/* stride of image data to use */
        int32 count[2]; /* # of pixels of image data to use */
        intn i,j,k;     /* local counting variables */

        /* fill the memory-only with the default pixel fill-values */
        HDmemfill(image0,fill_pixel,sizeof(fill_pixel),sizeof(image0)/sizeof(fill_pixel));
        sub_ptr=(TEST_VARTYPE *)sub_image;
        for(i=0; i<TEST_YDIM; i++)
          {
              for(j=0; j<TEST_XDIM; j++)
                {
                    if(((i>(TEST_YDIM/3)) && (i<(2*TEST_YDIM/3)))
                        && ((j>(TEST_XDIM/4)) && (j<(3*TEST_XDIM/4))))
                      {
                          if((j%2)==0)
                              HDmemcpy(&image0[i][j][0],pixel,sizeof(pixel));
                          else
                              HDmemcpy(&image0[i][j][0],pixel2,sizeof(pixel2));
                          HDmemcpy(sub_ptr,&image0[i][j][0],TEST_NCOMP*sizeof(TEST_VARTYPE));
                          sub_ptr+=TEST_NCOMP;
                      } /* end if */
                } /* end for */
          } /* end for */

        /* initialize the disk buffer */
        HDmemset(image,255,sizeof(image));

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Image B2a2bb",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Set the fill-value */
        ret=GRsetattr(riid,FILL_ATTR,TEST_NT,TEST_NCOMP,fill_pixel);
        CHECK(ret,FAIL,"GRsetattr");

        /* Check if creating chunked GR */
        if (flag)
          {
              /* Create chunked GR 
                 chunk is 2x2 which will create 6 chunks */
              cdims[0] = chunk_def.chunk_lengths[0] = 2;
              cdims[1] = chunk_def.chunk_lengths[1] = 2;
              ret = GRsetchunk(riid, chunk_def, HDF_CHUNK);
              CHECK(ret,FAIL,"GRsetchunk");
          }

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");


        /* Create sub-setted window with only the filled pixels in it */
        start[XDIM]=(TEST_XDIM/4)+1;
        start[YDIM]=(TEST_YDIM/3)+1;
        count[XDIM]=((3*TEST_XDIM/4)-(TEST_XDIM/4))-1;
        count[YDIM]=((2*TEST_YDIM/3)-(TEST_YDIM/3))-1;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRwriteimage(riid,start,stride,count,sub_image);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        /* Get the whole image back */
        start[XDIM]=start[YDIM]=0;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,image0,sizeof(image0)))
          {
              MESSAGE(3, printf("Error reading data for new image with user-defined fill-value, sub-setted image\n"););

              MESSAGE(8, for(i=0; i<TEST_YDIM; i++) \
                      for(j=0; j<TEST_XDIM; j++) \
                          for(k=0; k<TEST_NCOMP; k++) \
                              if(image[i][j][k]!=image0[i][j][k]) \
                                  printf("Location: [%d][%d][%d] image=%f, image0=%f \n",i,j,k,(double)image[i][j][k],(double)image0[i][j][k]); );
              num_errs++;
          } /* end if */

        /* check if we are doing chunked tests */
        if (flag)
          {
              /* Get chunk lengths */
              ret = GRgetchunkinfo(riid, &rchunk_def, &cflags);
              CHECK(ret,FAIL,"GRgetchunkinfo");

              rcdims = rchunk_def.chunk_lengths;

              /* check chunk lengths and to see if GR is chunked */
              if (cdims[0] != rcdims[0] || cdims[1] != rcdims[1] || cflags != HDF_CHUNK)
                {
                    fprintf(stderr, "Chunk Test. GRgetchunkinfo returned wrong values\n");
                    fprintf(stderr, "cdims[0]=%d,cdims[1]=%d \n",(int)cdims[0],(int)cdims[1]);
                    fprintf(stderr, "rcdims[0]=%d,rcdims[1]=%d \n",(int)rcdims[0],(int)rcdims[1]);
                    fprintf(stderr, "cflags =%d \n", (int)cflags );
                }

          }

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");

/* The following test is unnecessary, fill-values only make a difference when writing out data -QAK */
#ifdef QAK
    MESSAGE(8, printf("Check out I/O on new image with real data, with User-Defined fill-value, Reading Sub-setted Image\n"););
    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
#ifdef TEST_XDIM
#undef TEST_XDIM
#endif /* TEST_XDIM */
#define TEST_XDIM    19
#ifdef TEST_YDIM
#undef TEST_YDIM
#endif /* TEST_YDIM */
#define TEST_YDIM    23
#ifdef TEST_NCOMP
#undef TEST_NCOMP
#endif /* TEST_NCOMP */
#define TEST_NCOMP   4
#ifdef TEST_VARTYPE
#undef TEST_VARTYPE
#endif /* TEST_VARTYPE */
#define TEST_VARTYPE uint16
#ifdef TEST_NT
#undef TEST_NT
#endif /* TEST_NT */
#define TEST_NT      DFNT_UINT16

        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={TEST_XDIM,TEST_YDIM};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        TEST_VARTYPE image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE fill_pixel[TEST_NCOMP]={-3.4,4.5,-0.03,100.4};   /* pixel with fill values */
        TEST_VARTYPE pixel[TEST_NCOMP]={-20.00,4.8,0.3,1.0};   /* pixel with fill values */
        TEST_VARTYPE pixel2[TEST_NCOMP]={1.23,1.0,-6500.0,350.0};   /* pixel with fill values */
        TEST_VARTYPE image0[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE sub_image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE *sub_ptr;
        int32 start[2]; /* start of image data to use */
        int32 stride[2];/* stride of image data to use */
        int32 count[2]; /* # of pixels of image data to use */
        intn i,j,k;     /* local counting variables */

        /* fill the memory-only with the default pixel fill-values */
        HDmemset(image0,0,sizeof(image0));
        sub_ptr=(TEST_VARTYPE *)sub_image;
        for(i=0; i<TEST_YDIM; i++)
          {
              for(j=0; j<TEST_XDIM; j++)
                {
                    if((j%2)==0)
                        HDmemcpy(&image0[i][j][0],pixel,sizeof(pixel));
                    else
                        HDmemcpy(&image0[i][j][0],pixel2,sizeof(pixel2));
                    if(((i>(TEST_YDIM/3)) && (i<(2*TEST_YDIM/3)))
                        && ((j>(TEST_XDIM/4)) && (j<(3*TEST_XDIM/4))))
                      {
                          HDmemcpy(sub_ptr,&image0[i][j][0],TEST_NCOMP*sizeof(TEST_VARTYPE));
                          sub_ptr+=TEST_NCOMP;
                      } /* end if */
                } /* end for */
          } /* end for */

        /* initialize the disk buffer */
        HDmemset(image,255,sizeof(image));

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Image B2a2bb2",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        /* Set the fill-value */
        ret=GRsetattr(riid,FILL_ATTR,TEST_NT,TEST_NCOMP,fill_pixel);
        CHECK(ret,FAIL,"GRsetattr");

        /* Create whole image */
        start[XDIM]=start[YDIM]=0;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRwriteimage(riid,start,stride,dims,image0);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        /* Get the sub-set image back */
        start[XDIM]=(TEST_XDIM/4)+1;
        start[YDIM]=(TEST_YDIM/3)+1;
        count[XDIM]=((3*TEST_XDIM/4)-(TEST_XDIM/4))-1;
        count[YDIM]=((2*TEST_YDIM/3)-(TEST_YDIM/3))-1;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRreadimage(riid,start,stride,count,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,sub_image,count[XDIM]*count[YDIM]*sizeof(pixel)))
          {
              MESSAGE(3, printf("%d:Error reading data for new image with default fill-value, sub-setted image\n",__LINE__););
              num_errs++;
          } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
#endif /* QAK */

} /* end test_mgr_image_b2a2bb() */

static void test_mgr_image_b2a2cc(int flag)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */
    int32   cdims[2] = {1,1};    /* chunk dims */
    int32   *rcdims;             /* for SDgetchunkinfo() */
    HDF_CHUNK_DEF chunk_def;     /* Chunk defintion set */ 
    HDF_CHUNK_DEF rchunk_def;    /* Chunk defintion read */ 
    int32     cflags;            /* chunk flags */

/* B2a2cc - Read/Write images - with real Data - New Image - with User-Defined Fill Value - Sub-sampled Image */
    MESSAGE(8, printf("Check out I/O on new image with real data, with User-Defined fill-value, Writing Sub-sampled Image\n"););

    /* Open up the existing datafile and get the image information from it */
    if (flag)
        fid=Hopen(TESTFILE2,DFACC_RDWR,0);
    else
        fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
#ifdef TEST_XDIM
#undef TEST_XDIM
#endif /* TEST_XDIM */
#define TEST_XDIM    19
#ifdef TEST_YDIM
#undef TEST_YDIM
#endif /* TEST_YDIM */
#define TEST_YDIM    23
#ifdef TEST_NCOMP
#undef TEST_NCOMP
#endif /* TEST_NCOMP */
#define TEST_NCOMP   5
#ifdef TEST_VARTYPE
#undef TEST_VARTYPE
#endif /* TEST_VARTYPE */
#define TEST_VARTYPE int16
#ifdef TEST_NT
#undef TEST_NT
#endif /* TEST_NT */
#define TEST_NT      DFNT_INT16

        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={TEST_XDIM,TEST_YDIM};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        TEST_VARTYPE image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE fill_pixel[TEST_NCOMP]={-3,4,-13,100,1200};   /* pixel with fill values */
        TEST_VARTYPE pixel[TEST_NCOMP]={-20,4,0,1,-367};   /* pixel with fill values */
        TEST_VARTYPE pixel2[TEST_NCOMP]={1,-11,-6500,350,20};   /* pixel with fill values */
        TEST_VARTYPE image0[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE sub_image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE *sub_ptr;
        int32 start[2]; /* start of image data to use */
        int32 stride[2];/* stride of image data to use */
        int32 count[2]; /* # of pixels of image data to use */
        intn i,j,k;     /* local counting variables */

        /* fill the memory-only with the default pixel fill-values */
        HDmemfill(image0,fill_pixel,sizeof(fill_pixel),sizeof(image0)/sizeof(fill_pixel));
        sub_ptr=(TEST_VARTYPE *)sub_image;
        for(i=0; i<TEST_YDIM; i++)
          {
              for(j=0; j<TEST_XDIM; j++)
                {
                    if((i%2)!=0 && (j%2)!=0)
                      {
                          if((j%3)==0)
                              HDmemcpy(&image0[i][j][0],pixel,sizeof(TEST_VARTYPE)*TEST_NCOMP);
                          else
                              HDmemcpy(&image0[i][j][0],pixel2,sizeof(TEST_VARTYPE)*TEST_NCOMP);
                          HDmemcpy(sub_ptr,&image0[i][j][0],TEST_NCOMP*sizeof(TEST_VARTYPE));
                          sub_ptr+=TEST_NCOMP;
                      } /* end if */
                } /* end for */
          } /* end for */

        /* initialize the disk buffer */
        HDmemset(image,255,sizeof(TEST_VARTYPE)*(size_t)(TEST_YDIM*TEST_XDIM*TEST_NCOMP));

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Image B2a2cc",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Set the fill-value */
        ret=GRsetattr(riid,FILL_ATTR,TEST_NT,TEST_NCOMP,fill_pixel);
        CHECK(ret,FAIL,"GRsetattr");

        /* Check if creating chunked GR */
        if (flag)
          {
              /* Create chunked GR 
                 chunk is 2x2 which will create 6 chunks */
              cdims[0] = chunk_def.chunk_lengths[0] = 2;
              cdims[1] = chunk_def.chunk_lengths[1] = 2;
              ret = GRsetchunk(riid, chunk_def, HDF_CHUNK);
              CHECK(ret,FAIL,"GRsetchunk");
          }

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        /* Create sub-sampled window with only the filled pixels in it */
        start[XDIM]=1;
        start[YDIM]=1;
        count[XDIM]=TEST_XDIM/2;
        count[YDIM]=TEST_YDIM/2;
        stride[XDIM]=stride[YDIM]=2;
        ret=GRwriteimage(riid,start,stride,count,sub_image);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        /* Get the whole image back */
        start[XDIM]=start[YDIM]=0;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,image0,sizeof(TEST_VARTYPE)*TEST_YDIM*TEST_XDIM*TEST_NCOMP))
          {
              MESSAGE(3, printf("Error reading data for new image with user-defined fill-value, sub-sampled image\n"););

              MESSAGE(8, for(i=0; i<TEST_YDIM; i++) \
                      for(j=0; j<TEST_XDIM; j++) \
                          for(k=0; k<TEST_NCOMP; k++) \
                            if(image[i][j][k]!=image0[i][j][k]) \
                                  printf("Location: [%d][%d][%d] image=%d, image0=%d \n",(int)i,(int)j,(int)k,(int)image[i][j][k],(int)image0[i][j][k]); );
              num_errs++;
          } /* end if */

        /* check if we are doing chunked tests */
        if (flag)
          {
              /* Get chunk lengths */
              ret = GRgetchunkinfo(riid, &rchunk_def, &cflags);
              CHECK(ret,FAIL,"GRgetchunkinfo");

              rcdims = rchunk_def.chunk_lengths;

              /* check chunk lengths and to see if GR is chunked */
              if (cdims[0] != rcdims[0] || cdims[1] != rcdims[1] || cflags != HDF_CHUNK)
                {
                    fprintf(stderr, "Chunk Test. GRgetchunkinfo returned wrong values\n");
                    fprintf(stderr, "cdims[0]=%d,cdims[1]=%d \n",(int)cdims[0],(int)cdims[1]);
                    fprintf(stderr, "rcdims[0]=%d,rcdims[1]=%d \n",(int)rcdims[0],(int)rcdims[1]);
                    fprintf(stderr, "cflags =%d \n", (int)cflags );
                }

          }

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");

/* The following test is unnecessary, fill-values only make a difference when writing out data -QAK */
#ifdef QAK
    MESSAGE(8, printf("Check out I/O on new image with real data, with Default fill-value, Reading Sub-sampled Image\n"););
    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
#ifdef TEST_XDIM
#undef TEST_XDIM
#endif /* TEST_XDIM */
#define TEST_XDIM    19
#ifdef TEST_YDIM
#undef TEST_YDIM
#endif /* TEST_YDIM */
#define TEST_YDIM    23
#ifdef TEST_NCOMP
#undef TEST_NCOMP
#endif /* TEST_NCOMP */
#define TEST_NCOMP   4
#ifdef TEST_VARTYPE
#undef TEST_VARTYPE
#endif /* TEST_VARTYPE */
#define TEST_VARTYPE uint32
#ifdef TEST_NT
#undef TEST_NT
#endif /* TEST_NT */
#define TEST_NT      DFNT_UINT32

        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={TEST_XDIM,TEST_YDIM};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        TEST_VARTYPE image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE fill_pixel[TEST_NCOMP]={4000000,4800,3,1000};   /* pixel with fill values */
        TEST_VARTYPE fill_pixel2[TEST_NCOMP]={1230,1,65000,350000};   /* pixel with fill values */
        TEST_VARTYPE image0[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE sub_image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE *sub_ptr;
        int32 start[2]; /* start of image data to use */
        int32 stride[2];/* stride of image data to use */
        int32 count[2]; /* # of pixels of image data to use */
        intn i,j,k;     /* local counting variables */

        /* fill the memory-only with the default pixel fill-values */
        HDmemset(image0,0,sizeof(image0));
        sub_ptr=(TEST_VARTYPE *)sub_image;
        for(i=0; i<TEST_YDIM; i++)
          {
              for(j=0; j<TEST_XDIM; j++)
                {
                    if((j%2)==0)
                        HDmemcpy(&image0[i][j][0],fill_pixel,sizeof(fill_pixel));
                    else
                        HDmemcpy(&image0[i][j][0],fill_pixel2,sizeof(fill_pixel2));
                    if((i%2) && (j%2))
                      {
                          HDmemcpy(sub_ptr,&image0[i][j][0],TEST_NCOMP*sizeof(TEST_VARTYPE));
                          sub_ptr+=TEST_NCOMP;
                      } /* end if */
                } /* end for */
          } /* end for */

        /* initialize the disk buffer */
        HDmemset(image,255,sizeof(image));

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Image B2a2cc2",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        /* Create whole image */
        start[XDIM]=start[YDIM]=0;
        stride[XDIM]=stride[YDIM]=1;
        ret=GRwriteimage(riid,start,stride,dims,image0);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        /* Get the sub-sample image back */
        start[XDIM]=1;
        start[YDIM]=1;
        count[XDIM]=TEST_XDIM/2;
        count[YDIM]=TEST_YDIM/2;
        stride[XDIM]=stride[YDIM]=2;
        ret=GRreadimage(riid,start,stride,count,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,sub_image,count[XDIM]*count[YDIM]*sizeof(fill_pixel)))
          {
              MESSAGE(3, printf("%d:Error reading data for new image with default fill-value, sub-sampled image\n",__LINE__););
              num_errs++;
          } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
#endif /* QAK */

} /* end test_mgr_image_b2a2cc() */

static void test_mgr_image_b2b1(int flag)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */
    char datafile[512] = "";
    char *srcdir = getenv("srcdir");

/* B2b1 - Read/Write images - with real Data - Existing Image - Whole Image */
    MESSAGE(8, printf("Check out I/O from Existing Image - Whole Image\n"););

    /* Generate the correct name for the test file, by prepending the source path */
    if (srcdir && ((strlen(srcdir) + strlen(DATAFILE) + 1) < sizeof(datafile))) {
        strcpy(datafile, srcdir);
        strcat(datafile, "/");
    }
    strcat(datafile, DATAFILE);

    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(datafile,DFACC_READ,0);
    CHECK(fid,FAIL,"Hopen");

    /* Try initializing the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
        int32 n_datasets;       /* number of datasets */
        int32 n_attrs;          /* number of attributes */
        intn i;     /* local counting variables */
        
        ret=(intn)GRfileinfo(grid,&n_datasets,&n_attrs);
        CHECK(ret,FAIL,"GRfileinfo");

        for(i=0; i<n_datasets; i++)
          {
              int32 riid;               /* RI ID for an image */
              char name[MAX_IMG_NAME];  /* storage for the image's name */
              int32 ncomp;              /* number of components */
              int32 nt;                 /* NT of the components */
              int32 il;                 /* interlace of the image data */
              int32 dimsizes[2];        /* dimension sizes of the image */
              int32 n_attr;             /* number of attributes with each image */
              VOIDP img_data;           /* buffer for the image data */

              /* Attach to the image */
              riid=GRselect(grid,i);
              CHECK(riid,FAIL,"GRselect");

              /* Get the Image information */
              *name='\0';
              ret=GRgetiminfo(riid,name,&ncomp,&nt,&il,dimsizes,&n_attr);
              CHECK(ret,FAIL,"GRgetiminfo");

              /* Check the name for correctness */
              if(HDstrcmp(name,datafile_info[i].name))
                {
                    MESSAGE(3, printf("Error! Name for image %d is: %s, should be %s\n",i,name,datafile_info[i].name););
                    num_errs++;
                } /* end if */

              /* Check the # of components */
              if(ncomp!=datafile_info[i].ncomp)
                {
                    MESSAGE(3, printf("Error! Number of components for image %d is: %ld, should be %ld\n",i,(long)ncomp,(long)datafile_info[i].ncomp););
                    num_errs++;
                } /* end if */

              /* Check the NT of components */
              if(nt!=datafile_info[i].nt)
                {
                    MESSAGE(3, printf("Error! NT of components for image %d is: %ld, should be %ld\n",i,(long)nt,(long)datafile_info[i].nt););
                    num_errs++;
                } /* end if */

              /* Check the interlace of components */
              if(il!=datafile_info[i].il)
                {
                    MESSAGE(3, printf("Error! Interlace of components for image %d is: %ld, should be %ld\n",i,(long)il,(long)datafile_info[i].il););
                    num_errs++;
                } /* end if */

              /* Check the x-dimension of the image */
              if(dimsizes[XDIM]!=datafile_info[i].dimsizes[XDIM])
                {
                    MESSAGE(3, printf("Error! X-dim of image %d is: %ld, should be %ld\n",i,(long)dimsizes[XDIM],(long)datafile_info[i].dimsizes[XDIM]););
                    num_errs++;
                } /* end if */

              /* Check the y-dimension of the image */
              if(dimsizes[YDIM]!=datafile_info[i].dimsizes[YDIM])
                {
                    MESSAGE(3, printf("Error! Y-dim of image %d is: %ld, should be %ld\n",i,(long)dimsizes[YDIM],(long)datafile_info[i].dimsizes[YDIM]););
                    num_errs++;
                } /* end if */

              /* Check the # of attributes of the image */
              if(n_attr!=datafile_info[i].n_attr)
                {
                    MESSAGE(3, printf("Error! # of attributes for image %d is: %ld, should be %ld\n",i,(long)n_attr,(long)datafile_info[i].n_attr););
                    num_errs++;
                } /* end if */

              /* Check the image data itself */
              {
                  int32 start[2];
                  int32 stride[2];

                    img_data=HDmalloc((size_t)(dimsizes[0]*dimsizes[1]*ncomp*DFKNTsize(nt|DFNT_NATIVE)));
                    CHECK(img_data,NULL,"HDmalloc");

                    HDmemset(img_data,0,(size_t)(dimsizes[0]*dimsizes[1]*ncomp*DFKNTsize(nt|DFNT_NATIVE)));

                    start[0]=start[1]=0;
                    stride[0]=stride[1]=1;
                    ret=GRreadimage(riid,start,stride,dimsizes,img_data);
                    CHECK(ret,FAIL,"GRreadimage");

                    switch(i)
                      {
                          case 0:
                              if(0!=HDmemcmp(img_data,image00,sizeof(image00)))
                                {
                                    MESSAGE(3, printf("Error reading data for image %d\n",i););
                                    num_errs++;
                                } /* end if */
                              break;

                          case 1:
                              if(0!=HDmemcmp(img_data,image1,sizeof(image1)))
                                {
                                    MESSAGE(3, printf("Error reading data for image %d\n",i););
                                    num_errs++;
                                } /* end if */
                              break;

                          case 2:
                              if(0!=HDmemcmp(img_data,image2,sizeof(image2)))
                                {
                                    MESSAGE(3, printf("Error reading data for image %d\n",i););
                                    num_errs++;
                                } /* end if */
                              break;

                          case 3:
                              if(0!=HDmemcmp(img_data,image3,sizeof(image3)))
                                {
                                    MESSAGE(3, printf("Error reading data for image %d\n",i););
                                    num_errs++;
                                } /* end if */
                              break;

                          case 4:
                              if(0!=HDmemcmp(img_data,image4,sizeof(image4)))
                                {
                                    MESSAGE(3, printf("Error reading data for image %d\n",i););
                                    num_errs++;
                                } /* end if */
                              break;

                      } /* end switch */

                    HDfree(img_data);
              } /* end block */

              /* End access to the image */
              ret=GRendaccess(riid);
              CHECK(ret,FAIL,"GRendaccess");
          } /* end for */
      } /* end block */

    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");

} /* end test_mgr_image_b2b1() */

static void test_mgr_image_b2b2(int flag)
{
/* B2b2 - Read/Write images - with real Data - Existing Image - Sub-setted Image */
    /* This test is unnecessary, I think this case has been adequately covered above -QAK */
} /* end test_mgr_image_b2b2() */

static void test_mgr_image_b2b3(int flag)
{
/* B2b3 - Read/Write images - with real Data - Existing Image - Sub-sampled Image */
    /* This test is unnecessary, I think this case has been adequately covered above -QAK */
} /* end test_mgr_image_b2b3() */

static void test_mgr_image_chunk(int flag)
{
    int32   cdims[2] = {1,1};    /* chunk dims */
    int32   *rcdims;             /* for SDgetchunkinfo() */
    HDF_CHUNK_DEF chunk_def;     /* Chunk defintion set */ 
    HDF_CHUNK_DEF rchunk_def;    /* Chunk defintion read */ 
    int32     cflags;            /* chunk flags */
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */

    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(TESTFILE2,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
#ifdef TEST_XDIM
#undef TEST_XDIM
#endif /* TEST_XDIM */
#define TEST_XDIM    3
#ifdef TEST_YDIM
#undef TEST_YDIM
#endif /* TEST_YDIM */
#define TEST_YDIM    8
#ifdef TEST_NCOMP
#undef TEST_NCOMP
#endif /* TEST_NCOMP */
#define TEST_NCOMP   2
#ifdef TEST_VARTYPE
#undef TEST_VARTYPE
#endif /* TEST_VARTYPE */
#define TEST_VARTYPE int32
#ifdef TEST_NT
#undef TEST_NT
#endif /* TEST_NT */
#define TEST_NT      DFNT_INT32
        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={TEST_XDIM,TEST_YDIM};    /* dimensions for the empty image */
        uint16 ref;     /* RI ref #. */
        int32 index;    /* RI index # */
        TEST_VARTYPE image[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        TEST_VARTYPE fill_pixel[TEST_NCOMP]={1,-2};   /* pixel with fill values */
        TEST_VARTYPE fill_pixel2[TEST_NCOMP]={-2,1};   /* pixel with fill values */
        TEST_VARTYPE image0[TEST_YDIM][TEST_XDIM][TEST_NCOMP]; /* space for the image data */
        int32 start[2]; /* start of image data to grab */
        int32 stride[2];/* stride of image data to grab */
        intn i,j;       /* local counting variables */


        /* fill the memory-only with the default pixel fill-value */
        for(i=0; i<TEST_YDIM; i++)
          {
              for(j=0; j<TEST_XDIM; j++)
                {
                    if((j%2)==0)
                        HDmemcpy(&image0[i][j][0],fill_pixel,sizeof(fill_pixel));
                    else
                        HDmemcpy(&image0[i][j][0],fill_pixel2,sizeof(fill_pixel2));
                } /* end for */
          } /* end for */
        HDmemcpy(image,image0,sizeof(image0));

        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Chunk Image B2a1aa",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Create chunked GR 
           chunk is 2x2 which will create 6 chunks */
        cdims[0] = chunk_def.chunk_lengths[0] = 2;
        cdims[1] = chunk_def.chunk_lengths[1] = 2;
        ret = GRsetchunk(riid, chunk_def, HDF_CHUNK);
        CHECK(ret,FAIL,"GRsetchunk");

        /* Set Chunk cache to hold 2 chunks */
        ret = GRsetchunkcache(riid, 2, 0);
        CHECK(ret,FAIL,"GRsetchunkcache");

        /* Save the ref. # for later access */
        ref=GRidtoref(riid);
        CHECK(ref,(uint16)FAIL,"GRidtoref");

        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRwriteimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Shut down the GR interface */
        ret=GRend(grid);
        CHECK(ret,FAIL,"GRend");

        /* Initialize the GR interface again */
        grid=GRstart(fid);
        CHECK(grid,FAIL,"GRstart");

#if 0
        /* Create empty image with default fill value */
        riid=GRcreate(grid,"Test Image B2a1aa",TEST_NCOMP,TEST_NT,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

#endif

        /* Get the index of the newly created image */
        index=GRreftoindex(grid,ref);
        CHECK(index,FAIL,"GRreftoindex");

        /* Select the newly created image */
        riid=GRselect(grid,index);
        CHECK(riid,FAIL,"GRselect");

        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        if(0!=HDmemcmp(image,image0,sizeof(image0)))
          {
              MESSAGE(3, printf("%d:Error reading data for new image with default fill-value, whole image\n",__LINE__););
              num_errs++;
          } /* end if */

        /* Get chunk lengths */
        ret = GRgetchunkinfo(riid, &rchunk_def, &cflags);
        CHECK(ret,FAIL,"GRgetchunkinfo");

        rcdims = rchunk_def.chunk_lengths;

        /* check chunk lengths and to see if GR is chunked */
        if (cdims[0] != rcdims[0] || cdims[1] != rcdims[1] || cflags != HDF_CHUNK)
          {
              fprintf(stderr, "Chunk Test 1. GRgetchunkinfo returned wrong values\n");
              fprintf(stderr, "cdims[0]=%d,cdims[1]=%d \n",(int)cdims[0],(int)cdims[1]);
              fprintf(stderr, "rcdims[0]=%d,rcdims[1]=%d \n",(int)rcdims[0],(int)rcdims[1]);
              fprintf(stderr, "cflags =%d \n", (int)cflags );
          }

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");


#if 0

    /* Create chunked GR 
       chunk is 3x2 which will create 6 chunks.
       Use GZIP compression */
    cdims[0] = chunk_def.comp.chunk_lengths[0] = 3;
    cdims[1] = chunk_def.comp.chunk_lengths[1] = 2;
#if 0
    chunk_def.comp.comp_type = COMP_CODE_RLE;   /* RLE */

    chunk_def.comp.comp_type = COMP_CODE_SKPHUFF; /* Skipping Huffman */
    chunk_def.comp.cinfo.skphuff.skp_size = sizeof(uint16);
#endif
    chunk_def.comp.comp_type = COMP_CODE_DEFLATE; /* GZIP */
    chunk_def.comp.cinfo.deflate.level = 6;

    status = GRsetchunk(riid8, chunk_def, HDF_CHUNK | HDF_COMP);
    if(status == FAIL) 
      {
        fprintf(stderr, "Error! Chunk Test 7. Failed to create new chunked, GZIP Compressed data set\n");
        num_err++;
        goto test8;
      }
#endif

} /* end test_mgr_image_chunk() */

/****************************************************************
**
**  test_mgr_image(): Multi-file Raster Image I/O Test Routine
**  II. Create Images
**      A. GRcreate/GRselect/GRendaccess w/GRgetiminfo
**      B. Write/Read images
**          1. With no Data
**              a. Default fill value
**              b. user defined fill value
**          2. With real Data
**              a. New Image
**                  1. With default fill value
**                      aa. Whole image
**                      bb. Sub-setted image
**                      cc. Sub-sampled image
**                  2. With user defined vill value
**                      aa. Whole image
**                      bb. Sub-setted image
**                      cc. Sub-sampled image
**              b. Existing Image
**                  1. Whole image
**                  2. Sub-setted image
**                  3. Sub-sampled image
** 
****************************************************************/
static void
test_mgr_image(int flag)
{
    /* Output message about test being performed */
    MESSAGE(6, printf("Testing Multi-file Raster Image I/O routines\n"););
    test_mgr_image_b1a(flag);
    test_mgr_image_b1b(flag);
    test_mgr_image_b2a1aa(flag);
    test_mgr_image_b2a1bb1(flag);
    test_mgr_image_b2a1bb2(flag);
    test_mgr_image_b2a1cc1(flag);
    test_mgr_image_b2a1cc2(flag);
    test_mgr_image_b2a2aa(flag);
    test_mgr_image_b2a2bb(flag);
    test_mgr_image_b2a2cc(flag);
    test_mgr_image_b2b1(flag);
    test_mgr_image_b2b2(flag);
    test_mgr_image_b2b3(flag);
#if 0
    test_mgr_image_chunk(flag);
#endif
}   /* end test_mgr_image() */

/****************************************************************
**
**  test_mgr_index(): Multi-file Raster ID/Ref/Index Test Routine
** 
**  III. ID/Ref/Index Functions
**      A. GRidtoref
**      B. GRreftoindex
** 
****************************************************************/
static void
test_mgr_index(int flag)
{
    /* output message about test being performed */
    MESSAGE(6, printf("Testing Multi-File Raster id/ref/index routines\n"););

/* I believe that these are adequately tested in the test_mgr_image routine -QAK */
}   /* end test_mgr_index() */

/****************************************************************
**
**  test_mgr_interlace(): Multi-file Raster Interlace Test Routine
** 
**  IV. Interlace Functions [Need to be implemented]
**      A. GRreqlutil - tested in the palette test below.
**      B. GRreqimageil
** 
****************************************************************/
static void
test_mgr_interlace(int flag)
{
    int32 fid;              /* hdf file id */
    int32 grid;             /* grid for the interface */
    int32 n_datasets;       /* number of datasets */
    int32 n_attrs;          /* number of attributes */
    int32 ret;              /* generic return value */
    VOIDP image;            /* image to retrieve */

    /* Output message about test being performed */
    MESSAGE(6, printf("Testing Multi-file Raster Interlace routines\n"););

    /* open up the existing datafile and get the image information from it */
    if (flag)
        fid=Hopen(TESTFILE2,DFACC_RDWR,0);
    else
        fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* initialize the gr interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
        intn i,j;     /* local counting variables */
        
        ret=(intn)GRfileinfo(grid,&n_datasets,&n_attrs);
        CHECK(ret,FAIL,"GRfileinfo");

        for(i=0; i<n_datasets; i++)
          {
              int32 riid;               /* RI ID for an image */
              char name[MAX_IMG_NAME];  /* storage for the image's name */
              int32 ncomp;              /* number of components */
              int32 nt;                 /* NT of the components */
              int32 il;                 /* interlace of the image data */
              int32 start[2];
              int32 stride[2];
              int32 dimsizes[2];        /* dimension sizes of the image */
              int32 n_attr;             /* number of attributes with each image */
              VOIDP img_data;           /* buffer for the image data */

              /* Attach to the image */
              riid=GRselect(grid,i);
              CHECK(riid,FAIL,"GRselect");

              /* Get the Image information */
              *name='\0';
              ret=GRgetiminfo(riid,name,&ncomp,&nt,&il,dimsizes,&n_attr);
              CHECK(ret,FAIL,"GRgetiminfo");

              image=HDmalloc((size_t)(dimsizes[XDIM]*dimsizes[YDIM]*ncomp*DFKNTsize(nt|DFNT_NATIVE)));
              CHECK(image,NULL,"HDmalloc");

              start[0]=start[1]=0;
              stride[0]=stride[1]=1;
              ret=GRreadimage(riid,start,stride,dimsizes,image);

              /* Check the image data itself */
              for(j=(intn)MFGR_INTERLACE_PIXEL; j<=(intn)MFGR_INTERLACE_COMPONENT; j++)
                {
                    VOIDP pixel_buf;

                    img_data=HDmalloc((size_t)(dimsizes[0]*dimsizes[1]*ncomp*DFKNTsize(nt|DFNT_NATIVE)));
                    CHECK(img_data,NULL,"HDmalloc");

                    pixel_buf=HDmalloc((size_t)(dimsizes[0]*dimsizes[1]*ncomp*DFKNTsize(nt|DFNT_NATIVE)));
                    CHECK(pixel_buf,NULL,"HDmalloc");

                    HDmemset(img_data,0,(size_t)(dimsizes[0]*dimsizes[1]*ncomp*DFKNTsize(nt|DFNT_NATIVE)));

                    ret=GRreqimageil(riid,j);
                    CHECK(ret,FAIL,"GRreqimageil");

                    start[0]=start[1]=0;
                    stride[0]=stride[1]=1;
                    ret=GRreadimage(riid,start,stride,dimsizes,img_data);
                    CHECK(ret,FAIL,"GRreadimage");

                    GRIil_convert(image,MFGR_INTERLACE_PIXEL,pixel_buf,(gr_interlace_t)j,dimsizes,ncomp,nt);
                    if(0!=HDmemcmp(img_data,pixel_buf,
                          (size_t)(dimsizes[XDIM]*dimsizes[YDIM]*ncomp*DFKNTsize(nt|DFNT_NATIVE))))
                      {
                          MESSAGE(3, printf("Error reading data for image %d, j=%d\n",i,j););
                          num_errs++;
                      } /* end if */
                    HDfree(img_data);
                    HDfree(pixel_buf);
                } /* end for */

              HDfree(image);

              /* End access to the image */
              ret=GRendaccess(riid);
              CHECK(ret,FAIL,"GRendaccess");
          } /* end for */
      } /* end block */
    
    /* shut down the gr interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
}   /* end test_mgr_interlace() */

/****************************************************************
**
**  test_mgr_lut_a(): Multi-file Raster LUT/Palette Test Routine
** 
**  V. Palette Functions
**      A. GRgetlutid w/GRgetlutinfo
**      B. Read/Write Palettes
**          1. GRwritelut
**          2. GRreadlut
**	C. GRluttoref
** 
****************************************************************/
static void
test_mgr_lut_a(int flag)
{
    int32 fid;              /* hdf file id */
    int32 grid;             /* grid for the interface */
    int32 ret;              /* generic return value */

    /* Output message about test being performed */
    MESSAGE(6, printf("Testing Multi-file Raster Palette routines\n"););

    /* open up the existing datafile and get the image information from it */
    fid=Hopen(TESTFILE,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* initialize the gr interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

/* pick up here -QAK2 */
    {
        intn i,j;     /* local counting variables */
        int32 riid;               /* RI ID for an image */
        int32 lutid;              /* RI ID for an image */
        char name[MAX_IMG_NAME];  /* storage for the image's name */
        int32 ncomp;              /* number of components */
        int32 pal_ncomp;          /* number of palette components */
        int32 nt;                 /* NT of the components */
        int32 pal_nt;             /* NT of the palette components */
        int32 il;                 /* interlace of the image data */
        int32 pal_il;             /* interlace of the palette data */
        int32 dimsizes[2];        /* dimension sizes of the image */
        int32 pal_entries;        /* number of entries in the palette */
        int32 n_attr;             /* number of attributes with each image */
        uint8 *tmp_data;          /* temporary buffer pointer */
        VOIDP pal_data;           /* buffer for the palette data */
        uint16 pal_ref;		  /* reference number of the palette */

        /* Attach to the image */
        riid=GRselect(grid,0);
        CHECK(riid,FAIL,"GRselect");

        /* Get the Image information */
        *name='\0';
        ret=GRgetiminfo(riid,name,&ncomp,&nt,&il,dimsizes,&n_attr);
        CHECK(ret,FAIL,"GRgetiminfo");

        /* Get the number of palettes */
        ret=GRgetnluts(riid);
        VERIFY(ret,0,"GRgetnluts");

        lutid=GRgetlutid(riid,0);
        CHECK(lutid,FAIL,"GRgetlutid");
        
        /* Get the Palette information */
        ret=GRgetlutinfo(lutid,&pal_ncomp,&pal_nt,&pal_il,&pal_entries);
        CHECK(ret,FAIL,"GRgetlutinfo");

        /* Check the palette values, they should all be "nil" values */
        if(pal_ncomp!=0)
          {
              MESSAGE(3, printf("Error! Incorrect palette components\n"););
              num_errs++;
          } /* end if */
        if(pal_nt!=DFNT_NONE)
          {
              MESSAGE(3, printf("Error! Incorrect palette number-type\n"););
              num_errs++;
          } /* end if */
        if(pal_il!=(-1))
          {
              MESSAGE(3, printf("Error! Incorrect palette interlace, pal_il=%d\n",(int)pal_il););
              num_errs++;
          } /* end if */
        if(pal_entries!=0)
          {
              MESSAGE(3, printf("Error! Incorrect palette # of entries\n"););
              num_errs++;
          } /* end if */

        /* Set the palette components */
        pal_ncomp=3;
        pal_nt=DFNT_UINT8;
        pal_il=(int32)MFGR_INTERLACE_PIXEL;
        pal_entries=256;

        pal_data=HDmalloc((size_t)(pal_entries*pal_ncomp*DFKNTsize(pal_nt|DFNT_NATIVE)));
        CHECK(pal_data,NULL,"HDmalloc");

        /* Initialize the palette data, in 'pixel' interlace */
        tmp_data=(uint8 *)pal_data;
        for(j=0; j<pal_entries; j++)
            for(i=0; i<pal_ncomp; i++)
                *tmp_data++=(uint8)(j*i);

        /* Write the palette out */
        ret=GRwritelut(lutid,pal_ncomp,pal_nt,pal_il,pal_entries,pal_data);
        CHECK(ret,FAIL,"GRwritelut");

        /* Check the image data itself */
        for(j=(intn)MFGR_INTERLACE_PIXEL; j<=(intn)MFGR_INTERLACE_COMPONENT; j++)
          {
              VOIDP pixel_buf;
              int32 dimsizes2[2];

              tmp_data=HDmalloc((size_t)(pal_entries*pal_ncomp*DFKNTsize(pal_nt|DFNT_NATIVE)));
              CHECK(tmp_data,NULL,"HDmalloc");

              pixel_buf=HDmalloc((size_t)(pal_entries*pal_ncomp*DFKNTsize(pal_nt|DFNT_NATIVE)));
              CHECK(pixel_buf,NULL,"HDmalloc");

              HDmemset(tmp_data,0,(size_t)(pal_entries*pal_ncomp*DFKNTsize(pal_nt|DFNT_NATIVE)));

              ret=GRreqlutil(lutid,j);
              CHECK(ret,FAIL,"GRreqlutil");

              ret=GRreadlut(lutid,tmp_data);

              dimsizes2[XDIM]=1;
              dimsizes2[YDIM]=pal_entries;
              GRIil_convert(pal_data,MFGR_INTERLACE_PIXEL,pixel_buf,(gr_interlace_t)j,dimsizes2,pal_ncomp,pal_nt);
              if(0!=HDmemcmp(tmp_data,pixel_buf,
                    (size_t)(pal_entries*pal_ncomp*DFKNTsize(pal_nt|DFNT_NATIVE))))
                {
                    MESSAGE(3, printf("Error reading data for palette j=%d\n",j););
                    num_errs++;
                } /* end if */
              HDfree(tmp_data);
              HDfree(pixel_buf);
          } /* end for */

        HDfree(pal_data);

	/* This lutid should yield a valid reference number, which is not 0 - BMR */ 
        pal_ref=GRluttoref(lutid);
        CHECK(pal_ref,0,"GRluttoref");

	/* Now, this bogus lutid should cause GRluttoref to return a 0 - BMR */
        pal_ref=GRluttoref(0);
        VERIFY(pal_ref,0,"GRluttoref");
        
        /* End access to the image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
      } /* end block */
    
    /* shut down the gr interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
}   /* end test_mgr_lut_a() */

#define GR_LUTBFILE "gr2.hdf"
#define GR_LUTB_X_LENGTH 15 
#define GR_LUTB_Y_LENGTH 10 

/****************************************************************
**
**  test_mgr_lut_b(): Multi-file Raster LUT/Palette Test Routine
** 
**  V. Palette Functions - Test for writing palette with no image data to
**      file.
** 
****************************************************************/
static void
test_mgr_lut_b(int flag)
{
	int32 gr_id, ri_id, file_id, pal_id, status, num_entries, ri_idx=0;
	int32 data_type, ncomp, num_comp, interlace_mode; 
	uint8 palette_data[256*3];
	uint8 r_palette_data[256*3];
	intn i;
    int32 dims[2]={GR_LUTB_X_LENGTH,GR_LUTB_Y_LENGTH};

	/* Create and open the file. */
	file_id = Hopen(GR_LUTBFILE, DFACC_CREATE, 0);
    CHECK(file_id,FAIL,"Hopen");

	/* Initiate the GR interface. */
	gr_id = GRstart(file_id);
    CHECK(gr_id,FAIL,"GRstart");

    ncomp = 1;
    dims[0] = 20;
    dims[1] = 20;
    interlace_mode = MFGR_INTERLACE_PIXEL;

    ri_id = GRcreate (gr_id, "Image_1", ncomp, DFNT_UINT8, interlace_mode, dims);
    CHECK(ri_id,FAIL,"GRcreate");
 
	/* Initialize the palette to grayscale. */
	for (i = 0; i < 256; i++) {
	 	palette_data[i * 3] = i;
	 	palette_data[i * 3 + 1] = i;
	 	palette_data[i * 3 + 2] = i;
	}

	/* Set palette characteristics. */
	data_type = DFNT_UINT8;
	num_entries = 256;
	num_comp = 3;

	/* Get the id for the palette. */
	pal_id = GRgetlutid(ri_id, ri_idx);
    CHECK(pal_id,FAIL,"GRgetlutid");

	/* Write the palette to file. */
	status = GRwritelut(pal_id, num_comp, data_type, interlace_mode, num_entries, (VOIDP)palette_data);
    CHECK(status,FAIL,"GRgetlutid");

    status = GRendaccess(ri_id);
    CHECK(status,FAIL,"GRendaccess");
 
    status = GRend(gr_id);
    CHECK(status,FAIL,"GRend");
 
	status = Hclose(file_id);
    CHECK(status,FAIL,"Hclose");

	file_id = Hopen(GR_LUTBFILE, DFACC_READ, 0);
    CHECK(file_id,FAIL,"Hopen");

    gr_id = GRstart(file_id);
    CHECK(gr_id,FAIL,"GRstart");
 
    ri_idx = GRnametoindex(gr_id, "Image_1");
    CHECK(ri_idx,FAIL,"GRnametoindex");
 
    ri_id = GRselect (gr_id, ri_idx);
    CHECK(ri_id,FAIL,"GRselect");
 
    pal_id = GRgetlutid(ri_id, ri_idx);
    CHECK(pal_id,FAIL,"GRgetlutid");

	/* Read the palette data. */
	status = GRreadlut(pal_id, (VOIDP)r_palette_data);
    CHECK(status,FAIL,"GRreadlut");

    /* Verify correct palette contents */
    if(HDmemcmp(palette_data,r_palette_data,256*3)!=0) {
        MESSAGE(3, printf("Error reading data for palette\n"););
        num_errs++;
    } /* end if */


	/* Terminate access to the image. */
	status = GRendaccess(ri_id);
    CHECK(status,FAIL,"GRendaccess");

	/* Terminate access to the GR interface. */
	status = GRend(gr_id);
    CHECK(status,FAIL,"GRend");

	/* Close the file. */
	status = Hclose(file_id);
    CHECK(status,FAIL,"Hclose");
}   /* end test_mgr_lut_b() */

/****************************************************************
**
**  test_mgr_lut(): Multi-file Raster LUT/Palette Test Routine
** 
**  V. Palette Functions
**      A. GRgetlutid w/GRgetlutinfo
**      B. Read/Write Palettes
**          1. GRwritelut
**          2. GRreadlut
**	C. GRluttoref
** 
****************************************************************/
static void
test_mgr_lut(int flag)
{
    /* Output message about test being performed */
    MESSAGE(6, printf("Testing Multi-file Raster Palette routines\n"););

    test_mgr_lut_a(flag);
    test_mgr_lut_b(flag);
}   /* end test_mgr_lut() */

/****************************************************************
**
**  test_mgr_special(): Multi-file Raster Special Element Test Routine
** 
**  VI. Special Element Functions [Need to be implemented]
**      A. GRsetexternalfile
**      B. GRsetaccesstype
** 
****************************************************************/
static void
test_mgr_special(int flag)
{
    /* Output message about test being performed */
    MESSAGE(6, printf("Testing Multi-file Raster Special Element routines\n"););
}   /* end test_mgr_special() */

/****************************************************************
**
**  test_mgr_attr(): Multi-file Raster Attribute Test Routine
** 
**  VII. Atribute Functions
**      A. GRattrinfo
**      B. Read/Write Attributes
**          1. GRsetattr
**          2. GRgetattr
**      C. GRfindattr
** 
****************************************************************/
static void
test_mgr_attr(int flag)
{
    /* Output message about test being performed */
    MESSAGE(6, printf("Testing Multi-file Raster Attribute routines\n"););

/* I believe that these are adequately tested in the test_mgr_image routine -QAK */
}   /* end test_mgr_attr() */

#define OLDRLEFILE  "8bit.dat"
#define OLDGREYJPEGFILE  "greyjpeg.dat"
#define OLDJPEGFILE  "jpeg.dat"
#define JPEGX   46
#define JPEGY   23

static const uint8  jpeg_8bit_j80[JPEGY][JPEGX] =
{
    {200, 200, 200, 200, 200, 200, 200, 200, 202, 202, 201, 201, 201, 200, 200, 200, 201, 201, 200, 200, 200, 201, 202, 202, 202, 202, 201, 200, 200, 200, 200, 201, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200}, 
    {200, 200, 200, 200, 200, 200, 200, 200, 199, 199, 199, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 199, 199, 199, 199, 200, 200, 201, 201, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200}, 
    {200, 200, 200, 200, 200, 200, 200, 200, 199, 199, 199, 200, 200, 201, 201, 201, 200, 200, 201, 201, 201, 200, 199, 199, 199, 199, 200, 201, 201, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200}, 
    {200, 200, 200, 200, 200, 200, 200, 200, 201, 201, 201, 201, 201, 201, 201, 201, 201, 201, 202, 202, 202, 201, 200, 199, 202, 202, 202, 202, 202, 200, 199, 198, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200}, 
    {200, 200, 200, 200, 200, 200, 200, 200, 201, 201, 201, 200, 200, 200, 199, 199, 200, 200, 200, 201, 201, 200, 200, 200, 200, 200, 201, 201, 201, 200, 200, 199, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200}, 
    {200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 199, 199, 199, 198, 198, 198, 199, 199, 198, 198, 198, 199, 200, 201, 197, 197, 197, 198, 198, 200, 201, 202, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200}, 
    {200, 200, 200, 200, 200, 200, 200, 200, 199, 199, 200, 200, 201, 202, 202, 203, 201, 200, 199, 198, 199, 201, 204, 205, 203, 202, 200, 199, 198, 200, 202, 203, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200}, 
    {200, 200, 200, 200, 200, 200, 200, 200, 200, 201, 202, 203, 205, 207, 208, 209, 205, 203, 201, 200, 202, 205, 208, 211, 214, 211, 206, 202, 200, 200, 201, 202, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200}, 
    {199, 199, 198, 199, 200, 202, 204, 205, 202, 200, 197, 198, 201, 204, 205, 205, 220, 204, 207, 222, 225, 201, 206, 213, 205, 218, 219, 224, 222, 198, 222, 208, 222, 208, 235, 201, 189, 204, 198, 202, 199, 201, 209, 192, 192, 217}, 
    {200, 200, 199, 199, 200, 201, 202, 203, 210, 208, 205, 203, 201, 197, 191, 186, 170, 212, 216, 191, 222, 217, 218, 185, 216, 226, 200, 193, 207, 232, 193, 221, 210, 124, 127, 218, 223, 192, 197, 207, 207, 209, 210, 220, 209, 184}, 
    {202, 201, 201, 200, 200, 200, 201, 201, 196, 197, 200, 203, 203, 200, 195, 191, 196, 180, 168, 123, 34, 66, 56, 74, 57, 83, 73, 81, 94, 221, 72, 64, 120, 119, 83, 100, 207, 190, 198, 198, 94, 44, 57, 50, 213, 215}, 
    {54, 54, 54, 53, 53, 54, 54, 54, 49, 51, 55, 59, 62, 63, 63, 62, 54, 110, 65, 43, 69, 40, 54, 36, 53, 67, 57, 65, 66, 61, 94, 83, 83, 69, 102, 99, 64, 214, 195, 90, 28, 46, 63, 51, 43, 57}, 
    {52, 52, 53, 54, 54, 55, 55, 55, 56, 54, 51, 48, 47, 46, 48, 49, 56, 63, 48, 72, 67, 55, 51, 65, 50, 48, 46, 70, 49, 67, 63, 86, 59, 107, 60, 73, 40, 44, 102, 36, 40, 218, 192, 165, 101, 44}, 
    {198, 199, 200, 201, 202, 202, 202, 202, 199, 197, 194, 190, 188, 190, 194, 198, 196, 151, 88, 202, 61, 71, 148, 169, 165, 188, 139, 77, 86, 70, 81, 84, 163, 226, 206, 184, 52, 46, 48, 165, 204, 189, 179, 213, 208, 79}, 
    {199, 200, 201, 202, 202, 201, 200, 199, 201, 203, 204, 205, 206, 209, 214, 218, 218, 198, 195, 199, 182, 201, 202, 206, 201, 228, 240, 191, 131, 121, 110, 221, 211, 208, 207, 198, 213, 231, 204, 220, 197, 211, 207, 194, 207, 213}, 
    {201, 202, 202, 203, 202, 200, 199, 197, 199, 202, 206, 206, 204, 202, 202, 203, 205, 195, 210, 199, 211, 220, 210, 224, 224, 204, 215, 205, 227, 218, 229, 220, 203, 201, 199, 202, 206, 206, 195, 205, 203, 194, 202, 197, 210, 190}, 
    {200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 203, 206, 210, 213, 213, 210, 206, 203, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200}, 
    {200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 198, 199, 201, 202, 202, 201, 199, 198, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200}, 
    {200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 199, 199, 198, 197, 197, 198, 199, 199, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200}, 
    {200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 202, 201, 200, 199, 199, 200, 201, 202, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200}, 
    {200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 199, 199, 199, 199, 199, 199, 199, 199, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200}, 
    {200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 198, 199, 200, 201, 201, 200, 199, 198, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200}, 
    {200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 201, 201, 202, 202, 202, 202, 201, 201, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200, 200} 
};

static const uint8  jpeg_24bit_j80[JPEGY][JPEGX][3] =
{
    252, 106, 0, 252, 106, 0, 252, 106, 0, 252, 106, 0, 253, 105, 0, 253, 105, 0, 255, 104, 0, 
    255, 104, 0, 255, 105, 0, 255, 105, 0, 255, 105, 0, 255, 104, 0, 255, 104, 0, 255, 103, 0, 
    255, 102, 0, 252, 106, 0, 237, 111, 0, 235, 114, 0, 246, 111, 0, 248, 111, 0, 241, 114, 0, 
    243, 111, 0, 251, 103, 3, 255, 99, 4, 255, 105, 7, 255, 106, 4, 248, 110, 1, 250, 108, 0, 
    255, 99, 1, 255, 97, 6, 255, 101, 13, 251, 104, 9, 251, 106, 0, 248, 108, 0, 245, 109, 1, 
    239, 110, 6, 235, 111, 11, 234, 111, 17, 239, 107, 22, 245, 107, 9, 255, 106, 0, 
    255, 105, 0, 255, 102, 0, 255, 101, 0, 255, 101, 0, 255, 101, 0, 

    252, 105, 0, 252, 105, 0, 252, 105, 0, 252, 105, 0, 252, 105, 0, 253, 105, 0, 253, 105, 0, 
    253, 105, 0, 253, 102, 0, 253, 102, 0, 254, 101, 0, 254, 101, 0, 255, 102, 0, 255, 102, 0, 
    255, 101, 0, 253, 105, 0, 245, 109, 0, 243, 109, 0, 247, 105, 0, 248, 104, 0, 246, 105, 0, 
    251, 106, 1, 255, 104, 4, 255, 105, 5, 251, 98, 0, 248, 102, 0, 246, 105, 0, 251, 105, 0, 
    255, 101, 0, 255, 100, 2, 254, 104, 9, 251, 105, 6, 252, 106, 0, 252, 106, 0, 253, 105, 0, 
    251, 105, 4, 246, 106, 9, 244, 106, 15, 245, 105, 20, 251, 104, 11, 255, 104, 0, 
    255, 104, 0, 255, 102, 0, 255, 102, 0, 255, 104, 0, 255, 105, 0, 

    251, 105, 4, 251, 105, 4, 251, 105, 4, 251, 105, 4, 251, 105, 4, 252, 104, 4, 252, 104, 4, 
    252, 104, 4, 252, 103, 3, 253, 104, 4, 253, 104, 4, 253, 104, 4, 253, 104, 4, 254, 105, 5, 
    254, 105, 5, 255, 105, 2, 255, 105, 0, 255, 103, 0, 255, 102, 0, 255, 99, 0, 255, 97, 1, 
    255, 98, 3, 255, 102, 4, 255, 105, 1, 251, 104, 0, 251, 107, 0, 255, 108, 0, 255, 107, 0, 
    255, 104, 0, 255, 104, 0, 250, 105, 0, 247, 105, 0, 253, 106, 0, 255, 103, 0, 255, 96, 0, 
    255, 94, 4, 255, 95, 6, 255, 98, 9, 255, 98, 17, 255, 99, 13, 255, 99, 2, 255, 100, 0, 
    255, 103, 4, 248, 106, 8, 241, 109, 9, 237, 111, 9, 

    252, 104, 4, 252, 104, 4, 252, 104, 4, 252, 104, 4, 253, 104, 4, 252, 104, 4, 253, 104, 4, 
    253, 104, 4, 255, 106, 6, 255, 106, 6, 255, 106, 6, 254, 105, 5, 254, 105, 5, 253, 104, 4, 
    253, 104, 4, 255, 102, 1, 255, 98, 0, 255, 97, 0, 255, 100, 3, 255, 98, 9, 255, 95, 12, 
    255, 95, 9, 255, 98, 2, 252, 101, 0, 252, 108, 0, 254, 108, 0, 255, 103, 0, 255, 103, 0, 
    255, 103, 0, 253, 106, 0, 244, 109, 0, 244, 107, 0, 255, 105, 0, 255, 99, 0, 255, 95, 0, 
    255, 94, 1, 255, 94, 6, 255, 96, 8, 255, 98, 11, 255, 99, 9, 255, 98, 8, 255, 99, 6, 
    255, 102, 8, 248, 105, 11, 241, 108, 15, 238, 109, 17, 

    255, 102, 1, 255, 102, 1, 255, 102, 1, 255, 103, 1, 255, 102, 1, 255, 103, 1, 255, 102, 1, 
    255, 103, 1, 255, 103, 1, 255, 103, 1, 255, 103, 1, 255, 103, 1, 255, 103, 1, 255, 103, 1, 
    255, 103, 1, 255, 99, 1, 255, 93, 1, 255, 92, 6, 255, 93, 18, 255, 93, 23, 255, 92, 26, 
    255, 95, 20, 242, 105, 11, 237, 108, 4, 244, 105, 0, 255, 100, 0, 255, 90, 5, 255, 91, 7, 
    254, 99, 6, 245, 108, 2, 239, 114, 0, 244, 112, 1, 255, 99, 15, 255, 96, 15, 255, 101, 1, 
    255, 102, 0, 255, 102, 9, 253, 103, 9, 255, 103, 2, 255, 103, 1, 255, 100, 6, 255, 99, 4, 
    255, 100, 0, 255, 101, 0, 255, 100, 9, 255, 100, 15, 

    255, 101, 1, 255, 101, 1, 255, 101, 1, 255, 101, 1, 255, 101, 1, 255, 101, 1, 255, 101, 1, 
    255, 101, 1, 255, 102, 1, 255, 102, 1, 255, 103, 2, 255, 103, 2, 255, 104, 3, 255, 105, 3, 
    255, 105, 4, 255, 104, 5, 255, 100, 11, 255, 97, 16, 255, 94, 19, 255, 93, 22, 255, 95, 21, 
    254, 102, 19, 239, 115, 15, 238, 120, 14, 248, 108, 10, 255, 100, 11, 255, 88, 16, 
    255, 89, 15, 255, 100, 11, 244, 107, 3, 240, 111, 0, 245, 107, 0, 255, 98, 18, 255, 96, 18, 
    255, 103, 1, 251, 106, 0, 248, 105, 9, 246, 106, 8, 248, 108, 0, 251, 107, 0, 255, 103, 4, 
    255, 101, 4, 255, 101, 0, 255, 100, 0, 255, 99, 4, 255, 98, 9, 

    255, 100, 2, 255, 100, 2, 255, 100, 2, 255, 100, 2, 255, 100, 2, 255, 100, 2, 255, 101, 2, 
    255, 101, 2, 255, 104, 4, 255, 104, 4, 255, 103, 3, 255, 102, 2, 255, 102, 2, 254, 102, 1, 
    254, 100, 0, 251, 101, 4, 250, 103, 10, 255, 101, 11, 255, 99, 7, 255, 100, 3, 255, 102, 0, 
    250, 105, 0, 229, 112, 0, 230, 109, 2, 247, 98, 4, 255, 91, 8, 255, 85, 8, 255, 89, 7, 
    255, 100, 3, 253, 105, 0, 253, 103, 0, 255, 100, 0, 255, 99, 9, 255, 99, 9, 255, 102, 1, 
    255, 104, 0, 249, 106, 4, 245, 108, 2, 242, 112, 0, 244, 111, 0, 248, 106, 4, 251, 105, 6, 
    253, 105, 0, 255, 105, 0, 255, 103, 1, 255, 102, 4, 

    255, 99, 4, 255, 99, 4, 255, 99, 4, 255, 100, 4, 255, 99, 4, 255, 100, 4, 255, 100, 4, 
    255, 101, 4, 255, 102, 5, 255, 101, 4, 255, 98, 1, 250, 96, 0, 246, 92, 0, 242, 90, 0, 
    240, 86, 0, 236, 86, 0, 235, 92, 0, 244, 96, 0, 255, 96, 0, 255, 97, 0, 255, 97, 0, 
    248, 92, 0, 222, 87, 0, 214, 78, 0, 229, 68, 0, 248, 67, 0, 255, 72, 0, 255, 81, 0, 
    255, 94, 3, 255, 99, 5, 255, 97, 9, 255, 98, 9, 255, 101, 13, 249, 104, 15, 249, 104, 13, 
    249, 105, 9, 248, 106, 6, 246, 108, 1, 245, 110, 0, 245, 110, 0, 248, 106, 6, 249, 105, 9, 
    251, 105, 2, 253, 105, 0, 255, 103, 0, 255, 103, 0, 

    255, 98, 7, 255, 98, 6, 255, 98, 6, 255, 99, 6, 255, 94, 2, 252, 89, 0, 252, 89, 0, 
    253, 93, 0, 254, 94, 0, 254, 95, 1, 255, 96, 2, 254, 97, 2, 253, 96, 1, 250, 96, 0, 
    250, 93, 0, 246, 93, 0, 234, 86, 0, 229, 71, 0, 249, 64, 0, 247, 51, 0, 222, 30, 0, 
    255, 73, 0, 255, 83, 0, 242, 72, 0, 241, 67, 0, 243, 64, 0, 242, 60, 0, 228, 41, 0, 
    219, 28, 0, 255, 83, 26, 246, 56, 8, 223, 52, 0, 197, 60, 0, 174, 54, 0, 185, 64, 0, 
    233, 105, 30, 239, 101, 10, 251, 105, 4, 255, 110, 10, 248, 94, 0, 249, 98, 7, 255, 111, 25, 
    239, 83, 0, 255, 105, 13, 255, 99, 0, 255, 100, 0, 

    237, 105, 43, 234, 104, 42, 235, 105, 43, 236, 106, 44, 234, 104, 42, 228, 100, 37, 
    229, 101, 38, 232, 104, 41, 214, 89, 25, 224, 99, 35, 235, 111, 47, 243, 119, 55, 
    244, 120, 56, 244, 122, 57, 246, 124, 59, 244, 130, 59, 228, 126, 44, 235, 132, 53, 
    220, 100, 37, 210, 84, 36, 208, 85, 54, 185, 62, 46, 193, 64, 58, 253, 126, 119, 
    196, 79, 61, 156, 47, 18, 181, 78, 37, 229, 120, 81, 215, 89, 64, 185, 62, 47, 178, 73, 70, 
    192, 114, 102, 156, 111, 72, 222, 190, 139, 232, 195, 150, 109, 49, 0, 203, 101, 26, 
    236, 106, 20, 250, 104, 19, 232, 90, 24, 188, 75, 43, 183, 83, 68, 185, 83, 68, 183, 70, 40, 
    203, 68, 13, 247, 100, 33, 

    176, 117, 113, 174, 116, 112, 176, 118, 114, 178, 120, 116, 177, 119, 115, 175, 117, 113, 
    176, 118, 114, 178, 123, 118, 178, 123, 118, 174, 119, 114, 163, 108, 103, 150, 97, 91, 
    146, 93, 87, 162, 109, 103, 190, 137, 131, 203, 166, 148, 192, 185, 141, 174, 190, 145, 
    168, 193, 172, 180, 214, 226, 10, 45, 101, 87, 110, 206, 64, 61, 192, 100, 92, 229, 
    84, 94, 209, 92, 118, 202, 105, 143, 190, 108, 147, 176, 147, 170, 204, 74, 112, 151, 
    89, 171, 219, 34, 144, 181, 110, 227, 237, 115, 232, 223, 76, 187, 168, 130, 191, 157, 
    157, 124, 71, 216, 124, 57, 226, 108, 36, 207, 113, 87, 142, 120, 193, 54, 69, 186, 
    48, 66, 168, 78, 66, 150, 155, 87, 144, 190, 95, 139, 

    91, 67, 91, 89, 65, 89, 90, 66, 90, 93, 69, 93, 93, 69, 93, 92, 68, 92, 92, 70, 93, 
    95, 73, 96, 96, 74, 97, 95, 73, 96, 92, 68, 92, 85, 63, 86, 83, 61, 84, 93, 71, 94, 
    113, 91, 114, 119, 113, 123, 85, 113, 91, 133, 192, 174, 82, 171, 185, 4, 111, 165, 
    0, 108, 211, 0, 58, 198, 13, 72, 238, 0, 40, 205, 11, 77, 217, 47, 127, 240, 0, 95, 181, 
    32, 134, 208, 33, 129, 206, 13, 135, 212, 0, 168, 243, 12, 203, 255, 0, 143, 162, 
    44, 194, 192, 86, 226, 213, 123, 220, 209, 91, 110, 116, 155, 123, 124, 199, 145, 119, 
    188, 145, 154, 43, 40, 143, 10, 34, 166, 49, 83, 180, 21, 42, 125, 57, 44, 134, 99, 70, 163, 


    95, 70, 92, 91, 66, 88, 93, 65, 88, 95, 67, 90, 96, 68, 91, 94, 66, 89, 95, 67, 90, 
    98, 70, 93, 84, 56, 79, 88, 60, 83, 94, 64, 88, 94, 66, 89, 91, 63, 86, 85, 57, 80, 
    78, 50, 73, 70, 50, 61, 92, 90, 78, 43, 72, 70, 5, 73, 118, 22, 115, 195, 23, 124, 232, 
    27, 117, 231, 56, 114, 211, 40, 83, 162, 28, 74, 134, 0, 51, 114, 0, 64, 144, 18, 97, 190, 
    19, 114, 220, 2, 128, 228, 0, 159, 235, 37, 205, 242, 70, 177, 167, 124, 193, 162, 
    52, 106, 80, 61, 109, 129, 8, 60, 162, 14, 64, 179, 139, 181, 239, 31, 41, 77, 83, 30, 84, 
    180, 109, 139, 192, 142, 109, 234, 217, 189, 158, 185, 232, 2, 52, 137, 

    183, 122, 117, 178, 117, 112, 178, 115, 110, 180, 117, 112, 181, 118, 113, 179, 116, 111, 
    179, 116, 111, 181, 118, 113, 191, 126, 122, 190, 125, 121, 191, 126, 122, 196, 131, 127, 
    205, 137, 134, 207, 139, 136, 205, 137, 134, 199, 136, 127, 173, 116, 96, 249, 217, 204, 
    181, 186, 205, 123, 151, 190, 64, 97, 150, 88, 110, 159, 206, 203, 230, 181, 160, 175, 
    255, 230, 238, 133, 110, 126, 235, 230, 255, 88, 111, 165, 83, 137, 201, 57, 137, 198, 
    59, 159, 208, 87, 166, 183, 202, 217, 184, 99, 76, 24, 165, 128, 84, 207, 177, 185, 
    56, 58, 159, 43, 62, 180, 53, 75, 133, 235, 218, 236, 186, 84, 80, 227, 96, 50, 243, 136, 30, 
    189, 124, 30, 131, 128, 121, 117, 145, 182, 

    238, 109, 51, 232, 103, 45, 229, 100, 42, 231, 102, 44, 234, 104, 46, 232, 102, 44, 
    234, 101, 44, 236, 103, 46, 248, 113, 57, 243, 108, 52, 237, 102, 46, 232, 97, 41, 
    227, 90, 35, 217, 80, 25, 203, 66, 11, 192, 55, 1, 201, 66, 18, 226, 102, 50, 229, 127, 61, 
    226, 135, 64, 223, 130, 63, 221, 116, 58, 231, 108, 66, 244, 103, 73, 227, 68, 46, 
    197, 48, 24, 154, 42, 5, 202, 134, 89, 232, 210, 161, 193, 179, 144, 242, 195, 189, 
    158, 81, 71, 148, 43, 0, 240, 117, 50, 211, 76, 10, 215, 80, 35, 195, 67, 66, 178, 57, 62, 
    187, 72, 45, 187, 63, 11, 255, 108, 38, 250, 97, 3, 255, 126, 3, 244, 120, 6, 204, 100, 29, 
    193, 98, 50, 

    255, 101, 10, 255, 96, 5, 251, 92, 1, 253, 94, 3, 255, 95, 5, 255, 95, 5, 255, 95, 5, 
    255, 95, 6, 255, 91, 2, 250, 86, 0, 246, 81, 0, 246, 81, 0, 250, 85, 0, 255, 91, 2, 
    255, 93, 5, 255, 92, 11, 255, 88, 18, 253, 86, 8, 255, 100, 0, 249, 96, 0, 239, 80, 0, 
    247, 81, 0, 238, 62, 0, 222, 30, 0, 227, 20, 0, 255, 72, 15, 231, 69, 0, 198, 74, 0, 
    168, 82, 0, 174, 88, 1, 169, 39, 0, 191, 36, 0, 255, 102, 34, 255, 91, 10, 255, 98, 15, 
    255, 95, 15, 251, 67, 3, 255, 80, 15, 255, 85, 9, 255, 96, 6, 255, 91, 0, 255, 110, 0, 
    248, 94, 0, 254, 101, 0, 245, 92, 0, 255, 103, 10, 

    255, 104, 6, 255, 104, 6, 255, 103, 5, 255, 102, 4, 255, 101, 3, 253, 99, 1, 253, 99, 1, 
    254, 97, 0, 255, 101, 4, 255, 101, 4, 255, 101, 4, 255, 101, 4, 255, 101, 4, 255, 101, 4, 
    255, 101, 4, 255, 100, 6, 250, 86, 0, 245, 80, 0, 232, 73, 0, 226, 67, 0, 230, 69, 0, 
    240, 77, 0, 254, 86, 0, 255, 93, 6, 255, 95, 12, 255, 94, 10, 255, 96, 4, 245, 99, 0, 
    235, 103, 2, 237, 104, 9, 248, 102, 19, 255, 101, 19, 255, 101, 9, 255, 101, 6, 255, 101, 4, 
    255, 100, 4, 255, 99, 9, 255, 99, 9, 255, 100, 6, 255, 101, 2, 255, 102, 0, 255, 103, 0, 
    255, 104, 0, 255, 104, 0, 255, 104, 1, 255, 103, 2, 

    255, 105, 2, 255, 105, 2, 255, 104, 1, 255, 104, 1, 254, 103, 0, 253, 102, 0, 252, 101, 0, 
    252, 101, 0, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 
    255, 104, 1, 255, 103, 1, 255, 104, 6, 255, 103, 5, 255, 100, 2, 255, 99, 1, 255, 99, 1, 
    255, 102, 4, 255, 106, 8, 255, 109, 10, 255, 102, 3, 255, 101, 2, 255, 99, 0, 255, 99, 0, 
    255, 99, 0, 255, 100, 0, 255, 102, 2, 255, 103, 3, 255, 104, 1, 255, 104, 1, 255, 104, 1, 
    255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 
    255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 

    255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 254, 103, 0, 254, 103, 0, 254, 103, 0, 
    254, 103, 0, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 
    255, 104, 1, 255, 103, 1, 255, 100, 1, 255, 100, 2, 255, 101, 3, 255, 101, 3, 255, 101, 3, 
    255, 100, 2, 255, 99, 1, 255, 99, 0, 255, 103, 4, 255, 103, 3, 255, 103, 3, 255, 102, 2, 
    255, 102, 2, 255, 103, 3, 255, 103, 3, 255, 104, 4, 255, 104, 1, 255, 104, 1, 255, 104, 1, 
    255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 
    255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 

    255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 
    255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 
    255, 104, 1, 255, 103, 1, 255, 103, 4, 255, 102, 3, 255, 99, 1, 255, 99, 0, 255, 98, 0, 
    255, 99, 0, 255, 100, 1, 255, 101, 1, 255, 103, 3, 255, 103, 3, 255, 104, 4, 255, 104, 4, 
    255, 104, 4, 255, 104, 4, 255, 103, 3, 255, 103, 3, 255, 104, 1, 255, 104, 1, 255, 104, 1, 
    255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 
    255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 

    254, 103, 0, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 105, 2, 255, 105, 2, 
    255, 105, 2, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 
    255, 104, 1, 255, 103, 1, 255, 106, 6, 255, 105, 5, 255, 102, 3, 255, 102, 2, 255, 102, 2, 
    255, 103, 3, 255, 105, 5, 255, 106, 6, 255, 102, 2, 255, 102, 2, 255, 103, 3, 255, 104, 3, 
    255, 104, 3, 255, 104, 3, 255, 103, 2, 255, 103, 2, 255, 104, 1, 255, 104, 1, 255, 104, 1, 
    255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 
    255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 

    255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 105, 2, 255, 105, 2, 255, 105, 2, 
    255, 105, 2, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 
    255, 104, 1, 255, 104, 1, 251, 97, 0, 252, 98, 0, 255, 100, 0, 255, 101, 1, 255, 102, 2, 
    255, 101, 1, 254, 100, 0, 253, 99, 0, 255, 101, 1, 255, 103, 2, 255, 103, 2, 255, 103, 2, 
    255, 103, 2, 255, 103, 2, 255, 103, 2, 254, 102, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 
    255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 
    255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 

    255, 105, 2, 255, 105, 2, 255, 105, 2, 255, 105, 2, 255, 105, 2, 255, 105, 2, 255, 105, 2, 
    255, 105, 2, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 
    255, 104, 1, 255, 104, 1, 253, 102, 0, 255, 104, 1, 255, 105, 3, 255, 108, 5, 255, 108, 5, 
    255, 107, 4, 255, 105, 2, 254, 103, 0, 255, 104, 1, 253, 104, 1, 253, 104, 1, 253, 104, 1, 
    253, 104, 1, 253, 104, 1, 253, 104, 1, 253, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 
    255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1, 
    255, 104, 1, 255, 104, 1, 255, 104, 1, 255, 104, 1 
};

/* Sub-tests for test_mgr_old() */
static void test_mgr_old_a(int flag)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */
    char oldrlefile[512] = "";
    char *srcdir = getenv("srcdir");

    /* Generate the correct name for the test file, by prepending the source path */
    if (srcdir && ((strlen(srcdir) + strlen(OLDRLEFILE) + 1) < sizeof(oldrlefile))) {
        strcpy(oldrlefile, srcdir);
        strcat(oldrlefile, "/");
    }
    strcat(oldrlefile, OLDRLEFILE);

/* A - Read RLE compressed data from old raster image file */
    MESSAGE(8, printf("Read RLE compressed image\n"););

    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(oldrlefile,DFACC_RDWR,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={10,10};    /* dimensions for the empty image */
        uint8 image[10][10]; /* space for the image data */
        uint8 image0[10][10]; /* space for the image data */
        int32 start[2]; /* start of image data to grab */
        int32 stride[2];/* stride of image data to grab */
        intn i,j;       /* indices */

        /* Initialize data we are looking for in image */
        for (i = 0; i < 10; i++)
            for (j = 0; j < 10; j++)
                image0[i][j] = (uint8) (i + j);

        /* Get the first image in this file */
        riid=GRselect(grid,0);
        CHECK(riid,FAIL,"GRselect");

        /* Read the whole image in */
        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        /* Verify correct image contents */
        if(HDmemcmp(image,image0,10*10)!=0) {
            MESSAGE(3, printf("Error reading data for RLE compressed image\n"););
            num_errs++;
        } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
} /* end test_mgr_old_a() */

static void test_mgr_old_c(int flag)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */
    char oldgreyjpegfile[512] = "";
    char *srcdir = getenv("srcdir");

    /* Generate the correct name for the test file, by prepending the source path */
    if (srcdir && ((strlen(srcdir) + strlen(OLDGREYJPEGFILE) + 1) < sizeof(oldgreyjpegfile))) {
        strcpy(oldgreyjpegfile, srcdir);
        strcat(oldgreyjpegfile, "/");
    }
    strcat(oldgreyjpegfile, OLDGREYJPEGFILE);

/* C - Read 8-bit JPEG compressed data from old raster image file */
    MESSAGE(8, printf("Read 8-bit JPEG compressed image\n"););

    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(oldgreyjpegfile,DFACC_READ,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={JPEGX,JPEGY};    /* dimensions for the empty image */
        uint8 image[JPEGY][JPEGX]; /* space for the image data */
        int32 start[2]; /* start of image data to grab */
        int32 stride[2];/* stride of image data to grab */

        /* Get the first image in this file */
        riid=GRselect(grid,0);
        CHECK(riid,FAIL,"GRselect");

        /* Read the whole image in */
        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        /* Verify correct image contents */
        if(HDmemcmp(image,jpeg_8bit_j80,JPEGY*JPEGX)!=0) {
            MESSAGE(3, printf("Error reading data for 8-bit JPEG compressed image\n"););
            num_errs++;
        } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
} /* end test_mgr_old_c() */

static void test_mgr_old_e(int flag)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */
    char oldjpegfile[512] = "";
    char *srcdir = getenv("srcdir");

    /* Generate the correct name for the test file, by prepending the source path */
    if (srcdir && ((strlen(srcdir) + strlen(OLDJPEGFILE) + 1) < sizeof(oldjpegfile))) {
        strcpy(oldjpegfile, srcdir);
        strcat(oldjpegfile, "/");
    }
    strcat(oldjpegfile, OLDJPEGFILE);

/* E - Read 24-bit JPEG compressed data from old raster image file */
    MESSAGE(8, printf("Read 24-bit JPEG compressed image\n"););

    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(oldjpegfile,DFACC_READ,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={JPEGX,JPEGY};    /* dimensions for the empty image */
        uint8 image[JPEGY][JPEGX][3]; /* space for the image data */
        int32 start[2]; /* start of image data to grab */
        int32 stride[2];/* stride of image data to grab */

        /* Get the first image in this file */
        riid=GRselect(grid,0);
        CHECK(riid,FAIL,"GRselect");

        /* Read the whole image in */
        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        /* Verify correct image contents */
        if(HDmemcmp(image,jpeg_24bit_j80,JPEGY*JPEGX*3)!=0) {
            MESSAGE(3, printf("Error reading data for 24-bit JPEG compressed image\n"););
            num_errs++;
        } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
} /* end test_mgr_old_e() */

/****************************************************************
**
**  test_mgr_old(): Multi-file Raster Old-style Image Access tests
** 
**  VIII. Old-style raster image tests
**      A. Read data from RLE compressed image
**      B. Create RLE compressed image & write to it (not implemented)
**      C. Read data from 8-bit JPEG compressed image
**      D. Create 8-bit JPEG compressed image & write to it
**      E. Read data from 24-bit JPEG compressed image
**      F. Create 24-bit JPEG compressed image & write to it
** 
****************************************************************/
static void
test_mgr_old(int flag)
{
    /* Output message about test being performed */
    MESSAGE(6, printf("Testing Multi-file Raster Old-Style Access\n"););
    test_mgr_old_a(flag);
#ifdef NOT_IMPLEMENTED
    test_mgr_old_b(flag);
#endif
    test_mgr_old_c(flag);
#ifdef NOT_IMPLEMENTED
    test_mgr_old_d(flag);
#endif
    test_mgr_old_e(flag);
#ifdef NOT_IMPLEMENTED
    test_mgr_old_f(flag);
#endif /* NOT_YET */

}   /* end test_mgr_old() */

#define GZIPFILE    "gr_gzip.hdf"
#define JPEGFILE    "gr_jpeg.hdf"

/* Sub-tests for test_mgr_compress():
   - test_mgr_compress_a: Create/Write/Read gzip compressed image
   - test_mgr_compress_b: Create/Write/Read 8-bit JPEG compressed image
   - test_mgr_compress_c: Create/Read/Write 24-bit JPEG compressed Image
   - test_get_compress:   Retrieve various compression information of 
				compressed Image 
*/
static void test_mgr_compress_a(int flag)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */

/* A - Create/Write/Read gzip compressed image */
    MESSAGE(8, printf("Operate on gzip compressed images\n"););

    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(GZIPFILE,DFACC_ALL,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
        int32 riid;     /* RI ID for the new image */
        comp_coder_t comp_type;            /* Compression method */
        comp_info cinfo;            /* Compression parameters */
        int32 dims[2]={10,10};    /* dimensions for the empty image */
        uint8 image[10][10]; /* space for the image data */
        uint8 image0[10][10]; /* space for the image data */
        int32 start[2];     /* start of image data to grab */
        int32 stride[2];    /* stride of image data to grab */
        int32 count[2];     /* Size of image data to operate on */
        intn i,j;       /* indices */

        /* Initialize data we are going to write out */
        for (i = 0; i < 10; i++)
            for (j = 0; j < 10; j++)
                image0[i][j] = (uint8) (i + j);

        /* Get the first image in this file */
        riid=GRcreate(grid,"image1",1,DFNT_UINT8,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Set the compression method for the image */
        comp_type=COMP_CODE_DEFLATE;
        cinfo.deflate.level=7;
        ret=GRsetcompress(riid,comp_type,&cinfo);
        CHECK(ret,FAIL,"GRsetcompress");

        /* Write the whole image out */
        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRwriteimage(riid,start,stride,dims,image0);
        CHECK(ret,FAIL,"GRreadimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Check that the image made it out correctly */

        /* Get the first image in this file */
        riid=GRselect(grid,0);
        CHECK(riid,FAIL,"GRselect");

        /* Read the whole image in */
        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        /* Verify correct image contents */
        if(HDmemcmp(image,image0,10*10)!=0) {
            MESSAGE(3, printf("Error reading data for gzip compressed image\n"););
            num_errs++;
        } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Check for compressing image in the middle of writing data */

        /* Get the first image in this file */
        riid=GRcreate(grid,"image2",1,DFNT_UINT8,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Write half of the image out */
        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        count[0]=10; count[1]=5;
        ret=GRwriteimage(riid,start,stride,count,image0);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Set the compression method for the image */
        comp_type=COMP_CODE_DEFLATE;
        cinfo.deflate.level=7;
        ret=GRsetcompress(riid,comp_type,&cinfo);
        CHECK(ret,FAIL,"GRsetcompress");

        /* Write the second half of the image out */
        start[0]=0; start[1]=5;
        stride[0]=stride[1]=1;
        count[0]=10; count[1]=5;
        ret=GRwriteimage(riid,start,stride,count,&image0[5][0]);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Check that the image made it out correctly */
        HDmemset(image,0,10*10);

        /* Get the second image in this file */
        riid=GRselect(grid,1);
        CHECK(riid,FAIL,"GRselect");

        /* Read the whole image in */
        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        /* Verify correct image contents */
        if(HDmemcmp(image,image0,10*10)!=0) {
            MESSAGE(3, printf("Error reading 2nd data for gzip compressed image\n"););
            num_errs++;
        } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
} /* end test_mgr_compress_a() */

static void test_mgr_compress_b(int flag)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */

/* B - Create/Write/Read JPEG compressed image */
    MESSAGE(8, printf("Operate on 8-bit JPEG compressed images\n"););

    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(JPEGFILE,DFACC_ALL,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
        int32 riid;     /* RI ID for the new image */
        comp_coder_t comp_type;            /* Compression method */
        comp_info cinfo;            /* Compression parameters */
        int32 dims[2]={10,10};    /* dimensions for the empty image */
        int32 dims_out[2];          /* Buffer for retrieving dimensions */
        uint8 image[10][10]; /* space for the image data */
        uint8 image0[10][10]; /* space for the image data */
        int32 start[2];     /* start of image data to grab */
        int32 stride[2];    /* stride of image data to grab */
        int32 n_images, n_file_attrs;   /* File information variables */
        int32 interlace_mode, n_comps, n_attrs, datatype; /* Image information */
        char name[30];      /* Buffer for retrieving image name */
        uint8 attr;         /* Attribute data */
#ifdef NOT_IMPLEMENTED
        int32 count[2];     /* Size of image data to operate on */
#endif /* NOT_IMPLEMENTED */
        intn i,j;       /* indices */

        /* Initialize data we are going to write out */
        for (i = 0; i < 10; i++)
            for (j = 0; j < 10; j++)
                image0[i][j] = (uint8) (i + j);

        /* Get the first image in this file */
        riid=GRcreate(grid,"image1",1,DFNT_UINT8,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        attr = 100;
        ret=GRsetattr(riid, "JPEG_quality", DFNT_UINT8, 1, &attr);
        CHECK(ret,FAIL,"GRsetattr");

        /* Set the compression method for the image */
        comp_type=COMP_CODE_JPEG;
        cinfo.jpeg.quality=100;
        cinfo.jpeg.force_baseline=1;
        ret=GRsetcompress(riid,comp_type,&cinfo);
        CHECK(ret,FAIL,"GRsetcompress");

        /* Write the whole image out */
        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRwriteimage(riid,start,stride,dims,image0);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Close the interface */
        ret = GRend (grid);
        CHECK(ret,FAIL,"GRend");

        /* Check that the image made it out correctly */

        /* Re-open the interface */
        grid = GRstart (fid);
        CHECK(grid,FAIL,"GRstart");

        /* Get the file information */
        ret = GRfileinfo (grid, &n_images, &n_file_attrs);
        CHECK(ret,FAIL,"GRfileinfo");
        if (n_images != 1) {
            MESSAGE(3, printf("Wrong number of images found!\n"););
            num_errs++;
        }

        /* Check the name of the image in the file */
        ret = GRnametoindex (grid, "image1");
        CHECK(ret,FAIL,"GRnametoindex");

        /* Get the first image in this file */
        riid=GRselect(grid,0);
        CHECK(riid,FAIL,"GRselect");

        /* Get the attribute information */
        ret = GRgetiminfo(riid, name, &n_comps, &datatype, &interlace_mode, dims_out, &n_attrs);
        CHECK(ret,FAIL,"GRgetiminfo");
        if (n_attrs != 1) {
            MESSAGE(3, printf("Wrong number of attributes!\n"););
            num_errs++;
        }

        /* Read the whole image in */
        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        /* Verify correct image contents */
        if(HDmemcmp(image,image0,10*10)!=0) {
            MESSAGE(3, printf("Error reading data for gzip compressed image\n"););
            num_errs++;
        } /* end if */

        /* Close the image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

#ifdef NOT_IMPLEMENTED
        /* Check for compressing image in the middle of writing data */

        /* Get the first image in this file */
        riid=GRcreate(grid,"image2",1,DFNT_UINT8,MFGR_INTERLACE_PIXEL,dims);
        CHECK(riid,FAIL,"GRcreate");

        /* Write half of the image out */
        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        count[0]=10; count[1]=5;
        ret=GRwriteimage(riid,start,stride,count,image0);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Set the compression method for the image */
        comp_type=COMP_CODE_JPEG;
        cinfo.jpeg.quality=100;
        cinfo.jpeg.force_baseline=1;
        ret=GRsetcompress(riid,comp_type,&cinfo);
        CHECK(ret,FAIL,"GRsetcompress");

        /* Write the second half of the image out */
        start[0]=0; start[1]=5;
        stride[0]=stride[1]=1;
        count[0]=10; count[1]=5;
        ret=GRwriteimage(riid,start,stride,count,&image0[5][0]);
        CHECK(ret,FAIL,"GRwriteimage");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");

        /* Check that the image made it out correctly */
        HDmemset(image,0,10*10);

        /* Get the second image in this file */
        riid=GRselect(grid,1);
        CHECK(riid,FAIL,"GRselect");

        /* Read the whole image in */
        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRreadimage(riid,start,stride,dims,image);
        CHECK(ret,FAIL,"GRreadimage");

        /* Verify correct image contents */
        if(HDmemcmp(image,image0,10*10)!=0) {
            MESSAGE(3, printf("Error reading 2nd data for gzip compressed image\n"););
            num_errs++;
        } /* end if */

        /* Close the image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
#endif /* NOT_IMPLEMENTED */
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
} /* end test_mgr_compress_b() */

static void test_mgr_compress_c(int flag)
{
    intn  status;         /* status for functions returning an intn */
    int32 file_id,        /* HDF file identifier */
          gr_id,          /* GR interface identifier */
          ri_id,          /* raster image identifier */
          start[2],       /* start position to write for each dimension */
          edges[2],       /* number of elements to be written along each dimension */
          dim_sizes[2],   /* dimension sizes of the image array */
          interlace_mode, /* interlace mode of the image */
          data_type,      /* data type of the image data */
          i, j;
    uint8 image_buf[128][128][3];
    uint8 read_buf[128][128][3];
    comp_info  c_info;
    char gname[60];
    int32 nc, dt, im, dims[2], na;
 
    /* ---- End of variable declaration --- */
 
    /* Create and open the file and initialize GR interface */
    /* ---------------------------------------------------- */
    file_id = Hopen (JPEGFILE, DFACC_CREATE, 0);
    CHECK(file_id,FAIL,"Hopen");

    gr_id = GRstart (file_id);
    CHECK(gr_id,FAIL,"GRstart");
 
    /* Set data type, interlace mode, and dimensions of image */
    /* ------------------------------------------------------ */
    data_type = DFNT_UINT8;
    interlace_mode = MFGR_INTERLACE_PIXEL;
    dim_sizes[0] = 128;
    dim_sizes[1] = 128;
 
    /* Create the raster image array */
    /* ----------------------------- */
    ri_id = GRcreate (gr_id, "24-bit JPEG", 3, data_type, interlace_mode, dim_sizes);
    CHECK(ri_id,FAIL,"GRcreate");
 
    /* Set JPEG compression */
    /* -------------------- */
    c_info.jpeg.quality=75;
    c_info.jpeg.force_baseline=1;
    status = GRsetcompress (ri_id, COMP_CODE_JPEG, &c_info);
    CHECK(status,FAIL,"GRsetcompress");
 
    /* Fill the image data buffer with values */
    /* -------------------------------------- */
    for (i = 0; i < 128; i++)
       for (j = 0; j < 128; j++)
       {
          image_buf[i][j][0] = (i+j) + 1;    
          image_buf[i][j][1] = (i+j) + 1;    
          image_buf[i][j][2] = (i+j) + 1;    
       }
 
    /* Write data in the buffer into the image array */
    /* --------------------------------------------- */
    start[0] = start[1] = 0;
    edges[0] = 128;
    edges[1] = 128;
 
    status = GRwriteimage(ri_id, start, NULL, edges, (VOIDP)image_buf);
    CHECK(status,FAIL,"GRwriteimage");
 
    /* Terminate access to raster image and to GR */
    /* interface and, close the HDF file.         */
    /* ------------------------------------------ */
    status = GRendaccess (ri_id);
    CHECK(status,FAIL,"GRendaccess");

    status = GRend (gr_id);
    CHECK(status,FAIL,"GRend");
 
    /* Read back data and print */
    /* ------------------------ */
    gr_id = GRstart (file_id);
    CHECK(gr_id,FAIL,"GRstart");
 
    ri_id = GRselect (gr_id, 0);
    CHECK(ri_id,FAIL,"GRselect");
 
    status = GRgetiminfo (ri_id, gname, &nc, &dt, &im, dims, &na);
    CHECK(status,FAIL,"GRreadimage");
    VERIFY(nc,3,"GRgetiminfo");
    VERIFY(dt,DFNT_UINT8,"GRgetiminfo");
    VERIFY(dim_sizes[0],dims[0],"GRgetiminfo");
    VERIFY(dim_sizes[1],dims[1],"GRgetiminfo");
    VERIFY(na,0,"GRgetiminfo");
 
    start[0] = start[1] = 0;
    edges[0] = 128;
    edges[1] = 128;
    status = GRreadimage (ri_id, start, NULL, edges, (VOIDP)read_buf);
    CHECK(status,FAIL,"GRreadimage");
 
  /* Compare data read in */
    /* Verify correct image contents */
    if(fuzzy_memcmp(image_buf,read_buf,128*128*3,JPEG_FUZZ)!=0) {
        MESSAGE(3, printf("Error reading data for 24-big JPEG compressed image\n"););
        num_errs++;
    } /* end if */

   
    /* Close all interfaces */
    /* -------------------- */
    status = GRendaccess (ri_id);
    CHECK(status,FAIL,"GRendaccess");

    status = GRend (gr_id);
    CHECK(status,FAIL,"GRend");

    status = Hclose (file_id);
    CHECK(status,FAIL,"Hclose");
 
} /* end test_mgr_compress_c() */

/*--------------------------------------------------------------------------
    The following 2 routines are added when bug# 307 was fixed:

    - test_get_compress: tests the new functionality, getting compression
                information of compressed image data.  The test
        + creates a file and four compressed images written to the file,
          then closes the file.
        + re-opens the file, then reads and verifies each image's
          compression information
        The four images are created using the following compression
        methods in that order: RLE, Skipping Huffman, Deflate, and JPEG.
        For simplicity, all four images use the same data sample.

    - make_comp_image: is a helper that test_get_compress uses to create
                several compressed images.

 -BMR (Sept 7, 01)
--------------------------------------------------------------------------*/

#define	COMPFILE	"gr_comp.hdf"
#define	RLE_IMAGE	"Image with RLE Compression"
#define	DEFLATE_IMAGE	"Image with Deflate Compression"
#define	SKPHUFF_IMAGE	"Image with Skphuff Compression"
#define	JPEG_IMAGE	"Image with JPEG Compression"
#define	DEFLATE_LEVEL		7  /* arbitrary */
#define	SKPHUFF_SKIPSIZE	28  /* arbitrary */

intn make_comp_image( 
		int32 grid,
		char* img_name,
		comp_coder_t comp_type,    /* Compression method */
		comp_info* cinfo)    /* Compression parameters */
{
    int32 riid;         /* RI ID of the working image */
    int32 dims[2]={10,10};	/* dimensions for the empty image */
    uint8 image_data[10][10];	/* space for the image data */
    int32 start[2];		/* start of image data to grab */
    int32 stride[2];	/* stride of image data to grab */
    intn i,j;		/* indices */
    intn ret_value;        /* generic return value */

    /* Initialize data we are going to write out */
    for (i = 0; i < 10; i++)
        for (j = 0; j < 10; j++)
            image_data[i][j] = (uint8) (i + j + 10);

    /* Create the image */
    riid = GRcreate(grid, img_name, 1, DFNT_UINT8, MFGR_INTERLACE_PIXEL, dims);
    CHECK(riid, FAIL, "GRcreate");

    /* Set the compression as provided */
    ret_value = GRsetcompress(riid, comp_type, cinfo);
    CHECK(ret_value, FAIL, "GRsetcompress");

    /* Write the image out */
    start[0] = start[1] = 0;
    stride[0] = stride[1] = 1;
    ret_value = GRwriteimage(riid, start, stride, dims, image_data);
    CHECK(ret_value, FAIL, "GRwriteimage");

    /* Close the image */
    ret_value = GRendaccess(riid);
    CHECK(ret_value, FAIL, "GRendaccess");

    return ret_value;
}

static void test_get_compress(int flag)
{
    int32 fid;          /* HDF file ID */
    int32 grid;         /* GRID for the interface */
    int32 riid;     	/* RI ID of the working image */
    comp_coder_t comp_type;    /* Compression method */
    comp_info cinfo;    /* Compression parameters - union */
    intn status;        /* generic return value */

/* D - Retrieve compression information of compressed images */
    MESSAGE(8, printf("Verify the compression information of compressed images\n"););

    /*
     * Create a new file and several images with different compression
     * schemes then close the images and the file
     */

    /* Create an hdf file, and initiate the GR interface */
    fid = Hopen(COMPFILE, DFACC_CREATE, 0);
    CHECK(fid, FAIL, "Hopen");

    grid = GRstart(fid);
    CHECK(grid, FAIL, "GRstart");

    /* Create and write 4 images, with RLE, deflate, skipping huffman,
       and JPEG compression methods. */

    /* No compression info for the RLE image */
    HDmemset(&cinfo, 0, sizeof(cinfo)) ;

    /* Create and write the first compressed image in this file */
    make_comp_image(grid, RLE_IMAGE, COMP_CODE_RLE, &cinfo);

    /* Set the compression info for the second image with skipping 
       huffman method */
    HDmemset(&cinfo, 0, sizeof(cinfo)) ;
    cinfo.skphuff.skp_size = SKPHUFF_SKIPSIZE;

    /* Create and write the second compressed image in this file */
    make_comp_image(grid, SKPHUFF_IMAGE, COMP_CODE_SKPHUFF, &cinfo);

    /* Set the compression info for the third image with deflate method */
    HDmemset(&cinfo, 0, sizeof(cinfo)) ;
    cinfo.deflate.level = DEFLATE_LEVEL;

    /* Create and write the third compressed image in this file */
    make_comp_image(grid, DEFLATE_IMAGE, COMP_CODE_DEFLATE, &cinfo);

    /* Set the compression method for the fourth image */
    HDmemset(&cinfo, 0, sizeof(cinfo)) ;
    cinfo.jpeg.quality = 100;	/* won't be able to retrieved anyway */
    cinfo.jpeg.force_baseline = 1;

    /* Create and write the fourth compressed image in this file */
    make_comp_image(grid, JPEG_IMAGE, COMP_CODE_JPEG, &cinfo);

    /* Terminate access to the GR interface and close the file */
    status = GRend(grid);
    CHECK(status, FAIL, "GRend");
    status = Hclose(fid);
    CHECK(status, FAIL, "Hclose");

    /*
     * Re-open the file COMPFILE, and retrieve the compression information
     * of its two images 
     */
    fid = Hopen(COMPFILE, DFACC_READ, 0);
    CHECK(fid, FAIL, "Hopen");

    grid = GRstart(fid);
    CHECK(grid, FAIL, "GRstart");

    /* get access to the first image */
    riid = GRselect(grid, 0);
    CHECK(riid, FAIL, "GRselect");

    /* First image uses RLE compression method, so no info will be
       retrieved */
    status = GRgetcompress(riid, &comp_type, &cinfo);
    CHECK(status, FAIL, "GRsetcompress");
    VERIFY(comp_type, COMP_CODE_RLE, "GRgetcompress");

    /* end access to the first image */
    status = GRendaccess(riid);
    CHECK(status, FAIL, "GRendaccess");

    /* get the compression info of the second image, and then check 
     * the values against the values set earlier, which are:
     *		comp_type = COMP_CODE_SKPHUFF 
     *		skp_size = SKPHUFF_SKIPSIZE
     */

    /* get access to the second image */
    riid = GRselect(grid, 1);
    CHECK(riid, FAIL, "GRselect");

    comp_type = COMP_CODE_INVALID;  /* reset variables before retrieving info */
    HDmemset(&cinfo, 0, sizeof(cinfo)) ;
    status = GRgetcompress(riid, &comp_type, &cinfo);
    CHECK(status, FAIL, "GRsetcompress");
    VERIFY(comp_type, COMP_CODE_SKPHUFF, "GRgetcompress");
    VERIFY(cinfo.skphuff.skp_size, SKPHUFF_SKIPSIZE, "GRgetcompress");

    /* end access to the second image */
    status = GRendaccess(riid);
    CHECK(status, FAIL, "GRendaccess");

    /* get the compression info of the third image, and then check 
       the values against the values set earlier, which are:
		comp_type = COMP_CODE_DEFLATE 
		level = DEFLATE_LEVEL
    */

    /* get access to the third image */
    riid = GRselect(grid, 2);
    CHECK(riid, FAIL, "GRselect");

    comp_type = COMP_CODE_INVALID;  /* reset variables before retrieving info */
    HDmemset(&cinfo, 0, sizeof(cinfo)) ;
    status = GRgetcompress(riid, &comp_type, &cinfo);
    CHECK(status, FAIL, "GRsetcompress");
    VERIFY(comp_type, COMP_CODE_DEFLATE, "GRgetcompress");
    VERIFY(cinfo.deflate.level, DEFLATE_LEVEL, "GRgetcompress");

    /* Terminate access to the third image */
    status = GRendaccess(riid);
    CHECK(status, FAIL, "GRendaccess");

    /* get access to the fourth image */
    riid = GRselect(grid, 3);
    CHECK(riid, FAIL, "GRselect");

    /* get the compression info of the second image, but only check 
       the compression type value against that being set earlier 
       ('quality' and 'force_baseline' are currently not retrievable) */
    comp_type = COMP_CODE_INVALID;  /* reset variables before retrieving info */
    HDmemset(&cinfo, 0, sizeof(cinfo)) ;
    status = GRgetcompress(riid, &comp_type, &cinfo);
    CHECK(status, FAIL, "GRgetcompress");
    VERIFY(comp_type, COMP_CODE_JPEG, "GRgetcompress");
    VERIFY(cinfo.jpeg.quality, 0, "GRgetcompress");
    VERIFY(cinfo.jpeg.force_baseline, 0, "GRgetcompress");

    /* Terminate access to the third image */
    status = GRendaccess(riid);
    CHECK(status, FAIL, "GRendaccess");

    /* Terminate access and close the file */
    status = GRend(grid);
    CHECK(status, FAIL, "GRend");
    status = Hclose(fid);
    CHECK(status, FAIL, "Hclose");

} /* end test_get_compress */

/*--------------------------------------------------------------------------
    The test routine test_mgr_chunk_compress is added when bug# 307 was 
    fixed.

    test_mgr_chunk_compress tests the new functionality, getting 
    compression information of compressed chunked image data.  It 
        + creates the file CHKCOMPFILE and adds four compressed chunked 
	  images to it, then closes the file.
        + re-opens the file, then reads and verifies each chunked image's
          compression information
        The first image is only chunked and not compressed.
        The last three chunked images are compressed using the following
        methods in that order: RLE, Skipping Huffman, Deflate.
        For simplicity, all four images use the same data sample.
    Note: At this time JPEG is not working correctly for chunked images,
    but when it is, its tests should be added to this routines (and to 
    test_mgr_chunkwr_pixelone as well) appropriately, i.e. another image 
    should be added to the image list.

 -BMR (Oct 7, 01)
--------------------------------------------------------------------------*/

static void 
test_mgr_chunk_compress()
{
#define  CHKCOMPFILE  "gr_chunkcomp.hdf"
#define  X_LENGTH     10    /* number of columns in the image */
#define  Y_LENGTH     6     /* number of rows in the image */
#define  N_COMPS      3     /* number of components in the image */
#define  N_IMAGES     4     /* number of images tested used - 5 comp. methods */

   /************************* Variable declaration **************************/

   intn  status;         /* status for functions returning an intn */
   int32 file_id,        /* HDF file identifier */
         gr_id,          /* GR interface identifier */
         ri_id[N_IMAGES],       /* raster image identifier */
         origin[2],      /* start position to write for each dimension */
         dim_sizes[2],   /* dimension sizes of the image array */
         interlace_mode, /* interlace mode of the image */
         data_type,      /* data type of the image data */
         comp_flag,      /* compression flag */
         index,
         img_num;
   int32 start[2],
         stride[2],
         edge[2];
   comp_info cinfo;    /* Compression parameters - union */

   comp_coder_t comp_type;
   int16 data_out[3*Y_LENGTH*X_LENGTH];
   char *image_name[] = { "Image_NO", "Image_RL", "Image_Sk", "Image_DF"};
   HDF_CHUNK_DEF chunk_def[N_IMAGES];
   int16 chunk_buf[18];

   int16 chunk00[] = {        110, 111, 112, 120, 121, 122,
                              130, 131, 132, 140, 141, 142,
                              150, 151, 152, 160, 161, 162 };
 
   int16 chunk01[] = {    210, 211, 212, 220, 221, 222,
                          230, 231, 232, 240, 241, 242,
                          250, 251, 252, 260, 261, 262};
 
   int16 chunk14[] = {    1010, 1011, 1012, 1020, 1021, 1022,
                          1030, 1031, 1032, 1040, 1041, 1042,
                          1050, 1051, 1052, 1060, 1061, 1062};

   int16 data[]    = {
                110, 111, 112, 120, 121, 122, 210, 211, 212, 220, 221, 222, 0, 
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 130, 131, 132, 140, 
                141, 142, 230, 231, 232, 240, 241, 242, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                0, 0, 0, 0, 0, 0, 0, 0, 0, 150, 151, 152, 160, 161, 162, 250, 251, 
                252, 260, 261, 262, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                1010, 1011, 1012, 1020, 1021, 1022, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1030, 1031, 1032, 1040, 1041, 
                1042, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                0, 1050, 1051, 1052, 1060, 1061, 1062 }; 


   /********************** End of variable declaration **********************/
   /*
   * Create and open the file.
   */
   file_id = Hopen (CHKCOMPFILE, DFACC_CREATE, 0);
   CHECK(file_id, FAIL, "Hopen");

   /*
   * Initialize the GR interface.
   */
   gr_id = GRstart (file_id);
   CHECK(gr_id, FAIL, "GRstart");

   /*
   * Set the data type, interlace mode, and dimensions of the image.
   */
   data_type = DFNT_INT16;
   interlace_mode = MFGR_INTERLACE_PIXEL;
   dim_sizes[0] = Y_LENGTH;
   dim_sizes[1] = X_LENGTH;

   for (img_num = 0; img_num < N_IMAGES; img_num++ ) {  

   /*
   * Create the raster image array.
   */
   ri_id[img_num] = GRcreate (gr_id, image_name[img_num], N_COMPS, data_type, 
                     interlace_mode, dim_sizes);
   CHECK(ri_id[img_num], FAIL, "GRcreate");

   /* 
   * Create chunked image array.
   */
   switch (img_num) {
	case 0: 
		comp_flag = HDF_CHUNK;
		chunk_def[img_num].chunk_lengths[0] = 3;
		chunk_def[img_num].chunk_lengths[1] = 2;
		break;
	case 1 :
		comp_flag = HDF_CHUNK | HDF_COMP;
		chunk_def[img_num].comp.chunk_lengths[0] = 3;
		chunk_def[img_num].comp.chunk_lengths[1] = 2;
		chunk_def[img_num].comp.comp_type = COMP_CODE_RLE;
		break;
	case 2 :
		comp_flag = HDF_CHUNK | HDF_COMP;
		chunk_def[img_num].comp.chunk_lengths[0] = 3;
		chunk_def[img_num].comp.chunk_lengths[1] = 2;
		chunk_def[img_num].comp.comp_type = COMP_CODE_SKPHUFF;
		chunk_def[img_num].comp.cinfo.skphuff.skp_size = 2;
		break;
	case 3 :
		comp_flag = HDF_CHUNK | HDF_COMP;
		chunk_def[img_num].comp.chunk_lengths[0] = 3;
		chunk_def[img_num].comp.chunk_lengths[1] = 2;
		chunk_def[img_num].comp.comp_type = COMP_CODE_DEFLATE;
		chunk_def[img_num].comp.cinfo.deflate.level = 6;
		break;
#ifdef NOT_WORKING
	/* JPEG compression for chunked images is not working correctly 
	   yet.  Add test here when it is */
	case 4 :
		comp_flag = HDF_CHUNK | HDF_COMP;
		chunk_def[img_num].comp.chunk_lengths[0] = 3;
		chunk_def[img_num].comp.chunk_lengths[1] = 2;
		chunk_def[img_num].comp.comp_type = COMP_CODE_JPEG;
		chunk_def[img_num].comp.cinfo.jpeg.quality = 5;
		chunk_def[img_num].comp.cinfo.jpeg.force_baseline = 8;
		break;
#endif
	default:
		printf("Error\n");
		break;

   } /* end switch */
    
   status = GRsetchunk(ri_id[img_num], chunk_def[img_num], comp_flag);
   CHECK(status, FAIL, "GRsetchunk");

   /*
   * Write first data chunk ( 0, 0 ). 
   */
   origin[0] = origin[1] = 0;
   status = GRwritechunk(ri_id[img_num], origin, (VOIDP)chunk00);
   CHECK(status, FAIL, "GRwritechunk");

   /*
   * Write second data chunk ( 0, 1 ). 
   */
   origin[0] = 0; origin[1] = 1;
   status = GRwritechunk(ri_id[img_num], origin, (VOIDP)chunk01);
   CHECK(status, FAIL, "GRwritechunk");

   /*
   * Write third data chunk ( 1, 4 ). 
   */
   origin[0] = 1; origin[1] = 4;
   status = GRwritechunk(ri_id[img_num], origin, (VOIDP)chunk14);
   CHECK(status, FAIL, "GRwritechunk");
   /*
   * Read third chunk back.
   */
   origin[0] = 1; origin[1] = 4;
   status = GRreadchunk(ri_id[img_num], origin, (VOIDP)chunk_buf);
   CHECK(status, FAIL, "GRreadchunk");

   /*
   * Terminate access to the GR interface and close the HDF file.
   */
   status = GRendaccess (ri_id[img_num]);
   CHECK(status, FAIL, "GRendaccess");
 }  /* end for*/

   status = GRend (gr_id);
   CHECK(status, FAIL, "GRend");
   status = Hclose (file_id);
   CHECK(status, FAIL, "Hclose");

    /* 
    * Open the file.
    */

    file_id = Hopen (CHKCOMPFILE, DFACC_WRITE, 0); 
    CHECK(file_id, FAIL, "Hopen");

   /*
   * Initialize the GR interface.
   */
   gr_id = GRstart (file_id);
   CHECK(gr_id, FAIL, "GRstart");

   for (img_num = 0; img_num < N_IMAGES; img_num++ ) {  

   /*
   * Find the index of the specified image.
   */
   index = GRnametoindex(gr_id, image_name[img_num]);
   CHECK(index, FAIL, "GRnametoindex");
   
   /* 
   * Select the image.
   */
   ri_id[img_num] = GRselect(gr_id, index);
   CHECK(ri_id[img_num], FAIL, "GRselect");

   /*
   * Get and verify the image's compression information
   */
    comp_type = COMP_CODE_INVALID;  /* reset variables before retrieving info */
    HDmemset(&cinfo, 0, sizeof(cinfo)) ;

    status = GRgetcompress(ri_id[img_num], &comp_type, &cinfo);
    CHECK(status, FAIL, "GRsetcompress");
    switch (img_num) {
	case 0: 
	    VERIFY(comp_type, COMP_CODE_NONE, "GRgetcompress");
	    break;
	case 1 :
	    VERIFY(comp_type, COMP_CODE_RLE, "GRgetcompress");
	    break;
	case 2 :
	    VERIFY(comp_type, COMP_CODE_SKPHUFF, "GRgetcompress");
	    VERIFY(cinfo.skphuff.skp_size, 
		   chunk_def[img_num].comp.cinfo.skphuff.skp_size, "GRgetcompress");
	    break;
	case 3 :
	    VERIFY(comp_type, COMP_CODE_DEFLATE, "GRgetcompress");
	    VERIFY(cinfo.deflate.level, 
		   chunk_def[img_num].comp.cinfo.deflate.level, "GRgetcompress");
	    break;
#ifdef NOT_WORKING
	/* JPEG is not working correctly yet.  Add test here when it is */
	case 4 :  /* only return comp type for JPEG */
	    VERIFY(comp_type, COMP_CODE_JPEG, "GRgetcompress");
	    break;
#endif
	default:
	    printf("Error\n");
	    break;
   } /* end switch */

   /*
   * Read third chunk back.
   */
   origin[0] = 1; origin[1] = 4;
   status = GRreadchunk(ri_id[img_num], origin, (VOIDP)chunk_buf);
   CHECK(status, FAIL, "GRreadchunk");
   if (0 != HDmemcmp(chunk_buf, chunk14 , sizeof(chunk14)))
      {
            MESSAGE(3, printf("Error in reading chunk at line %d\n",__LINE__););
            MESSAGE(3, printf("Image #%d\n", (int)img_num););
            num_errs++;
      } /* end if */
   /*
   * Read the whole image.
   */
   start[0] = start[1] = 0;
   stride[0] = stride[1] = 1;
   edge[0] = Y_LENGTH;
   edge[1] = X_LENGTH;
   status = GRreadimage(ri_id[img_num], start, stride, edge, (VOIDP)data_out);
   CHECK(status, FAIL, "GRreadimage");
   if (0!= HDmemcmp(data_out, data, sizeof(data)))
      {
            MESSAGE(3, printf("%d: Error reading data for the whole image\n",__LINE__););
            MESSAGE(3, printf("%d: Compression method\n", (int)img_num););
            num_errs++;
      } /* end if */

   status = GRendaccess (ri_id[img_num]);
   CHECK(status, FAIL, "GRendaccess");

   } /* end for */    
   /*
   * Terminate access to the GR interface and close the HDF file.
   */
   status = GRend (gr_id);
   CHECK(status, FAIL, "GRend");
   status = Hclose (file_id);
   CHECK(status, FAIL, "Hclose");
}  /* end of test_mgr_chunk_compress */


/****************************************************************
**
**  test_mgr_compress(): Multi-file Raster Compression tests
** 
**  IX. Compressed image tests
**      A. Create/Read/Write gzip compressed Image
**      B. Create/Read/Write 8-bit JPEG compressed Image
**      C. Create/Read/Write 24-bit JPEG compressed Image
**      D. Retrieve various compression information of compressed Image
**	E. Retrieve various compression info. of compressed, chunked images
** 
****************************************************************/
static void
test_mgr_compress(int flag)
{
    /* Output message about test being performed */
    MESSAGE(6, printf("Testing Multi-file Raster Compression Testing\n"););

    test_mgr_compress_a(flag);
    test_mgr_compress_b(flag);
    test_mgr_compress_c(flag);
    test_get_compress(flag);

}   /* end test_mgr_compress() */

#define GR_R24FILE    "gr_r24.dat"
#define GR_R24XDIM      8
#define GR_R24YDIM      10

/* Sub-tests for test_mgr_r24() */
static void test_mgr_r24_a(int flag)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */
    char gr_r24file[512] = "";
    char *srcdir = getenv("srcdir");

    /* Generate the correct name for the test file, by prepending the source path */
    if (srcdir && ((strlen(srcdir) + strlen(GR_R24FILE) + 1) < sizeof(gr_r24file))) {
        strcpy(gr_r24file, srcdir);
        strcat(gr_r24file, "/");
    }
    strcat(gr_r24file, GR_R24FILE);

/* A - Write/Read DF24 image */
    MESSAGE(8, printf("Operate on DF24 images\n"););

    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(gr_r24file,DFACC_READ,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={GR_R24XDIM,GR_R24YDIM};    /* dimensions for the empty image */
        uint8 image[GR_R24YDIM][GR_R24XDIM][3]; /* space for the image data */
        uint8 image0[GR_R24YDIM][GR_R24XDIM][3]; /* space for the image data */
        int32 start[2];     /* start of image data to grab */
        int32 stride[2];    /* stride of image data to grab */
        int32 ncomp;        /* Number of components in the DF24 image */
        int32 nt;           /* Number-type of the DF24 image */
        int32 dimsizes[2];  /* Dimensions of the DF24 image */
        intn i,j,k;                 /* indices */

        /* Initialize data we are going to write out */
        for(i=0; i<GR_R24YDIM; i++)
            for(j=0; j<GR_R24XDIM; j++)
                for(k=0; k<3; k++)
                    image[i][j][k]=(k+1)*j;

        /* Get the first image in this file */
        riid=GRselect(grid,0);
        CHECK(riid,FAIL,"GRselect");

        /* Check the image information */
        ret=GRgetiminfo(riid,NULL,&ncomp,&nt,NULL,dimsizes,NULL);
        CHECK(ret,FAIL,"GRgetiminfo");
        VERIFY(ncomp,3,"GRgetiminfo");
        VERIFY(nt,DFNT_UCHAR8,"GRgetiminfo");
        VERIFY(dimsizes[0],dims[0],"GRgetiminfo");
        VERIFY(dimsizes[1],dims[1],"GRgetiminfo");

        /* Read the whole image in */
        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRreadimage(riid,start,stride,dims,image0);
        CHECK(ret,FAIL,"GRreadimage");

        /* Verify correct image contents */
        if(HDmemcmp(image,image0,GR_R24YDIM*GR_R24XDIM*3)!=0) {
            MESSAGE(3, printf("Error reading data for DF24 image\n"););
            num_errs++;
        } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
} /* end test_mgr_r24_a() */

/****************************************************************
**
**  test_mgr_r24(): Multi-file Raster/DF24 Compatibility Tests
** 
**  X. Multi-File Raster/DF24 Compatibility Tests
**      A. Read/Write DF24 Image
** 
****************************************************************/
static void
test_mgr_r24(int flag)
{
    /* Output message about test being performed */
    MESSAGE(6, printf("Testing Multi-file Raster/DF24 Compatibility\n"););

    test_mgr_r24_a(flag);

}   /* end test_mgr_r24() */

#define GR_R8FILE    "gr_r8.hdf"
#define GR_R8XDIM      8
#define GR_R8YDIM      10

/* Sub-tests for test_mgr_r8() */
static void test_mgr_r8_a(int flag)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */
    uint8 palette[256][3];
    uint8 picture[GR_R8YDIM][GR_R8XDIM];
    intn i,j;               /* indices */

/* A - Write/Read DF8 image with palette */
    MESSAGE(8, printf("Operate on DF8 images\n"););

    /* initialize the palette */
    for (i = 0; i < 256; i++)
    {
        for (j = 0; j < 3; j++)
        {
            palette[i][j] = i;
        }
    }

    /* initialize the image */
    for (j = 0; j < GR_R8XDIM; j++)
    {
        for (i = 0; i < GR_R8YDIM; i++)
        {
            picture[i][j] = i+j;
        }
    }

    /* Write out the test data */
    ret = DFR8setpalette((VOIDP) palette);
    CHECK(ret,FAIL,"DFR8setpalette");
    ret = DFR8putimage(GR_R8FILE, (VOIDP) picture, GR_R8XDIM, GR_R8YDIM, COMP_RLE);
    CHECK(ret,FAIL,"DFR8putimage");

    /* Open up the existing datafile and get the image information from it */
    fid=Hopen(GR_R8FILE,DFACC_READ,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
        int32 riid;     /* RI ID for the image */
        int32 pal_id;     /* Palette ID for the LUT */
        int32 dims[2]={GR_R8XDIM,GR_R8YDIM};    /* dimensions for the empty image */
        uint8 image[GR_R8YDIM][GR_R8XDIM]; /* space for the image data */
        uint8 image0[GR_R8YDIM][GR_R8XDIM]; /* space for the image data */
        int32 start[2];     /* start of image data to grab */
        int32 stride[2];    /* stride of image data to grab */
        int32 ncomp;        /* Number of components in the DFR8 image */
        int32 nt;           /* Number-type of the DFR8 image */
        int32 dimsizes[2];  /* Dimensions of the DFR8 image */
        int32 interlace;    /* Palette interlace */
        int32 num_entries;  /* Number of palette entries */

        /* Initialize data we are expecting to read in */
        for(i=0; i<GR_R8YDIM; i++)
            for(j=0; j<GR_R8XDIM; j++)
                image[i][j]=i+j;

        /* Get the first image in this file */
        riid=GRselect(grid,0);
        CHECK(riid,FAIL,"GRselect");

        /* Check the image information */
        ret=GRgetiminfo(riid,NULL,&ncomp,&nt,NULL,dimsizes,NULL);
        CHECK(ret,FAIL,"GRgetiminfo");
        VERIFY(ncomp,1,"GRgetiminfo");
        VERIFY(nt,DFNT_UCHAR8,"GRgetiminfo");
        VERIFY(dimsizes[0],dims[0],"GRgetiminfo");
        VERIFY(dimsizes[1],dims[1],"GRgetiminfo");

        /* Read the whole image in */
        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRreadimage(riid,start,stride,dims,image0);
        CHECK(ret,FAIL,"GRreadimage");

        /* Verify correct image contents */
        if(HDmemcmp(image,image0,GR_R8YDIM*GR_R8XDIM)!=0) {
            MESSAGE(3, printf("Error reading data for DF8 image\n"););
            num_errs++;
        } /* end if */

        pal_id = GRgetlutid(riid, 0);
        CHECK(pal_id,FAIL,"GRgetlutid");

        ncomp=nt=0;
        ret = GRgetlutinfo(pal_id, &ncomp, &nt, &interlace, &num_entries);
        CHECK(ret,FAIL,"GRgetlutinfo");
        VERIFY(ncomp,3,"GRgetlutinfo");
        VERIFY(nt,DFNT_UINT8,"GRgetlutinfo");
        VERIFY(interlace,0,"GRgetlutinfo");
        VERIFY(num_entries,256,"GRgetlutinfo");

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
} /* end test_mgr_r8_a() */

/****************************************************************
**
**  test_mgr_r8(): Multi-file Raster/DF8 Compatibility Tests
** 
**  XI. Multi-File Raster/DF8 Compatibility Tests
**      A. Read/Write DF8 Image with palette
** 
****************************************************************/
static void
test_mgr_r8(int flag)
{
    /* Output message about test being performed */
    MESSAGE(6, printf("Testing Multi-file Raster/DF8 Compatibility\n"););

    test_mgr_r8_a(flag);

}   /* end test_mgr_r8() */

#ifdef LATER
#define GR_DFPFILE    "gr_pal.hdf"

/* Initialization for test_mgr_pal() */
static void test_mgr_pal_init(void)
{
    uint8 pal1[256][3];      /* Palette data to write out */
    uint8 pal2[256][3];      /* Palette data to write out */
    intn i,j;                /* indices */

    for(i=0; i<256; i++)
        for(j=0; j<3; j++) {
            pal1[i][j]=i;
            pal2[i][j]=(j==0 ? i : (255-i));
        } /* end for */

    DFPputpal(GR_DFPFILE,pal1,0,"w");
    DFPaddpal(GR_DFPFILE,pal2);
}

/* Sub-tests for test_mgr_pal() */
static void test_mgr_pal_a(int flag)
{
    int32 fid;              /* HDF file ID */
    int32 grid;             /* GRID for the interface */
    int32 ret;              /* generic return value */
    char gr_r24file[512] = "";
    char *srcdir = getenv("srcdir");

    /* Generate the correct name for the test file, by prepending the source path */
    if (srcdir && ((strlen(srcdir) + strlen(GR_R24FILE) + 1) < sizeof(gr_r24file))) {
        strcpy(gr_r24file, srcdir);
        strcat(gr_r24file, "/");
    }
    strcat(gr_r24file, GR_R24FILE);

/* A - Write/Read DFP palette */
    MESSAGE(8, printf("Operate on DFP palette\n"););

    /* Open up the existing datafile and get the palette information from it */
    fid=Hopen(gr_r24file,DFACC_READ,0);
    CHECK(fid,FAIL,"Hopen");

    /* Initialize the GR interface */
    grid=GRstart(fid);
    CHECK(grid,FAIL,"GRstart");

    {
        int32 riid;     /* RI ID for the new image */
        int32 dims[2]={GR_R24XDIM,GR_R24YDIM};    /* dimensions for the empty image */
        uint8 image[GR_R24YDIM][GR_R24XDIM][3]; /* space for the image data */
        uint8 image0[GR_R24YDIM][GR_R24XDIM][3]; /* space for the image data */
        int32 start[2];     /* start of image data to grab */
        int32 stride[2];    /* stride of image data to grab */
        int32 ncomp;        /* Number of components in the DF24 image */
        int32 nt;           /* Number-type of the DF24 image */
        int32 dimsizes[2];  /* Dimensions of the DF24 image */
        intn i,j,k;                 /* indices */

        /* Initialize data we are going to write out */
        for(i=0; i<GR_R24YDIM; i++)
            for(j=0; j<GR_R24XDIM; j++)
                for(k=0; k<3; k++)
                    image[i][j][k]=(k+1)*j;

        /* Get the first image in this file */
        riid=GRselect(grid,0);
        CHECK(riid,FAIL,"GRselect");

        /* Check the image information */
        ret=GRgetiminfo(riid,NULL,&ncomp,&nt,NULL,dimsizes,NULL);
        CHECK(ret,FAIL,"GRgetiminfo");
        VERIFY(ncomp,3,"GRgetiminfo");
        VERIFY(nt,DFNT_UCHAR8,"GRgetiminfo");
        VERIFY(dimsizes[0],dims[0],"GRgetiminfo");
        VERIFY(dimsizes[1],dims[1],"GRgetiminfo");

        /* Read the whole image in */
        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        ret=GRreadimage(riid,start,stride,dims,image0);
        CHECK(ret,FAIL,"GRreadimage");

        /* Verify correct image contents */
        if(HDmemcmp(image,image0,GR_R24YDIM*GR_R24XDIM*3)!=0) {
            MESSAGE(3, printf("Error reading data for DF24 image\n"););
            num_errs++;
        } /* end if */

        /* Close the empty image */
        ret=GRendaccess(riid);
        CHECK(ret,FAIL,"GRendaccess");
    }
    
    /* Shut down the GR interface */
    ret=GRend(grid);
    CHECK(ret,FAIL,"GRend");

    /* Close the file */
    ret=Hclose(fid);
    CHECK(ret,FAIL,"Hclose");
} /* end test_mgr_pal_a() */

/****************************************************************
**
**  test_mgr_pal(): Multi-file Raster/DFP Compatibility Tests
** 
**  XII. Multi-File Raster/DFP Compatibility Tests
**      A. Read/Write DFP palettes
** 
****************************************************************/
static void
test_mgr_pal(int flag)
{
    /* Output message about test being performed */
    MESSAGE(6, printf("Testing Multi-file Raster/DFP Compatibility\n"););

    test_mgr_pal_init();
    test_mgr_pal_a(flag);

}   /* end test_mgr_pal() */
#endif /* LATER */

static void 
test_mgr_chunkwr_pixelone()
{
   /*
    * This function tests GR chunking write/read operations for the
    * following types of compressions:
    *                COMP_NONE
    *                COMP_CODE_RLE
    *                COMP_CODE_SKPHUFF
    *                COMP_CODE_DEFLATE
    * and PIXEL interlace mode.
    */                    
#define  CHUNKFILE     "ChunkedGR.hdf"
#define  X_LENGTH      10    /* number of columns in the image */
#define  Y_LENGTH      6     /* number of rows in the image */
#define  N_COMPS       3     /* number of components in the image */
#define  COMP_METH     4     /* number of compression methods used - 4 (0-based) */

   /************************* Variable declaration **************************/

   intn  status;         /* status for functions returning an intn */
   int32 file_id,        /* HDF file identifier */
         gr_id,          /* GR interface identifier */
         ri_id[4],       /* raster image identifier */
         origin[2],      /* start position to write for each dimension */
         dim_sizes[2],   /* dimension sizes of the image array */
         interlace_mode, /* interlace mode of the image */
         data_type,      /* data type of the image data */
         comp_flag,      /* compression flag */
         index,
         i;
   int32 start[2],
         stride[2],
         edge[2];
   int16 data_out[3*Y_LENGTH*X_LENGTH];
   char *image_name[] = { "Image_NO", "Image_RL", "Image_Sk", "Image_DF"};
   HDF_CHUNK_DEF chunk_def;
   int16 chunk_buf[18];

   int16 chunk00[] = {        110, 111, 112, 120, 121, 122,
                              130, 131, 132, 140, 141, 142,
                              150, 151, 152, 160, 161, 162 };
 
 
   int16 chunk01[] = {    210, 211, 212, 220, 221, 222,
                          230, 231, 232, 240, 241, 242,
                          250, 251, 252, 260, 261, 262};
 
   int16 chunk14[] = {    1010, 1011, 1012, 1020, 1021, 1022,
                          1030, 1031, 1032, 1040, 1041, 1042,
                          1050, 1051, 1052, 1060, 1061, 1062};

   int16 data[]    = {
                110, 111, 112, 120, 121, 122, 210, 211, 212, 220, 221, 222, 0, 
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 130, 131, 132, 140, 
                141, 142, 230, 231, 232, 240, 241, 242, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                0, 0, 0, 0, 0, 0, 0, 0, 0, 150, 151, 152, 160, 161, 162, 250, 251, 
                252, 260, 261, 262, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                1010, 1011, 1012, 1020, 1021, 1022, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1030, 1031, 1032, 1040, 1041, 
                1042, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                0, 1050, 1051, 1052, 1060, 1061, 1062 }; 


   /********************** End of variable declaration **********************/
   /*
   * Create and open the file.
   */
   file_id = Hopen (CHUNKFILE, DFACC_WRITE, 0);
   CHECK(file_id, FAIL, "Hopen");

   /*
   * Initialize the GR interface.
   */
   gr_id = GRstart (file_id);
   CHECK(gr_id, FAIL, "GRstart");

   /*
   * Set the data type, interlace mode, and dimensions of the image.
   */
   data_type = DFNT_INT16;
   interlace_mode = MFGR_INTERLACE_PIXEL;
   dim_sizes[0] = Y_LENGTH;
   dim_sizes[1] = X_LENGTH;

   for (i = 0; i < COMP_METH; i++ ) {  

   /*
   * Create the raster image array.
   */
   ri_id[i] = GRcreate (gr_id, image_name[i], N_COMPS, data_type, 
                     interlace_mode, dim_sizes);
   CHECK(ri_id[i], FAIL, "GRcreate");

   /* 
   * Create chunked image array.
   */
   switch (i) {
              case 0: 
                      comp_flag = HDF_CHUNK;
                      chunk_def.chunk_lengths[0] = 3;
                      chunk_def.chunk_lengths[1] = 2;
                      break;
              case 1 :
                      comp_flag = HDF_CHUNK | HDF_COMP;
                      chunk_def.comp.chunk_lengths[0] = 3;
                      chunk_def.comp.chunk_lengths[1] = 2;
                      chunk_def.comp.comp_type = COMP_CODE_RLE;
                      break;
              case 2 :
                {
                      comp_flag = HDF_CHUNK | HDF_COMP;
                      chunk_def.comp.chunk_lengths[0] = 3;
                      chunk_def.comp.chunk_lengths[1] = 2;
                      chunk_def.comp.comp_type = COMP_CODE_SKPHUFF;
                      chunk_def.comp.cinfo.skphuff.skp_size = 2;
                      break;
                }
              case 3 :
                { 
                      comp_flag = HDF_CHUNK | HDF_COMP;
                      chunk_def.comp.chunk_lengths[0] = 3;
                      chunk_def.comp.chunk_lengths[1] = 2;
                      chunk_def.comp.comp_type = COMP_CODE_DEFLATE;
                      chunk_def.comp.cinfo.deflate.level = 6;
                      break;
                }
              default:
                {
                      printf("Error\n");
                      break;
                }

   } /* end switch */
    
   status = GRsetchunk(ri_id[i], chunk_def, comp_flag);
   CHECK(status, FAIL, "GRsetchunk");

   /*
   * Write first data chunk ( 0, 0 ). 
   */
   origin[0] = origin[1] = 0;
   status = GRwritechunk(ri_id[i], origin, (VOIDP)chunk00);
   CHECK(status, FAIL, "GRwritechunk");

   /*
   * Write second data chunk ( 0, 1 ). 
   */
   origin[0] = 0; origin[1] = 1;
   status = GRwritechunk(ri_id[i], origin, (VOIDP)chunk01);
   CHECK(status, FAIL, "GRwritechunk");

   /*
   * Write third data chunk ( 1, 4 ). 
   */
   origin[0] = 1; origin[1] = 4;
   status = GRwritechunk(ri_id[i], origin, (VOIDP)chunk14);
   CHECK(status, FAIL, "GRwritechunk");
   /*
   * Read third chunk back.
   */
   origin[0] = 1; origin[1] = 4;
   status = GRreadchunk(ri_id[i], origin, (VOIDP)chunk_buf);
   CHECK(status, FAIL, "GRreadchunk");

   /*
   * Terminate access to the GR interface and close the HDF file.
   */
   status = GRendaccess (ri_id[i]);
   CHECK(status, FAIL, "GRendaccess");
 }  /* end for*/
   status = GRend (gr_id);
   CHECK(status, FAIL, "GRend");
   status = Hclose (file_id);
   CHECK(status, FAIL, "Hclose");

    /* 
    * Open the file.
    */

    file_id = Hopen (CHUNKFILE, DFACC_WRITE, 0); 
    CHECK(file_id, FAIL, "Hopen");

   /*
   * Initialize the GR interface.
   */
   gr_id = GRstart (file_id);
   CHECK(gr_id, FAIL, "GRstart");

   for (i = 0; i < COMP_METH; i++ ) { 

   /*
   * Find the index of the specified image.
   */
   index = GRnametoindex(gr_id, image_name[i]);
   CHECK(index, FAIL, "GRnametoindex");
   
   /* 
   * Select the image.
   */
   ri_id[i] = GRselect(gr_id, index);
   CHECK(ri_id[i], FAIL, "GRselect");

   /*
   * Read third chunk back.
   */
   origin[0] = 1; origin[1] = 4;
   status = GRreadchunk(ri_id[i], origin, (VOIDP)chunk_buf);
   CHECK(status, FAIL, "GRreadchunk");
   if (0 != HDmemcmp(chunk_buf, chunk14 , sizeof(chunk14)))
      {
            MESSAGE(3, printf("%d: Error in reading chunk\n",__LINE__););
            MESSAGE(3, printf("%d: Compression method\n", (int)i););
            num_errs++;
      } /* end if */
   /*
   * Read the whole image.
   */
   start[0] = start[1] = 0;
   stride[0] = stride[1] = 1;
   edge[0] = Y_LENGTH;
   edge[1] = X_LENGTH;
   status = GRreadimage(ri_id[i], start, stride, edge, (VOIDP)data_out);
   CHECK(status, FAIL, "GRreadimage");
   if (0!= HDmemcmp(data_out, data, sizeof(data)))
      {
            MESSAGE(3, printf("%d: Error reading data for the whole image\n",__LINE__););
            MESSAGE(3, printf("%d: Compression method\n", (int)i););
            num_errs++;
      } /* end if */

   status = GRendaccess (ri_id[i]);
   CHECK(status, FAIL, "GRendaccess");

   } /* end for */    
   /*
   * Terminate access to the GR interface and close the HDF file.
   */
   status = GRend (gr_id);
   CHECK(status, FAIL, "GRend");
   status = Hclose (file_id);
   CHECK(status, FAIL, "Hclose");
}
static void 
test_mgr_chunkwr_pixel(int flag)
{
   /*
    * This function tests GR chunking write/read operations for the
    * following types of compressions:
    *                COMP_NONE
    *                COMP_CODE_RLE
    *                COMP_CODE_SKPHUFF
    *                COMP_CODE_DEFLATE
    * and PIXEL interlace mode.
    */                    
/* Writing images to one file does not work , we will write each image to
   the different files
#define  FILE_NAME     "ChunkedGR.hdf"
*/
#define  X_LENGTH      10    /* number of columns in the image */
#define  Y_LENGTH      6     /* number of rows in the image */
#define  N_COMPS       3     /* number of components in the image */
#define  COMP_METH     4     /* number of compression methods used - 4 (0-based) */

   /************************* Variable declaration **************************/

   intn  status;         /* status for functions returning an intn */
   int32 file_id,        /* HDF file identifier */
         gr_id,          /* GR interface identifier */
         ri_id[4],       /* raster image identifier */
         origin[2],      /* start position to write for each dimension */
         dim_sizes[2],   /* dimension sizes of the image array */
         interlace_mode, /* interlace mode of the image */
         data_type,      /* data type of the image data */
         comp_flag,      /* compression flag */
         index,
         i;
   int32 start[2],
         stride[2],
         edge[2];
   int16 data_out[3*Y_LENGTH*X_LENGTH];
   char *image_name[] = { "Image_NO", "Image_RL", "Image_Sk", "Image_DF"};
   char *file_name[] = { "ChunkedGR_NO.hdf", 
                          "ChunkedGR_RL.hdf",
                          "ChunkedGR_SK.hdf",
                          "ChunkedGR_DF.hdf" };
   HDF_CHUNK_DEF chunk_def;
   int16 chunk_buf[18];

   int16 chunk00[] = {        110, 111, 112, 120, 121, 122,
                              130, 131, 132, 140, 141, 142,
                              150, 151, 152, 160, 161, 162 };
 
 
   int16 chunk01[] = {    210, 211, 212, 220, 221, 222,
                          230, 231, 232, 240, 241, 242,
                          250, 251, 252, 260, 261, 262};
 
   int16 chunk14[] = {    1010, 1011, 1012, 1020, 1021, 1022,
                          1030, 1031, 1032, 1040, 1041, 1042,
                          1050, 1051, 1052, 1060, 1061, 1062};

   int16 data[]    = {
                110, 111, 112, 120, 121, 122, 210, 211, 212, 220, 221, 222, 0, 
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 130, 131, 132, 140, 
                141, 142, 230, 231, 232, 240, 241, 242, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                0, 0, 0, 0, 0, 0, 0, 0, 0, 150, 151, 152, 160, 161, 162, 250, 251, 
                252, 260, 261, 262, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                1010, 1011, 1012, 1020, 1021, 1022, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1030, 1031, 1032, 1040, 1041, 
                1042, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                0, 1050, 1051, 1052, 1060, 1061, 1062 }; 


   /********************** End of variable declaration **********************/

   i = flag;
   /*
   * Create and open the file.
   */
   /*  This call is commented out, since writing to one file does not work
   file_id = Hopen (FILE_NAME, DFACC_WRITE, 0);
   */
   file_id = Hopen (file_name[i], DFACC_CREATE, 0);
   CHECK(file_id, FAIL, "Hopen");

   /*
   * Initialize the GR interface.
   */
   gr_id = GRstart (file_id);
   CHECK(gr_id, FAIL, "GRstart");

   /*
   * Set the data type, interlace mode, and dimensions of the image.
   */
   data_type = DFNT_INT16;
   interlace_mode = MFGR_INTERLACE_PIXEL;
   dim_sizes[0] = Y_LENGTH;
   dim_sizes[1] = X_LENGTH;

  /* for (i = 0; i < COMP_METH; i++ ) { */ 

   /*
   * Create the raster image array.
   */
   ri_id[i] = GRcreate (gr_id, image_name[i], N_COMPS, data_type, 
                     interlace_mode, dim_sizes);
   CHECK(ri_id[i], FAIL, "GRcreate");

   /* 
   * Create chunked image array.
   */
   switch (i) {
              case 0: 
                      comp_flag = HDF_CHUNK;
                      chunk_def.chunk_lengths[0] = 3;
                      chunk_def.chunk_lengths[1] = 2;
                      break;
              case 1 :
                      comp_flag = HDF_CHUNK | HDF_COMP;
                      chunk_def.comp.chunk_lengths[0] = 3;
                      chunk_def.comp.chunk_lengths[1] = 2;
                      chunk_def.comp.comp_type = COMP_CODE_RLE;
                      break;
              case 2 :
                {
                      comp_flag = HDF_CHUNK | HDF_COMP;
                      chunk_def.comp.chunk_lengths[0] = 3;
                      chunk_def.comp.chunk_lengths[1] = 2;
                      chunk_def.comp.comp_type = COMP_CODE_SKPHUFF;
                      chunk_def.comp.cinfo.skphuff.skp_size = 2;
                      break;
                }
              case 3 :
                { 
                      comp_flag = HDF_CHUNK | HDF_COMP;
                      chunk_def.comp.chunk_lengths[0] = 3;
                      chunk_def.comp.chunk_lengths[1] = 2;
                      chunk_def.comp.comp_type = COMP_CODE_DEFLATE;
                      chunk_def.comp.cinfo.deflate.level = 6;
                      break;
                }
              default:
                {
                      printf("Error\n");
                      break;
                }

   } /* end switch */
    
   status = GRsetchunk(ri_id[i], chunk_def, comp_flag);
   CHECK(status, FAIL, "GRsetchunk");

   /*
   * Write first data chunk ( 0, 0 ). 
   */
   origin[0] = origin[1] = 0;
   status = GRwritechunk(ri_id[i], origin, (VOIDP)chunk00);
   CHECK(status, FAIL, "GRwritechunk");

   /*
   * Write second data chunk ( 0, 1 ). 
   */
   origin[0] = 0; origin[1] = 1;
   status = GRwritechunk(ri_id[i], origin, (VOIDP)chunk01);
   CHECK(status, FAIL, "GRwritechunk");

   /*
   * Write third data chunk ( 1, 4 ). 
   */
   origin[0] = 1; origin[1] = 4;
   status = GRwritechunk(ri_id[i], origin, (VOIDP)chunk14);
   CHECK(status, FAIL, "GRwritechunk");
   /*
   * Read third chunk back.
   */
   origin[0] = 1; origin[1] = 4;
   status = GRreadchunk(ri_id[i], origin, (VOIDP)chunk_buf);

   /*
   * Terminate access to the GR interface and close the HDF file.
   */
   status = GRendaccess (ri_id[i]);
   CHECK(status, FAIL, "GRendaccess");
/* } */ /* end for*/
   status = GRend (gr_id);
   CHECK(status, FAIL, "GRend");
   status = Hclose (file_id);
   CHECK(status, FAIL, "Hclose");

    /* 
    * Open the file.
    */

/* This does not work, GRreadchunk will fail!
    file_id = Hopen (FILE_NAME, DFACC_READ, 0);
*/
 /*   file_id = Hopen (FILE_NAME, DFACC_WRITE, 0); */
    file_id = Hopen (file_name[i], DFACC_WRITE, 0);
    CHECK(file_id, FAIL, "Hopen");

   /*
   * Initialize the GR interface.
   */
   gr_id = GRstart (file_id);
   CHECK(gr_id, FAIL, "GRstart");

   /*for (i = 0; i < COMP_METH; i++ ) { */

   /*
   * Find the index of the specified image.
   */
   index = GRnametoindex(gr_id, image_name[i]);
   CHECK(index, FAIL, "GRnametoindex");
   
   /* 
   * Select the image.
   */
   ri_id[i] = GRselect(gr_id, index);
   CHECK(ri_id[i], FAIL, "GRselect");
   /*
   * Read third chunk back.
   */
   origin[0] = 1; origin[1] = 4;
   status = GRreadchunk(ri_id[i], origin, (VOIDP)chunk_buf);
   CHECK(status, FAIL, "GRreadchunk");
   if (0 != HDmemcmp(chunk_buf, chunk14 , sizeof(chunk14)))
      {
            MESSAGE(3, printf("%d: Error in reading chunk\n",__LINE__););
            MESSAGE(3, printf("%d: Compression method\n", (int)i););
            num_errs++;
      } /* end if */
   /*
   * Read the whole image.
   */
   start[0] = start[1] = 0;
   stride[0] = stride[1] = 1;
   edge[0] = Y_LENGTH;
   edge[1] = X_LENGTH;
   status = GRreadimage(ri_id[i], start, stride, edge, (VOIDP)data_out);
   CHECK(status, FAIL, "GRreadimage");
   if (0!= HDmemcmp(data_out, data, sizeof(data)))
      {
            MESSAGE(3, printf("%d: Error reading data for the whole image\n",__LINE__););
            MESSAGE(3, printf("%d: Compression method\n", (int)i););
            num_errs++;
      } /* end if */

   status = GRendaccess (ri_id[i]);
   CHECK(status, FAIL, "GRendaccess");

   /*} *//* end for */    
   /*
   * Terminate access to the GR interface and close the HDF file.
   */
   status = GRend (gr_id);
   CHECK(status, FAIL, "GRend");
   status = Hclose (file_id);
   CHECK(status, FAIL, "Hclose");
}
/****************************************************************
**
**  test_mgr_chunkwr(): GR chunking test 
** 
**  XIII. GR write/read chunking test with enabled compression and
**       different interlace modes.
**
**      A. Write/read GR chunks with different kinds of compressions
**         and PIXEL Interlace Mode  (test_mgr_chunkrw_pixel) 
**   
** 
****************************************************************/
static void 
test_mgr_chunkwr(void)
{
    /* Output message about test being performed */
    MESSAGE(6, printf("Testing GR chunking WRITE/READ\n"););
    
    test_mgr_chunkwr_pixelone();
    test_mgr_chunk_compress();
    test_mgr_chunkwr_pixel(0);
    test_mgr_chunkwr_pixel(1);
    test_mgr_chunkwr_pixel(2);
    test_mgr_chunkwr_pixel(3);

}   /* end test_mgr_chunkwr() */
/****************************************************************
**
**  test_mgr(): Main multi-file raster image test routine
** 
****************************************************************/
void
test_mgr(void)
{
    /*
        Each major outline portion has it's own main function:
        I. Interface Initialization      - test_mgr_init
        II. Create Images                - test_mgr_image
        III. ID/Ref/Index Functions      - test_mgr_index
        IV. Interlace Functions          - test_mgr_interlace
        V. Palette Functions             - test_mgr_lut
        VI. Special Element Functions    - test_mgr_special
        VII. Atribute Functions          - test_mgr_attr
        VIII. Access to old-style images - test_mgr_old
        IX. Compressed Image Functions   - test_mgr_compress
        X. DF24 Compatibility tests      - test_mgr_r24
        XI. DF8 Compatibility tests      - test_mgr_r8
	XII. DFP Compatibility tests     - test_mgr_pal
        XIII.  Chunking write/read test
            with enabled compression     - test_mgr_chunkwr
	XIV. Szip Compression test       - test_mgr_szip

    */

    /* Output message about test being performed */
    MESSAGE(5, printf("Testing Multi-file Raster routines\n"););
    test_mgr_init();
    test_mgr_image(0); /* normal GR */
    test_mgr_image(1); /* chunked GR */
    test_mgr_index(0);
    test_mgr_interlace(0); /* read from normal GR */
    test_mgr_interlace(1); /* read from chunked GR */
    test_mgr_lut(0);
    test_mgr_special(0);
    test_mgr_attr(0);
    test_mgr_old(0);
    test_mgr_compress(0);
    test_mgr_r24(0);
    test_mgr_r8(0);
#ifdef LATER
    test_mgr_pal(0);    /* read in old-style DFP palette tests */
#endif /* LATER */
    test_mgr_chunkwr();

#ifdef H4_GR_SZIP
/* szip not supported for GR */
#ifdef H4_HAVE_LIBSZ

    test_mgr_szip();   /* write/read with szip compression */
#else                  /* skip szip test it and report */
    printf("         -- ***** GR SZIP test skipped *****\n");
#endif
#endif

    /* Added after fixing bug #814 to test eliminating of duplicate images */
    test_mgr_dup_images();

}   /* test_mgr() */

