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
static char RcsId[] = "@(#)$Revision: 1.73 $";
#endif

/* $Id: hdftest.c,v 1.73 2005/02/11 06:07:58 bmribler Exp $ */

#include "mfhdf.h"

#ifdef macintosh
    #include <LowMem.h>
#endif

#ifdef HDF

#include "hdftest.h"

#define UFOFILE   "file.UFO"	/* non-existing file */
#define FILE1     "test1.hdf"
#define FILE2     "test2.hdf"
#define EXTTST    "exttst.hdf"    /* main file for external file test */
#define EXTFILE   "extfile.hdf"   /* external file created in test */
#define NBITFILE  "nbit.hdf"
#define COMPFILE1 "comptst1.hdf"
#define COMPFILE2 "comptst2.hdf"
#define COMPFILE3 "comptst3.hdf"
#define COMPFILE4 "comptst4.hdf"
#define COMPFILE5 "comptst5.hdf"
#define COMPFILE6 "comptst6.hdf"
#define COMPFILE7 "comptst7.hdf"
#define CHKFILE   "chktst.hdf"    /* Chunking test file */
#define CNBITFILE "chknbit.hdf"   /* Chunking w/ NBIT compression */

/* Which tests to run? */
#define EXTERNAL_TEST 
#define NBIT_TEST 
#define COMP_TEST 
#define CHUNK_TEST
/*  commented out for now because of 'long' handling on 64-bit
    machines by this version of the netCDF library is broken. 
    The new version of the netCDF library(2.4.3?) has fixed 
    this I think. To fix it here requires merging in those fixes.*/

#define NETCDF_READ_TEST 

/* Macintosh console stuff */
#if defined __MWERKS__
#include <console.h>
#endif

/* Following section used in CHUNK tests */
#ifdef CHUNK_TEST
/* Dimensions of slab */
static int32  edge_dims[3]  = {2, 3, 4};  /* size of slab dims */
static int32  start_dims[3] = {0, 0, 0};  /* starting dims  */
static int32  d_dims[3]     = {2, 3, 4};
static int32  cdims[3]      = {1, 2, 3};  /* chunk lengths */

/* Chunk teseting - arrays for chunk writes */
/* float32 arrays */
#ifdef NOT_USED
static float32  chunk1_f32[4] =
        {(float32) 0.0, (float32) 1.0, (float32) 2.0, (float32) 3.0};

static float32  chunk2_f32[4] =
        {(float32) 10.0, (float32) 11.0, (float32) 12.0, (float32) 13.0};

static float32  chunk3_f32[4] =
        {(float32) 20.0, (float32) 21.0, (float32) 22.0, (float32) 23.0};

static float32  chunk4_f32[4] =
        {(float32) 100.0, (float32) 101.0, (float32) 102.0, (float32) 103.0};

static float32  chunk5_f32[4] =
        {(float32) 110.0, (float32) 111.0, (float32) 112.0, (float32) 113.0};

static float32  chunk6_f32[4] =
        {(float32) 120.0, (float32) 121.0, (float32) 122.0, (float32) 123.0};

/* uint16 arrays */
static uint16  chunk1_u16[4] =        { 0, 1, 2, 3};

static uint16  chunk2_u16[4] =        { 10, 11, 12, 13};

static uint16  chunk3_u16[4] =        { 20, 21, 22, 23};

static uint16  chunk4_u16[4] =        { 100, 101, 102, 103};

static uint16  chunk5_u16[4] =        { 110, 111, 112, 113};

static uint16  chunk6_u16[4] =        { 120, 121, 122, 123};
#endif /* NOT_USED */

/* uint16 chunk arrays used in example 1 */
static uint16  chunk1_2u16[6] = {11, 21, 
                                 12, 22, 
                                 13, 23};

static uint16  chunk2_2u16[6] = {31, 41, 
                                 32, 42, 
                                 33, 43};

static uint16  chunk3_2u16[6] = {14, 24, 
                                 15, 25, 
                                 16, 26};

static uint16  chunk4_2u16[6] = {34, 44, 
                                 35, 45, 
                                 36, 46};

static uint16  chunk5_2u16[6] = {17, 27, 
                                 18, 28, 
                                 19, 29};

static uint16  chunk6_2u16[6] = {37, 47, 
                                 38, 48, 
                                 39, 49};

/* for visual layout in Example 1*/
static uint16  u16_2data[9][4] =
{ 
   {11, 21, 31, 41},
   {12, 22, 32, 42},
   {13, 23, 33, 43},
   {14, 24, 34, 44},
   {15, 25, 35, 45},
   {16, 26, 36, 46},
   {17, 27, 37, 47},
   {18, 28, 38, 48},
   {19, 29, 39, 49},
};

/* for comparison in example 1 */
static uint16 u16_2cdata[5][2] =
{
  {23, 33},
  {24, 34},
  {25, 35},
  {26, 36},
  {27, 37}
};

/* uint8 arrays */
static uint8  chunk1_u8[4] = { 0, 1, 2, 3};

static uint8  chunk2_u8[4] = { 10, 11, 12, 13};

static uint8  chunk3_u8[4] = { 20, 21, 22, 23};

static uint8  chunk4_u8[4] = { 100, 101, 102, 103};

static uint8  chunk5_u8[4] = { 110, 111, 112, 113};

static uint8  chunk6_u8[4] = { 120, 121, 122, 123};

/* data arrays layed out in memory  */
/* for comparison */
static float32  f32_data[2][3][4] =
{
    {
        {(float32) 0.0, (float32) 1.0, (float32) 2.0, (float32) 3.0},
        {(float32) 10.0, (float32) 11.0, (float32) 12.0, (float32) 13.0},
        {(float32) 20.0, (float32) 21.0, (float32) 22.0, (float32) 23.0}},
    {
        {(float32) 100.0, (float32) 101.0, (float32) 102.0, (float32) 103.0},
        {(float32) 110.0, (float32) 111.0, (float32) 112.0, (float32) 113.0},
        {(float32) 120.0, (float32) 121.0, (float32) 122.0, (float32) 123.0}}};

static uint16  u16_data[2][3][4] =
{
    {
        { 0, 1, 2, 3},
        { 10, 11, 12, 13},
        { 20, 21, 22, 23}},
    {
        { 100, 101, 102, 103},
        { 110, 111, 112, 113},
        { 120, 121, 122, 123}}};

static uint8  u8_data[2][3][4] =
{
    {
        { 0, 1, 2, 3},
        { 10, 11, 12, 13},
        { 20, 21, 22, 23}},
    {
        { 100, 101, 102, 103},
        { 110, 111, 112, 113},
        { 120, 121, 122, 123}}};

extern int test_szip_compression();
extern int test_checkempty();
extern int test_idtest();
extern int test_sd();

static intn
test_chunk()
{
    int32 fchk; /* File handles */
    int32 nt;                /* Number type */
    int32 dimsize[10];       /* dimension sizes */
    int32 newsds, newsds2; /* SDS handles */
    int32   newsds4, newsds5, newsds6, newsds7, newsds8;   /* Chunked SDS ids */
    float32 inbuf_f32[2][3][4];  /* float32 Data array read from from file */
    uint16  inbuf_u16[2][3][4];  /* uint16 Data array read from from file */
    uint16  inbuf1_2u16[9][4];   /* Data array read for Example 1 */
    uint16  inbuf_2u16[5][2];    /* Data array read for Example 1 */
    uint8   inbuf_u8[2][3][4];   /* uint8 Data array read from from file */
    uint8   ru8_data[4];         /* chunk input buffer */
    int32   *rcdims;             /* for SDgetchunkinfo() */
    uint16  fill_u16 = 0;        /* fill value */
    HDF_CHUNK_DEF chunk_def;     /* Chunk defintion set */ 
    HDF_CHUNK_DEF rchunk_def;    /* Chunk defintion read */ 
    comp_coder_t comp_type;      /* to retrieve compression type into */
    comp_info cinfo;             /* compression information structure */
    int32   cflags;              /* chunk flags */
    int32   index;       /* Index of dataset in file */
    intn    status;      /* status flag */
    intn    i,j,k;       /* loop variables */
    int32   start[10], end[10]; /* start, end, stride arrays */
    int32   idata[100];
    int32   rdata[100];
    float32 max;
    int     num_errs = 0;    /* number of errors so far */

    /* Create file 'chktst.hdf' */
    fchk = SDstart(CHKFILE, DFACC_CREATE);
    CHECK(fchk, FAIL, "SDstart");

    /* 
     * Test 1. Create a 9x4 SDS of uint16 in file 1 
     *         With chunks of 3x2, will create 6 chunks.
     */
    d_dims[0] = 9;
    d_dims[1] = 4;
    newsds8 = SDcreate(fchk, "DataSetChunked_2D_1", DFNT_UINT16, 2, d_dims);
    if(newsds8 == FAIL) 
      {
        fprintf(stderr, "Chunk Test 1. Failed to create a new data set \n");
        num_errs++;
        goto test2;
      }

    /* set fill value */
    fill_u16 = 0;
    status = SDsetfillvalue(newsds8, (VOIDP) &fill_u16);
    CHECK(status, FAIL, "Chunk Test 1. SDsetfillvalue");

    /* Create chunked SDS 
       chunk is 3x2 which will create 6 chunks */
    cdims[0] = chunk_def.chunk_lengths[0] = 3;
    cdims[1] = chunk_def.chunk_lengths[1] = 2;
    status = SDsetchunk(newsds8, chunk_def, HDF_CHUNK);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 1. Failed to create new chunked data set\n");
        num_errs++;
        goto test2;
      }

    /* Set Chunk cache to hold 2 chunks */
    status = SDsetchunkcache(newsds8, 2, 0);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 1. SDsetchunkcache failed\n");
        num_errs++;
        goto test2;
      }


    /* Write data */
    start_dims[0] = 0;
    start_dims[1] = 0;
    edge_dims[0] = 9;
    edge_dims[1] = 4;
    status = SDwritedata(newsds8, start_dims, NULL, edge_dims, (VOIDP) u16_2data);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 1. Failed to write u16_2data to new chunked data set\n");
        num_errs++;
        goto test2;
      }

    /* read a portion of data back in using SDreaddata*/
    start_dims[0] = 0;
    start_dims[1] = 0;
    edge_dims[0] = 9;
    edge_dims[1] = 4;
    status = SDreaddata(newsds8, start_dims, NULL, edge_dims, (VOIDP) inbuf1_2u16);
    CHECK(status, FAIL, "Chunk Test 1. SDreaddata");
    for (i = 0; i < 9; i++)
      {
        for (j = 0; j < 4; j++)
          {
              if (inbuf1_2u16[i][j] != u16_2data[i][j])
                {
                    fprintf(stderr,"Chunk Test 1. inbuf1_2u16[%d][%d]=%d,",
                            i,j,inbuf1_2u16[i][j]);
                    fprintf(stderr,"u16_cdata[%d][%d]=%d,",
                            i,j,u16_2data[i][j]);
                    fprintf(stderr,"\n");
                    num_errs++;
                }
          }
      }
    /* Get chunk lengths */
    status = SDgetchunkinfo(newsds8, &rchunk_def, &cflags);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 1. SDgetchunkinfo failed \n");
        num_errs++;
        goto test2;
      }

    rcdims = rchunk_def.chunk_lengths;

    /* check chunk lengths and to see if SDS is chunked */
    if (cdims[0] != rcdims[0] || cdims[1] != rcdims[1] || cflags != HDF_CHUNK)
      {
        fprintf(stderr, "Chunk Test 1. SDgetchunkinfo returned wrong values\n");
        num_errs++;
        goto test2;
      }

    /* Close down this SDS*/    
    status = SDendaccess(newsds8);
    CHECK(status, FAIL, "Chunk Test 1. SDendaccess");

    /* 
      Test 2. 2-D 9x4 SDS of uint16 with 3x2 chunks
                 Write data using SDwritechunk().
                 Read data using SDreaddata().
    */
  test2:

    /* create a  9x4 SDS of uint16 in file 1 */
    d_dims[0] = 9;
    d_dims[1] = 4;
    newsds7 = SDcreate(fchk, "DataSetChunked_2D_2", DFNT_UINT16, 2, d_dims);
    if(newsds7 == FAIL) 
      {
        fprintf(stderr, "Chunk Test 2. Failed to create a new data set \n");
        num_errs++;
        goto test3;
      }

    /* set fill value */
    fill_u16 = 0;
    status = SDsetfillvalue(newsds7, (VOIDP) &fill_u16);
    CHECK(status, FAIL, "Chunk Test 2. SDsetfillvalue");

    /* Create chunked SDS 
       chunk is 3x2 which will create 6 chunks */
    cdims[0] = chunk_def.chunk_lengths[0] = 3;
    cdims[1] = chunk_def.chunk_lengths[1] = 2;
    status = SDsetchunk(newsds7, chunk_def, HDF_CHUNK);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 2. Failed to create new chunked data set\n");
        num_errs++;
        goto test3;
      }

    /* Set Chunk cache to hold 2 chunks */
    status = SDsetchunkcache(newsds7, 2, 0);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 2.SDsetchunkcache failed\n");
        num_errs++;
        goto test3;
      }

    /* Write data use SDwriteChunk */

    /* Write chunk 1 */
    start_dims[0] = 0;
    start_dims[1] = 0;
    status = SDwritechunk(newsds7, start_dims, (VOIDP) chunk1_2u16);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 2.SDwritechunk failed to write chunk 1\n");
        num_errs++;
        goto test3;
      }

    /* Write chunk 4 */
    start_dims[0] = 1;
    start_dims[1] = 1;
    status = SDwritechunk(newsds7, start_dims, (VOIDP) chunk4_2u16);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 2.SDwritechunk failed to write chunk 4\n");
        num_errs++;
        goto test3;
      }

    /* Write chunk 2 */
    start_dims[0] = 0;
    start_dims[1] = 1;
    status = SDwritechunk(newsds7, start_dims, (VOIDP) chunk2_2u16);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 2.SDwritechunk failed to write chunk 2\n");
        num_errs++;
        goto test3;
      }

    /* Write chunk 5 */
    start_dims[0] = 2;
    start_dims[1] = 0;
    status = SDwritechunk(newsds7, start_dims, (VOIDP) chunk5_2u16);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 2.SDwritechunk failed to write chunk 5\n");
        num_errs++;
        goto test3;
      }

    /* Write chunk 3 */
    start_dims[0] = 1;
    start_dims[1] = 0;
    status = SDwritechunk(newsds7, start_dims, (VOIDP) chunk3_2u16);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 2.SDwritechunk failed to write chunk 3\n");
        num_errs++;
        goto test3;
      }

    /* Write chunk 6 */
    start_dims[0] = 2;
    start_dims[1] = 1;
    status = SDwritechunk(newsds7, start_dims, (VOIDP) chunk6_2u16);
     if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 2.SDwritechunk failed to write chunk 6\n");
        num_errs++;
        goto test3;
      }

    /* read a portion of data back in using SDreaddata
       i.e  5x2 subset of the whole array */
    start_dims[0] = 2;
    start_dims[1] = 1;
    edge_dims[0] = 5;
    edge_dims[1] = 2;
    status = SDreaddata(newsds7, start_dims, NULL, edge_dims, (VOIDP) inbuf_2u16);
    CHECK(status, FAIL, "Chunk Test 2. SDreaddata");
   /* This 5x2 array should look somethink like this
         {{23, 24, 25, 26, 27},
          {33, 34, 35, 36, 37}}    
    */
    for (i = 0; i < 5; i++)
      {
        for (j = 0; j < 2; j++)
          {
              if (inbuf_2u16[i][j] != u16_2cdata[i][j])
                {
                    fprintf(stderr,"Chunk Test 2. inbuf_2u16[%d][%d]=%d,",
                            i,j,inbuf_2u16[i][j]);
                    fprintf(stderr,"u16_2cdata[%d][%d]=%d,",
                            i,j,u16_2cdata[i][j]);
                    fprintf(stderr,"\n");
                    num_errs++;
                }
          }
      }
    /* Get chunk lengths */
    status = SDgetchunkinfo(newsds7, &rchunk_def, &cflags);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 2.SDgetchunkinfo failed \n");
        num_errs++;
        goto test3;
      }

    rcdims = rchunk_def.chunk_lengths;

    /* check chunk lengths and see if SDS is chunked */
    if (cdims[0] != rcdims[0] || cdims[1] != rcdims[1] || cflags != HDF_CHUNK)
      {
        fprintf(stderr, "Chunk Test 2.SDgetchunkinfo returned wrong values\n");
        num_errs++;
        goto test3;
      }

    /* Close down this SDS*/    
    status = SDendaccess(newsds7);
    CHECK(status, FAIL, "Chunk Test 2. SDendaccess");

    /* 
     * Next 3 differnet number types are tested with 3-D arrays
     */
  test3:
    /* 
     * Test 3. create a new chunked SDS of float32 in file 1 
     */
    d_dims[0] = 2;
    d_dims[1] = 3;
    d_dims[2] = 4;
    newsds4 = SDcreate(fchk, "DataSetChunked_3D_1", DFNT_FLOAT32, 3, d_dims);
    if(newsds4 == FAIL) 
      {
        fprintf(stderr, "Chunk Test 3. Failed to create a new 3D float32 data set \n");
        num_errs++;
        goto test4;
      }

    max = 0.0;
    status = SDsetfillvalue(newsds4, (VOIDP) &max);
    CHECK(status, FAIL, "Chunk Test 3. SDsetfillvalue");

    /* Set chunking */
    cdims[0] = chunk_def.chunk_lengths[0] = 1;
    cdims[1] = chunk_def.chunk_lengths[1] = 2;
    cdims[2] = chunk_def.chunk_lengths[2] = 3;
    status = SDsetchunk(newsds4, chunk_def, HDF_CHUNK);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 3. Failed to create new chunked data set\n");
        num_errs++;
        goto test4;
      }

    /* Write data out */
    start_dims[0] = 0;
    start_dims[1] = 0;
    start_dims[2] = 0;
    edge_dims[0] = 2;
    edge_dims[1] = 3;
    edge_dims[2] = 4;
    status = SDwritedata(newsds4, start_dims, NULL, edge_dims, (VOIDP) f32_data);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 3. Failed to write f32_data to new chunked data set\n");
        num_errs++;
        goto test4;
      }

    /* read data back in */
    start_dims[0] = 0;
    start_dims[1] = 0;
    start_dims[2] = 0;
    edge_dims[0] = 2;
    edge_dims[1] = 3;
    edge_dims[2] = 4;
    status = SDreaddata(newsds4, start_dims, NULL, edge_dims, (VOIDP) inbuf_f32);
    CHECK(status, FAIL, "Chunk Test 3. SDreaddata");

    for (i = 0; i < d_dims[0]; i++)
      {
        for (j = 0; j < d_dims[1]; j++)
          {
            for (k = 0; k < d_dims[2]; k++)
              {
                  if (inbuf_f32[i][j][k] != f32_data[i][j][k])
                    {
                        fprintf(stderr,"Chunk Test 3. inbuf_f32[%d][%d][%d]=%f,",
                                i,j,k,inbuf_f32[i][j][k]);
                        fprintf(stderr,"f32_data[%d][%d][%d]=%f,",
                                i,j,k,f32_data[i][j][k]);
                        fprintf(stderr,"\n");
                        num_errs++;
                    }
              }
          }
      }

    /* Close down SDS*/    
    status = SDendaccess(newsds4);
    CHECK(status, FAIL, "Chunk Test 3. SDendaccess");


    /* 
     * Test 4. Create a new chunked SDS of uint16 in file 1 
     */
  test4:

    d_dims[0] = 2;
    d_dims[1] = 3;
    d_dims[2] = 4;
    newsds5 = SDcreate(fchk, "DataSetChunked_3D_2", DFNT_UINT16, 3, d_dims);
    if(newsds5 == FAIL) 
      {
        fprintf(stderr, "Chunk Test 4. Failed to set a new uint16 3D data set chunked\n");
        num_errs++;
        goto test5;
      }

    /* set fill value */
    fill_u16 = 0;
    status = SDsetfillvalue(newsds5, (VOIDP) &fill_u16);
    CHECK(status, FAIL, "Chunk Test 4. SDsetfillvalue");

    /* Set chunking, chunk is 1x2x3 */
    cdims[0] = chunk_def.chunk_lengths[0] = 1;
    cdims[1] = chunk_def.chunk_lengths[1] = 2;
    cdims[2] = chunk_def.chunk_lengths[2] = 3;
    status = SDsetchunk(newsds5, chunk_def, HDF_CHUNK);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 4. Failed to create new chunked data set\n");
        num_errs++;
        goto test5;
      }

    /* Set Chunk cache */
    status = SDsetchunkcache(newsds5, 4, 0);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 4. SDsetchunkcache failed\n");
        num_errs++;
        goto test5;
      }

    /* Write data */
    start_dims[0] = 0;
    start_dims[1] = 0;
    start_dims[2] = 0;
    edge_dims[0] = 2;
    edge_dims[1] = 3;
    edge_dims[2] = 4;
    status = SDwritedata(newsds5, start_dims, NULL, edge_dims, (VOIDP) u16_data);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 4. Failed to write u16_data to new chunked data set\n");
        num_errs++;
        goto test5;
      }

    /* read data back in */
    start_dims[0] = 0;
    start_dims[1] = 0;
    start_dims[2] = 0;
    edge_dims[0] = 2;
    edge_dims[1] = 3;
    edge_dims[2] = 4;
    status = SDreaddata(newsds5, start_dims, NULL, edge_dims, (VOIDP) inbuf_u16);
    CHECK(status, FAIL, "Chunk Test 4. SDreaddata");
    for (i = 0; i < d_dims[0]; i++)
      {
        for (j = 0; j < d_dims[1]; j++)
          {
            for (k = 0; k < d_dims[2]; k++)
              {
                  if (inbuf_u16[i][j][k] != u16_data[i][j][k])
                    {
                        fprintf(stderr,"Chunk Test 4. inbuf_u16[%d][%d][%d]=%d,",
                                i,j,k,inbuf_u16[i][j][k]);
                        fprintf(stderr,"u16_data[%d][%d][%d]=%d,",
                                i,j,k,u16_data[i][j][k]);
                        fprintf(stderr,"\n");
                        num_errs++;
                    }
              }
          }
      }
    /* Check chunk info */
    status = SDgetchunkinfo(newsds5, &rchunk_def, &cflags);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 4. SDgetchunkinfo failed \n");
        num_errs++;
        goto test5;
      }

    rcdims = rchunk_def.chunk_lengths;

    if (cdims[0] != rcdims[0] || cdims[1] != rcdims[1] || cdims[2] != rcdims[2] 
        || cflags != HDF_CHUNK)
      {
        fprintf(stderr, "Chunk Test 4. SDgetchunkinfo returned wrong values\n");
        num_errs++;
        goto test5;
      }

    /* Close down SDS*/    
    status = SDendaccess(newsds5);
    CHECK(status, FAIL, "Chunk Test 4. SDendaccess");


    /* 
     * Test 5. Create a new chunked SDS of uint8 in file 1 
     */
  test5:

    d_dims[0] = 2;
    d_dims[1] = 3;
    d_dims[2] = 4;
    newsds6 = SDcreate(fchk, "DataSetChunked_3D_3", DFNT_UINT8, 3, d_dims);
    if(newsds6 == FAIL) 
      {
        fprintf(stderr, "Chunk Test 5. Failed to set a new uint8 3D data set chunked\n");
        num_errs++;
        goto test6;
      }

    /* Set chunking, chunk is 1x1x4 */
    cdims[0] = chunk_def.chunk_lengths[0] = 1;
    cdims[1] = chunk_def.chunk_lengths[1] = 1;
    cdims[2] = chunk_def.chunk_lengths[2] = 4;
    status = SDsetchunk(newsds6, chunk_def, HDF_CHUNK);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 5. Failed to create new chunked data set\n");
        num_errs++;
        goto test6;
      }

#if 0
    /* Write data using SDwritedata() */
    start_dims[0] = 0;
    start_dims[1] = 0;
    start_dims[2] = 0;
    edge_dims[0] = 2;
    edge_dims[1] = 3;
    edge_dims[2] = 4;
    status = SDwritedata(newsds6, start_dims, NULL, edge_dims, (VOIDP) wu8_data);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 5. Failed to write wu8_data to new chunked data set\n");
        num_errs++;
        goto test6;
      }
#endif

    /* Write data use SDwriteChunk */
    start_dims[0] = 0;
    start_dims[1] = 0;
    start_dims[2] = 0;
    status = SDwritechunk(newsds6, start_dims, (VOIDP) chunk1_u8);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 5. SDwritechunk failed to write chunk 1\n");
        num_errs++;
        goto test6;
      }

    start_dims[0] = 1;
    start_dims[1] = 0;
    start_dims[2] = 0;
    status = SDwritechunk(newsds6, start_dims, (VOIDP) chunk4_u8);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 5. SDwritechunk failed to write chunk 4\n");
        num_errs++;
        goto test6;
      }

    start_dims[0] = 0;
    start_dims[1] = 1;
    start_dims[2] = 0;
    status = SDwritechunk(newsds6, start_dims, (VOIDP) chunk2_u8);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 5. SDwritechunk failed to write chunk 2\n");
        num_errs++;
        goto test6;
      }

    start_dims[0] = 1;
    start_dims[1] = 1;
    start_dims[2] = 0;
    status = SDwritechunk(newsds6, start_dims, (VOIDP) chunk5_u8);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 5. SDwritechunk failed to write chunk 5\n");
        num_errs++;
        goto test6;
      }

    start_dims[0] = 0;
    start_dims[1] = 2;
    start_dims[2] = 0;
    status = SDwritechunk(newsds6, start_dims, (VOIDP) chunk3_u8);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 5. SDwritechunk failed to write chunk 3\n");
        num_errs++;
        goto test6;
      }

    start_dims[0] = 1;
    start_dims[1] = 2;
    start_dims[2] = 0;
    status = SDwritechunk(newsds6, start_dims, (VOIDP) chunk6_u8);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 5. SDwritechunk failed to write chunk 6\n");
        num_errs++;
        goto test6;
      }

    /* read data back in */
    start_dims[0] = 0;
    start_dims[1] = 0;
    start_dims[2] = 0;
    edge_dims[0] = 2;
    edge_dims[1] = 3;
    edge_dims[2] = 4;
    status = SDreaddata(newsds6, start_dims, NULL, edge_dims, (VOIDP) inbuf_u8);
    CHECK(status, FAIL, "Chunk Test 5. SDreaddata");
    for (i = 0; i < d_dims[0]; i++)
      {
        for (j = 0; j < d_dims[1]; j++)
          {
            for (k = 0; k < d_dims[2]; k++)
              {
                  if (inbuf_u8[i][j][k] != u8_data[i][j][k])
                    {
                        fprintf(stderr,"Chunk Test 5. inbuf_u8[%d][%d][%d]=%d,",
                                i,j,k,inbuf_u8[i][j][k]);
                        fprintf(stderr,"u8_data[%d][%d][%d]=%d,",
                                i,j,k,u8_data[i][j][k]);
                        fprintf(stderr,"\n");
                        num_errs++;
                    }
              }
          }
      }

    start_dims[0] = 0;
    start_dims[1] = 0;
    start_dims[2] = 0;
    status = SDreadchunk(newsds6, start_dims, (VOIDP) ru8_data);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 5. SDreadchunk failed to read chunk 1\n");
        num_errs++;
        goto test6;
      }

    /* Verify chunk */
    for (i = 0; i < 4; i++)
       {
          if (ru8_data[i] != chunk1_u8[i])
            {
                printf("Chunk Test 5. chunk1_u8: Wrong data at %d, out %d in %d\n", 
                 i, chunk1_u8[i], ru8_data[i]);
                num_errs++;
            }
       }

    start_dims[0] = 0;
    start_dims[1] = 1;
    start_dims[2] = 0;
    status = SDreadchunk(newsds6, start_dims, (VOIDP) ru8_data);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 5. SDreadchunk failed to read chunk 2\n");
        num_errs++;
        goto test6;
      }

    /* Verify chunk */
    for (i = 0; i < 4; i++)
       {
          if (ru8_data[i] != chunk2_u8[i])
            {
                printf("Chunk Test 5. chunk2_u8: Wrong data at %d, out %d in %d\n", 
                 i, chunk2_u8[i], ru8_data[i]);
                num_errs++;
            }
       }
    start_dims[0] = 0;
    start_dims[1] = 2;
    start_dims[2] = 0;
    status = SDreadchunk(newsds6, start_dims, (VOIDP) ru8_data);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 5. SDreadchunk failed to read chunk 3\n");
        num_errs++;
        goto test6;
      }

    /* Verify chunk */
    for (i = 0; i < 4; i++)
       {
          if (ru8_data[i] != chunk3_u8[i])
            {
                printf("Chunk Test 5. chunk3_u8: Wrong data at %d, out %d in %d\n", 
                 i, chunk3_u8[i], ru8_data[i]);
                num_errs++;
            }
       }

    start_dims[0] = 1;
    start_dims[1] = 0;
    start_dims[2] = 0;
    status = SDreadchunk(newsds6, start_dims, (VOIDP) ru8_data);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 5. SDreadchunk failed to read chunk 4\n");
        num_errs++;
        goto test6;
      }

    /* Verify chunk */
    for (i = 0; i < 4; i++)
       {
          if (ru8_data[i] != chunk4_u8[i])
            {
                printf("Chunk Test 5. chunk4_u8: Wrong data at %d, out %d in %d\n", 
                 i, chunk4_u8[i], ru8_data[i]);
                num_errs++;
            }
       }

    start_dims[0] = 1;
    start_dims[1] = 1;
    start_dims[2] = 0;
    status = SDreadchunk(newsds6, start_dims, (VOIDP) ru8_data);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 5. SDreadchunk failed to read chunk 5\n");
        num_errs++;
        goto test6;
      }

    /* Verify chunk */
    for (i = 0; i < 4; i++)
       {
          if (ru8_data[i] != chunk5_u8[i])
            {
                printf("Chunk Test 5. chunk5_u8: Wrong data at %d, out %d in %d\n", 
                 i, chunk5_u8[i], ru8_data[i]);
                num_errs++;
            }
       }

    start_dims[0] = 1;
    start_dims[1] = 2;
    start_dims[2] = 0;
    status = SDreadchunk(newsds6, start_dims, (VOIDP) ru8_data);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 5. SDreadchunk failed to read chunk 6\n");
        num_errs++;
        goto test6;
      }

    /* Verify chunk */
    for (i = 0; i < 4; i++)
       {
          if (ru8_data[i] != chunk6_u8[i])
            {
                printf("Chunk Test 5. chunk6_u8: Wrong data at %d, out %d in %d\n", 
                 i, chunk6_u8[i], ru8_data[i]);
                num_errs++;
            }
       }

    /* Close down SDS*/    
    status = SDendaccess(newsds6);
    CHECK(status, FAIL, "Chunk Test 5. SDendaccess");


    /* ---------------------------------------------------------------
     *  Chunking with Compression 
     ----------------------------------------------------------------*/


    /* 
     * Test 6. Create a new chunked SDS of uint8 in file 1 
     *         Compress using Skipping Huffman. Write using SDwriteChunk
     *         Read back in using SDreaddata and SDreadChunk. 
     *	       Retrieve and verify the compression information.
     *         Use Skipping Huffman compression
     *         Note: a template is created first then the SDS 
     *               is re-slected for writing/reading.
     */
  test6:

    d_dims[0] = 2;
    d_dims[1] = 3;
    d_dims[2] = 4;
    newsds6 = SDcreate(fchk, "DataSetChunked_3D_SKIP_HUF_2", DFNT_UINT8, 3, d_dims);
    if(newsds6 == FAIL) 
      {
        fprintf(stderr, "Chunk Test 6. Failed to set a new uint8 3D data set chunked\n");
        num_errs++;
        goto test7;
      }

    /* Set chunking, chunk is 1x1x4 */
    cdims[0] = chunk_def.comp.chunk_lengths[0] = 1;
    cdims[1] = chunk_def.comp.chunk_lengths[1] = 1;
    cdims[2] = chunk_def.comp.chunk_lengths[2] = 4;
#if 0
    chunk_def.comp.comp_type = COMP_CODE_RLE;
#endif
    /* the test for SDgetcompress relies on this compression setting , so 
       if the setting is changed, please ensure that the verification of 
       the next call to SDgetcompress below is still valid - BMR */
    chunk_def.comp.comp_type = COMP_CODE_SKPHUFF; /* Skipping Huffman */
    chunk_def.comp.cinfo.skphuff.skp_size = sizeof(uint16);

    status = SDsetchunk(newsds6, chunk_def, HDF_CHUNK | HDF_COMP);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 6. Failed to create new chunked, Skipping Huffman compressed data set\n");
        num_errs++;
        goto test7;
      }

    /* Close down SDS ie. template creation*/    
    status = SDendaccess(newsds6);
    CHECK(status, FAIL, "Chunk Test 6. SDendaccess");

    newsds6 = FAIL;

    /* Select same SDS again, first get index */
    if ((index = SDnametoindex(fchk,"DataSetChunked_3D_SKIP_HUF_2")) == FAIL)
      {
          fprintf(stderr, "Chunk Test 6. SDnametoindex  Failed for  Skipping Huffman compressed data set\n");
          num_errs++;
          goto test7;
      }

    if ((newsds6 = SDselect(fchk,index)) == FAIL)
      {
          fprintf(stderr, "Chunk Test 6. SDselect Failed to re-select new chunked, Skipping Huffman compressed data set\n");
          num_errs++;
          goto test7;
      }

    /*
     * Retrieve and verify the compression info - bug# 307, 10/10/01 - BMR
     */
    comp_type = COMP_CODE_INVALID;  /* reset variables before retrieving info */
    HDmemset(&cinfo, 0, sizeof(cinfo)) ;
    status = SDgetcompress(newsds6, &comp_type, &cinfo);
    CHECK(status, FAIL, "SDgetcompress");
    VERIFY(comp_type, chunk_def.comp.comp_type, "SDgetcompress");
    VERIFY(cinfo.skphuff.skp_size, chunk_def.comp.cinfo.skphuff.skp_size, "SDgetcompress");
    /* end of test for bug#307 */

    /* Write data use SDwriteChunk */
    start_dims[0] = 0;
    start_dims[1] = 0;
    start_dims[2] = 0;
    status = SDwritechunk(newsds6, start_dims, (VOIDP) chunk1_u8);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 6. SDwritechunk failed to write chunk 1\n");
        num_errs++;
        goto test7;
      }

    start_dims[0] = 1;
    start_dims[1] = 0;
    start_dims[2] = 0;
    status = SDwritechunk(newsds6, start_dims, (VOIDP) chunk4_u8);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 6. SDwritechunk failed to write chunk 4\n");
        num_errs++;
        goto test7;
      }

    start_dims[0] = 0;
    start_dims[1] = 1;
    start_dims[2] = 0;
    status = SDwritechunk(newsds6, start_dims, (VOIDP) chunk2_u8);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 6. SDwritechunk failed to write chunk 2\n");
        num_errs++;
        goto test7;
      }

    start_dims[0] = 1;
    start_dims[1] = 1;
    start_dims[2] = 0;
    status = SDwritechunk(newsds6, start_dims, (VOIDP) chunk5_u8);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 6. SDwritechunk failed to write chunk 5\n");
        num_errs++;
        goto test7;
      }

    start_dims[0] = 0;
    start_dims[1] = 2;
    start_dims[2] = 0;
    status = SDwritechunk(newsds6, start_dims, (VOIDP) chunk3_u8);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 6. SDwritechunk failed to write chunk 3\n");
        num_errs++;
        goto test7;
      }

    start_dims[0] = 1;
    start_dims[1] = 2;
    start_dims[2] = 0;
    status = SDwritechunk(newsds6, start_dims, (VOIDP) chunk6_u8);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 6. SDwritechunk failed to write chunk 6\n");
        num_errs++;
        goto test7;
      }

    /* read data back in */
    start_dims[0] = 0;
    start_dims[1] = 0;
    start_dims[2] = 0;
    edge_dims[0] = 2;
    edge_dims[1] = 3;
    edge_dims[2] = 4;
    status = SDreaddata(newsds6, start_dims, NULL, edge_dims, (VOIDP) inbuf_u8);
    CHECK(status, FAIL, "Chunk Test 6. SDreaddata");
    for (i = 0; i < d_dims[0]; i++)
      {
        for (j = 0; j < d_dims[1]; j++)
          {
            for (k = 0; k < d_dims[2]; k++)
              {
                  if (inbuf_u8[i][j][k] != u8_data[i][j][k])
                    {
                        fprintf(stderr,"Chunk Test 6. inbuf_u8[%d][%d][%d]=%d,",
                                i,j,k,inbuf_u8[i][j][k]);
                        fprintf(stderr,"u8_data[%d][%d][%d]=%d,",
                                i,j,k,u8_data[i][j][k]);
                        fprintf(stderr,"\n");
                        num_errs++;
                    }
              }
          }
      }

    start_dims[0] = 0;
    start_dims[1] = 0;
    start_dims[2] = 0;
    status = SDreadchunk(newsds6, start_dims, (VOIDP) ru8_data);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 6. SDreadchunk failed to read chunk 1\n");
        num_errs++;
        goto test7;
      }

    /* Verify chunk */
    for (i = 0; i < 4; i++)
       {
          if (ru8_data[i] != chunk1_u8[i])
            {
                printf("Chunk Test 6. chunk1_u8: Wrong data at %d, out %d in %d\n", 
                 i, chunk1_u8[i], ru8_data[i]);
                num_errs++;
            }
       }

    start_dims[0] = 0;
    start_dims[1] = 1;
    start_dims[2] = 0;
    status = SDreadchunk(newsds6, start_dims, (VOIDP) ru8_data);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 6. SDreadchunk failed to read chunk 2\n");
        num_errs++;
        goto test7;
      }

    /* Verify chunk */
    for (i = 0; i < 4; i++)
       {
          if (ru8_data[i] != chunk2_u8[i])
            {
                printf("Chunk Test 6. chunk2_u8: Wrong data at %d, out %d in %d\n", 
                 i, chunk2_u8[i], ru8_data[i]);
                num_errs++;
            }
       }
    start_dims[0] = 0;
    start_dims[1] = 2;
    start_dims[2] = 0;
    status = SDreadchunk(newsds6, start_dims, (VOIDP) ru8_data);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 6. SDreadchunk failed to read chunk 3\n");
        num_errs++;
        goto test7;
      }

    /* Verify chunk */
    for (i = 0; i < 4; i++)
       {
          if (ru8_data[i] != chunk3_u8[i])
            {
                printf("Chunk Test 6. chunk3_u8: Wrong data at %d, out %d in %d\n", 
                 i, chunk3_u8[i], ru8_data[i]);
                num_errs++;
            }
       }

    start_dims[0] = 1;
    start_dims[1] = 0;
    start_dims[2] = 0;
    status = SDreadchunk(newsds6, start_dims, (VOIDP) ru8_data);
    if(status == FAIL) 
      {
        fprintf(stderr, "SDreadchunk failed to read chunk 4\n");
        num_errs++;
        goto test7;
      }

    /* Verify chunk */
    for (i = 0; i < 4; i++)
       {
          if (ru8_data[i] != chunk4_u8[i])
            {
                printf("Chunk Test 6. chunk4_u8: Wrong data at %d, out %d in %d\n", 
                 i, chunk4_u8[i], ru8_data[i]);
                num_errs++;
            }
       }

    start_dims[0] = 1;
    start_dims[1] = 1;
    start_dims[2] = 0;
    status = SDreadchunk(newsds6, start_dims, (VOIDP) ru8_data);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 6. SDreadchunk failed to read chunk 5\n");
        num_errs++;
        goto test7;
      }

    /* Verify chunk */
    for (i = 0; i < 4; i++)
       {
          if (ru8_data[i] != chunk5_u8[i])
            {
                printf("Chunk Test 6. chunk5_u8: Wrong data at %d, out %d in %d\n", 
                 i, chunk5_u8[i], ru8_data[i]);
                num_errs++;
            }
       }

    start_dims[0] = 1;
    start_dims[1] = 2;
    start_dims[2] = 0;
    status = SDreadchunk(newsds6, start_dims, (VOIDP) ru8_data);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 6. SDreadchunk failed to read chunk 6\n");
        num_errs++;
        goto test7;
      }

    /* Verify chunk */
    for (i = 0; i < 4; i++)
       {
          if (ru8_data[i] != chunk6_u8[i])
            {
                printf("Chunk Test 6. chunk6_u8: Wrong data at %d, out %d in %d\n", 
                 i, chunk6_u8[i], ru8_data[i]);
                num_errs++;
            }
       }

    /* Close down SDS*/    
    status = SDendaccess(newsds6);
    CHECK(status, FAIL, "Chunk Test 6. SDendaccess");

    /* 
     * Test 7. Create a  9x4 SDS of uint16 in file 1 
     *         Write using SDwritedata, read back in using SDreaddata
     *         Use GZIP compression.
     */
  test7:

    d_dims[0] = 9;
    d_dims[1] = 4;
    newsds7 = SDcreate(fchk, "DataSetChunked_2D_GZIP_1", DFNT_UINT16, 2, d_dims);
    if(newsds7 == FAIL) 
      {
        fprintf(stderr, "Chunk Test 7. Failed to create a new 2D uint16 data set \n");
        num_errs++;
        goto test8;
      }

    /* set fill value */
    fill_u16 = 0;
    status = SDsetfillvalue(newsds7, (VOIDP) &fill_u16);
    CHECK(status, FAIL, "Chunk Test 7. SDsetfillvalue");

    /* Create chunked SDS 
       chunk is 3x2 which will create 6 chunks.
       Use GZIP compression */
    cdims[0] = chunk_def.comp.chunk_lengths[0] = 3;
    cdims[1] = chunk_def.comp.chunk_lengths[1] = 2;
#if 0
    chunk_def.comp.comp_type = COMP_CODE_RLE;   /* RLE */

    chunk_def.comp.comp_type = COMP_CODE_SKPHUFF; /* Skipping Huffman */
    chunk_def.comp.cinfo.skphuff.skp_size = sizeof(uint16);
#endif
    /* the test for SDgetcompress relies on this compression setting , so 
       if the setting is changed, please ensure that the verification of 
       the next call to SDgetcompress below is still valid - BMR */
    chunk_def.comp.comp_type = COMP_CODE_DEFLATE; /* GZIP */
    chunk_def.comp.cinfo.deflate.level = 6;

    status = SDsetchunk(newsds7, chunk_def, HDF_CHUNK | HDF_COMP);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 7. Failed to create new chunked, GZIP Compressed data set\n");
        num_errs++;
        goto test8;
      }

    /* Set Chunk cache to hold 2 chunks */
    status = SDsetchunkcache(newsds7, 2, 0);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 7. SDsetchunkcache failed\n");
        num_errs++;
        goto test8;
      }


    /* Write data */
    start_dims[0] = 0;
    start_dims[1] = 0;
    edge_dims[0] = 9;
    edge_dims[1] = 4;
    status = SDwritedata(newsds7, start_dims, NULL, edge_dims, (VOIDP) u16_2data);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 7. Failed to write u16_2data to new chunked data set\n");
        num_errs++;
        goto test8;
      }

    /* read a portion of data back in using SDreaddata*/
    start_dims[0] = 0;
    start_dims[1] = 0;
    edge_dims[0] = 9;
    edge_dims[1] = 4;
    status = SDreaddata(newsds7, start_dims, NULL, edge_dims, (VOIDP) inbuf1_2u16);
    CHECK(status, FAIL, "Chunk Test 7. SDreaddata");
    for (i = 0; i < 9; i++)
      {
        for (j = 0; j < 4; j++)
          {
              if (inbuf1_2u16[i][j] != u16_2data[i][j])
                {
                    fprintf(stderr,"Chunk Test 7. inbuf1_2u16[%d][%d]=%d,",
                            i,j,inbuf1_2u16[i][j]);
                    fprintf(stderr,"u16_cdata[%d][%d]=%d,",
                            i,j,u16_2data[i][j]);
                    fprintf(stderr,"\n");
                    num_errs++;
                }
          }
      }
    /* Get chunk lengths */
    status = SDgetchunkinfo(newsds7, &rchunk_def, &cflags);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 7. SDgetchunkinfo failed \n");
        num_errs++;
        goto test8;
      }

    rcdims = rchunk_def.comp.chunk_lengths;

    /* check chunk lengths and see if SDS is compressed and chunked */
    if (cdims[0] != rcdims[0] || cdims[1] != rcdims[1] 
        || cflags != (HDF_CHUNK | HDF_COMP))
      {
        fprintf(stderr, "Chunk Test 7. SDgetchunkinfo returned wrong values\n");
        num_errs++;
        goto test8;
      }

    /* Close down this SDS*/    
    status = SDendaccess(newsds7);
    CHECK(status, FAIL, "Chunk Test 7. SDendaccess");

    /*
     * Added to test getting compression information for chunked SDS - 
     * bug# 307, 10/10/01 - BMR
     */
    /* Select same SDS again, first get index */
    if ((index = SDnametoindex(fchk,"DataSetChunked_2D_GZIP_1")) == FAIL)
      {
          fprintf(stderr, "Chunk Test 7. SDnametoindex  Failed for GZIP compressed data set\n");
          num_errs++;
          goto test8;
      }

    if ((newsds7 = SDselect(fchk,index)) == FAIL)
      {
          fprintf(stderr, "Chunk Test 7. SDselect Failed to re-select new chunked, GZIP compressed data set\n");
          num_errs++;
          goto test8;
      }

    /*
     * Retrieve and verify the compression info - bug# 307, 10/10/01 - BMR
     */
    comp_type = COMP_CODE_INVALID;  /* reset variables before retrieving info */
    HDmemset(&cinfo, 0, sizeof(cinfo)) ;
    status = SDgetcompress(newsds7, &comp_type, &cinfo);
    CHECK(status, FAIL, "SDgetcompress");
    VERIFY(comp_type, chunk_def.comp.comp_type, "SDgetcompress");
    VERIFY(cinfo.deflate.level, chunk_def.comp.cinfo.deflate.level, "SDgetcompress");
    /* end of test for bug#307 */

    /* Close down file 'chktst.hdf' */
    status = SDend(fchk);
    CHECK(status, FAIL, "SDend");

    /* ---------------------------------------------------------------
     *  Chunking with NBIT Compression 
     ----------------------------------------------------------------*/

    /*
     * Chunking with NBIT
     */
  test8:

    /* Create file */
    fchk = SDstart(CNBITFILE, DFACC_CREATE);
    CHECK(fchk, FAIL, "Chunk Test 8. SDstart");

    /* Create dataset */
    nt = DFNT_INT32;
    dimsize[0] = 5;
    dimsize[1] = 5;
    newsds = SDcreate(fchk, "Chunked_NBitDataSet", nt, 2, dimsize);
    if(newsds == FAIL) 
      {
        fprintf(stderr, "Chunk Test 8. SDcreate Failed to create a new chunked, nbit data set \n");
        num_errs++;
        goto done;
      }

    for(i = 0; i < 25; i++)
        idata[i] = i*10;
    /* Create chunked SDS with NBIT compression.
       chunk is 2x2 which will create 9 chunks.*/
    cdims[0] = chunk_def.nbit.chunk_lengths[0] = 2;
    cdims[1] = chunk_def.nbit.chunk_lengths[1] = 2;
    chunk_def.nbit.start_bit = 6;
    chunk_def.nbit.bit_len   = 7;
    chunk_def.nbit.sign_ext  = FALSE;
    chunk_def.nbit.fill_one  = FALSE;
    status = SDsetchunk(newsds, chunk_def, HDF_CHUNK | HDF_NBIT);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 8. SDsetchunk Failed to create new chunked, NBIT data set\n");
        num_errs++;
        goto done;
      }

    /* write out the data */
    start[0] = start[1] = 0;
    end[0]   = end[1]   = 5;
    status = SDwritedata(newsds, start, NULL, end, (VOIDP) idata);
    CHECK(status, FAIL, "Chunk Test 8. SDwritedata");

    /* end access to SDS */
    status = SDendaccess(newsds);
    CHECK(status, FAIL, "Chunk Test 8. SDendaccess");

    /* need to close to flush n-bit info to file */
    status = SDend(fchk);
    CHECK(status, FAIL, "Chunk Test 8. SDend");

    /* open file again */
    fchk = SDstart(CNBITFILE, DFACC_RDWR);
    CHECK(fchk, FAIL, "Chunk Test 8. SDstart (again)");

    /* Select SDS */
    newsds2 = SDselect(fchk, 0);
    if(newsds2 == FAIL) 
      {
        fprintf(stderr, "Chunk Test 8. Failed to select a data set for n-bit access\n");
        num_errs++;
        goto done;
      }

    /* read the n-bit data back in */
    start[0] = start[1] = 0;
    end[0]   = end[1]   = 5;
    status = SDreaddata(newsds2, start, NULL, end, (VOIDP) rdata);
    CHECK(status, FAIL, "Chunk Test 8. SDreaddata");

    /* Verify the data */
    for(i = 0; i < 25; i++)
      {
        if((idata[i]&0x7f) != rdata[i]) 
          {
            fprintf(stderr,"Chunk Test 8. Bogus val in loc %d in n-bit dset want %ld got %ld\n",
		    i, (long)idata[i], (long)rdata[i]);
            num_errs++;
          }
      }

    /* Get chunk lengths */
    status = SDgetchunkinfo(newsds2, &rchunk_def, &cflags);
    if(status == FAIL) 
      {
        fprintf(stderr, "Chunk Test 8. SDgetchunkinfo failed \n");
        num_errs++;
        goto done;
      }

    rcdims = rchunk_def.nbit.chunk_lengths;

    /* check chunk lengths and see if SDS is nbit-compressed and chunked */
    if (cdims[0] != rcdims[0] || cdims[1] != rcdims[1] 
        || cflags != (HDF_CHUNK | HDF_NBIT))
      {
        fprintf(stderr, "Chunk Test 8. SDgetchunkinfo returned wrong values\n");
        fprintf(stderr, "Chunk Test 8. cflags =%d \n",(int)cflags);
        fprintf(stderr, "Chunk Test 8. cdims[%d] =%d \n", 0, (int)cdims[0]);
        fprintf(stderr, "Chunk Test 8. cdims[%d] =%d \n", 1, (int)cdims[1]);
        fprintf(stderr, "Chunk Test 8. rcdims[%d] =%d \n", 0, (int)rcdims[0]);
        fprintf(stderr, "Chunk Test 8. rcdims[%d] =%d \n", 1, (int)cdims[1]);
        num_errs++;
        goto done;
      }
    /*
     * Retrieve and verify the compression info - bug# 307, 10/10/01 - BMR
     */
    comp_type = COMP_CODE_INVALID;  /* reset variables before retrieving info */
    HDmemset(&cinfo, 0, sizeof(cinfo)) ;
    status = SDgetcompress(newsds2, &comp_type, &cinfo);
    CHECK(status, FAIL, "SDgetcompress");

    /* Note: the struct nbit in the union HDF_CHUNK_DEF seems like an extra
	thing since comp_info also has nbit, but the HDF_CHUNK_DEF.nbit was
	used to set the compression info so it's also used here to verify */ 
    VERIFY(comp_type, COMP_CODE_NBIT, "SDgetcompress");
    VERIFY(cinfo.nbit.sign_ext, chunk_def.nbit.sign_ext, "SDgetcompress");
    VERIFY(cinfo.nbit.fill_one, chunk_def.nbit.fill_one, "SDgetcompress");
    VERIFY(cinfo.nbit.start_bit, chunk_def.nbit.start_bit, "SDgetcompress");
    VERIFY(cinfo.nbit.bit_len, chunk_def.nbit.bit_len, "SDgetcompress");
    /* end of test for bug#307 */

    /* end access to SDS */
    status = SDendaccess(newsds2);
    CHECK(status, FAIL, "Chunk Test 8. SDendaccess");

    /* close file */
    status = SDend(fchk);
    CHECK(status, FAIL, "Chunk Test 8. SDend");

  done:

    return num_errs;
} /* test_chunk() */
#endif /* CHUNK_TEST */

#ifdef NETCDF_READ_TEST
static int16  netcdf_u16[2][3] = {{1, 2, 3}, 
                                   {4, 5, 6}};

char    testfile[512] = "";

/* Tests reading of netCDF file 'test1.nc' using the SDxxx inteface.
   Note not all features of reading SDS from netCDF files are tested here.
   Hopefully more tests will be added over time as needed/required. */
static intn
test_netcdf_reading()
{
	int32 sd_id;
    int32 sds_id;
    int32 n_datasets;
    int32 n_file_attrs;
    int32 index;
	int32 rank;
    int32 num_type;
    int32 attributes;
	int32 dim_sizes[MAX_VAR_DIMS];
    int32 start[MAX_VAR_DIMS];
    int32 edges[MAX_VAR_DIMS];
    int16 array_data[2][3];
	char name[MAX_NC_NAME];
    int32 status;
    intn i, j;
    int     num_errs = 0;    /* number of errors so far */
    const char *basename = "test1.nc";
    char   *srcdir = getenv("srcdir");

    /* Generate the correct name for the test file, by prepending the source path */
    if (srcdir && ((strlen(srcdir) + strlen(basename) + 1) < sizeof(testfile))) {
        strcpy(testfile, srcdir);
        strcat(testfile, "/");
    }
    strcat(testfile, basename);

    /* Open the file 'test1.nc' and initialize the SDxxx interface. */
    sd_id = SDstart(testfile, DFACC_RDONLY);
    CHECK(sd_id, FAIL, "netCDF Read Test 1. SDstart failed on file test1.nc");

	/* Determine the contents of the file. */
	status = SDfileinfo(sd_id, &n_datasets, &n_file_attrs);
    CHECK(status, FAIL, "netCDF Read Test 1. SDfileinfo failed on file test1.nc");

    /* There should be 8 datasets in the file and 1 file level attribute */
    if (n_datasets != 8 )
      {
          fprintf(stderr,"netCDF Read Test 1: SDfileinfo returned wrong number of datasets in file test1.nc \n");
          num_errs++;
      }

    if (n_file_attrs != 1 )
      {
          fprintf(stderr,"netCDF Read Test 1: SDfileinfo returned wrong number of file attributes in file test1.nc \n");
          num_errs++;
      }

	/* Access and find the 2-dim dataset of data-type shorts(DFNT_INT16). 
       in the file while querying every data set in the file. 
       There should only be one dataset that matches and is named 'order'.*/
	for (index = 0; index < n_datasets; index++) 
      {
          sds_id = SDselect(sd_id, index);
          CHECK(sds_id, FAIL, "netCDF Read Test 1. SDselect failed for dataset in  file test1.nc");

          status = SDgetinfo(sds_id, name, &rank, dim_sizes, &num_type, &attributes);
          CHECK(status, FAIL, "netCDF Read Test 1. SDgetinfo failed for dataset in  file test1.nc");

          /* look for the dataset 'order' based on rank and number type */
          if (rank == 2 && num_type == (int32)DFNT_INT16)
            { /* should only be one of these */
                start[0] =  start[1] = 0; 
                edges [0] = dim_sizes[0];
                edges [1] = dim_sizes[1];
                status = SDreaddata (sds_id, start, NULL, edges, (VOIDP) array_data);
                CHECK(status, FAIL, "netCDF Read Test 1. SDreaddata failed for dataset in  file test1.nc");

                /* check the data against our buffer 'netcdf_u16[][]' */
                for (j = 0; j < dim_sizes[0]; j++ )
                  {
                      for (i = 0; i < dim_sizes[1]; i++)
                        {
                            if (array_data[j][i] != netcdf_u16[j][i])
                              {
                                  fprintf(stderr,"netCDF Read Test 1: bogus val read: wanted netcdf[%d][%d]=%d, read array[%d][%d]=%d \n",
                                          j,i,netcdf_u16[j][i], j,i, array_data[j][i] );
                              }
                        } /* end for inner */
                  } /* end for outer */
            }

          /* end access to this SDS */
          status = SDendaccess(sds_id);
          CHECK(status, FAIL, "netCDF Read Test 1. SDendaccess failed for dataset in  file test1.nc");
      } /* end querying every dataset in file */

	/* Terminate access to the SD interface and close the file. */
	status = SDend(sd_id);
    CHECK(status, FAIL, "netCDF Read Test 1. SDend failed for file test1.nc");

    return num_errs;
} /* test_netcdf_reading() */
#endif /* NETCDF_READ_TEST */

/* This test is added to test SDsetdimscale while fixing bug #172.
   Beside the fact that the main program was already very long, adding
   this separate test routine will also define a place for additional
   dimension tests to be appended in the future.  Also, hopefully, some
   day, the main program can be shortened and some of its
   dimension-related tests can be moved into this test routine - 04/18/01
*/

/********************************************************************
   Name: test_dimensions()

   Description: 
	This test routine is used to test various dimension operations.
	The main contents include:
	- creates the file dim.hdf
	- creates an SDS of size 15 x 10, and sets values to its dimensions
	- creates another SDS of size 4 x 3, and sets values to its 
		first dimension

	The followings are included in this test routine:
	- SDgetdimid
	- SDsetdimname
	- SDdiminfo
	- SDgetdimscale
        - SDsetdimscale with the following situations:
        	+ not called before SDdiminfo 
		+ called with number type = 0
        	+ called with an unsigned number type
 		+ called with the dataset's number type 
 		+ called before writing data to dataset
 		+ called after closing dataset and file, and then 
		  reopening

   Return value:
	The number of errors occurred in this routine.

*********************************************************************/
#define FILE_DIM  "dim.hdf"	/* created and used by test_dimensions */
#define LENGTH0 15		/* first SDS dimension */
#define LENGTH1 10
#define LENGTH2 4		/* second SDS dimension */
#define LENGTH3 3
#define RANK 	2		/* both SDS' rank */
#define DS0_NAME "HDF Data 0"	/* first SDS name */
#define DS1_NAME "HDF Data 1"	/* second SDS name */
#define DIM0_NAME " Dimension 0"	/* name of first SDS' first dim */
#define DIM1_NAME " Dimension 1"	/* name of first SDS' second dim */
#define DIM2_NAME " Dimension 2"	/* name of second SDS' first dim */

static intn
test_dimensions()
{
    int32  fid, sds_id, status, dim0_id, dim1_id, sds_idx;
    int32  dims[2], start[2], edges[2], rank;
    int16  array1_data[LENGTH0][LENGTH1];	/* data for first SDS */
    uint32 array2_data[LENGTH2][LENGTH3];	/* data for second SDS */
    int32  dim_sizes[MAX_VAR_DIMS];		/* read dimensions */
    intn   i, j;
    int32  array_rank, num_type, attributes;
    char   dim_name[MAX_NC_NAME], name[MAX_NC_NAME];
    uint8  scale0 [] = {0,1,2,3,4,5,6,7,8,9,10,11,12,13,255};
    int16  scale1 [] = {0,1,2,3,4,5,6,7,8,9};
    char8  scale2 [] = {'d','i','m','2'}, scale2_out[4];
    float32 scalef[] = {1., 2., 3., 4.};
    int32  size, dim_data_type, dim_num_attrs;
    int    num_errs = 0;    /* number of errors so far */

    /* Create the file defined by FILE_DIM and initiate the SD interface. */
    fid = SDstart(FILE_DIM, DFACC_CREATE);
    CHECK(fid, FAIL, "SDstart");

    /*
     * Add a LENGTH0 x LENGTH1 array, which is named by DS0_NAME and of
     * type 16-bit signed integer, to the file... 
     */

    /* Define the rank and dimensions of the data set. */
    rank = RANK;
    dims[0] = LENGTH0;
    dims[1] = LENGTH1;

    /* Create the array data set. */
    sds_id = SDcreate(fid, DS0_NAME, DFNT_INT16, rank, dims);
    CHECK(sds_id, FAIL, "SDcreate");

    /*
     * Settings for the first dimension.  Note that all these settings
     * are done before writing the data to the dataset; just to make sure
     * that it's possible...
     */

    /* Get the first dimension id */
    dim0_id = SDgetdimid (sds_id, 0);
    CHECK(dim0_id, FAIL, "SDgetdimid");

    /* Set the first dimension name to the name defined by DIM0_NAME */
    status = SDsetdimname (dim0_id, DIM0_NAME);
    CHECK(status, FAIL, "SDsetdimname");

    /* Set scale of type unsigned int-8 for the first dimension */
    status = SDsetdimscale (dim0_id, dims[0], DFNT_UINT8, scale0);
    CHECK(status, FAIL, "SDsetdimscale");

    /* Read the first dimension and verify its information */
    status = SDdiminfo(dim0_id, dim_name, &size, &dim_data_type, &dim_num_attrs);
    CHECK(status, FAIL, "SDdiminfo");
    VERIFY (strcmp(dim_name, DIM0_NAME), 0, "SDdiminfo");
    VERIFY (size, dims[0], "SDdiminfo");
    VERIFY (dim_data_type, DFNT_UINT8, "SDdiminfo"); /* bug #172 is fixed! */
    VERIFY (dim_num_attrs, 0, "SDdiminfo");

    /* 
     * Write the data stored in the array 'array1_data' to the dataset...
     */

    /* Fill the buffer with values. */
    for (j = 0; j < LENGTH0; j++) {
        for (i = 0; i < LENGTH1; i++)
            array1_data[j][i] = (i + j) + 1;
    }

    /* Define the area for writing to the dataset */
    for (i = 0; i < rank; i++) {
        start[i] = 0;
        edges[i] = dims[i];
    }

    status = SDwritedata(sds_id, start, NULL, edges, (VOIDP)array1_data); 
    CHECK(status, FAIL, "SDwritedata");

    /* Terminate access to the array. */
    status = SDendaccess(sds_id);
    CHECK(status, FAIL, "SDendaccess");

    /* Terminate access to the SD interface and close the file. */
    status = SDend(fid);
    CHECK(status, FAIL, "SDend");

    /* Re-open FILE_DIM in W mode and select the dataset named by DS0_NAME */
    fid = SDstart (FILE_DIM, DFACC_WRITE);
    CHECK(fid, FAIL, "SDstart");

    /* Look for DS0_NAME dataset */
    sds_idx = SDnametoindex (fid, DS0_NAME);
    CHECK(sds_idx, FAIL, "SDnametoindex");

    /* Select that dataset */
    sds_id = SDselect (fid, sds_idx);
    CHECK(sds_id, FAIL, "SDselect");

    /* Get info of the dataset and verify them: it is a LENGTH0 x LENGTH1 
       array of type DFNT_INT16 and is named by DS0_NAME */
    status = SDgetinfo(sds_id, name, &array_rank, dim_sizes, &num_type, &attributes);
    CHECK(status, FAIL, "SDgetinfo");
    VERIFY (strcmp(name, DS0_NAME), 0, "SDgetinfo");
    VERIFY (array_rank, RANK, "SDgetinfo");
    VERIFY (num_type, DFNT_INT16, "SDgetinfo");
    VERIFY (dim_sizes[0], LENGTH0, "SDgetinfo");
    VERIFY (dim_sizes[1], LENGTH1, "SDgetinfo");

    /*
     * Setting name for the second dimension, but not scale yet...
     */

    /* Get the second dimension id */
    dim1_id = SDgetdimid (sds_id, 1);
    CHECK(dim1_id, FAIL, "SDgetdimid");

    /* Set the second dimension name to the name defined by DIM1_NAME */
    status = SDsetdimname (dim1_id, DIM1_NAME);
    CHECK(status, FAIL, "SDsetdimname");

    /* Read the second dimension and verify its information; since the
       scale of this dimension is not set yet, the datatype should be 0 */
    status = SDdiminfo(dim1_id, dim_name, &size, &dim_data_type, &dim_num_attrs);
    CHECK(status, FAIL, "SDdiminfo");
    VERIFY (strcmp(dim_name, DIM1_NAME), 0, "SDdiminfo");
    VERIFY (size, dims[1], "SDdiminfo");
    VERIFY (dim_data_type, 0, "SDdiminfo");
    VERIFY (dim_num_attrs, 0, "SDdiminfo");

    /* Set dimension scale for the second dimension; its type will be the
       same as that of the of the dataset */
    status = SDsetdimscale (dim1_id, dim_sizes[1], num_type, scale1);
    CHECK(status, FAIL, "SDsetdimscale");

    /* Read the second dimension and verify its information; since the
       scale of this dimension is now set, the datatype should be DFNT_INT16 */
    status = SDdiminfo(dim1_id, dim_name, &size, &dim_data_type, &dim_num_attrs);
    CHECK(status, FAIL, "SDdiminfo");
    VERIFY (dim_data_type, DFNT_INT16, "SDdiminfo");

    /* Terminate access to the array. */
    status = SDendaccess(sds_id);
    CHECK(status, FAIL, "SDendaccess");

    /*
     * Add a data array named by DS1_NAME to the file... 
     */

    /*
     * Add another dataset to the file; this dataset is a LENGTH2 x LENGTH3
     * array, named by DS1_NAME and of type 32-bit unsigned integer...
     */

    /* Define the rank and dimensions of the data set */
    rank = RANK;
    dims[0] = LENGTH2;
    dims[1] = LENGTH3;

    for (i = 0; i < rank; i++)
        edges[i] = dims[i];

    /* Create the data set */
    sds_id = SDcreate(fid, DS1_NAME, DFNT_UINT32, rank, dims);
    CHECK(sds_id, FAIL, "SDcreate");

    /* Fill the stored-data array with values. */
    for (j = 0; j < LENGTH2; j++) {
        for (i = 0; i < LENGTH3; i++)
            array2_data[j][i] = 10 * (j + i);
    }

    /* Write the data stored in the array 'array2_data' to the dataset */
    status = SDwritedata(sds_id, start, NULL, edges, (VOIDP)array2_data); 
    CHECK(status, FAIL, "SDwritedata");

    /* Get info of the dataset and verify its type */
    status = SDgetinfo(sds_id, name, &array_rank, dim_sizes, &num_type, &attributes);
    CHECK(status, FAIL, "SDgetinfo");
    VERIFY (strcmp(name, DS1_NAME), 0, "SDgetinfo");
    VERIFY (array_rank, RANK, "SDgetinfo");
    VERIFY (num_type, DFNT_UINT32, "SDgetinfo");
    VERIFY (dim_sizes[0], LENGTH2, "SDgetinfo");
    VERIFY (dim_sizes[1], LENGTH3, "SDgetinfo");

    /*
     * Settings for the first dimension of the dataset named by DS1_NAME... 
     */

    /* Get the first dimension id */
    dim0_id = SDgetdimid (sds_id, 0);
    CHECK(dim0_id, FAIL, "SDgetdimid");

    /* Set the first dimension name to the name defined by DIM2_NAME */
    status = SDsetdimname (dim0_id, DIM2_NAME);
    CHECK(status, FAIL, "SDsetdimname");

    /* Verify that when 0 is passed into SDsetdimscale for number type,
       then the dimension scale will be set to DFNT_FLOAT32 */
    status = SDsetdimscale (dim0_id, dims[0], 0, scalef); 
    CHECK(status, FAIL, "SDsetdimscale");

    status = SDdiminfo(dim0_id, dim_name, &size, &dim_data_type, &dim_num_attrs);
    CHECK(status, FAIL, "SDdiminfo");
    VERIFY (strcmp(dim_name, DIM2_NAME), 0, "SDdiminfo");
    VERIFY (size, dims[0], "SDdiminfo");
    VERIFY (dim_data_type, DFNT_FLOAT32, "SDdiminfo");
    VERIFY (dim_num_attrs, 0, "SDdiminfo");

    /* 
     * Now, set dimension scale for that first dimension again, but this
     * time, to 8-bit signed char; then verify the type and the scale values 
     */

    /* Set scale of type 8-bit signed char for this dimension */
    status = SDsetdimscale (dim0_id, dims[0], DFNT_CHAR, scale2);
    CHECK(status, FAIL, "SDsetdimscale");

    /* Read the info of this dimension its new number type */ 
    status = SDdiminfo(dim0_id, dim_name, &size, &dim_data_type, &dim_num_attrs);
    CHECK(status, FAIL, "SDdiminfo");
    VERIFY (dim_data_type, DFNT_CHAR, "SDdiminfo");

    /* Read dimension scale values and verify them */
    status = SDgetdimscale (dim0_id, (VOIDP)scale2_out);
    CHECK(status, FAIL, "SDgetdimscale");
    for(i=0; i < LENGTH2; i++)
	VERIFY (scale2_out[i], scale2[i], "SDgetdimscale");

    /* Terminate access to the array. */
    status = SDendaccess(sds_id);
    CHECK(status, FAIL, "SDendaccess");

    /* Terminate access to the SD interface and close the file. */
    status = SDend(fid);
    CHECK(status, FAIL, "SDend");

    /* Return the number of errors that's been kept track of so far */
    return num_errs;
} /* test_dimensions */

static intn
test_compression()
{
    int32 fcomp; /* File handle */
    int32 nt;                /* Number type */
    int32 dimsize[10];       /* dimension sizes */
    int32 newsds, newsds2; 	/* SDS handles */
    comp_coder_t comp_type;	/* to retrieve compression type into */
    comp_info cinfo;            /* compression information structure */
    int32   idata[100];
    int32   rdata[100];
    int32   fillval;
    intn  i;
    int   num_errs = 0;    /* number of errors in compression test so far */
    intn  status;      /* status flag */
    int32   start[10], end[10]; /* start and end arrays */

#ifdef QAK
printf("writing 1st compressed dataset, basic skipping huffman\n");
#endif /* QAK */
    fcomp = SDstart(COMPFILE1, DFACC_CREATE);
    CHECK(fcomp, FAIL, "SDstart");

    nt = DFNT_INT32;
    dimsize[0] = 5;
    dimsize[1] = 5;
    newsds = SDcreate(fcomp, "CompDataSet1", nt, 2, dimsize);
    if(newsds == FAIL) {
        fprintf(stderr, "Failed to create a new data set for compression testing\n");
        num_errs++;
    }

    for(i = 0; i < 25; i++)
        idata[i] = i*10;

#ifdef QAK
printf("before SDsetcompress\n");
#endif /* QAK */
    cinfo.skphuff.skp_size=4;
    status = SDsetcompress(newsds,COMP_CODE_SKPHUFF,&cinfo);
    CHECK(status, FAIL, "SDcompress");

    start[0] = start[1] = 0;
    end[0]   = end[1]   = 5;
#ifdef QAK
printf("before SDwritedata\n");
#endif /* QAK */
    status = SDwritedata(newsds, start, NULL, end, (VOIDP) idata);
    CHECK(status, FAIL, "SDwritedata");

#ifdef QAK
printf("before SDendaccess\n");
#endif /* QAK */
    status = SDendaccess(newsds);
    CHECK(status, FAIL, "SDendaccess");

    /* need to close to flush compressed info to file */
    status = SDend(fcomp);
    CHECK(status, FAIL, "SDend");

    /* read the compressed data back in */
#ifdef QAK
printf("reading compressed dataset\n");
#endif /* QAK */
    fcomp = SDstart(COMPFILE1, DFACC_RDWR);
    CHECK(fcomp, FAIL, "SDstart (again)");

    newsds2 = SDselect(fcomp, 0);
    if(newsds == FAIL) {
        fprintf(stderr, "Failed to select a data set for compressed access\n");
        num_errs++;
    }

    /*
     * Retrieve and verify the compression info - bug# 307, 9/7/01 - BMR
     */
    comp_type = COMP_CODE_INVALID;  /* reset variables before retrieving info */
    HDmemset(&cinfo, 0, sizeof(cinfo)) ;
    status = SDgetcompress(newsds2, &comp_type, &cinfo);
    CHECK(status, FAIL, "SDgetcompress");
    VERIFY(comp_type, COMP_CODE_SKPHUFF, "SDgetcompress");
    VERIFY(cinfo.skphuff.skp_size, 4, "SDgetcompress");

    start[0] = start[1] = 0;
    end[0]   = end[1]   = 5;
#ifdef QAK
printf("before SDreaddata\n");
#endif /* QAK */
    status = SDreaddata(newsds2, start, NULL, end, (VOIDP) rdata);
    CHECK(status, FAIL, "SDreaddata");

    for(i = 0; i < 25; i++)
        if(idata[i] != rdata[i]) {
            fprintf(stderr,"Bogus val in loc %d in compressed dset want %ld got %ld\n", i, (long)idata[i], (long)rdata[i]);
            num_errs++;
        }

#ifdef QAK
printf("before SDendaccess\n");
#endif /* QAK */
    status = SDendaccess(newsds2);
    CHECK(status, FAIL, "SDendaccess");

    status = SDend(fcomp);
    CHECK(status, FAIL, "SDend");

#ifdef QAK
printf("writing 2nd compressed dataset, partially filled & skipping huffman\n");
#endif /* QAK */
    fcomp = SDstart(COMPFILE2, DFACC_CREATE);
    CHECK(fcomp, FAIL, "SDstart");

    nt = DFNT_INT32;
    dimsize[0] = 5;
    dimsize[1] = 5;
    newsds = SDcreate(fcomp, "CompDataSet2", nt, 2, dimsize);
    if(newsds == FAIL) {
        fprintf(stderr, "Failed to create a new data set for compression testing\n");
        num_errs++;
    }

    fillval=43;
#ifdef QAK
printf("before SDsetfillvalue\n");
#endif /* QAK */
    status = SDsetfillvalue(newsds,(VOIDP)&fillval);
    CHECK(status, FAIL, "SDsetfillvalue");

#ifdef QAK
printf("before SDsetcompress\n");
#endif /* QAK */
    cinfo.skphuff.skp_size=4;
    status = SDsetcompress(newsds,COMP_CODE_SKPHUFF,&cinfo);
    CHECK(status, FAIL, "SDsetcompress");

    /* fill the array with the standard info */
    for(i = 0; i < 25; i++)
        idata[i] = i*10;
    /* overwrite selected portions with the fill value */
    for(i = 0; i < 5; i++)
        idata[i] = fillval;
    for(i = 20; i < 25; i++)
        idata[i] = fillval;

    start[0] = 1;
    start[1] = 0;
    end[0]   = 3;
    end[1]   = 5;
#ifdef QAK
printf("before SDwritedata\n");
#endif /* QAK */
    status = SDwritedata(newsds, start, NULL, end, (VOIDP) &idata[5]);
    CHECK(status, FAIL, "SDwritedata");

#ifdef QAK
printf("before SDendaccess\n");
#endif /* QAK */
    status = SDendaccess(newsds);
    CHECK(status, FAIL, "SDendaccess");

    /* need to close to flush compressed info to file */
    status = SDend(fcomp);
    CHECK(status, FAIL, "SDend");

    /* read the compressed data back in */
#ifdef QAK
printf("reading compressed dataset\n");
#endif /* QAK */
    fcomp = SDstart(COMPFILE2, DFACC_RDWR);
    CHECK(fcomp, FAIL, "SDstart (again)");

    newsds2 = SDselect(fcomp, 0);
    if(newsds == FAIL) {
        fprintf(stderr, "Failed to select a data set for compressed access\n");
        num_errs++;
    }

    start[0] = start[1] = 0;
    end[0]   = end[1]   = 5;
#ifdef QAK
printf("before SDreaddata\n");
#endif /* QAK */
    status = SDreaddata(newsds2, start, NULL, end, (VOIDP) rdata);
    CHECK(status, FAIL, "SDreaddata");

    for(i = 0; i < 25; i++)
        if(idata[i] != rdata[i]) {
            fprintf(stderr,"Bogus val in loc %d in compressed dset want %ld got %ld\n", i, (long)idata[i], (long)rdata[i]);
            num_errs++;
        }

#ifdef QAK
printf("before SDendaccess\n");
#endif /* QAK */
    status = SDendaccess(newsds2);
    CHECK(status, FAIL, "SDendaccess");

    status = SDend(fcomp);
    CHECK(status, FAIL, "SDend");

#ifdef QAK
printf("creating 3rd compressed dataset, compressed template & skipping huffman\n");
#endif /* QAK */
    fcomp = SDstart(COMPFILE3, DFACC_CREATE);
    CHECK(fcomp, FAIL, "SDstart");

    nt = DFNT_INT32;
    dimsize[0] = 5;
    dimsize[1] = 5;
    newsds = SDcreate(fcomp, "CompDataSet3", nt, 2, dimsize);
    if(newsds == FAIL) {
        fprintf(stderr, "Failed to create a new data set for compression testing\n");
        num_errs++;
    }

    fillval=56;
#ifdef QAK
printf("before SDsetfillvalue\n");
#endif /* QAK */
    status = SDsetfillvalue(newsds,(VOIDP)&fillval);
    CHECK(status, FAIL, "SDsetfillvalue");

#ifdef QAK
printf("before SDsetcompress\n");
#endif /* QAK */
    cinfo.skphuff.skp_size=4;
    status = SDsetcompress(newsds,COMP_CODE_SKPHUFF,&cinfo);
    CHECK(status, FAIL, "SDsetcompress");

#ifdef QAK
printf("before SDendaccess\n");
#endif /* QAK */
    status = SDendaccess(newsds);
    CHECK(status, FAIL, "SDendaccess");

    /* need to close to flush compressed info to file */
    status = SDend(fcomp);
    CHECK(status, FAIL, "SDend");

    /* read the compressed data back in */
#ifdef QAK
printf("reading compressed dataset\n");
#endif /* QAK */
    fcomp = SDstart(COMPFILE3, DFACC_RDWR);
    CHECK(fcomp, FAIL, "SDstart (again)");

    newsds2 = SDselect(fcomp, 0);
    if(newsds == FAIL) {
        fprintf(stderr, "Failed to select a data set for compressed access\n");
        num_errs++;
    }

    start[0] = start[1] = 0;
    end[0]   = end[1]   = 5;
#ifdef QAK
printf("before SDreaddata\n");
#endif /* QAK */
    status = SDreaddata(newsds2, start, NULL, end, (VOIDP) rdata);
    CHECK(status, FAIL, "SDreaddata");

    for(i = 0; i < 25; i++)
        if(fillval != rdata[i]) {
            fprintf(stderr,"Bogus val in loc %d in compressed dset want %ld got %ld\n", i, (long)idata[i], (long)rdata[i]);
            num_errs++;
        }

#ifdef QAK
printf("before SDendaccess\n");
#endif /* QAK */
    status = SDendaccess(newsds2);
    CHECK(status, FAIL, "SDendaccess");

    status = SDend(fcomp);
    CHECK(status, FAIL, "SDend");

#ifdef QAK
printf("creating 4th compressed dataset, compressed template read, then partial write & skipping huffman\n");
#endif /* QAK */
    fcomp = SDstart(COMPFILE4, DFACC_CREATE);
    CHECK(fcomp, FAIL, "SDstart");

    nt = DFNT_INT32;
    dimsize[0] = 5;
    dimsize[1] = 5;
    newsds = SDcreate(fcomp, "CompDataSet4", nt, 2, dimsize);
    if(newsds == FAIL) {
        fprintf(stderr, "Failed to create a new data set for compression testing\n");
        num_errs++;
    }

    fillval=67;
#ifdef QAK
printf("before SDsetfillvalue\n");
#endif /* QAK */
    status = SDsetfillvalue(newsds,(VOIDP)&fillval);
    CHECK(status, FAIL, "SDsetfillvalue");

#ifdef QAK
printf("before SDsetcompress\n");
#endif /* QAK */
    cinfo.skphuff.skp_size=4;
    status = SDsetcompress(newsds,COMP_CODE_SKPHUFF,&cinfo);
    CHECK(status, FAIL, "SDsetcompress");

#ifdef QAK
printf("before SDendaccess\n");
#endif /* QAK */
    status = SDendaccess(newsds);
    CHECK(status, FAIL, "SDendaccess");

    /* need to close to flush compressed info to file */
    status = SDend(fcomp);
    CHECK(status, FAIL, "SDend");

    /* read the compressed data back in */
#ifdef QAK
printf("reading compressed dataset\n");
#endif /* QAK */
    fcomp = SDstart(COMPFILE4, DFACC_RDWR);
    CHECK(fcomp, FAIL, "SDstart (again)");

    newsds2 = SDselect(fcomp, 0);
    if(newsds == FAIL) {
        fprintf(stderr, "Failed to select a data set for compressed access\n");
        num_errs++;
    }

    start[0] = start[1] = 0;
    end[0]   = end[1]   = 5;
#ifdef QAK
printf("before SDreaddata\n");
#endif /* QAK */
    status = SDreaddata(newsds2, start, NULL, end, (VOIDP) rdata);
    CHECK(status, FAIL, "SDreaddata");

    for(i = 0; i < 25; i++)
        if(fillval != rdata[i]) {
            fprintf(stderr,"Bogus val in loc %d in compressed dset want %ld got %ld\n", i, (long)idata[i], (long)rdata[i]);
            num_errs++;
        }

#ifdef QAK
printf("before SDendaccess\n");
#endif /* QAK */
    status = SDendaccess(newsds2);
    CHECK(status, FAIL, "SDendaccess");

    status = SDend(fcomp);
    CHECK(status, FAIL, "SDend");

#ifdef QAK
printf("writing compressed dataset\n");
#endif /* QAK */
    fcomp = SDstart(COMPFILE4, DFACC_RDWR);
    CHECK(fcomp, FAIL, "SDstart (again)");

    newsds2 = SDselect(fcomp, 0);
    if(newsds == FAIL) {
        fprintf(stderr, "Failed to select a data set for compressed access\n");
        num_errs++;
    }
    /* fill the array with the standard info */
    for(i = 0; i < 25; i++)
        idata[i] = i*10;
    /* overwrite selected portions with the fill value */
    for(i = 0; i < 10; i++)
        idata[i] = fillval;
    for(i = 20; i < 25; i++)
        idata[i] = fillval;

    start[0] = 2;
    start[1] = 0;
    end[0]   = 2;
    end[1]   = 5;
#ifdef QAK
printf("before SDwritedata\n");
#endif /* QAK */
    status = SDwritedata(newsds, start, NULL, end, (VOIDP) &idata[10]);
    CHECK(status, FAIL, "SDwritedata");


#ifdef QAK
printf("before SDendaccess\n");
#endif /* QAK */
    status = SDendaccess(newsds);
    CHECK(status, FAIL, "SDendaccess");

    /* need to close to flush compressed info to file */
    status = SDend(fcomp);
    CHECK(status, FAIL, "SDend");

    fcomp = SDstart(COMPFILE4, DFACC_RDWR);
    CHECK(fcomp, FAIL, "SDstart (again)");

    newsds2 = SDselect(fcomp, 0);
    if(newsds == FAIL) {
        fprintf(stderr, "Failed to select a data set for compressed access\n");
        num_errs++;
    }

    start[0] = start[1] = 0;
    end[0]   = end[1]   = 5;
#ifdef QAK
printf("before SDreaddata\n");
#endif /* QAK */
    status = SDreaddata(newsds2, start, NULL, end, (VOIDP) rdata);
    CHECK(status, FAIL, "SDreaddata");

    for(i = 0; i < 25; i++)
        if(idata[i] != rdata[i]) {
            fprintf(stderr,"Bogus val in loc %d in compressed dset want %ld got %ld\n", i, (long)idata[i], (long)rdata[i]);
            num_errs++;
        }

#ifdef QAK
printf("before SDendaccess\n");
#endif /* QAK */
    status = SDendaccess(newsds2);
    CHECK(status, FAIL, "SDendaccess");

    status = SDend(fcomp);
    CHECK(status, FAIL, "SDend");

#ifdef QAK
printf("writing 5th compressed dataset, basic RLE\n");
#endif /* QAK */
    fcomp = SDstart(COMPFILE5, DFACC_CREATE);
    CHECK(fcomp, FAIL, "SDstart");

    nt = DFNT_INT32;
    dimsize[0] = 5;
    dimsize[1] = 5;
    newsds = SDcreate(fcomp, "CompDataSet5", nt, 2, dimsize);
    if(newsds == FAIL) {
        fprintf(stderr, "Failed to create a new data set for compression testing\n");
        num_errs++;
    }

    for(i = 0; i < 25; i++)
        idata[i] = i*10;

#ifdef QAK
printf("before SDsetcompress\n");
#endif /* QAK */
    status = SDsetcompress(newsds,COMP_CODE_RLE,&cinfo);
    CHECK(status, FAIL, "SDcompress");

    start[0] = start[1] = 0;
    end[0]   = end[1]   = 5;
#ifdef QAK
printf("before SDwritedata\n");
#endif /* QAK */
    status = SDwritedata(newsds, start, NULL, end, (VOIDP) idata);
    CHECK(status, FAIL, "SDwritedata");

#ifdef QAK
printf("before SDendaccess\n");
#endif /* QAK */
    status = SDendaccess(newsds);
    CHECK(status, FAIL, "SDendaccess");

    /* need to close to flush compressed info to file */
    status = SDend(fcomp);
    CHECK(status, FAIL, "SDend");

    /* read the compressed data back in */
#ifdef QAK
printf("reading compressed dataset\n");
#endif /* QAK */
    fcomp = SDstart(COMPFILE5, DFACC_RDWR);
    CHECK(fcomp, FAIL, "SDstart (again)");

    newsds2 = SDselect(fcomp, 0);
    if(newsds == FAIL) {
        fprintf(stderr, "Failed to select a data set for compressed access\n");
        num_errs++;
    }

    /*
     * Retrieve and verify the compression info - bug# 307, 9/7/01 - BMR
     */
    comp_type = COMP_CODE_INVALID;  /* reset variables before retrieving info */
    HDmemset(&cinfo, 0, sizeof(cinfo)) ;
    status = SDgetcompress(newsds2, &comp_type, &cinfo);
    CHECK(status, FAIL, "SDgetcompress");
    VERIFY(comp_type, COMP_CODE_RLE, "SDgetcompress");

    start[0] = start[1] = 0;
    end[0]   = end[1]   = 5;
#ifdef QAK
printf("before SDreaddata\n");
#endif /* QAK */
    status = SDreaddata(newsds2, start, NULL, end, (VOIDP) rdata);
    CHECK(status, FAIL, "SDreaddata");

    for(i = 0; i < 25; i++)
        if(idata[i] != rdata[i]) {
            fprintf(stderr,"Bogus val in loc %d in compressed dset want %ld got %ld\n", i, (long)idata[i], (long)rdata[i]);
            num_errs++;
        }

#ifdef QAK
printf("before SDendaccess\n");
#endif /* QAK */
    status = SDendaccess(newsds2);
    CHECK(status, FAIL, "SDendaccess");

    status = SDend(fcomp);
    CHECK(status, FAIL, "SDend");

#ifdef QAK
printf("writing 6th compressed dataset, no encoding\n");
#endif /* QAK */
    fcomp = SDstart(COMPFILE6, DFACC_CREATE);
    CHECK(fcomp, FAIL, "SDstart");

    nt = DFNT_INT32;
    dimsize[0] = 5;
    dimsize[1] = 5;
    newsds = SDcreate(fcomp, "CompDataSet6", nt, 2, dimsize);
    if(newsds == FAIL) {
        fprintf(stderr, "Failed to create a new data set for compression testing\n");
        num_errs++;
    }

    for(i = 0; i < 25; i++)
        idata[i] = i*10;

#ifdef QAK
printf("before SDsetcompress\n");
#endif /* QAK */
    status = SDsetcompress(newsds,COMP_CODE_NONE,&cinfo);
    CHECK(status, FAIL, "SDcompress");

    start[0] = start[1] = 0;
    end[0]   = end[1]   = 5;
#ifdef QAK
printf("before SDwritedata\n");
#endif /* QAK */
    status = SDwritedata(newsds, start, NULL, end, (VOIDP) idata);
    CHECK(status, FAIL, "SDwritedata");

#ifdef QAK
printf("before SDendaccess\n");
#endif /* QAK */
    status = SDendaccess(newsds);
    CHECK(status, FAIL, "SDendaccess");

    /* need to close to flush compressed info to file */
    status = SDend(fcomp);
    CHECK(status, FAIL, "SDend");

    /* read the compressed data back in */
#ifdef QAK
printf("reading compressed dataset\n");
#endif /* QAK */
    fcomp = SDstart(COMPFILE6, DFACC_RDWR);
    CHECK(fcomp, FAIL, "SDstart (again)");

    newsds2 = SDselect(fcomp, 0);
    if(newsds == FAIL) {
        fprintf(stderr, "Failed to select a data set for compressed access\n");
        num_errs++;
    }

    /*
     * Retrieve and verify the compression info - bug# 307, 9/7/01 - BMR
     */
    comp_type = COMP_CODE_INVALID;  /* reset variables before retrieving info */
    HDmemset(&cinfo, 0, sizeof(cinfo)) ;
    status = SDgetcompress(newsds2, &comp_type, &cinfo);
    CHECK(status, FAIL, "SDgetcompress");
    VERIFY(comp_type, COMP_CODE_NONE, "SDgetcompress");

    start[0] = start[1] = 0;
    end[0]   = end[1]   = 5;
#ifdef QAK
printf("before SDreaddata\n");
#endif /* QAK */
    status = SDreaddata(newsds2, start, NULL, end, (VOIDP) rdata);
    CHECK(status, FAIL, "SDreaddata");

    for(i = 0; i < 25; i++)
        if(idata[i] != rdata[i]) {
            fprintf(stderr,"Bogus val in loc %d in compressed dset want %ld got %ld\n", i, (long)idata[i], (long)rdata[i]);
            num_errs++;
        }

#ifdef QAK
printf("before SDendaccess\n");
#endif /* QAK */
    status = SDendaccess(newsds2);
    CHECK(status, FAIL, "SDendaccess");

    status = SDend(fcomp);
    CHECK(status, FAIL, "SDend");

#ifdef QAK
printf("writing 7th compressed dataset, deflate encoding\n");
#endif /* QAK */
    fcomp = SDstart(COMPFILE7, DFACC_CREATE);
    CHECK(fcomp, FAIL, "SDstart");

    nt = DFNT_INT32;
    dimsize[0] = 5;
    dimsize[1] = 5;
    newsds = SDcreate(fcomp, "CompDataSet7", nt, 2, dimsize);
    if(newsds == FAIL) {
        fprintf(stderr, "Failed to create a new data set for compression testing\n");
        num_errs++;
    }

    for(i = 0; i < 25; i++)
        idata[i] = i*10;

#ifdef QAK
printf("before SDsetcompress\n");
#endif /* QAK */
    cinfo.deflate.level=6;
    status = SDsetcompress(newsds,COMP_CODE_DEFLATE,&cinfo);
    CHECK(status, FAIL, "SDcompress");

    start[0] = start[1] = 0;
    end[0]   = end[1]   = 5;
#ifdef QAK
printf("before SDwritedata\n");
#endif /* QAK */
    status = SDwritedata(newsds, start, NULL, end, (VOIDP) idata);
    CHECK(status, FAIL, "SDwritedata");

#ifdef QAK
printf("before SDendaccess\n");
#endif /* QAK */
    status = SDendaccess(newsds);
    CHECK(status, FAIL, "SDendaccess");

    /* need to close to flush compressed info to file */
    status = SDend(fcomp);
    CHECK(status, FAIL, "SDend");

    /* read the compressed data back in */
#ifdef QAK
printf("reading compressed dataset\n");
#endif /* QAK */
    fcomp = SDstart(COMPFILE7, DFACC_RDWR);
    CHECK(fcomp, FAIL, "SDstart (again)");

    newsds2 = SDselect(fcomp, 0);
    if(newsds == FAIL) {
        fprintf(stderr, "Failed to select a data set for compressed access\n");
        num_errs++;
    }

    /*
     * Retrieve and verify the compression info - bug# 307, 9/7/01 - BMR
     */
    comp_type = COMP_CODE_INVALID;  /* reset variables before retrieving info */
    HDmemset(&cinfo, 0, sizeof(cinfo)) ;
    status = SDgetcompress(newsds2, &comp_type, &cinfo);
    CHECK(status, FAIL, "SDgetcompress");
    VERIFY(comp_type, COMP_CODE_DEFLATE, "SDgetcompress");
    VERIFY(cinfo.deflate.level, 6, "SDgetcompress");

    start[0] = start[1] = 0;
    end[0]   = end[1]   = 5;
#ifdef QAK
printf("before SDreaddata\n");
#endif /* QAK */
    status = SDreaddata(newsds2, start, NULL, end, (VOIDP) rdata);
    CHECK(status, FAIL, "SDreaddata");

    for(i = 0; i < 25; i++)
        if(idata[i] != rdata[i]) {
            fprintf(stderr,"Bogus val in loc %d in compressed dset want %ld got %ld\n", i, (long)idata[i], (long)rdata[i]);
            num_errs++;
        }

#ifdef QAK
printf("before SDendaccess\n");
#endif /* QAK */
    status = SDendaccess(newsds2);
    CHECK(status, FAIL, "SDendaccess");

#ifdef QAK
printf("before SDend\n");
#endif /* QAK */
    status = SDend(fcomp);
    CHECK(status, FAIL, "SDend");

    /* Return the number of errors that's been kept track of so far */
    return num_errs;

}   /* end test_compression */

int 
main(int argc, char *argv[])
{
    int32 f1, f2, fext, fnbit; /* File handles */
    int32 nt;                /* Number type */
    int32 dimsize[10];       /* dimension sizes */
    int32 newsds, newsds2, newsds3; /* SDS handles */
    int32 sdsid;                    /* SDS handle */
    int32 dimid, dimid1, dimid2; /* Dimension handles */
    int32 num_sds;               /* number of SDS in file */
    int32 num_gattr;             /* Number of global attributes */
    int32 offset;                /* offset for ? */
    int32 index;       /* Index of dataset in file */
    int32 ival;
    int32 sdid;        /* another SDS handle */
    int32 rank;        /* rank of SDS */
    intn  status;      /* status flag */
    intn  i;           /* loop variables */
    intn  nattrs;      /* Number of attributes again? */
    char name[90];
    char text[256];
    int32   start[10], end[10], stride[10]; /* start, end, stride arrays */
    int32   scale[10];
    char    l[80], u[80], fmt[80], c[80];
    int32   count;
    int32   fillval;
    int32   idata[100];
    int32   rdata[100];
    int16   sdata[100];
    int32   ndg_saved_ref;  /* used to save a ref of an SDS in one of the test */
    uint8   iuval;
    float32 data[1000], max, min, imax, imin;
    float64 cal, cale, ioff, ioffe;
    int     num_errs = 0;    /* number of errors so far */


#ifdef macintosh
	Ptr	currStackBase, newApplLimit, currApplLimit, currHeapEnd;


	/*	Expand the stack.  hdf_write_var( ) causes the stack to collide with 
		the 68K application heap when only the default stack size is used. */
	currStackBase = LMGetCurStackBase( );
	newApplLimit = (Ptr) ( (long) currStackBase - 65536L );
	currApplLimit = GetApplLimit( );
	if ( newApplLimit > currApplLimit )	/* If we're about to shrink the stack, ... */
		 newApplLimit = currApplLimit;	/* ... then don't. */

	currHeapEnd = LMGetHeapEnd( );
	if ( newApplLimit < currHeapEnd )	/* If we're about overlap the stack and heap, */
		 newApplLimit = currHeapEnd;	/* ... then don't. */

	SetApplLimit( newApplLimit );
#endif

#if defined __MWERKS__
    argc = ccommand(&argv);
#endif

    ncopts = NC_VERBOSE;

    /* Testing SDstart */
    /* Try start non-existing file with RDONLY and RDWR. Both should fail. */
    f1 = SDstart(UFOFILE, DFACC_RDONLY);

    if (f1 != FAIL)
      {
          fprintf(stderr, "SDstart(..., RDONLY) should fail\n");
          num_errs++;
          SDend(f1);
      }

    f1 = SDstart(UFOFILE, DFACC_RDWR);
    if (f1 != FAIL)
      {
          fprintf(stderr, "SDstart(..., RDWR) should fail\n");
          num_errs++;
          SDend(f1);
      }

    /* -------hmm what are testing here?----------------- */

    /* Create two files */
    f1 = SDstart(FILE1, DFACC_CREATE);
    CHECK(f1, FAIL, "SDstart");

    f2 = SDstart(FILE2, DFACC_CREATE);
    CHECK(f2, FAIL, "SDstart");

    /* whats in these empty files */
    status = SDfileinfo(f1, &num_sds, &num_gattr);
    CHECK(status, FAIL, "SDfileinfo");

    if(num_gattr != 0) 
      {
          fprintf(stderr, "File %s still has stuff in it\n", FILE1);
          num_errs++;
      }

    /* create a 4 by 8 dataset called DataSetAlpha in file test1.hdf */
    dimsize[0] = 4;
    dimsize[1] = 8;
    newsds = SDcreate(f1, "DataSetAlpha", DFNT_FLOAT32, 2, dimsize);
    CHECK(newsds, FAIL, "SDcreate: Failed to create a new data set DataSetAlpha ");

    /* save the ref number for the first dataset --- will check at very end */
    ndg_saved_ref = SDidtoref(newsds);
    CHECK(ndg_saved_ref, 0, "SDidtoref: Failed to get NDG ref for DataSetAlpha ");

    /* create datatset DataSetGamma in file test1.hdf */
    newsds3 = SDcreate(f1, "DataSetGamma", DFNT_FLOAT64, 1, dimsize);
    CHECK(newsds3, FAIL, "SDcreate:Failed to create a new data set gamma");

    /* get info on number of datasets and global attributes in file */
    status = SDfileinfo(f1, &num_sds, &num_gattr);
    CHECK(status, FAIL, "SDfileinfo");

    if(num_sds != 2) 
      {
        fprintf(stderr, "Wrong number of datasets in file 1\n");
        num_errs++;
      }

    /* get dimension handle for first dimension? of DataSetGamma */
    dimid = SDgetdimid(newsds3, 0);
    CHECK(dimid, FAIL, "SDgetdimid:Failed to get dimension id");

    /* reset the dimension name to Mydim? */
    status = SDsetdimname(dimid, "MyDim");
    CHECK(status, FAIL, "SDsetdimname: Failed to set dimension name to 'MyDim'");

    /* Set dimension attribute to 'TRUE' */
    status = SDsetattr(dimid, "DimensionAttribute", DFNT_CHAR8, 4, "TRUE");
    CHECK(status, FAIL, "SDsetattr: Failed to set Dimension attribute");
    
    /* hmm. look it back up again. */
    status = SDfindattr(dimid, "DimensionAttribute");
    if(status != 0) 
      {
        fprintf(stderr, "SDfindattr: Bad index for finding 'DimensionAttribute' %d\n",
                status);
        num_errs++;
      }

    /* Find out info about first atribute for dimension  */
    status = SDattrinfo(dimid, (int32) 0, name, &nt, &count);
    CHECK(status, FAIL, "SDattrinfo");

    /* read first attribute in, assume CHAR here. */
    status = SDreadattr(dimid, 0, text);
    CHECK(status, FAIL, "SDreadattr");
    
    /* Compare value reterieved to what was written */
    if(HDstrncmp(text, "TRUE", count)) 
      {
        fprintf(stderr, "SDreadattr: Invalid dimension attribute read <%s>\n", text);
        num_errs++;
      }

    /* get First dimension of dataset 'DataSetAlpha' */
    dimid = SDgetdimid(newsds, 0);
    CHECK(dimid, FAIL, "SDgetdimid: Failed to get dimension id");

    /* Set this name of this dimension to 'Mydim' */
    status = SDsetdimname(dimid, "MyDim");
    CHECK(status, FAIL, "SDsetdimname");


    /* Set the scales for this dimension also */
    scale[0] = 1;
    scale[1] = 5;
    scale[2] = 7;
    scale[3] = 24;
    status = SDsetdimscale(dimid, 4, DFNT_INT32, (VOIDP) scale);
    CHECK(status, FAIL, "SDsetdimscale");

    /* Set the dimension strings for the dimension also */
    status = SDsetdimstrs(dimid, "DimLabel", NULL, "TheFormat");
    CHECK(status, FAIL, "SDsetdimstrs");

    /* verify that we can read the dimensions values with SDreaddata */
    start[0] = 0;
    end[0]   = 4;
    status = SDreaddata(dimid, start, NULL, end, (VOIDP) idata);
    CHECK(status, FAIL, "SDreaddata");

    /* compare retrieved values for scale */
    for(i = 0; i < 4; i++) 
      {
        if(idata[i] != scale[i]) 
          {
              fprintf(stderr, "SDreaddata() returned %ld not %ld in location %d\n", 
                      (long)idata[i], (long)scale[i], i);
              num_errs++;
          }
      }

    /* hmm...lets store an attribute here for the dimension */
    max = 3.1415;
    status = SDsetattr(dimid, "DimAttr", DFNT_FLOAT32, 1, (VOIDP) &max);
    CHECK(status, FAIL, "SDsetattr");

    /* lets make sure we can read it too */
    status = SDattrinfo(dimid, 3, name, &nt, &count);
    CHECK(status, FAIL, "SDattrinfo");

    if(nt != DFNT_FLOAT32) 
      {
        fprintf(stderr, "Wrong number type for SDattrinfo(dim)\n");
        num_errs++;
      }

    if(count != 1) 
      {
        fprintf(stderr, "Wrong count for SDattrinfo(dim)\n");
        num_errs++;
      }

    if(strcmp(name, "DimAttr")) 
      {
        fprintf(stderr, "Wrong name for SDattrinfo(dim)\n");
        num_errs++;
      }

    /* get second dimension of data set 'DataSetAlpha' */
    dimid2 = SDgetdimid(newsds, 1);
    CHECK(dimid2, FAIL, "SDgetdimid: Failed to get second dimension id");

    /* lets store an attribute for the dimension without explicitly 
       creating the coord var first */
    ival = -256;
    status = SDsetattr(dimid2, "Integer", DFNT_INT32, 1, (VOIDP) &ival);
    CHECK(status, FAIL, "SDsetattr");

    /* lets make sure we can read it too */
    status = SDattrinfo(dimid2, 0, name, &nt, &count);
    CHECK(status, FAIL, "SDattrinfo");

    if(nt != DFNT_INT32) 
      {
        fprintf(stderr, "Wrong number type for SDattrinfo(dim)\n");
        num_errs++;
      }

    if(count != 1) 
      {
        fprintf(stderr, "Wrong count for SDattrinfo(dim)\n");
        num_errs++;
      }

    if(strcmp(name, "Integer")) 
      {
        fprintf(stderr, "Wrong name for SDattrinfo(dim)\n");
        num_errs++;
      }

    /* read dimension attribute back in */
    ival = 0;
    status = SDreadattr(dimid2, 0, (VOIDP) &ival);
    CHECK(status, FAIL, "SDreatattr");
    
    if(ival != -256) 
      {
        fprintf(stderr, "Wrong value for SDreadattr(dim)\n");
        num_errs++;
      }

    /* add an unsigned integer as an dimension attribute */
    iuval = 253;
    status = SDsetattr(dimid2, "UnsignedInteger", DFNT_UINT8, 1, (VOIDP) &iuval);
    CHECK(status, FAIL, "SDsetattr");

    /* lets make sure we can read it too */
    status = SDattrinfo(dimid2, 1, name, &nt, &count);
    CHECK(status, FAIL, "SDattrinfo");

    if(nt != DFNT_UINT8) 
      {
        fprintf(stderr, "Wrong number type for SDattrinfo(dim)\n");
        num_errs++;
      }

    if(count != 1) 
      {
        fprintf(stderr, "Wrong count for SDattrinfo(dim)\n");
        num_errs++;
      }

    if(strcmp(name, "UnsignedInteger")) 
      {
        fprintf(stderr, "Wrong name for SDattrinfo(dim)\n");
        num_errs++;
      }

    /* read second dimension attribute back in */
    iuval = 0;
    status = SDreadattr(dimid2, 1, (VOIDP) &iuval);
    CHECK(status, FAIL, "SDreatattr");
    
    if(iuval != 253) 
      {
        fprintf(stderr, "Wrong value for SDreadattr(dim)\n");
        num_errs++;
      }

    /* Find index of data set 'DataSetAlpha' in file test1.hdf */
    status = SDnametoindex(f1, "DataSetAlpha");
    if(status != 0) 
      {
        fprintf(stderr, "Couldn't find data set in file 1\n");
        num_errs++;
      }

    /* Try finding data set in test2.hdf, should fail? */
    status = SDnametoindex(f2, "DataSetAlpha");
    if(status != FAIL) 
      {
        fprintf(stderr, "Found data set in wrong file 2\n");
        num_errs++;
      }

    /* Try finding non-existent dataset in file, should fail */
    status = SDnametoindex(f1, "BogusDataSet");
    if(status != FAIL) 
      {
        fprintf(stderr, "Found bogus data set in file 1\n");
        num_errs++;
      }

    /* Set fill value for data set 'DataSetAlpha' assume we still have valid
       handle at this point...*/
    max = -17.5;
    status = SDsetfillvalue(newsds, (VOIDP) &max);
    CHECK(status, FAIL, "SDsetfillvalue");

    /* initialize array to write out */
    for(i = 0; i < 10; i++)
        data[i] = (float32) i;

    /* write out (1,1)->(3,3) array out */ 
    start[0] = start[1] = 1;
    end[0]   = end[1]   = 3;
    status = SDwritedata(newsds, start, NULL, end, (VOIDP) data);
    CHECK(status, FAIL, "SDwritedata");

    /* set the range for data set 'DataSetAlpha' */
    max = 10.0;
    min = 4.6;
    status = SDsetrange(newsds, (VOIDP) &max, (VOIDP) &min);
    CHECK(status, FAIL, "SDsetrange");

    /* Brillant...., retrieve it right back....*/
    status = SDgetrange(newsds, (VOIDP) &imax, (VOIDP) &imin);
    CHECK(status, FAIL, "SDsetrange");

    /* set a character attribute for data set 'DataSetAlpha' */
    status = SDsetattr(newsds, "spam", DFNT_CHAR8, 6, "Hi mom");
    CHECK(status, FAIL, "SDsetattr");

    /* Set the data strings for data set 'DataSetAlpha' */
    status = SDsetdatastrs(newsds, "TheLabel", "TheUnits", NULL, "TheCordsys");
    CHECK(status, FAIL, "SDsetdatastrs");

    /* Brilliant.....retrieve them right back */
    status = SDgetdatastrs(newsds, l, u, fmt, c, 80);
    CHECK(status, FAIL, "SDgetdatastrs");

    if(HDstrcmp(l, "TheLabel")) 
      {
        fprintf(stderr, "Bogus label returned (%s)\n", l);
        num_errs++;
      }
    if(HDstrcmp(u, "TheUnits")) 
      {
        fprintf(stderr, "Bogus units returned (%s)\n", u);
        num_errs++;
      }
    if(HDstrcmp(fmt, "")) 
      {
        fprintf(stderr, "Bogus format returned\n");
        num_errs++;
      }
    if(HDstrcmp(c, "TheCordsys")) 
      {
        fprintf(stderr, "Bogus cordsys returned\n");
        num_errs++;
      }

    /* retrieve CHAR attribute for 'DataSetAlpha' */
    status = SDfindattr(newsds, "spam");
    if(status != 2) 
      {
        fprintf(stderr, "Bad index for SDfindattr\n");
        num_errs++;
      }

    /* retrieve non-existent CHAR attribute for 'DataSetAlpha'. 
       Should fail. */
    status = SDfindattr(newsds, "blarf");
    if(status != FAIL) 
      {
        fprintf(stderr, "SDfindattr found non-existant attribute\n");
        num_errs++;
      }

    /* hmm....set global attributes for File 'test1.hdf' */
    status = SDsetattr(f1, "F-attr", DFNT_CHAR8, 10, "globulator");
    CHECK(status, FAIL, "SDsetattr");

    /* get info about the global attribute just created....*/
    status = SDattrinfo(f1, (int32) 0, name, &nt, &count);
    CHECK(status, FAIL, "SDattrinfo");

    /* read this global attribute back in ....*/
    status = SDreadattr(f1, 0, text);
    CHECK(status, FAIL, "SDreadattr");
    
    if(HDstrncmp(text, "globulator", count)) 
      {
        fprintf(stderr, "Invalid global attribute read <%s>\n", text);
        num_errs++;
      }

    /* Get number of SDS and global attributes in file 'test2.hdf'.
       It should be empty...*/
    status = SDfileinfo(f2, &num_sds, &num_gattr);
    if(num_sds != 0) 
      {
        fprintf(stderr, "File2 still has stuff in it\n");
        num_errs++;
      }

    /* Set calibration info for dataset 'DataSetGamma' in file 'test1.hdf' */
    cal   = 1.0;
    cale  = 5.0;
    ioff  = 3.0;
    ioffe = 2.5;
    nt    = DFNT_INT8;
    status = SDsetcal(newsds3, cal, cale, ioff, ioffe, nt);
    CHECK(status, FAIL, "SDsetcal");

    /* create a record variable in file 'test2.hdf' */
    dimsize[0] = SD_UNLIMITED;
    dimsize[1] = 6;
    newsds2 = SDcreate(f2, "DataSetBeta", DFNT_INT16, 2, dimsize);
    CHECK(newsds2, FAIL, "SDcreate: Failed to create new data set 'DataSetBeta'");

    /* get info on number of SDSs and global attributes in file 'test2.hdf'
       There should be only 1 SDS */
    status = SDfileinfo(f2, &num_sds, &num_gattr);
    if(num_sds != 1) 
      {
        fprintf(stderr, "Wrong number of datasets in file 2\n");
        num_errs++;
      }

    for(i = 0; i < 50; i++)
        sdata[i] = i;

    /* Write data to dataset 'DataSetBeta' in file 'test2.hdf' */
    start[0] = start[1] = 0;
    end[0]   = 8;
    end[1]   = 6;
    status = SDwritedata(newsds2, start, NULL, end, (VOIDP) sdata);
    CHECK(status, FAIL, "SDwritedata");

    /* Now read part of an earlier dataset,'DataSetAlpha', 
       back in from file 'test1.hdf' */
    start[0] = start[1] = 0;
    end[0]   = end[1]   = 3;
    status = SDreaddata(newsds, start, NULL, end, (VOIDP) data);
    CHECK(status, FAIL, "SDreaddata");

    /* verify the data values retrieved from 'DataSetAlpha' */
    if(data[0] != -17.5) 
      {
        fprintf(stderr, "Wrong value returned loc 0: %f\n",(float)data[0]);
        num_errs++;
      }
    if(data[3] != -17.5) 
      {
        fprintf(stderr, "Wrong value returned loc 3: %f\n",(float)data[3]);
        num_errs++;
      }
    if(data[5] != 1.0) 
      {
        fprintf(stderr, "Wrong value returned loc 5: %f\n",(float)data[5]);
        num_errs++;
      }
    if(data[6] != -17.5) 
      {
        fprintf(stderr, "Wrong value returned loc 6: %f\n",(float)data[6]);
        num_errs++;
      }
    if(data[8] != 4.0) 
      {
        fprintf(stderr, "Wrong value returned loc 8: %f\n",(float)data[8]);
        num_errs++;
      }

    for(i = 0; i < 50; i++)
        sdata[i] = 0;

    /* read data back in from 'DataSetBeta' from file 'test2.hdf' */
    start[0] = start[1] = 1;
    end[0]   = 3;
    end[1]   = 3;
    stride[0] = 2;
    stride[1] = 2;
    status = SDreaddata(newsds2, start, stride, end, (VOIDP) sdata);
    CHECK(status, FAIL, "SDreaddata");

    /* why do we print these 10 values here?....*/
    for(i = 0; i < 10; i++)
        printf("%d := %d\n", i, sdata[i]);
    
    /* why do we set calibration info and then use SDgetcal() 
       on dataset 'DataSetGamma' ? */
    cal   = 1.0;
    cale  = 5.0;
    ioff  = 3.0;
    ioffe = 2.5;
    nt    = DFNT_INT8;
    status = SDgetcal(newsds3, &cal, &cale, &ioff, &ioffe, &nt);
    CHECK(status, FAIL, "SDgetcal");

    /* Verify calibration data for data set 'DataSetGamma' */
    if(cal != 1.0) 
      {
        fprintf(stderr, "Wrong calibration info\n");
        num_errs++;
      }

    if(cale != 5.0) 
      {
        fprintf(stderr, "Wrong calibration info\n");
        num_errs++;
      }

    if(ioff != 3.0) 
      {
        fprintf(stderr, "Wrong calibration info\n");
        num_errs++;
      }

    if(ioffe != 2.5) 
      {
        fprintf(stderr, "Wrong calibration info\n");
        num_errs++;
      }

    if(nt != DFNT_INT8) 
      {
        fprintf(stderr, "Wrong calibration info\n");
        num_errs++;
      }

    /* end access to data set 'DataSetAlpha' */
    status = SDendaccess(newsds);
    CHECK(status, FAIL, "SDendaccess");

    /* end access to data set 'DataSetBeta' */
    status = SDendaccess(newsds2);
    CHECK(status, FAIL, "SDendaccess");

    /* end access to data set 'DataSetGamma' */
    status = SDendaccess(newsds3);
    CHECK(status, FAIL, "SDendaccess");

    /* Close access to file 'test1.hdf' */
    status = SDend(f1);
    CHECK(status, FAIL, "SDend");

    /* Close access to file 'test2.hdf' */
    status = SDend(f2);
    CHECK(status, FAIL, "SDend");


    /*
     * New set of tests?.....
     */

    /* test SDsetfillmode   */
    /* test fixed size SDS   */
    /* create an empty SDS, set SD_NOFILL.
       Change the fill mode to SD_FILL, and write a slab of data */

    /* open file 'test1.hdf' */
    f1 = SDstart(FILE1, DFACC_RDWR);
    CHECK(f1, FAIL, "SDstart (again)");

    /* Set fill mode on file to not write out fill values */
    status = SDsetfillmode(f1, SD_NOFILL);
    CHECK(status, FAIL, "SDsetfillmode: (SD_NOFILL)");

    /* Create data set 'FIXED1' in file test1.hdf */
    dimsize[0]=5;
    dimsize[1]=6;
    sdid = SDcreate(f1, "FIXED1", DFNT_INT32, 2, dimsize);
    CHECK(sdid, FAIL, "SDcreate:Fail to create data set 'FIXED1' in 'test1.hdf'");

    for (i=0; i<30; i++)
        idata[i] = i+100;

    /* Set fill value attribute for data set 'FIXED1' using SDsetattr().
       Same affect as using SDsetfillvalue(). */
    fillval = -300;
    status = SDsetattr(sdid, "_FillValue", DFNT_INT32, 1,
               (VOIDP) &fillval); /* can use SDsetfillvalue */
    CHECK(status, FAIL, "SDsetattr");

    /* end access to data set 'FIXED1' */
    status = SDendaccess(sdid);
    CHECK(status, FAIL, "SDendaccess");

    /* get index of dataset in file 'test1.hdf' called 'FIXED1' */
    index = SDnametoindex(f1, "FIXED1");
    CHECK(index, FAIL, "SDnametoindex");

    /* Select data set 'FIXED1' based on it's index */
    sdid = SDselect(f1, index);
    CHECK(sdid, FAIL, "SDselect");

    /* change the fill mode for the file back to writing out the fill
       values. */
    status = SDsetfillmode(f1, SD_FILL);
    CHECK(status, FAIL, "SDsetfillmode");

    /* Write data to data set 'FIXED1'.
       Note that SD_FILL mode is on. */
    start[0]=2;
    start[1]=0;
    end[0]=1;
    end[1]=6;
    status = SDwritedata(sdid, start, NULL, end, (VOIDP)idata);
    CHECK(status, FAIL, "SDwritedata: (SD_FILL)");

    /* end access to data set 'FIXED1' */
    status = SDendaccess(sdid);
    CHECK(status, FAIL, "SDendaccess");

    /* set the fill mode for 'test1.hdf' to no-fill */
    status = SDsetfillmode(f1, SD_NOFILL);
    CHECK(status, FAIL, "SDsetfillmode (SD_NOFILL)");

    /* create a data set 'FIXED' in file 'test1.hdf' */
    sdid = SDcreate(f1, "FIXED", DFNT_INT32, 2, dimsize);
    CHECK(sdid,FAIL,"SDcreate:Failed to create data set 'FIXED' in file 'test1.hdf'");

    for (i=0; i<30; i++)
        idata[i] = i+100;

    /* Set fill value for data set 'FIXED' using SDsetfillvalue() */
    fillval = -300;
    status = SDsetfillvalue(sdid, (VOIDP) &fillval);
    CHECK(status, FAIL, "SDsetfillvalue");

    /* write out the first 2 records to data set 'FIXED' with SD_NOFILL mode */
    start[0]=2;
    start[1]=0;
    end[0]=1;
    end[1]=6;
    status = SDwritedata(sdid, start, NULL, end, (VOIDP)idata);
    CHECK(status, FAIL, "SDwritedata: (SD_NOFILL)");

    /* end access to data set 'FIXED' */
    status = SDendaccess(sdid);
    CHECK(status, FAIL, "SDendaccess");

    /* close file 'test1.hdf' */
    status = SDend(f1);
    CHECK(status, FAIL, "SDend");

    /* open again, write record 4 with SD_FILL mode */
    /* fill values already written out in the first SDwritedata,
       fillmode changes should not affect the fill values */

    /* open file 'test1.hdf' */
    f1 = SDstart(FILE1, DFACC_RDWR);
    CHECK(f1, FAIL, "SDstart: test1.hdf");

    /* Set fill mode to SD_FILL */
    status = SDsetfillmode(f1, SD_FILL);
    CHECK(status, FAIL, "SDsetfillmode: (SD_FILL)");

    /* get index of data set 'FIXED' */
    index = SDnametoindex(f1, "FIXED");
    CHECK(index, FAIL, "SDnametoindex: (FIXED)");

    /* Select the data set 'FIXED' based on its index */
    sdid = SDselect(f1, index);
    CHECK(sdid, FAIL, "SDselect: (FIXED)");

    /* Write record 4 */
    start[0]=4;
    start[1]=0;
    end[0]=1;
    end[1]=6;
    status = SDwritedata(sdid, start, NULL, end, (VOIDP)idata);
    CHECK(status, FAIL, "SDwritedata (SD_FILL)");

    /* end access to data set 'FIXED' */
    status = SDendaccess(sdid);
    CHECK(status, FAIL, "SDendaccess");

    /* close file 'test1.hdf' */
    status = SDend(f1);
    CHECK(status, FAIL, "SDend");

    /* read back and check fill values */

    /* open file 'test1.hdf' back up */
    f1 = SDstart(FILE1, DFACC_RDWR);
    CHECK(f1, FAIL, "SDstart: test1.hdf");

    /* get index of data set 'FIXED' */
    index = SDnametoindex(f1, "FIXED");
    CHECK(index, FAIL, "SDnametoindex (FIXED)");

    /* Select the data set 'FIXED' based on its index */
    sdid = SDselect(f1, index);
    CHECK(sdid, FAIL, "SDselect (FIXED)");

    /* read data back in from data set 'FIXED' */
    start[0]=0;
    start[1]=0;
    end[0]=5;
    end[1]=6;
    status = SDreaddata(sdid, start, NULL, end, (VOIDP)idata);
    CHECK(status, FAIL, "SDreaddata(FIXED)");

    /* verify the data */
    for (i=12; i<18; i++)  
      {
        if ((idata[i] != 100 + (i-12)) ||
            (idata[i+12] != 100 + (i-12))) 
          {
           fprintf(stderr, "line %d, wrong value: should be %d, got %d %d\n",
                           __LINE__,100 + i-12, (int)idata[i], (int)idata[i+12]);
           num_errs++;
          }
      }
    
    for (i=18; i<24; i++)  
      {
        if (idata[i] ==fillval) 
          {
           fprintf(stderr, "line %d, wrong value: should not be %d, got %d\n",
                           __LINE__,(int)fillval, (int)idata[i]);
           num_errs++;
          }
      }

    /* end access to data set 'FIXED' */
    status = SDendaccess(sdid);
    CHECK(status, FAIL, "SDendaccess");

    /* read back in data set 'FIXED1' , with fill values */

    /* get index of data set 'FIXED1' from file 'test1.hdf' */
    index = SDnametoindex(f1, "FIXED1");
    CHECK(index, FAIL, "SDnametoindex (FIXED1)");

    /* select dataset 'FIXED1' based on its index in the file */
    sdid = SDselect(f1, index);
    CHECK(sdid, FAIL, "SDselect (FIXED1)");

    /* read data from data set 'FIXED1' */
    start[0]=0;
    start[1]=0;
    end[0]=5;
    end[1]=6;
    status = SDreaddata(sdid, start, NULL, end, (VOIDP)idata);
    CHECK(status, FAIL, "SDreaddata(FIXED)");

    /* verify the data */
    for (i=12; i<18; i++)  
      {
        if (idata[i] != (100 + (i-12)))  
          {
           fprintf(stderr, "line %d, wrong value: should be %d, got %d \n",
                           __LINE__, 100 + i-12, (int)idata[i]);
           num_errs++;
          }
      }

    for (i=18; i<24; i++)  
      {
        if (idata[i] != fillval) 
          {
           fprintf(stderr, "line %d, wrong value: should be %d, got %d\n",
                           __LINE__, (int)fillval, (int)idata[i]);
           num_errs++;
          }
      }

    /* end access to data set 'FIXED1' in file 'test1.hdf' */
    status = SDendaccess(sdid);
    CHECK(status, FAIL, "SDendaccess");

    /* close file 'test1.hdf' */
    status = SDend(f1);
    CHECK(status, FAIL, "SDend");

   /* 
    * test UNLIMITED size SDS   
    */

    /* open file 'test1.hdf' */
    f1 = SDstart(FILE1, DFACC_RDWR);
    CHECK(f1, FAIL, "SDstart (file1)");

    /* set fill mode to no-fill */
    status = SDsetfillmode(f1, SD_NOFILL);
    CHECK(status, FAIL, "SDsetfillmode (SD_NOFILL)");

    /* Set first dimension to UNLIMITED.
       Create data set 'UNLIMITED_SDS' in file 'test1.hdf' */
    dimsize[0]=SD_UNLIMITED;
    dimsize[1]=6;
    sdid = SDcreate(f1, "UNLIMITED_SDS", DFNT_INT32, 2, dimsize);
    CHECK(sdid, FAIL, "SDcreate:Failed to create data set 'UNLIMITED_SDS' in file 'test1.hdf'");

    for (i=0; i<24; i++)
        idata[i] = i;

    /* Set fill value for data set 'UNLIMITED_SDS' */
    fillval = -300;
    status = SDsetfillvalue(sdid, (VOIDP) &fillval);
    CHECK(status, FAIL, "SDsetfillvalue");

    /* write out the third record with SD_NOFILL mode on */
    start[0]=2;
    start[1]=0;
    end[0]=1;
    end[1]=6;
    status = SDwritedata(sdid, start, NULL, end, (VOIDP)idata);
    CHECK(status, FAIL, "SDwritedata: (SD_NOFILL, UNLIMITED)");

    /* end access to data set 'UNLIMITED_SDS' in file 'test1.hdf' */
    status = SDendaccess(sdid);
    CHECK(status, FAIL, "SDendaccess");

    /* Close file 'test1.hdf' */
    status = SDend(f1);
    CHECK(status, FAIL, "SDend");

    /* open again, write record 4 with SD_FILL mode */

    /* open file 'test1.hdf' again */
    f1 = SDstart(FILE1, DFACC_RDWR);
    CHECK(f1, FAIL, "SDstart: test1.hdf");

    /* set fill mode to SD_FILL */
    status = SDsetfillmode(f1, SD_FILL);
    CHECK(status, FAIL, "SDsetfillmode: (SD_FILL)");

    /* get index of data set 'UNLIMITED_SDS' */
    index = SDnametoindex(f1, "UNLIMITED_SDS");
    CHECK(index, FAIL, "SDnametoindex: (UNLIMITED)");

    /* select data set 'UNLIMITED_SDS' based on its index in the file */
    sdid = SDselect(f1, index);
    CHECK(sdid, FAIL, "SDselect: (UNLIMITED)");

    /* write 4?th record to data set */
    start[0]=4;
    start[1]=0;
    end[0]=1;
    end[1]=6;
    status = SDwritedata(sdid, start, NULL, end, (VOIDP)idata);
    CHECK(status, FAIL, "SDwritedata: (SD_FILL)");

    /* end access to data set 'UNLIMITED_SDS' */
    status = SDendaccess(sdid);
    CHECK(status, FAIL, "SDendaccess");

    /* close file 'test1.hdf' */
    status = SDend(f1);
    CHECK(status, FAIL, "SDend");

    /* read back and check fill values */

    /* open file 'test1.hdf' again */
    f1 = SDstart(FILE1, DFACC_RDWR);
    CHECK(f1, FAIL, "SDstart: (test1.hdf)");

    /* get index of data set 'UNLIMITED_SDS' */
    index = SDnametoindex(f1, "UNLIMITED_SDS");
    CHECK(index, FAIL, "SDnametoindex: (UNLIMITED_SDS)");

    /* select data set 'UNLIMITED_SDS' based on it's index in the file */
    sdid = SDselect(f1, index);
    CHECK(sdid, FAIL, "SDselect: (UNLIMITED_SDS)");

    /* read data from data set 'UNLIMITED_SDS' */
    start[0]=0;
    start[1]=0;
    end[0]=5;
    end[1]=6;
    status = SDreaddata(sdid, start, NULL, end, (VOIDP)idata);
    CHECK(status, FAIL, "SDwritedata(NO_FILL)");

    /* verify the data */
    for (i=12; i<18; i++)  
      {
        if ((idata[i] != (i-12)) || (idata[i+12] != (i-12))) 
          {
           fprintf(stderr, "line %d, wrong value for %d: should be %d, got %d\n",
                           __LINE__, i-12, (int)idata[i], (int)idata[i+12]);
           num_errs++;
          }
      }

    for (i=18; i<24; i++)  
      {
        if (idata[i] !=fillval) 
          {
           fprintf(stderr, "line %d, wrong value: should be %d, got %d\n",
                           __LINE__, (int)fillval, (int)idata[i]);
           num_errs++;
          }
      }

    /* end access to data set 'UNLIMITED_SDS' */
    status = SDendaccess(sdid);
    CHECK(status, FAIL, "SDendaccess");

    /* close file 'test1.hdf' */
    status = SDend(f1);
    CHECK(status, FAIL, "SDend");

   /* 
    * test SDsetdimval_incomp() 
    */

    /* open file 'test1.hdf' */
    f1 = SDstart(FILE1, DFACC_RDWR);
    CHECK(f1, FAIL, "SDstart (file1)");

    /* set first dimension to be UNLIMITED.
       Create data set 'dimval_1_compat' */
    dimsize[0]=SD_UNLIMITED;
    dimsize[1]=6;
    sdid = SDcreate(f1, "dimval_1_compat", DFNT_INT32, 2, dimsize);
    CHECK(sdid,FAIL,"SDcreate:Failed to create data set 'dimval_1_compat' in file 'test1.hdf'");

    /* get handle for first dimension of data set 'dimval_1_compat' */
    dimid=SDgetdimid(sdid, 0);
    CHECK(dimid, FAIL, "SDgetdimid");

    /* get handle for second dimension of data set 'dimval_1_compat' */
    dimid1=SDgetdimid(sdid, 1);
    CHECK(dimid1, FAIL, "SDgetdimid");

    /* set second dimension as being backward compatible, default is
       non-compatible  */
    status = SDsetdimval_comp(dimid1, SD_DIMVAL_BW_COMP);
    CHECK(status, FAIL, "SDsetdimval_comp");

    for (i=0; i<6; i++)
        scale[i]=i*5;

    /* set the scale for the second dimension */
    status = SDsetdimscale(dimid1, 6, DFNT_INT32, scale);
    CHECK(status, FAIL, "SDsetdimscale");

    for (i=0; i<24; i++)
        idata[i] = i;

    /* write data to data set 'dimval_1_compat' in file 'test1.hdf' */
    start[0]=0;
    start[1]=0;
    end[0]=4;
    end[1]=6;
    status = SDwritedata(sdid, start, NULL, end, (VOIDP)idata);
    CHECK(status, FAIL, "SDwritedata");

    /* end access to data set 'dimval_1_compat' */
    status = SDendaccess(sdid);
    CHECK(status, FAIL, "SDendaccess");

    /* close file 'test1.hdf */
    status = SDend(f1);
    CHECK(status, FAIL, "SDend");

    /* read back and change dimval compatibility  */

    /* open file 'test1.hdf' again */
    f1 = SDstart(FILE1, DFACC_RDWR);
    CHECK(f1, FAIL, "SDstart: (again2)");

    /* get index of data set 'dimval_1_compat' in file 'test1.hdf' */
    index = SDnametoindex(f1, "dimval_1_compat");
    CHECK(index,FAIL,"SDnametoindex: failed to get index for data set 'dimval_non_compat' in file 'test1.hdf'");

    /* select data set 'dimval_1_compat' based on it's index in file */
    sdid = SDselect(f1, index);
    CHECK(sdid,FAIL,"SDselect:Failed to select data set 'dimval_1_compat' in file 'test1.hdf'");

    /* info on data set 'dimval_1_compat' */
    status = SDgetinfo(sdid, name, (int32 *)&rank, dimsize, &nt, (int32 *)&nattrs);
    CHECK(status, FAIL, "SDgetinfo");

    /* verify correctness of information */
    if (rank!=2 || dimsize[0]!=4 || dimsize[1]!=6 || nt!=DFNT_INT32) 
      {
        fprintf(stderr, "SDgetinfo returned wrong values\n");
          num_errs++;
      }

    /* get handle for first dimension of data set 'dimval_1_compat' */
    dimid=SDgetdimid(sdid,0);
    CHECK(dimid, FAIL, "SDgetdimid");

    /* get dimension info for first dimension */
    status = SDdiminfo(dimid, name, (int32 *)&dimsize[0], &nt, (int32 *)&nattrs);
    CHECK(status, FAIL, "SDdiminfo");

    /* verify correctness of information */
    if (dimsize[0]!=SD_UNLIMITED || nt!= 0 )  
      {
          fprintf(stderr, "SDdiminfo returned wrong values\n");
          num_errs++;
      }
    /* is it backward non-compatible? */
    status = SDisdimval_bwcomp(dimid);
    if (status != SD_DIMVAL_BW_INCOMP)
       { 
           fprintf(stderr, "SDisdimvalcomp returned wrong value for dimension.\n");
            num_errs++;
       }
    /* re-set first dimension as backward compatible */
    status = SDsetdimval_comp(dimid, SD_DIMVAL_BW_COMP);
    CHECK(status, FAIL, "SDsetdimval_comp");

    /* get handle for second dimension of data set 'dimval_1_compat' */
    dimid1=SDgetdimid(sdid,1);
    CHECK(dimid1, FAIL, "SDgetdimid");

    /* get dimension info for second dimension */
    status = SDdiminfo(dimid1, name, (int32 *)&dimsize[1], &nt, (int32 *)&nattrs);
    CHECK(status, FAIL, "SDdiminfo");

    /* verify correctness of information */
    if (dimsize[1]!=6 || nt!= DFNT_INT32 )  
      {
          fprintf(stderr, "Failed on SDgetinfo call\n");
          num_errs++;
      }

    /* read data back from data set 'dimval_1_compat' */
    status = SDreaddata(sdid, start, NULL, end, (VOIDP)idata);
    CHECK(status, FAIL, "SDwritedata");

    /* verify data */
    for (i=0; i<24; i++)  
      {
        if (idata[i] != i) 
          {
           fprintf(stderr, "line %d, wrong value: should be %d, got %d\n",
                           __LINE__, i, (int)idata[i]);
           num_errs++;
          }
      }

    /* see if second dimension is backward compatible. 
       should be compatible */
    status = SDisdimval_bwcomp(dimid1);
    if (status != SD_DIMVAL_BW_COMP)  
      {
          fprintf(stderr, "SDisdimvalcomp returned wrong value for dimension\n");
          num_errs++;
      }

    /* re-set second dimension as backward non-compatible */
    status = SDsetdimval_comp(dimid1, SD_DIMVAL_BW_INCOMP);
    CHECK(status, FAIL, "SDsetdimval_comp");

    /* end access to data set 'dimval_1_compat' */
    status = SDendaccess(sdid);
    CHECK(status, FAIL, "SDendaccess");

    /* close file 'test1.hdf' */
    status = SDend(f1);
    CHECK(status, FAIL, "SDend");

    /* open one last time to check that NDG ref has been constant */
    /* check SDsetdimval_compat */

    /* open file 'test1.hdf' again */
    f1 = SDstart(FILE1, DFACC_RDWR);
    CHECK(f1, FAIL, "SDstart (again3)");

    /* get index of data set 'dimval_1_compat' in file 'test1.hdf' */
    index = SDnametoindex(f1, "dimval_1_compat");
    CHECK(index,FAIL,"SDnametoindex: failed to get index for data set 'dimval_1_compat' in file 'test1.hdf'");

    /* select data set 'dimval_1_compat' based on it's index in file */
    sdid = SDselect(f1, index);
    CHECK(sdid,FAIL,"SDselect:Failed to select data set 'dimval_1_compat' in file 'test1.hdf'");

    /* info on data set 'dimval_1_compat' */
    status = SDgetinfo(sdid, name, (int32 *)&rank, dimsize, &nt, (int32 *)&nattrs);
    CHECK(status, FAIL, "SDgetinfo");

    /* verify correctness of information */
    if (rank!=2 || dimsize[0]!=4 || dimsize[1]!=6 || nt!=DFNT_INT32) 
      {
        fprintf(stderr, "SDgetinfo returned wrong values\n");
          num_errs++;
      }

    /* get handle for second dimension of data set 'dimval_1_compat' */
    dimid1=SDgetdimid(sdid,1);
    CHECK(dimid1, FAIL, "SDgetdimid");

    /* get dimension info for second dimension */
    status = SDdiminfo(dimid1, name, (int32 *)&dimsize[1], &nt, (int32 *)&nattrs);
    CHECK(status, FAIL, "SDdiminfo");

    /* verify correctness of information */
    if (dimsize[1]!=6 || nt!= DFNT_INT32 )  
      {
          fprintf(stderr, "Failed on SDgetinfo call\n");
          num_errs++;
      }

    /* see if second dimensionis backward compatible. 
       should be backward non-compatible */
    status = SDisdimval_bwcomp(dimid1);
    if (status != SD_DIMVAL_BW_INCOMP)  
      {
          fprintf(stderr, "SDisdimvalcomp returned wrong value\n");
          num_errs++;
      }
    /* re-set second dimension as backward compatible */
    status = SDsetdimval_comp(dimid1, SD_DIMVAL_BW_COMP);
    CHECK(status, FAIL, "SDsetdimval_comp");
    /* end access to data set 'dimval_1_compat' */
    status = SDendaccess(sdid);
    CHECK(status, FAIL, "SDendaccess");

    /*
     * used saved ref at the begining to retrieve the data set
     */
    
    /* get the index of the data set to which this 'ref' belongs to */
    index = SDreftoindex(f1, ndg_saved_ref);
    CHECK(index,FAIL,"SDreftoindex: failed to get index for 'ndg_saved_ref'");

    /* get handle for this data set (DataSetAlpha) */
    sdsid = SDselect(f1, index);
    CHECK(sdsid,FAIL,"SDselect: Failed to get handle for data set 'DataSetAlpha' ");

    /* check if ref of this is the same as the one saved earlier */
    if(ndg_saved_ref != SDidtoref(sdsid)) 
      {
        fprintf(stderr, "Saved NDG ref != to SDindextoref of same\n");
        num_errs++;
      }

    /* end access to data set 'DataSetAlpha' in file 'test1.hdf' */    
    status = SDendaccess(sdsid);
    CHECK(status, FAIL, "SDendaccess");

    /* close file 'test1.hdf' */
    status = SDend(f1);
    CHECK(status, FAIL, "SDend");

#ifdef EXTERNAL_TEST

    /*
     * Test the External File storage stuff
     */

    /* Create file 'exttst.hdf' */
    fext = SDstart(EXTTST, DFACC_CREATE);
    CHECK(fext, FAIL, "SDstart");

    /* Create data set 'ExteneralDataSet' in file 'exttst.hdf' */
    nt = DFNT_INT32 | DFNT_NATIVE;
    dimsize[0] = 5;
    dimsize[1] = 5;
    newsds = SDcreate(fext, "ExternalDataSet", nt, 2, dimsize);
    CHECK(newsds, FAIL, "SDcreate: Failed to create a new data set 'ExternalDataSet' for external promotion");

    /* initialize data to write out */
    for(i = 0; i < 25; i++)
        idata[i] = i;

    /* Write data to all of data set 'ExternalDataSet' in file 'exttst.hdf' */
    start[0] = start[1] = 0;
    end[0]   = end[1]   = 5;
    status = SDwritedata(newsds, start, NULL, end, (VOIDP) idata);
    CHECK(status, FAIL, "SDwritedata");

    /* Now promote data set 'ExternalDataSet' to an external data set
       in the file 'extfile.hdf' */
    status = SDsetexternalfile(newsds, EXTFILE, 0);
    CHECK(status, FAIL, "SDsetexternalfile");

    for(i = 0; i < 10; i++)
        idata[i] = i * 10;

    /* Now write data to part of newly promoted data set 'ExternalDataSet'
       which is now an external data set */
    start[0] = start[1] = 0;
    end[0]   = 2;
    end[1]   = 5;
    status = SDwritedata(newsds, start, NULL, end, (VOIDP) idata);
    CHECK(status, FAIL, "SDwritedata");

    /* end access to data set 'ExternalDataSet' */
    status = SDendaccess(newsds);
    CHECK(status, FAIL, "SDendaccess");

    /* need to close to flush external info to file 'exttst.hdf' */
    status = SDend(fext);
    CHECK(status, FAIL, "SDend");

    /* Open file 'exttst.hdf' again */
    fext = SDstart(EXTTST, DFACC_RDWR);
    CHECK(fext, FAIL, "SDstart (again)");

    /* Create a "wrapper" data set in file 'exttst.hdf'. i.e. a data set 
       that will point to data in an already existing external file */
    dimsize[0] = 3;
    dimsize[1] = 3;
    newsds2 = SDcreate(fext, "WrapperDataSet", nt, 2, dimsize);
    CHECK(newsds2, FAIL, "SDcreate:Failed to create a new data set('WrapperDataSet') for external wrapping");

    /* Promote the regular data set  to a "wrapper" one by making
       it point to where the real data is in the external file 'extfile.hdf'.
       Note that only a subset of the real data('ExternalDataSet') is pointed to
       by the "wrapper" data set. */
    offset = DFKNTsize(nt) * 2;
    status = SDsetexternalfile(newsds2, EXTFILE, offset);
    CHECK(status, FAIL, "SDsetexternalfile");

    /* now read data back from this "wrapper" data set */
    start[0] = start[1] = 0;
    end[0]   = end[1]   = 3;
    status = SDreaddata(newsds2, start, NULL, end, (VOIDP) idata);
    CHECK(status, FAIL, "SDreaddata");

    /* verify data read back in */
    for(i = 0; i < 8; i++)
      {
        if(idata[i] != (i + 2) * 10) 
          {
            fprintf(stderr, "Bogus val in loc %d in wrapper dset want %d  got %ld\n", 
		    i, (i + 2) * 10, (long)idata[i]);
            num_errs++;
          }
      }

    if(idata[8] != 10) 
      {
        fprintf(stderr, "Bogus val in last loc in wrapper dset want 10  got %ld\n",
		(long)idata[8]);
        num_errs++;
      }

    /* End access to data set "WrapperDataSet" */
    status = SDendaccess(newsds2);
    CHECK(status, FAIL, "SDendaccess");

    /* Close file 'exttst.hdf' */
    status = SDend(fext);
    CHECK(status, FAIL, "SDend");

#endif /* EXTERNAL_TEST */


#ifdef NBIT_TEST

    /*
     * Test the N-Bit storage stuff
     */

    /* Create file 'nbit.hdf' */
    fnbit = SDstart(NBITFILE, DFACC_CREATE);
    CHECK(fnbit, FAIL, "SDstart");

    /* Create data set 'NBitDataSet' in file 'nbit.hdf' */
    nt = DFNT_INT32;
    dimsize[0] = 5;
    dimsize[1] = 5;
    newsds = SDcreate(fnbit, "NBitDataSet", nt, 2, dimsize);
    CHECK(newsds,FAIL,"SDcreate:Failed to create a new data set('NBitDataSet') for n-bit testing");

    /* Initialize data to write out */
    for(i = 0; i < 25; i++)
        idata[i] = i*10;

    /* Promote the data set 'NBitDataSet' to an NBIT data set */
    status = SDsetnbitdataset(newsds,6,7,FALSE,FALSE);
    CHECK(status, FAIL, "SDsetnbitdataset");

    /* Write data to the NBIT data set 'NBitDataSet' */
    start[0] = start[1] = 0;
    end[0]   = end[1]   = 5;
    status = SDwritedata(newsds, start, NULL, end, (VOIDP) idata);
    CHECK(status, FAIL, "SDwritedata");

    /* end access to NBIT data set 'NBitDataSet' */
    status = SDendaccess(newsds);
    CHECK(status, FAIL, "SDendaccess");

    /* need to close to flush n-bit info to file.
       hmm... */
    status = SDend(fnbit);
    CHECK(status, FAIL, "SDend");

    /* read the n-bit data back in */
    fnbit = SDstart(NBITFILE, DFACC_RDWR);
    CHECK(fnbit, FAIL, "SDstart (again)");

    /* Select the NBIT data set back in, assume it is the first one */
    newsds2 = SDselect(fnbit, 0);
    CHECK(newsds2, FAIL,"SDselect:Failed to select a data set for n-bit access");

    /* read data back in from the NBIT data set */
    start[0] = start[1] = 0;
    end[0]   = end[1]   = 5;
    status = SDreaddata(newsds2, start, NULL, end, (VOIDP) rdata);
    CHECK(status, FAIL, "SDreaddata");

    /* verify the data */
    for(i = 0; i < 25; i++)
      {
        if((idata[i]&0x7f) != rdata[i]) 
          {
            fprintf(stderr,"Bogus val in loc %d in n-bit dset want %ld got %ld\n",
		    i, (long)idata[i], (long)rdata[i]);
            num_errs++;
          }
      }

    /* end access to NBIT data set */
    status = SDendaccess(newsds2);
    CHECK(status, FAIL, "SDendaccess");

    /* close file 'nbit.hdf' */
    status = SDend(fnbit);
    CHECK(status, FAIL, "SDend");

#endif /* NBIT_TEST */

#ifdef COMP_TEST
    /*
     * Test the compressed storage routines
     */
    status = test_compression();
    CHECK(status, FAIL, "test_chunk");
    num_errs = num_errs + status;

#endif /* COMP_TEST */

#ifdef CHUNK_TEST
    status = test_chunk();
    CHECK(status, FAIL, "test_chunk");
    num_errs = num_errs + status;
#endif /* CHUNK_TEST */

#ifdef NETCDF_READ_TEST
    status = test_netcdf_reading();
    CHECK(status, FAIL, "test_netcdf_reading");
    num_errs = num_errs + status;
#endif /* NETCDF_READ_TEST */

    /* BMR: Added a test routine dedicated for testing dimensions.  A
       test on SDsetdimscale for an unsigned type was added while I was
       fixing bug #172.  I didn't want to add this test into the already
       very long main program so, instead, I added this routine, which can
       be appended with other dimension tests in the future.  Also, some
       day, the main program can be shortened and some of its dimension-related
       tests can be moved into this test routine - 04/18/01 */
    status = test_dimensions();
    num_errs = num_errs + status;

    /* BMR: Added a test routine dedicated for testing SDcheckempty. 09/17/04 */
    status = test_checkempty();
    num_errs = num_errs + status;

    /* BMR: Added a test routine dedicated for testing SDidtype. 01/21/05 */
    status = test_idtype();
    num_errs = num_errs + status;

    /* BMR: Added a test routine dedicated for testing the behavior of
     * several functions when the SDS has rank=0. 02/4/05 */
    /* BMR: SDcreate fails on Copper when rank=0.  EP decided to remove
     * this test until further study can be made on this feature.
    status = test_rank0();
    num_errs = num_errs + status; */

#ifdef H4_HAVE_LIBSZ
    status = test_szip_compression();  /* defined in tszip.c */
    num_errs = num_errs + status;
#else
   /*  printf("****** SD Szip test skipped *****\n"); */
#endif /* H4_HAVE_LIBSZ */

    status = test_sd();  /* defined in tsd.c */
    num_errs = num_errs + status;
    printf("num_err == %d\n", num_errs);

    exit(num_errs);
    return num_errs;
}

#endif /* HDF */


