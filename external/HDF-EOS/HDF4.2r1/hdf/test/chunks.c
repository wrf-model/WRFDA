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
static char RcsId[] = "@(#)$Revision: 1.18 $";
#endif

/* $Id: chunks.c,v 1.18 1997/11/05 19:39:19 koziol Exp $ */

/*
 * This file tests the Special Chunking Element(HMCxxx) layer of the HDF library.
 *
 * NOTE: No failure conditions are tested yet....
 *
 * Outline of Tests done:
 *    1. First test simple writing of 2-D element with no ghost/partial chunks.
 *       4x4 array of uint8, each chunk 2x2(4 bytes) -> 4 chunks(16 bytes).
 *       Write out 12 bytes to all 4 chunks with only partial writes
 *       to the last 2 chunks..
 *       Write again to last 2 chunks with whole data chunks 
 *
 *    2. Now create a new chunked 2-D element with same parameters
 *       before but write to 2 chunks of element using whole chunks.
 *       The rest of he data should contain fill values.
 *
 *    3. Create a new element but now there will be partial chunks
 *       because chunk lengths are not an even multiple of dimesion lengths.
 *       Set dimension to 4x4 array with 4 chunks where each chunk is 
 *       3x2 = 6 bytes. Real data size is 16 bytes, size with chunks is 
 *       6 bytes x 4 chunks = 24 bytes 
 *
 *    4. Now create 3-D chunked element with no partial chunks.
 *       Write to only part of the chunked element. The rest
 *       should be filled with fill values.
 *       Set dimension to 4x6x8 array with 8 chunks 
 *       where each chunk is 2x3x4= 24 bytes , read data size 192 bytes 
 *       data size with chunks is 192 bytes also.
 *
 *    5. Now create another 3-D chunked element with partial chunks.
 *       Write to part of element, rest is filled with fill value.
 *       Set dimension to 4x6x8 array with 8 chunks , real data 192 bytes
 *       where each chunk is 3x4x5= 60 bytes , 
 *       data size with chunks is 60 bytes x 8 chunks = 480 bytes  
 *
 *    6. Now create 3-D chunked element with no partial chunks.
 *       Write using HMCwriteChunk(). Read data back in first
 *       using Hread() and verify. Then read data back in using
 *       HMCreadChunk() and verify.
 *       Set dimension to 2x3x4 array with 6 chunks 
 *       where each chunk is 1x1x4= 4 bytes , total data size 24 bytes 
 *
 *    7. Now create 3-D chunked element with no partial chunks.
 *       Set dimension to 2x3x4 array with 6 chunks. Number type is uint16 
 *       where each chunk is 1x1x4= 4x2(nt_size) = 8 bytes , 
 *       total data size 48 bytes 
 *
 *    8. Now create 3-D chunked element with no partial chunks.
 *       Set dimension to 2x3x4 array with 6 chunks. Numbertype is float32 
 *       where each chunk is 1x1x4= 4x4(nt_size) = 16 bytes , 
 *       total data size 96 bytes .
 *    
 *    9. Create 4-D element with partial chunks.
 *       Write only half the data out(5,000 bytes)
 *       Set dimension to 10x10x10x10 array -> real data 10,000 bytes .
 *       120 chunks with chunks of 2x3x4x5 = 120 bytes,
 *       data size with chunks is 120 bytes x 120 chunks = 14,400 bytes 
 *
 *    10. *NOT ENABLED*
 *       The rest of the tests here are commented out
 *       They are some extra high order tests to replicate
 *       some test done on EOS-DEM data 
 *       Set dimension to 12000x12000 array with 2,500 chunks 
 *       whith chunk of 240x240 = 57,600 bytes
 *
 *    11. Create a new element but now there will be partial chunks and Compression.
 *       Set dimension to 4x4 array with 4 chunks where each chunk is 3x2 = 6 bytes.
 *       Real data size is 16 bytes, size with chunks is 
 *       6 bytes x 4 chunks = 24 bytes.
 *       The element will be compressed with RLE scheme.
 *
 *    12. Now create 3-D chunked, Compressed element with no partial chunks.
 *       Write using HMCwriteChunk(). Read data back in first
 *       using Hread() and verify. Then read data back in using
 *       HMCreadChunk() and verify.
 *       Set dimension to 2x3x4 array with 6 chunks 
 *       where each chunk is 1x1x4= 4 bytes , total data size 24 bytes 
 *       The element is compressed using RLE scheme.
 *
 *  For all the tests the data is read back in and verified.
 *
 *  Routines tested using User level H-level calls:
 *   Hstartread()         -> HMCPstread()
 *   Hstartwrite()        -> HMCPstwrite()
 *   Hread()              -> HMCPread()
 *   Hwrite()             -> HMCPwrite()
 *   Hseek()              -> HMCPseek()
 *   Hendaccess()         -> HMCPendaccess()
 *   Hinquire()           -> HMCPinquire()
 *   HDget_special_info() -> HMCPinfo()
 *
 * Routines test by direct calling of Chunking routines:
 *   HMCcreate()
 *   HMCsetMaxCache()
 *   HMCwriteChunk()
 *   HMCreadChunk()
 *
 *
 * Author -GeorgeV
 *
 */

#include "tproto.h"
#include "hchunks.h"

#define TESTFILE_NAME "tchunks.hdf"
#define BUFSIZE       12288

/* Some static data buffers */
static uint8  outbuf[BUFSIZE],  /* output data buffer */
               inbuf[BUFSIZE];   /* input data buffer */

/* used to verify data in Test 2. */
static uint8  outbuf_2[16] = {0,0,2,3,0,0,6,7,8,9,0,0,12,13,0,0};

/* used to in Tests 1,2 */
static    uint8      cptr3[4] = {10,11,14,15};
static    uint8      cptr2[4] = {8,9,12,13};
static    uint8      cptr1[4] = {2,3,6,7};
#ifdef QAK
static    uint8      cptr0[4] = {0,1,4,5};
#endif /* QAK */

/* for writing/verifying some chunks used in Test 6*/
static uint8  chunk1[4] = { 0, 1, 2, 3};

static uint8  chunk2[4] = { 10, 11, 12, 13};

static uint8  chunk3[4] = { 20, 21, 22, 23};

static uint8  chunk4[4] = { 100, 101, 102, 103};

static uint8  chunk5[4] = { 110, 111, 112, 113};

static uint8  chunk6[4] = { 120, 121, 122, 123};

/* datay layout of arrays in memory */
/* for comparison in Test 8 */
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

/* for comparison in Test 7 */
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

/* for comparison in Test 6 */
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

/*
 * main entry point to tests the Special Chunking layer...
 *
 * -GeorgeV
 */
void
test_chunks(void)
{
    int32       fid;
    int32       aid1, aid2;
    int32       fileid, length, offset, posn;
    uint16      tag, ref;
    int16       acc_mode, special;
    register int i, j, k;
#ifdef BIG_TEST
    int32       x,y;
    int32      nseek = 0;
#endif
    int32       ret;
    HCHUNK_DEF   chunk[1];       /* Chunk definition, see 'hchunks.h' */
    int32       dims[5];
    int32      fill_val_len = 1;
    uint8      fill_val_u8 = 0;      /* test 6 */
    uint16     fill_val_u16 = 0;     /* test 7 */
    float32    fill_val_f32 = (float32)0.0; /* test 8 */
    uint8      inbuf_u8[2][3][4];
    uint16     inbuf_u16[2][3][4];   /* input data buffer */
    float32    inbuf_f32[2][3][4];   /* input data buffer */
    sp_info_block_t info_block;      /* special info block */
    comp_info  cinfo;
    model_info minfo;
    intn       errors = 0;

    /* intialize out buffer */
    for (i = 0; i < BUFSIZE; i++)
        outbuf[i] = (char) (i % 256);

    /* allocate space for chunk dimensions */
    if ((chunk[0].pdims = (DIM_DEF *)HDmalloc(5*sizeof(DIM_DEF))) == NULL)
      {
          printf("test_chunks: error allocatin space for chunk dims\n");
          errors++;
          goto done;
      }

    /* Create file first */
    MESSAGE(5, printf("Creating a file %s\n", TESTFILE_NAME); );
    fid = Hopen(TESTFILE_NAME, DFACC_CREATE, 0);
    CHECK(fid, FAIL, "Hopen");


    /*
      1.First test simple writing of 2-D element with no ghost/partial chunks.
      Set dimension to 4x4 array with 4 chunks 
      where each chunk is 2x2.
      Write out 12 bytes to all 4 chunks with only partial writes
      to the last 2 chunks..
      Write again to last 2 chunks with whole data chunks 
      */
    chunk[0].num_dims   = 2; /* 2-D */
    chunk[0].chunk_size = 4; /* 2x2 = 4 bytes */
    chunk[0].nt_size    = 1; /* number type size */
    chunk[0].chunk_flag = 0; /* nothing set */
    chunk[0].comp_type = COMP_CODE_NONE; /* nothing set */
    chunk[0].model_type = COMP_MODEL_STDIO; /* nothing set */
    chunk[0].cinfo = NULL; /* nothing set */
    chunk[0].minfo = NULL; /* nothing set */

    chunk[0].pdims[0].dim_length   = 4;
    chunk[0].pdims[0].chunk_length = 2;
    chunk[0].pdims[0].distrib_type = 1;

    chunk[0].pdims[1].dim_length   = 4;
    chunk[0].pdims[1].chunk_length = 2;
    chunk[0].pdims[1].distrib_type = 1;

    MESSAGE(5, printf("Test 1. Create a new element as a 2-D, uint8 chunked element\n"););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      0,(int)chunk[0].pdims[0].dim_length, 
                      0,(int)chunk[0].pdims[0].chunk_length););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      1,(int)chunk[0].pdims[1].dim_length, 
                      1,(int)chunk[0].pdims[1].chunk_length););

    /* Create element     tag, ref,  nlevels, fill_len, fill,  chunk array */
    aid1 = HMCcreate(fid, 1020, 2, 1, fill_val_len, &fill_val_u8, (HCHUNK_DEF *)chunk);
    CHECK(aid1, FAIL, "HMCcreate");

#if 0
    /* write 16 bytes out */
    ret = Hwrite(aid1, 16, outbuf);
    if (ret != 16)
      {
          fprintf(stderr, "ERROR: Hwrite returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Wrote first 16 bytes to 2-D, uint8 chunked element to file\n"); );
    /* end access */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the file\n"););

    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

#endif

    /* write 12 bytes out */
    ret = Hwrite(aid1, 12, outbuf);
    if (ret != 12)
      {
          fprintf(stderr, "ERROR: Hwrite returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Wrote first 12 bytes to 2-D, uint8 chunked element to file\n"); );
    /* end access */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the file\n"););

    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    MESSAGE(5, printf("Open 2-D chunked element again for writing\n"); );

    /* Open file for writing last 2 chunks now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");

    /* start write access   tag,  ref */
    aid1 = Hstartwrite(fid, 1020, 2, 16);
    CHECK(aid1, FAIL, "Hstartwrite");

    /* Try writing to last chunk in the element */
    dims[0] = 1;
    dims[1] = 1;
    ret = HMCwriteChunk(aid1, dims, cptr3);
    CHECK(ret, FAIL, "HMCwriteChunk");

    MESSAGE(5, printf("Wrote to 4th chunk(4of4 chunks) in file\n"););

    /* Try writing to 2nd to last chunk in the element */
    dims[0] = 1;
    dims[1] = 0;
    ret = HMCwriteChunk(aid1, dims, cptr2);
    CHECK(ret, FAIL, "HMCwriteChunk");

    MESSAGE(5, printf("Wrote to 3 chunk (3of4 chunks) in file\n"););
    /* end access */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the file\n"););
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    MESSAGE(5, printf("Open 2-D, uint8 chunked element again for reading \n"); );
    /* Open file for reading now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");

    /* start read access   tag,  ref */
    aid1 = Hstartread(fid, 1020, 2);
    CHECK(aid1, FAIL, "Hstartread");

    /* inquire about element */
    ret = Hinquire(aid1, &fileid, &tag, &ref, &length, &offset, &posn,
                   &acc_mode, &special);

    CHECK(ret, FAIL, "Hinquire");
    if (!special)
      {
          fprintf(stderr, "ERROR: Hinquire does not think element is special line %d\n",
                  __LINE__);
          errors++;
          goto done;
      }
    /* Check values from Hinquire */
    if (ref != 2 || length != 16)
      {
          fprintf(stderr, "ERROR: Hinquire does not return the correct values \n");
          fprintf(stderr, "       tag =%d, ref=%d, length=%d \n",tag,ref,(int)length);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Get/Check special info data\n"); );

    /* get special info about element */
    ret = HDget_special_info(aid1, &info_block);
    CHECK(aid1, FAIL, "HDget_special_info");

    /* check special info */
    if (info_block.ndims != chunk[0].num_dims /* 2-D */)
      {
          fprintf(stderr, "ERROR: HDget_specail_info does not return the correct values \n");
          errors++;
          goto done;
      }

    /* check chunk_lengths */
    if (info_block.cdims != NULL)
      {
          if ((info_block.cdims[0] != 2) || (info_block.cdims[1] != 2))
            {
                fprintf(stderr, "ERROR: HDget_specail_info does not return the correct values \n");
                errors++;
                goto done;
            }

          /* free allocated space by routine */
          HDfree(info_block.cdims);
      }
    else
      {
          fprintf(stderr, "ERROR: HDget_specail_info does not return the correct values \n");
          errors++;
          goto done;
      }


    /* read back in buffer  */
    ret = Hread(aid1, 16, inbuf);
    VERIFY(ret, 16, "Hread");
    if (ret != 16)
      {
          fprintf(stderr, "ERROR: Hread returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    /* verify the data */
    MESSAGE(5, printf("Verifying 16 bytes of data\n"); );
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != outbuf[i])
            {
                printf("Wrong data at %d, out %d in %d\n", i, outbuf[i], inbuf[i]);
                errors++;
            }

      }
    if (errors)
        goto done;

    /* end access and close file */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the file\n"););
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    /* 
       2. Now create a new chunked 2-D element with same parameters
       before but write to 2 chunks of element using whole chunks.
       The rest of he data should contain fill values.
       */

    /* Open file for writing again */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");

    MESSAGE(5, printf("Test 2. Create another new element as a 2-D, uint8 chunked element\n"););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      0,(int)chunk[0].pdims[0].dim_length, 
                      0,(int)chunk[0].pdims[0].chunk_length););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      1,(int)chunk[0].pdims[1].dim_length, 
                      1,(int)chunk[0].pdims[1].chunk_length););

    /* Create element     tag, ref,  nlevels, fill_len, fill, chunk array */
    aid1 = HMCcreate(fid, 1020, 3, 1, fill_val_len, &fill_val_u8, (HCHUNK_DEF *)chunk);
    CHECK(aid1, FAIL, "HMCcreate");

    /* Try writing to 2 chunk in the element */
    dims[0] = 1;
    dims[1] = 0;
    ret = HMCwriteChunk(aid1, dims, cptr2);
    CHECK(ret, FAIL, "HMCwriteChunk");

    MESSAGE(5, printf("Wrote to 3 chunk (3of4) in file\n"); );

    /* Try writing to 1 chunk in the element */
    dims[0] = 0;
    dims[1] = 1;
    ret = HMCwriteChunk(aid1, dims, cptr1);
    CHECK(ret, FAIL, "HMCwriteChunk");

    MESSAGE(5, printf("Wrote to 2nd chunk (2of4 chunks) in file\n"); );

    /* end access and close file */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the file\n"); );
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    /* Now reopen and read back 16 bytes */
    MESSAGE(5, printf("Open 2-D, uint8 chunked element again for reading \n"); );
    /* Open file for reading now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");

    /* start read access   tag,  ref */
    aid1 = Hstartread(fid, 1020, 3);
    CHECK(aid1, FAIL, "Hstartread");

    /* inquire about element */
    ret = Hinquire(aid1, &fileid, &tag, &ref, &length, &offset, &posn,
                   &acc_mode, &special);

    CHECK(ret, FAIL, "Hinquire");
    if (!special)
      {
          fprintf(stderr, "ERROR: Hinquire does not think element is special line %d\n",
                  __LINE__);
          errors++;
          goto done;
      }

    /* Check values from Hinquire */
    if ( ref != 3 || length != 16)
      {
          fprintf(stderr, "ERROR: Hinquire does not return the correct values \n");
          fprintf(stderr, "       tag =%d, ref=%d, length=%d \n",tag,ref,(int)length);
          errors++;
          goto done;
      }

    /* read back in buffer  */
    ret = Hread(aid1, 16, inbuf);
    VERIFY(ret, 16, "Hread");
    if (ret != 16)
      {
          fprintf(stderr, "ERROR: Hread returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    /* verify the data, check against 'outbuf_2'  
       some of the data should be filled with fill value of 0 */
    MESSAGE(5, printf("Verifying 16 bytes of data, there will be some fill values\n"); );
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != outbuf_2[i])
            {
                printf("Wrong data at %d, out %d in %d\n", i, outbuf_2[i], inbuf[i]);
                errors++;
            }
      }
    if (errors)
        goto done;

    /* end access and close file */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the file\n"); );
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");


    /* 
       3. Create a new element but now there will be partial chunks
       because chunk lengths are not an even multiple of dimesion lengths.
       Set dimension to 4x4 array with 4 chunks where each chunk is 3x2 = 6 bytes.
       Real data size is 16 bytes, size with chunks is 
       6 bytes x 4 chunks = 24 bytes 
       */
    chunk[0].num_dims   = 2;
    chunk[0].chunk_size = 6; /* 3x2 = 6 bytes */
    chunk[0].nt_size    = 1; /* number type size */
    chunk[0].chunk_flag = 0; /* nothing set */
    chunk[0].comp_type = COMP_CODE_NONE; /* nothing set */
    chunk[0].model_type = COMP_MODEL_STDIO; /* nothing set */
    chunk[0].cinfo = NULL; /* nothing set */
    chunk[0].minfo = NULL; /* nothing set */

    chunk[0].pdims[0].dim_length   = 4;
    chunk[0].pdims[0].chunk_length = 3;  /* made this 3 */
    chunk[0].pdims[0].distrib_type = 1;

    chunk[0].pdims[1].dim_length   = 4;
    chunk[0].pdims[1].chunk_length = 2;
    chunk[0].pdims[1].distrib_type = 1;

    /* Open file for writing odd size chunks now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");

    MESSAGE(5, printf("Test 3. Create another new element as a 2-D, uint8 chunked element\n"););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      0,(int)chunk[0].pdims[0].dim_length, 
                      0,(int)chunk[0].pdims[0].chunk_length););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      1,(int)chunk[0].pdims[1].dim_length, 
                      1,(int)chunk[0].pdims[1].chunk_length););

    /* Create element     tag, ref,  nlevels, fill_len, fill, chunk array */
    aid1 = HMCcreate(fid, 1020, 5, 1, fill_val_len, &fill_val_u8, (HCHUNK_DEF *)chunk);
    CHECK(aid1, FAIL, "HMCcreate");

    /* write 16 bytes out */
    ret = Hwrite(aid1, 16, outbuf);
    VERIFY(ret, 16, "Hwrite");
    if (ret != 16)
      {
          fprintf(stderr, "ERROR: Hwrite returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Wrote to 4of4 chunks (16 bytes) in file\n"););
    /* end access */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the file\n"); );
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    MESSAGE(5, printf("Open 2-D, uint8 chunked element again for reading\n"); );

    /* Open file for reading now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");

    /* start read access   tag,  ref */
    aid1 = Hstartread(fid, 1020, 5);
    CHECK(aid1, FAIL, "Hstartread");

    /* inquire about element */
    ret = Hinquire(aid1, &fileid, &tag, &ref, &length, &offset, &posn,
                   &acc_mode, &special);

    CHECK(ret, FAIL, "Hinquire");
    if (!special)
      {
          fprintf(stderr, "ERROR: Hinquire does not think element is special line %d\n",
                  __LINE__);
          errors++;
          goto done;
      }

    /* Check values from Hinquire */
    if ( ref != 5 || length != 16)
      {
          fprintf(stderr, "ERROR: Hinquire does not return the correct values \n");
          fprintf(stderr, "       tag =%d, ref=%d, length=%d \n",tag,ref,(int)length);
          errors++;
          goto done;
      }

    /* read back in buffer  */
    ret = Hread(aid1, 16, inbuf);
    VERIFY(ret, 16, "Hread");
    if (ret != 16)
      {
          fprintf(stderr, "ERROR: Hread returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    /* verify the data */
    MESSAGE(5, printf("Verifying 16 bytes of data\n"); );
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != outbuf[i])
            {
                printf("Wrong data at %d, out %d in %d\n", i, outbuf[i], inbuf[i]);
                errors++;
            }
          inbuf[i] = '\0';
      }
    if (errors)
        goto done;

    /* end access and close file */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the file\n"); );
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");


    /* 
       4. Now create 3-D chunked element with no partial chunks.
       Write to only part of the chunked element. The rest
       should be filled with fill values.
       Set dimension to 4x6x8 array with 8 chunks 
       where each chunk is 2x3x4= 24 bytes , total data size 192 bytes 
       */
    chunk[0].num_dims   = 3;
    chunk[0].chunk_size = 24; /* 2x3x4 bytes */
    chunk[0].nt_size    = 1; /* number type size */
    chunk[0].chunk_flag = 0; /* nothing set */
    chunk[0].comp_type = COMP_CODE_NONE; /* nothing set */
    chunk[0].model_type = COMP_MODEL_STDIO; /* nothing set */
    chunk[0].cinfo = NULL; /* nothing set */
    chunk[0].minfo = NULL; /* nothing set */

    chunk[0].pdims[0].dim_length   = 4;
    chunk[0].pdims[0].chunk_length = 2;  
    chunk[0].pdims[0].distrib_type = 1;

    chunk[0].pdims[1].dim_length   = 6;
    chunk[0].pdims[1].chunk_length = 3;
    chunk[0].pdims[1].distrib_type = 1;

    chunk[0].pdims[2].dim_length   = 8;
    chunk[0].pdims[2].chunk_length = 4;
    chunk[0].pdims[2].distrib_type = 1;

    /* Open file for writing last odd size chunks now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");
    MESSAGE(5, printf("Test 4. Create another new element as a 3-D, uint8 chunked element(192 bytes)\n"););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      0,(int)chunk[0].pdims[0].dim_length, 
                      0,(int)chunk[0].pdims[0].chunk_length););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      1,(int)chunk[0].pdims[1].dim_length, 
                      1,(int)chunk[0].pdims[1].chunk_length););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      2,(int)chunk[0].pdims[2].dim_length, 
                      2,(int)chunk[0].pdims[2].chunk_length););

    /* Create element     tag, ref,  nlevels, fill_len, fill, chunk array */
    aid1 = HMCcreate(fid, 1020, 6, 1, fill_val_len, &fill_val_u8, (HCHUNK_DEF *)chunk);
    CHECK(aid1, FAIL, "HMCcreate");

    /* write only 112 bytes out */
    ret = Hwrite(aid1, 112, outbuf);
    VERIFY(ret, 112, "Hwrite");
    if (ret != 112)
      {
          fprintf(stderr, "ERROR: Hwrite returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Wrote 112of192 bytes to 3-D, uint8 chunked element \n"); );
    /* end access */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the files\n");
            );
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    MESSAGE(5, printf("Open 3-D, uint8 chunked element again for reading\n"); );
    /* Open file for reading now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");

    /* start read access   tag,  ref */
    aid1 = Hstartread(fid, 1020, 6);
    CHECK(aid1, FAIL, "Hstartread");

    /* inquire about element */
    ret = Hinquire(aid1, &fileid, &tag, &ref, &length, &offset, &posn,
                   &acc_mode, &special);

    CHECK(ret, FAIL, "Hinquire");
    if (!special)
      {
          fprintf(stderr, "ERROR: Hinquire does not think element is special line %d\n",
                  __LINE__);
          errors++;
          goto done;
      }

    /* Check values from Hinquire */
    if ( ref != 6 || length != 192)
      {
          fprintf(stderr, "ERROR: Hinquire does not return the correct values \n");
          fprintf(stderr, "       tag =%d, ref=%d, length=%d \n",tag,ref,(int)length);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Get/Check special info data\n"); );

    /* get special info about element */
    ret = HDget_special_info(aid1, &info_block);
    CHECK(aid1, FAIL, "HDget_special_info");

    /* check special info */
    if (info_block.ndims != chunk[0].num_dims /* 2-D */)
      {
          fprintf(stderr, "ERROR: HDget_specail_info does not return the correct values \n");
          errors++;
          goto done;
      }

    /* check chunk_lengths */
    if (info_block.cdims != NULL)
      {
          if ((info_block.cdims[0] != 2) 
              || (info_block.cdims[1] != 3)
              || (info_block.cdims[2] != 4))
            {
                fprintf(stderr, "ERROR: HDget_specail_info does not return the correct values \n");
                errors++;
                goto done;
            }

          /* free allocated space by routine */
          HDfree(info_block.cdims);
      }
    else
      {
          fprintf(stderr, "ERROR: HDget_specail_info does not return the correct values \n");
          errors++;
          goto done;
      }


    /* read back in buffer  */
    ret = Hread(aid1, 112, inbuf);
    VERIFY(ret, 112, "Hread");
    if (ret != 112)
      {
          fprintf(stderr, "ERROR: Hread returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    /* verify the data */
    MESSAGE(5, printf("Verifying first 112 bytes data\n"); );
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != outbuf[i])
            {
                printf("Wrong data at %d, out %d in %d\n", i, outbuf[i], inbuf[i]);
                errors++;
            }
          inbuf[i] = '\0';
      }
    if (errors)
        goto done;

    /* end access and close file */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the file\n"););
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");


    /* 
       5. Now create another 3-D chunked element with partial chunks.
       Write to part of element, rest is filled with fill value.
       Set dimension to 4x6x8 array with 8 chunks , real data 192 bytes
       where each chunk is 3x4x5= 60 bytes , 
       data size with chunks is 60 bytes x 8 chunks = 480 bytes 
       */
    chunk[0].num_dims   = 3;
    chunk[0].chunk_size = 60; /* 3x4x5 = 60 bytes */
    chunk[0].nt_size    = 1; /* number type size */
    chunk[0].chunk_flag = 0; /* nothing set */
    chunk[0].comp_type = COMP_CODE_NONE; /* nothing set */
    chunk[0].model_type = COMP_MODEL_STDIO; /* nothing set */
    chunk[0].cinfo = NULL; /* nothing set */
    chunk[0].minfo = NULL; /* nothing set */

    chunk[0].pdims[0].dim_length   = 4;
    chunk[0].pdims[0].chunk_length = 3;  
    chunk[0].pdims[0].distrib_type = 1;

    chunk[0].pdims[1].dim_length   = 6;
    chunk[0].pdims[1].chunk_length = 4;
    chunk[0].pdims[1].distrib_type = 1;

    chunk[0].pdims[2].dim_length   = 8;
    chunk[0].pdims[2].chunk_length = 5;
    chunk[0].pdims[2].distrib_type = 1;

    /* set fill value to 1 */
    fill_val_u8 = 1;
    fill_val_len = 1;

    /* Open file for writing last odd size chunks now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");

    MESSAGE(5, printf("Test 5. Create another new element as a 3-D, uint8 chunked element(192bytes)\n"););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      0,(int)chunk[0].pdims[0].dim_length, 
                      0,(int)chunk[0].pdims[0].chunk_length););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      1,(int)chunk[0].pdims[1].dim_length, 
                      1,(int)chunk[0].pdims[1].chunk_length););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      2,(int)chunk[0].pdims[2].dim_length, 
                      2,(int)chunk[0].pdims[2].chunk_length););

    /* Create element     tag, ref,  nlevels, fill_len, fill, chunk array */
    aid1 = HMCcreate(fid, 1020, 7, 1, fill_val_len, &fill_val_u8, (HCHUNK_DEF *)chunk);
    CHECK(aid1, FAIL, "HMCcreate");

    /* Set max chunks to cache to 3x4 = 12 chunks */
    MESSAGE(5, printf("Set max # of chunks to cache for chunked element to 12 \n"); );
    ret = HMCsetMaxcache(aid1, 12, 0);
    VERIFY(ret, 12, "HMCsetMaxcache");
    
    /* write 112 bytes out */
    ret = Hwrite(aid1, 112, outbuf);
    VERIFY(ret, 112, "Hwrite");
    if (ret != 112)
      {
          fprintf(stderr, "ERROR: Hwrite returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Wrote 112of192 bytes to 3-D, uint8 chunked element \n"); );

    /* end access */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the file\n"););
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    MESSAGE(5, printf("Open 3-D, uint8 chunked element again for reading\n"); );
    /* Open file for reading now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");

    /* start read access   tag,  ref */
    aid1 = Hstartread(fid, 1020, 7);
    CHECK(aid1, FAIL, "Hstartread");

    /* inquire about element */
    ret = Hinquire(aid1, &fileid, &tag, &ref, &length, &offset, &posn,
                   &acc_mode, &special);

    CHECK(ret, FAIL, "Hinquire");
    if (!special)
      {
          fprintf(stderr, "ERROR: Hinquire does not think element is special line %d\n",
                  __LINE__);
          errors++;
          goto done;
      }

    /* Check values from Hinquire */
    if ( ref != 7 || length != 192)
      {
          fprintf(stderr, "ERROR: Hinquire does not return the correct values \n");
          fprintf(stderr, "       tag =%d, ref=%d, length=%d \n",tag,ref,(int)length);
          errors++;
          goto done;
      }

    /* read back in buffer  */
    ret = Hread(aid1, 112, inbuf);
    VERIFY(ret, 112, "Hread");
    if (ret != 112)
      {
          fprintf(stderr, "ERROR: Hread returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    /* verify the data */
    MESSAGE(5, printf("Verifying data\n"););
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != outbuf[i])
            {
                printf("Wrong data at %d, out %d in %d\n", i, outbuf[i], inbuf[i]);
                errors++;
            }
          inbuf[i] = '\0';
      }
    if (errors)
        goto done;

    /* read back 20 bytes now, they should be filled with fill values */
    MESSAGE(5, printf("reading some more data\n"););
    ret = Hread(aid1, 20, inbuf);
    VERIFY(ret, 20, "Hread");
    if (ret != 20)
      {
          fprintf(stderr, "ERROR: Hread returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    /* verify the data */
    MESSAGE(5, printf("Verifying data, should be full of fill values\n"); );
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != fill_val_u8)
            {
                printf("Wrong data at %d, out %d in %d\n", i, fill_val_u8, inbuf[i]);
                errors++;
            }
      }
    if (errors)
        goto done;

    /* end access and close file */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the file\n"); );
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");


    /* 
       6. Now create 3-D chunked element with no partial chunks.
       Write using HMCwriteChunk(). Read data back in first
       using Hread() and verify. Then read data back in using
       HMCreadChunk() and verify.
       Set dimension to 2x3x4 array with 6 chunks 
       where each chunk is 1x1x4= 4 bytes , total data size 24 bytes 
       */
    chunk[0].num_dims   = 3;
    chunk[0].chunk_size = 4; /* 1x1x4 bytes */
    chunk[0].nt_size    = 1; /* number type size */
    chunk[0].chunk_flag = 0; /* nothing set */
    chunk[0].comp_type = COMP_CODE_NONE; /* nothing set */
    chunk[0].model_type = COMP_MODEL_STDIO; /* nothing set */
    chunk[0].cinfo = NULL; /* nothing set */
    chunk[0].minfo = NULL; /* nothing set */
#if 0
    chunk[0].chunk_flag = SPECIAL_COMP; /* compression */
    chunk[0].comp_type  = COMP_CODE_RLE; /* RLE */
    chunk[0].model_type = COMP_MODEL_STDIO; /* STDIO */
    chunk[0].cinfo = &cinfo; /* nothing set */
    chunk[0].minfo = &minfo; /* nothing set */
#endif

    chunk[0].pdims[0].dim_length   = 2;
    chunk[0].pdims[0].chunk_length = 1;  
    chunk[0].pdims[0].distrib_type = 1;

    chunk[0].pdims[1].dim_length   = 3;
    chunk[0].pdims[1].chunk_length = 1;
    chunk[0].pdims[1].distrib_type = 1;

    chunk[0].pdims[2].dim_length   = 4;
    chunk[0].pdims[2].chunk_length = 4;
    chunk[0].pdims[2].distrib_type = 0; /* NONE */

    /* set fill value to 1 */
    fill_val_u8 = 1;
    fill_val_len = 1;

    /* Open file for writing last odd size chunks now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");
    MESSAGE(5, printf("Test 6. Create another new element as a 3-D, uint8 chunked element(192 bytes)\n"););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      0,(int)chunk[0].pdims[0].dim_length, 
                      0,(int)chunk[0].pdims[0].chunk_length););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      1,(int)chunk[0].pdims[1].dim_length, 
                      1,(int)chunk[0].pdims[1].chunk_length););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      2,(int)chunk[0].pdims[2].dim_length, 
                      2,(int)chunk[0].pdims[2].chunk_length););

    /* Create element     tag, ref,  nlevels, fill_len, fill, chunk array */
    aid1 = HMCcreate(fid, 1020, 12, 1, fill_val_len, &fill_val_u8, (HCHUNK_DEF *)chunk);
    CHECK(aid1, FAIL, "HMCcreate");

    /* Create element     tag, ref,  nlevels, fill_len, fill, chunk array */
    aid2 = HMCcreate(fid, 1020, 18, 1, fill_val_len, &fill_val_u8, (HCHUNK_DEF *)chunk);
    CHECK(aid1, FAIL, "HMCcreate");

    /* write 24 bytes out */
    ret = Hwrite(aid2, 24, u8_data);
    if (ret != 24)
      {
          fprintf(stderr, "ERROR: Hwrite returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    /* write data out as chunks */
    MESSAGE(5, printf("Writing to 3-D, uint8 chunked element using HMCwriteChunk\n"); );

    /* Write data use SDwriteChunk */
    dims[0] = 0;
    dims[1] = 0;
    dims[2] = 0;
    ret = HMCwriteChunk(aid1, dims, chunk1);
    CHECK(ret, FAIL, "HMCwriteChunk");

    dims[0] = 1;
    dims[1] = 0;
    dims[2] = 0;
    ret = HMCwriteChunk(aid1, dims, chunk4);
    CHECK(ret, FAIL, "HMCwriteChunk");

    dims[0] = 0;
    dims[1] = 1;
    dims[2] = 0;
    ret = HMCwriteChunk(aid1, dims, chunk2);
    CHECK(ret, FAIL, "HMCwriteChunk");

    dims[0] = 1;
    dims[1] = 1;
    dims[2] = 0;
    ret = HMCwriteChunk(aid1, dims, chunk5);
    CHECK(ret, FAIL, "HMCwriteChunk");

    dims[0] = 0;
    dims[1] = 2;
    dims[2] = 0;
    ret = HMCwriteChunk(aid1, dims, chunk3);
    CHECK(ret, FAIL, "HMCwriteChunk");

    dims[0] = 1;
    dims[1] = 2;
    dims[2] = 0;
    ret = HMCwriteChunk(aid1, dims, chunk6);
    CHECK(ret, FAIL, "HMCwriteChunk");

    /* end access */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    /* end access */
    ret = Hendaccess(aid2);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the files\n"););
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    MESSAGE(5, printf("Open 3-D, uint8 chunked element again for reading\n"); );
    /* Open file for reading now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");

    /* start read access   tag,  ref */
    aid1 = Hstartread(fid, 1020, 12);
    CHECK(aid1, FAIL, "Hstartread");

    /* start read access   tag,  ref */
    aid2 = Hstartread(fid, 1020, 18);
    CHECK(aid1, FAIL, "Hstartread");

    /* inquire about element */
    ret = Hinquire(aid1, &fileid, &tag, &ref, &length, &offset, &posn,
                   &acc_mode, &special);

    CHECK(ret, FAIL, "Hinquire");
    if (!special)
      {
          fprintf(stderr, "ERROR: Hinquire does not think element is special line %d\n",
                  __LINE__);
          errors++;
          goto done;
      }

    /* Check values from Hinquire */
    if ( ref != 12 || length != 24)
      {
          fprintf(stderr, "ERROR: Hinquire does not return the correct values \n");
          fprintf(stderr, "       tag =%d, ref=%d, length=%d \n",tag,ref,(int)length);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Get/Check special info data\n"); );

    /* get special info about element */
    ret = HDget_special_info(aid1, &info_block);
    CHECK(aid1, FAIL, "HDget_special_info");

    /* check special info */
    if (info_block.ndims != chunk[0].num_dims /* 2-D */)
      {
          fprintf(stderr, "ERROR: HDget_specail_info does not return the correct values \n");
          errors++;
          goto done;
      }

    /* check chunk_lengths */
    if (info_block.cdims != NULL)
      {
          if ((info_block.cdims[0] != 1) 
              || (info_block.cdims[1] != 1)
              || (info_block.cdims[2] != 4))
            {
                fprintf(stderr, "ERROR: HDget_specail_info does not return the correct values \n");
                errors++;
                goto done;
            }

          /* free allocated space by routine */
          HDfree(info_block.cdims);
      }
    else
      {
          fprintf(stderr, "ERROR: HDget_specail_info does not return the correct values \n");
          errors++;
          goto done;
      }


    /* read back in buffer  */
    ret = Hread(aid1, 24, inbuf_u8);
    VERIFY(ret, 24, "Hread");
    if (ret != 24)
      {
          fprintf(stderr, "ERROR: Hread returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    /* verify the data */
    MESSAGE(5, printf("Verifying 24 bytes data from Hread\n"); );
    for (i = 0; i < 2; i++)
      {
        for (j = 0; j < 3; j++)
          {
            for (k = 0; k < 4; k++)
              {
                  if (inbuf_u8[i][j][k] != u8_data[i][j][k])
                    {
                        printf("Wrong data at inbuf_u8[%d][%d][%d], out %d in %d\n", 
                               i,j,k, u8_data[i][j][k], inbuf_u8[i][j][k]);
                        errors++;
                    }
              }
          }
      }

    if (errors)
        goto done;

    MESSAGE(5, printf("Verifying 24 bytes using HMCreadChunk\n"); );
    /* read data back as chunks */
    dims[0] = 0;
    dims[1] = 0;
    dims[2] = 0;
    ret = HMCreadChunk(aid2, dims, inbuf);
    CHECK(ret, FAIL, "HMCreadChunk");
    if (ret != 4)
      {
          fprintf(stderr, "ERROR: HMCreadChunk returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Verifying chunk1 4 bytes data\n"); );
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != chunk1[i])
            {
                printf("Wrong data at %d, out %d in %d\n", i, chunk1[i], inbuf[i]);
                errors++;
            }
      }

    dims[0] = 0;
    dims[1] = 1;
    dims[2] = 0;
    ret = HMCreadChunk(aid2, dims, inbuf);
    CHECK(ret, FAIL, "HMCreadChunk");
    if (ret != 4)
      {
          fprintf(stderr, "ERROR: HMCreadChunk returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Verifying chunk2 4 bytes data\n"); );
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != chunk2[i])
            {
                printf("Wrong data at %d, out %d in %d\n", i, chunk2[i], inbuf[i]);
                errors++;
            }
      }

    dims[0] = 0;
    dims[1] = 2;
    dims[2] = 0;
    ret = HMCreadChunk(aid2, dims, inbuf);
    CHECK(ret, FAIL, "HMCreadChunk");
    if (ret != 4)
      {
          fprintf(stderr, "ERROR: HMCreadChunk returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Verifying chunk3 4 bytes data\n"); );
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != chunk3[i])
            {
                printf("Wrong data at %d, out %d in %d\n", i, chunk3[i], inbuf[i]);
                errors++;
            }
      }

    dims[0] = 1;
    dims[1] = 0;
    dims[2] = 0;
    ret = HMCreadChunk(aid2, dims, inbuf);
    CHECK(ret, FAIL, "HMCreadChunk");
    if (ret != 4)
      {
          fprintf(stderr, "ERROR: HMCreadChunk returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Verifying chunk4 4 bytes data\n"); );
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != chunk4[i])
            {
                printf("Wrong data at %d, out %d in %d\n", i, chunk4[i], inbuf[i]);
                errors++;
            }
      }

    dims[0] = 1;
    dims[1] = 1;
    dims[2] = 0;
    ret = HMCreadChunk(aid2, dims, inbuf);
    CHECK(ret, FAIL, "HMCreadChunk");
    if (ret != 4)
      {
          fprintf(stderr, "ERROR: HMCreadChunk returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Verifying chunk5 4 bytes data\n"); );
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != chunk5[i])
            {
                printf("Wrong data at %d, out %d in %d\n", i, chunk5[i], inbuf[i]);
                errors++;
            }
      }

    dims[0] = 1;
    dims[1] = 2;
    dims[2] = 0;
    ret = HMCreadChunk(aid2, dims, inbuf);
    CHECK(ret, FAIL, "HMCreadChunk");
    if (ret != 4)
      {
          fprintf(stderr, "ERROR: HMCreadChunk returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Verifying chunk6 4 bytes data\n"); );
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != chunk6[i])
            {
                printf("Wrong data at %d, out %d in %d\n", i, chunk6[i], inbuf[i]);
                errors++;
            }
      }

    /* end access and close file */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    /* end access and close file */
    ret = Hendaccess(aid2);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the file\n"););
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");


    /* The following tests will work if Number type conversion
       is done on the ouput data, punt for now since 'hdftest'
       tests these same tests with number type conversion  */
#if  !(defined(UNICOS) || defined(_UNICOS) || defined(_CRAYMPP))

    /* 
       7. Now create 3-D chunked element with no partial chunks.
       Set dimension to 2x3x4 array with 6 chunks. Number type is uint16 
       where each chunk is 1x1x4= 4x2(nt_size) = 8 bytes , 
       total data size 48 bytes 
       */
    chunk[0].num_dims   = 3;
    chunk[0].chunk_size = 4; /* 1x1x4 bytes, logical  */
    chunk[0].nt_size    = 2; /* number type size, uint16 */
    chunk[0].chunk_flag = 0; /* nothing set */
    chunk[0].comp_type = COMP_CODE_NONE; /* nothing set */
    chunk[0].model_type = COMP_MODEL_STDIO; /* nothing set */
    chunk[0].cinfo = NULL; /* nothing set */
    chunk[0].minfo = NULL; /* nothing set */
#if 0
    chunk[0].chunk_flag = SPECIAL_COMP; /* compression */
    chunk[0].comp_type  = COMP_CODE_RLE; /* RLE */
    chunk[0].model_type = COMP_MODEL_STDIO; /* STDIO */
    chunk[0].cinfo = &cinfo; /* nothing set */
    chunk[0].minfo = &minfo; /* nothing set */
#endif

    chunk[0].pdims[0].dim_length   = 2;
    chunk[0].pdims[0].chunk_length = 1;  
    chunk[0].pdims[0].distrib_type = 1;

    chunk[0].pdims[1].dim_length   = 3;
    chunk[0].pdims[1].chunk_length = 1;
    chunk[0].pdims[1].distrib_type = 1;

    chunk[0].pdims[2].dim_length   = 4;
    chunk[0].pdims[2].chunk_length = 4;
    chunk[0].pdims[2].distrib_type = 0; /* NONE */

    fill_val_len = 2;
    fill_val_u16 = 0;

    /* Open file for writing last odd size chunks now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");
    MESSAGE(5, printf("Test 7. Create another new element as a 3-D, uint16 chunked element(48 bytes)\n"););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      0,(int)chunk[0].pdims[0].dim_length, 
                      0,(int)chunk[0].pdims[0].chunk_length););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      1,(int)chunk[0].pdims[1].dim_length, 
                      1,(int)chunk[0].pdims[1].chunk_length););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      2,(int)chunk[0].pdims[2].dim_length, 
                      2,(int)chunk[0].pdims[2].chunk_length););

    /* Create element     tag, ref,  nlevels, fill_len, fill, chunk array */
    aid1 = HMCcreate(fid, 1020, 14, 1, fill_val_len, &fill_val_u16, (HCHUNK_DEF *)chunk);
    CHECK(aid1, FAIL, "HMCcreate");

    /* write 48 bytes out */
    ret = Hwrite(aid1, 48, u16_data);
    VERIFY(ret, 48, "Hwrite");
    if (ret != 48)
      {
          fprintf(stderr, "ERROR: Hwrite returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Wrote 48 bytes to 3-D, uint16 chunked element \n"); );

    /* end access */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the files\n");
            );
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    MESSAGE(5, printf("Open 3-D, uint16 chunked element again for reading\n"); );
    /* Open file for reading now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");

    /* start read access   tag,  ref */
    aid1 = Hstartread(fid, 1020, 14);
    CHECK(aid1, FAIL, "Hstartread");

    /* read back in buffer  */
    ret = Hread(aid1, 48, inbuf_u16);
    VERIFY(ret, 48, "Hread");
    if (ret != 48)
      {
          fprintf(stderr, "ERROR: Hread returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    /* verify the data */
    MESSAGE(5, printf("Verifying 48 bytes data\n"); );
    for (i = 0; i < 2; i++)
      {
        for (j = 0; j < 3; j++)
          {
            for (k = 0; k < 4; k++)
              {
                  if (inbuf_u16[i][j][k] != u16_data[i][j][k])
                    {
                        printf("Wrong data at inbuf_u16[%d][%d][%d], out %d in %d\n", 
                               i,j,k, u16_data[i][j][k], inbuf_u16[i][j][k]);
                        errors++;
                    }
              }
          }
      }

    if (errors)
        goto done;

    /* end access and close file */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the file\n"););
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");


    /* 
       8. Now create 3-D chunked element with no partial chunks.
       Set dimension to 2x3x4 array with 6 chunks. Numbertype is float32 
       where each chunk is 1x1x4= 4x4(nt_size) = 16 bytes , 
       total data size 96 bytes 
       */
    chunk[0].num_dims   = 3;
    chunk[0].chunk_size = 4; /* 1x1x4 bytes, logical */
    chunk[0].nt_size    = 4; /* number type size, float32 */
    chunk[0].chunk_flag = 0; /* nothing set */
    chunk[0].comp_type = COMP_CODE_NONE; /* nothing set */
    chunk[0].model_type = COMP_MODEL_STDIO; /* nothing set */
    chunk[0].cinfo = NULL; /* nothing set */
    chunk[0].minfo = NULL; /* nothing set */
#if 0
    chunk[0].chunk_flag = SPECIAL_COMP; /* compression */
    chunk[0].comp_type  = COMP_CODE_RLE; /* RLE */
    chunk[0].model_type = COMP_MODEL_STDIO; /* STDIO */
    chunk[0].cinfo = &cinfo; /* nothing set */
    chunk[0].minfo = &minfo; /* nothing set */
#endif

    chunk[0].pdims[0].dim_length   = 2;
    chunk[0].pdims[0].chunk_length = 1;  
    chunk[0].pdims[0].distrib_type = 1;

    chunk[0].pdims[1].dim_length   = 3;
    chunk[0].pdims[1].chunk_length = 1;
    chunk[0].pdims[1].distrib_type = 1;

    chunk[0].pdims[2].dim_length   = 4;
    chunk[0].pdims[2].chunk_length = 4;
    chunk[0].pdims[2].distrib_type = 0; /* NONE */

    fill_val_len = 4;
    fill_val_f32 = (float32)0.0;

    /* Open file for writing last odd size chunks now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");

    MESSAGE(5, printf("Test 8. Create another new element as a 3-D, float32 chunked element(96 bytes)\n"););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      0,(int)chunk[0].pdims[0].dim_length, 
                      0,(int)chunk[0].pdims[0].chunk_length););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      1,(int)chunk[0].pdims[1].dim_length, 
                      1,(int)chunk[0].pdims[1].chunk_length););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      2,(int)chunk[0].pdims[2].dim_length, 
                      2,(int)chunk[0].pdims[2].chunk_length););

    /* Create element     tag, ref,  nlevels, fill_len, fill, chunk array */
    aid1 = HMCcreate(fid, 1020, 15, 1, fill_val_len, &fill_val_f32, (HCHUNK_DEF *)chunk);
    CHECK(aid1, FAIL, "HMCcreate");

    if (aid1 == FAIL)
      {
        HEprint(stderr,0);
        errors++;
        goto done;
      }

    /* write 96 bytes out */
    ret = Hwrite(aid1, 96, f32_data);
    VERIFY(ret, 96, "Hwrite");
    if (ret != 96)
      {
          fprintf(stderr, "ERROR: Hwrite returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Wrote 96 bytes to 3-D, float32 chunked element \n"); );

    /* end access */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the files\n");
            );
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    MESSAGE(5, printf("Open 3-D, float32 chunked element again for reading\n"); );
    /* Open file for reading now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");

    /* start read access   tag,  ref */
    aid1 = Hstartread(fid, 1020, 15);
    CHECK(aid1, FAIL, "Hstartread");

    /* read back in buffer  */
    ret = Hread(aid1, 96, inbuf_f32);
    VERIFY(ret, 96, "Hread");
    if (ret != 96)
      {
          fprintf(stderr, "ERROR: Hread returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    /* verify the data */
    MESSAGE(5, printf("Verifying 96 bytes data\n"); );
    for (i = 0; i < 2; i++)
      {
        for (j = 0; j < 3; j++)
          {
            for (k = 0; k < 4; k++)
              {
                  if (inbuf_f32[i][j][k] != f32_data[i][j][k])
                    {
                        printf("Wrong data at inbuf_f32[%d][%d][%d], out %f in %f\n", 
                               i,j,k, f32_data[i][j][k], inbuf_f32[i][j][k]);
                        errors++;
                    }
              }
          }
      }

    if (errors)
        goto done;


    /* end access and close file */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the file\n"););
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

#endif /*  !(defined(UNICOS) || defined(_UNICOS) || defined(_CRAYMPP)) */

    /* 
       9. Create 4-D element with partial chunks.
       Write only half the data out(5,000 bytes)
       Set dimension to 10x10x10x10 array  real data 10,000 bytes .
       120 chunks whit chunks of 2x3x4x5 = 120 bytes,
       data size with chunks is 120 bytes x 120 chunks = 14,400 bytes 
       */
    chunk[0].num_dims   = 4;
    chunk[0].chunk_size = 120;
    chunk[0].nt_size    = 1; /* number type size */
    chunk[0].chunk_flag = 0; /* nothing set */
    chunk[0].comp_type = COMP_CODE_NONE; /* nothing set */
    chunk[0].model_type = COMP_MODEL_STDIO; /* nothing set */
    chunk[0].cinfo = NULL; /* nothing set */
    chunk[0].minfo = NULL; /* nothing set */
#if 0
    chunk[0].chunk_flag = SPECIAL_COMP; /* compression */
    chunk[0].comp_type  = COMP_CODE_RLE; /* RLE */
    chunk[0].model_type = COMP_MODEL_STDIO; /* STDIO */
    chunk[0].cinfo = &cinfo; /* nothing set */
    chunk[0].minfo = &minfo; /* nothing set */
#endif

    chunk[0].pdims[0].dim_length   = 10;
    chunk[0].pdims[0].chunk_length = 2;
    chunk[0].pdims[0].distrib_type = 1;

    chunk[0].pdims[1].dim_length   = 10;
    chunk[0].pdims[1].chunk_length = 3;
    chunk[0].pdims[1].distrib_type = 1;

    chunk[0].pdims[2].dim_length   = 10;
    chunk[0].pdims[2].chunk_length = 4;
    chunk[0].pdims[2].distrib_type = 1;

    chunk[0].pdims[3].dim_length   = 10;
    chunk[0].pdims[3].chunk_length = 5;
    chunk[0].pdims[3].distrib_type = 1;

    /* set fill value to 1 */
    fill_val_u8 = 1;
    fill_val_len = 1;

    /* Open file for reading now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");

    MESSAGE(5, printf("Test 9. Create another new element as a 4-D, uint8 chunked element\n"););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      0,(int)chunk[0].pdims[0].dim_length, 
                      0,(int)chunk[0].pdims[0].chunk_length););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      1,(int)chunk[0].pdims[1].dim_length, 
                      1,(int)chunk[0].pdims[1].chunk_length););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      2,(int)chunk[0].pdims[2].dim_length, 
                      2,(int)chunk[0].pdims[2].chunk_length););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      3,(int)chunk[0].pdims[3].dim_length, 
                      3,(int)chunk[0].pdims[3].chunk_length););

    /* Create element     tag, ref,  nlevels, fill_len, fill, chunk array */
    aid1 = HMCcreate(fid, 1020, 9, 1, fill_val_len, &fill_val_u8, (HCHUNK_DEF *)chunk);
    CHECK(aid1, FAIL, "HMCcreate");

    /* write 5000 bytes out */
    ret = Hwrite(aid1, 5000, outbuf);
    VERIFY(ret, 5000, "Hwrite");
    if (ret != 5000)
      {
          fprintf(stderr, "ERROR: Hwrite returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Wrote 5000of10000 bytes to 4-D chunked element \n"); );
    /* end access */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the file\n"););
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    /* Open file for reading now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");

    MESSAGE(5, printf("Open 4-D chunked element again for reading\n"); );

    /* start read access   tag,  ref */
    aid1 = Hstartread(fid, 1020, 9);
    CHECK(aid1, FAIL, "Hstartread");

    /* inquire about element */
    ret = Hinquire(aid1, &fileid, &tag, &ref, &length, &offset, &posn,
                   &acc_mode, &special);

    CHECK(ret, FAIL, "Hinquire");
    if (!special)
      {
          fprintf(stderr, "ERROR: Hinquire does not think element is special line %d\n",
                  __LINE__);
          errors++;
          goto done;
      }

    /* Check values from Hinquire */
    if ( ref != 9 || length != 10000)
      {
          fprintf(stderr, "ERROR: Hinquire does not return the correct values \n");
          fprintf(stderr, "       tag =%d, ref=%d, length=%d \n",tag,ref,(int)length);
          errors++;
          goto done;
      }

    /* verify the data */
    MESSAGE(5, printf("Verifying 5000 bytes of data\n"); );

    /* read back in buffer  */
    ret = Hread(aid1, 5000, inbuf);
    VERIFY(ret, 5000, "Hread");
    if (ret != 5000)
      {
          fprintf(stderr, "ERROR: Hread returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != outbuf[i])
            {
                printf("Wrong data at %d, out %d in %d\n", i, outbuf[i], inbuf[i]);
                errors++;
            }
      }
    if (errors)
        goto done;

    /* verify some more data */
    MESSAGE(5, printf("seek to 7000 bytes in element and read 1000 bytes \n"); );

    /* seek past initial write  buffer  */
    ret = Hseek(aid1, 7000, 0);
    VERIFY(ret, 0, "Hseek");

    /* read back in buffer  */
    ret = Hread(aid1, 1000, inbuf);
    VERIFY(ret, 1000, "Hread");
    if (ret != 1000)
      {
          fprintf(stderr, "ERROR: Hread returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    /* verify the data, should be fill values */
    MESSAGE(5, printf("Verifying 1000 bytes of data, should be fill values\n"); );

    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != fill_val_u8)
            {
                printf("Wrong data at %d, out %d in %d\n", i, fill_val_u8, inbuf[i]);
                errors++;
            }
      }
    if (errors)
        goto done;

    /* end access and close file */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the file\n"););
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

#ifdef BIG_TEST

    /* 
       10. The rest of the tests here are commented out
       They are some extra high order tests to replicate
       some test done on EOS-DEM data  -GV.....
       Set dimension to 12000x12000 array with 2,500 chunks 
       whith chunk of 240x240 = 57,600 bytes
       */
    chunk[0].num_dims   = 2;
    chunk[0].chunk_size = 57600;
    chunk[0].nt_size    = 1; /* number type size */
    chunk[0].chunk_flag = 0; /* nothing set */
    chunk[0].comp_type = COMP_CODE_NONE; /* nothing set */
    chunk[0].model_type = COMP_MODEL_STDIO; /* nothing set */
    chunk[0].cinfo = NULL; /* nothing set */
    chunk[0].minfo = NULL; /* nothing set */

    chunk[0].pdims[0].dim_length   = 12000;
    chunk[0].pdims[0].chunk_length = 240;
    chunk[0].pdims[0].distrib_type = 1;

    chunk[0].pdims[1].dim_length   = 12000;
    chunk[0].pdims[1].chunk_length = 240;
    chunk[0].pdims[1].distrib_type = 1;

#if 0
    /* Open file for reading now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");
#endif

    /* Create element     tag, ref,  nlevels, fill_len, fill, chunk array */
    aid1 = HMCcreate(fid, 1020, 10, 1, fill_val_len, &fill_val_u8, (HCHUNK_DEF *)chunk);
    CHECK(aid1, FAIL, "HMCcreate");

    for (j = 0; j < 12000; j++)
      {
          printf("%d,",j);
          /* write 12000 bytes out */
          ret = Hwrite(aid1, 12000, outbuf);
          if (ret != 12000)
            {
                fprintf(stderr, "ERROR: Hwrite returned the wrong length: %d\n", (int) ret);
                errors++;
            }
      }
    printf("\n");

    /* end access */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the files\n");
            );
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");


    /* Open file for reading now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");

    /* start read access   tag,  ref */
    aid1 = Hstartread(fid, 1020, 10);
    CHECK(aid1, FAIL, "Hstartread");

    /* inquire about element */
    ret = Hinquire(aid1, &fileid, &tag, &ref, &length, &offset, &posn,
                   &acc_mode, &special);

    CHECK(ret, FAIL, "Hinquire");
    if (!special)
      {
          fprintf(stderr, "ERROR: Hinquire does not think element is special line %d\n",
                  __LINE__);
          errors++;
      }

    /* verify the data */
    MESSAGE(5, printf("Verifying data\n");
            );

    for (j = 0; j < 12000; j++)
      {

          if ((j % 1000) == 0)
            {
                printf("read %d,",j);
                /* read back in buffer  */
                ret = Hread(aid1, 12000, inbuf);

                if (ret != 12000)
                  {
                      fprintf(stderr, "ERROR: Hread returned the wrong length: %d\n", (int) ret);
                      errors++;
                  }

                for (i = 0; i < ret; i++)
                  {
                      if (inbuf[i] != outbuf[i])
                        {
                            printf("Wrong data at %d, out %d in %d\n", i, outbuf[i], inbuf[i]);
                            errors++;
                        }
                  }
            }
          else
            {
                /* seek to next buffer  */
                ret = Hseek(aid1, 12000, 1);
                VERIFY(ret, 0, "Hseek");
            }
      }

    /* Mimic read pattern of EOS-DEM test  behaviour */
    x = 1000;
    y = 1000;
    nseek = (12000 * y) + x;
    printf("seek to initial byte postion = %d \n",nseek);
    /* seek to next buffer  */
    ret = Hseek(aid1, nseek, 0);
    VERIFY(ret, 0, "Hseek");

    for (j = 0; j < 11; j++)
      {
          printf("read 2000x2000 square at x=%d,y=%d\n",x,y);

          for (k = 0; k < 2000; k++)
            {
                /* read back in buffer  */
                ret = Hread(aid1, 2000, inbuf);

                if (ret != 2000)
                  {
                      fprintf(stderr, "ERROR: Hread returned the wrong length: %d\n", (int) ret);
                      errors++;
                  }

                /* seek to next buffer  */
                ret = Hseek(aid1, 10000, 1);
                VERIFY(ret, 0, "Hseek");
            }

          x = x + 100;
          y = y + 1000;
          nseek = (12000 * y) + x;
          printf("seek to next byte postion = %d\n",nseek);
          /* seek to next buffer  */
          ret = Hseek(aid1, nseek, 0);
          VERIFY(ret, 0, "Hseek");
      }

    /* end access and close file */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the files\n");
            );
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

#endif /* BIG_TEST */

    /* 
     Chunking with Compression testing..... 
     */

    /* 
       11. Create a new element but now there will be partial chunks and Compression.
       Set dimension to 4x4 array with 4 chunks where each chunk is 3x2 = 6 bytes.
       Real data size is 16 bytes, size with chunks is 
       6 bytes x 4 chunks = 24 bytes .

       The element will be compressed with RLE scheme.

       */
    chunk[0].num_dims   = 2;
    chunk[0].chunk_size = 6; /* 3x2 = 6 bytes */
    chunk[0].nt_size    = 1; /* number type size */
    chunk[0].chunk_flag = SPECIAL_COMP; /* compression */
#if 0
    chunk[0].comp_type = COMP_CODE_SKPHUFF; /* Skipping Huffman */
    cinfo.skphuff.skp_size = sizeof(uint8);

    chunk[0].comp_type = COMP_CODE_DEFLATE; /* GZIP */
    cinfo.deflate.level = 6;

#endif
    chunk[0].comp_type  = COMP_CODE_RLE; /* RLE */

    chunk[0].model_type = COMP_MODEL_STDIO; /* STDIO */
    chunk[0].cinfo = &cinfo; /* nothing set */
    chunk[0].minfo = &minfo; /* nothing set */

    chunk[0].pdims[0].dim_length   = 4;
    chunk[0].pdims[0].chunk_length = 3;  /* made this 3 */
    chunk[0].pdims[0].distrib_type = 1;

    chunk[0].pdims[1].dim_length   = 4;
    chunk[0].pdims[1].chunk_length = 2;
    chunk[0].pdims[1].distrib_type = 1;

    /* Open file for writing odd size chunks now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");

    MESSAGE(5, printf("Test 11. Create another new element as a 2-D, uint8 chunked, RLE Compressed element\n"););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      0,(int)chunk[0].pdims[0].dim_length, 
                      0,(int)chunk[0].pdims[0].chunk_length););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      1,(int)chunk[0].pdims[1].dim_length, 
                      1,(int)chunk[0].pdims[1].chunk_length););

    /* Create element     tag, ref,  nlevels, fill_len, fill, chunk array */
    aid1 = HMCcreate(fid, 1020, 20, 1, fill_val_len, &fill_val_u8, (HCHUNK_DEF *)chunk);
    CHECK(aid1, FAIL, "HMCcreate");

    /* write 16 bytes out */
    ret = Hwrite(aid1, 16, outbuf);
    VERIFY(ret, 16, "Hwrite");
    if (ret != 16)
      {
          fprintf(stderr, "ERROR: Hwrite returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Wrote to 4of4 chunks (16 bytes) in file\n"););
    /* end access */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the file\n"); );
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    MESSAGE(5, printf("Open 2-D, uint8 chunked, RLE Compressed element again for reading\n"); );

    /* Open file for reading now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");

    /* start read access   tag,  ref */
    aid1 = Hstartread(fid, 1020, 20);
    CHECK(aid1, FAIL, "Hstartread");

    /* inquire about element */
    ret = Hinquire(aid1, &fileid, &tag, &ref, &length, &offset, &posn,
                   &acc_mode, &special);

    CHECK(ret, FAIL, "Hinquire");
    if (!special)
      {
          fprintf(stderr, "ERROR: Hinquire does not think element is special line %d\n",
                  __LINE__);
          errors++;
          goto done;
      }

    /* Check values from Hinquire */
    if ( ref != 20 || length != 16)
      {
          fprintf(stderr, "ERROR: Hinquire does not return the correct values \n");
          fprintf(stderr, "       tag =%d, ref=%d, length=%d \n",tag,ref,(int)length);
          errors++;
          goto done;
      }

    /* read back in buffer  */
    ret = Hread(aid1, 16, inbuf);
    VERIFY(ret, 16, "Hread");
    if (ret != 16)
      {
          fprintf(stderr, "ERROR: Hread returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    /* verify the data */
    MESSAGE(5, printf("Verifying 16 bytes of data\n"); );
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != outbuf[i])
            {
                printf("Wrong data at %d, out %d in %d\n", i, outbuf[i], inbuf[i]);
                errors++;
            }
          inbuf[i] = '\0';
      }
    if (errors)
        goto done;

    /* end access and close file */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the file\n"); );
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    /* 
       12. Now create 3-D chunked, Compressed element with no partial chunks.
       Write using HMCwriteChunk(). Read data back in first
       using Hread() and verify. Then read data back in using
       HMCreadChunk() and verify.
       Set dimension to 2x3x4 array with 6 chunks 
       where each chunk is 1x1x4= 4 bytes , total data size 24 bytes 
       
       The element is compressed using RLE scheme.
       */
    chunk[0].num_dims   = 3;
    chunk[0].chunk_size = 4; /* 1x1x4 bytes */
    chunk[0].nt_size    = 1; /* number type size */
    chunk[0].chunk_flag = SPECIAL_COMP; /* compression */
#if 0

    chunk[0].comp_type  = COMP_CODE_RLE; /* RLE */

    chunk[0].comp_type = COMP_CODE_SKPHUFF; /* Skipping Huffman */
    cinfo.skphuff.skp_size = sizeof(uint8);
#endif
    chunk[0].comp_type = COMP_CODE_DEFLATE; /* GZIP */
    cinfo.deflate.level = 6;

    chunk[0].model_type = COMP_MODEL_STDIO; /* STDIO */
    chunk[0].cinfo = &cinfo; /* nothing set */
    chunk[0].minfo = &minfo; /* nothing set */

    chunk[0].pdims[0].dim_length   = 2;
    chunk[0].pdims[0].chunk_length = 1;  
    chunk[0].pdims[0].distrib_type = 1;

    chunk[0].pdims[1].dim_length   = 3;
    chunk[0].pdims[1].chunk_length = 1;
    chunk[0].pdims[1].distrib_type = 1;

    chunk[0].pdims[2].dim_length   = 4;
    chunk[0].pdims[2].chunk_length = 4;
    chunk[0].pdims[2].distrib_type = 0; /* NONE */

    /* set fill value to 1 */
    fill_val_u8 = 1;
    fill_val_len = 1;

    /* Open file for writing last odd size chunks now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");
    MESSAGE(5, printf("Test 12. Create another new element as a 3-D, uint8 chunked, GZIP Compressed element(192 bytes)\n"););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      0,(int)chunk[0].pdims[0].dim_length, 
                      0,(int)chunk[0].pdims[0].chunk_length););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      1,(int)chunk[0].pdims[1].dim_length, 
                      1,(int)chunk[0].pdims[1].chunk_length););
    MESSAGE(5, printf(" dim_length[%d]=%d, chunk_length[%d]=%d \n",
                      2,(int)chunk[0].pdims[2].dim_length, 
                      2,(int)chunk[0].pdims[2].chunk_length););

    /* Create element     tag, ref,  nlevels, fill_len, fill, chunk array */
    aid1 = HMCcreate(fid, 1020, 21, 1, fill_val_len, &fill_val_u8, (HCHUNK_DEF *)chunk);
    CHECK(aid1, FAIL, "HMCcreate");

    /* Create element     tag, ref,  nlevels, fill_len, fill, chunk array */
    aid2 = HMCcreate(fid, 1020, 22, 1, fill_val_len, &fill_val_u8, (HCHUNK_DEF *)chunk);
    CHECK(aid1, FAIL, "HMCcreate");

    /* write 24 bytes out */
    ret = Hwrite(aid2, 24, u8_data);
    if (ret != 24)
      {
          fprintf(stderr, "ERROR: Hwrite returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    /* write data out as chunks */
    MESSAGE(5, printf("Writing to 3-D, uint8 chunked, GZIP Compressed element using HMCwriteChunk\n"); );

    /* Write data use SDwriteChunk */
    dims[0] = 0;
    dims[1] = 0;
    dims[2] = 0;
    ret = HMCwriteChunk(aid1, dims, chunk1);
    CHECK(ret, FAIL, "HMCwriteChunk");

    dims[0] = 1;
    dims[1] = 0;
    dims[2] = 0;
    ret = HMCwriteChunk(aid1, dims, chunk4);
    CHECK(ret, FAIL, "HMCwriteChunk");

    dims[0] = 0;
    dims[1] = 1;
    dims[2] = 0;
    ret = HMCwriteChunk(aid1, dims, chunk2);
    CHECK(ret, FAIL, "HMCwriteChunk");

    dims[0] = 1;
    dims[1] = 1;
    dims[2] = 0;
    ret = HMCwriteChunk(aid1, dims, chunk5);
    CHECK(ret, FAIL, "HMCwriteChunk");

    dims[0] = 0;
    dims[1] = 2;
    dims[2] = 0;
    ret = HMCwriteChunk(aid1, dims, chunk3);
    CHECK(ret, FAIL, "HMCwriteChunk");

    dims[0] = 1;
    dims[1] = 2;
    dims[2] = 0;
    ret = HMCwriteChunk(aid1, dims, chunk6);
    CHECK(ret, FAIL, "HMCwriteChunk");

    /* end access */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    /* end access */
    ret = Hendaccess(aid2);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the files\n"););
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    MESSAGE(5, printf("Open 3-D, uint8 chunked, GZIP Compressed element again for reading\n"); );
    /* Open file for reading now */
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");

    /* start read access   tag,  ref */
    aid1 = Hstartread(fid, 1020, 21);
    CHECK(aid1, FAIL, "Hstartread");

    /* start read access   tag,  ref */
    aid2 = Hstartread(fid, 1020, 22);
    CHECK(aid1, FAIL, "Hstartread");

    /* inquire about element */
    ret = Hinquire(aid1, &fileid, &tag, &ref, &length, &offset, &posn,
                   &acc_mode, &special);

    CHECK(ret, FAIL, "Hinquire");
    if (!special)
      {
          fprintf(stderr, "ERROR: Hinquire does not think element is special line %d\n",
                  __LINE__);
          errors++;
          goto done;
      }

    /* Check values from Hinquire */
    if ( ref != 21 || length != 24)
      {
          fprintf(stderr, "ERROR: Hinquire does not return the correct values \n");
          fprintf(stderr, "       tag =%d, ref=%d, length=%d \n",tag,ref,(int)length);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Get/Check special info data\n"); );

    /* get special info about element */
    ret = HDget_special_info(aid1, &info_block);
    CHECK(aid1, FAIL, "HDget_special_info");

    /* check special info */
    if (info_block.ndims != chunk[0].num_dims /* 2-D */)
      {
          fprintf(stderr, "ERROR: HDget_specail_info does not return the correct values \n");
          errors++;
          goto done;
      }

    /* check chunk_lengths */
    if (info_block.cdims != NULL)
      {
          if ((info_block.cdims[0] != 1) 
              || (info_block.cdims[1] != 1)
              || (info_block.cdims[2] != 4))
            {
                fprintf(stderr, "ERROR: HDget_specail_info does not return the correct values \n");
                errors++;
                goto done;
            }

          /* free allocated space by routine */
          HDfree(info_block.cdims);
      }
    else
      {
          fprintf(stderr, "ERROR: HDget_specail_info does not return the correct values \n");
          errors++;
          goto done;
      }


    /* read back in buffer  */
    ret = Hread(aid1, 24, inbuf_u8);
    VERIFY(ret, 24, "Hread");
    if (ret != 24)
      {
          fprintf(stderr, "ERROR: Hread returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    /* verify the data */
    MESSAGE(5, printf("Verifying 24 bytes data from Hread\n"); );
    for (i = 0; i < 2; i++)
      {
        for (j = 0; j < 3; j++)
          {
            for (k = 0; k < 4; k++)
              {
                  if (inbuf_u8[i][j][k] != u8_data[i][j][k])
                    {
                        printf("Wrong data at inbuf_u8[%d][%d][%d], out %d in %d\n", 
                               i,j,k, u8_data[i][j][k], inbuf_u8[i][j][k]);
                        errors++;
                    }
              }
          }
      }

    if (errors)
        goto done;

    MESSAGE(5, printf("Verifying 24 bytes from uint8 chunked, GZIP Compressed element using HMCreadChunk\n"); );
    /* read data back as chunks */
    dims[0] = 0;
    dims[1] = 0;
    dims[2] = 0;
    ret = HMCreadChunk(aid2, dims, inbuf);
    CHECK(ret, FAIL, "HMCreadChunk");
    if (ret != 4)
      {
          fprintf(stderr, "ERROR: HMCreadChunk returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Verifying chunk1 4 bytes data\n"); );
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != chunk1[i])
            {
                printf("Wrong data at %d, out %d in %d\n", i, chunk1[i], inbuf[i]);
                errors++;
            }
      }

    dims[0] = 0;
    dims[1] = 1;
    dims[2] = 0;
    ret = HMCreadChunk(aid2, dims, inbuf);
    CHECK(ret, FAIL, "HMCreadChunk");
    if (ret != 4)
      {
          fprintf(stderr, "ERROR: HMCreadChunk returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Verifying chunk2 4 bytes data\n"); );
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != chunk2[i])
            {
                printf("Wrong data at %d, out %d in %d\n", i, chunk2[i], inbuf[i]);
                errors++;
            }
      }

    dims[0] = 0;
    dims[1] = 2;
    dims[2] = 0;
    ret = HMCreadChunk(aid2, dims, inbuf);
    CHECK(ret, FAIL, "HMCreadChunk");
    if (ret != 4)
      {
          fprintf(stderr, "ERROR: HMCreadChunk returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Verifying chunk3 4 bytes data\n"); );
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != chunk3[i])
            {
                printf("Wrong data at %d, out %d in %d\n", i, chunk3[i], inbuf[i]);
                errors++;
            }
      }

    dims[0] = 1;
    dims[1] = 0;
    dims[2] = 0;
    ret = HMCreadChunk(aid2, dims, inbuf);
    CHECK(ret, FAIL, "HMCreadChunk");
    if (ret != 4)
      {
          fprintf(stderr, "ERROR: HMCreadChunk returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Verifying chunk4 4 bytes data\n"); );
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != chunk4[i])
            {
                printf("Wrong data at %d, out %d in %d\n", i, chunk4[i], inbuf[i]);
                errors++;
            }
      }

    dims[0] = 1;
    dims[1] = 1;
    dims[2] = 0;
    ret = HMCreadChunk(aid2, dims, inbuf);
    CHECK(ret, FAIL, "HMCreadChunk");
    if (ret != 4)
      {
          fprintf(stderr, "ERROR: HMCreadChunk returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Verifying chunk5 4 bytes data\n"); );
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != chunk5[i])
            {
                printf("Wrong data at %d, out %d in %d\n", i, chunk5[i], inbuf[i]);
                errors++;
            }
      }

    dims[0] = 1;
    dims[1] = 2;
    dims[2] = 0;
    ret = HMCreadChunk(aid2, dims, inbuf);
    CHECK(ret, FAIL, "HMCreadChunk");
    if (ret != 4)
      {
          fprintf(stderr, "ERROR: HMCreadChunk returned the wrong length: %d\n", (int) ret);
          errors++;
          goto done;
      }

    MESSAGE(5, printf("Verifying chunk6 4 bytes data\n"); );
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != chunk6[i])
            {
                printf("Wrong data at %d, out %d in %d\n", i, chunk6[i], inbuf[i]);
                errors++;
            }
      }

    /* end access and close file */
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    /* end access and close file */
    ret = Hendaccess(aid2);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing the file\n"););
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");


  done:
    /* Don't forget to free dimensions allocate for chunk definition */
    if (chunk[0].pdims != NULL)
        HDfree(chunk[0].pdims);

    num_errs += errors;     /* increment global error count */
} /* test_chunks() */
