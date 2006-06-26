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

/*
   FILE
   nbit.c
   Test HDF N-Bit dataset I/O routines

   REMARKS
   These tests depend on the bitio layer, the compression layer and
   the SD layer...

   DESIGN

   BUGS/LIMITATIONS

   EXPORTED ROUTINES

   AUTHOR
   Quincey Koziol

   MODIFICATION HISTORY
   1/19/94 - Started coding
 */

/* $Id: nbit.c,v 1.21 2003/12/10 21:13:35 epourmal Exp $ */

#ifdef RCSID
static char RcsId[] = "@(#)$Revision: 1.21 $";
#endif

#include "tproto.h"
#define TESTFILE_NAME "tnbit.hdf"
#define DATAFILE_NAME "nbit.dat"

#define NBIT_TAG1   1000
#define NBIT_REF1   1000
#define NBIT_SIZE1  4096
#define NBIT_BITS1  6
#define NBIT_MASK1  0x3f

#define NBIT_TAG2   1001
#define NBIT_REF2   1001
#define NBIT_SIZE2  4096
#define NBIT_BITS2  6
#define NBIT_MASK2  0x3f

#define NBIT_TAG3   1002
#define NBIT_REF3   1002
#define NBIT_SIZE3  4096
#define NBIT_BITS3  12
#define NBIT_MASK3A 0x0fff
#define NBIT_MASK3B 0xffff

#define NBIT_TAG4   1003
#define NBIT_REF4   1003
#define NBIT_SIZE4  4096
#define NBIT_BITS4  14
#define NBIT_MASK4A 0xffff
#define NBIT_MASK4B 0xffff

#define NBIT_TAG5   1004
#define NBIT_REF5   1004
#define NBIT_SIZE5  4096
#define NBIT_BITS5  27
#define NBIT_MASK5A 0x07ffffff
#define NBIT_MASK5B 0xffffffffUL

#define NBIT_TAG6   1005
#define NBIT_REF6   1005
#define NBIT_SIZE6  4096
#define NBIT_BITS6  29
#define NBIT_MASK6A 0xffffffffUL
#define NBIT_MASK6B 0xffffffffUL

#define NBIT_TAG7   1006
#define NBIT_REF7   1006
#define NBIT_SIZE7  4096
#define NBIT_BITS7  4
#define NBIT_OFF7   6
#define NBIT_MASK7A 0x78
#define NBIT_MASK7B 0x87

#define NBIT_TAG8   1007
#define NBIT_REF8   1007
#define NBIT_SIZE8  4096
#define NBIT_BITS8  4
#define NBIT_OFF8   5
#define NBIT_MASK8  0x03

#define NBIT_TAG9   1008
#define NBIT_REF9   1008
#define NBIT_SIZE9  4096
#define NBIT_BITS9  8
#define NBIT_OFF9   12
#define NBIT_MASK9A 0xe01f
#define NBIT_MASK9B 0xffff

#define NBIT_TAG10   1009
#define NBIT_REF10   1009
#define NBIT_SIZE10  4096
#define NBIT_BITS10  9
#define NBIT_OFF10   10
#define NBIT_MASK10A 0x0003
#define NBIT_MASK10B 0xffff

#define NBIT_TAG11   1010
#define NBIT_REF11   1010
#define NBIT_SIZE11  4096
#define NBIT_BITS11  18
#define NBIT_OFF11   27
#define NBIT_MASK11A 0xf00003ffUL
#define NBIT_MASK11B 0xffffffffUL

#define NBIT_TAG12   1011
#define NBIT_REF12   1011
#define NBIT_SIZE12  4096
#define NBIT_BITS12  27
#define NBIT_OFF12   31
#define NBIT_MASK12A 0x0000001f
#define NBIT_MASK12B 0xffffffffUL

static void test_nbit1(int32 fid);
static void test_nbit2(int32 fid);
static void test_nbit3(int32 fid);
static void test_nbit4(int32 fid);
static void test_nbit5(int32 fid);
static void test_nbit6(int32 fid);
static void test_nbit7(int32 fid);
static void test_nbit8(int32 fid);
static void test_nbit9(int32 fid);
static void test_nbit10(int32 fid);
static void test_nbit11(int32 fid);
static void test_nbit12(int32 fid);

static void
test_nbit1(int32 fid)
{
    int32       aid1;
    uint16      ref1;
    int i;
    int32       ret;
    intn        errors = 0;
    model_info  m_info;
    comp_info   c_info;
    uint8      *outbuf, *inbuf;
    uint8       test_val;

    outbuf = (uint8 *) HDmalloc(NBIT_SIZE1 * sizeof(uint8));
    inbuf = (uint8 *) HDmalloc(NBIT_SIZE1 * sizeof(uint8));

    for (i = 0; i < NBIT_SIZE1; i++)    /* fill with pseudo-random data */
        outbuf[i] = (uint8) (i * 3);

    ref1 = Hnewref(fid);
    CHECK(ref1, 0, "Hnewref");

    MESSAGE(5, printf("Create a new element as an unsigned 8-bit n-bit element\n");
        );
    c_info.nbit.nt = DFNT_UINT8;
    c_info.nbit.sign_ext = FALSE;
    c_info.nbit.fill_one = FALSE;
    c_info.nbit.start_bit = NBIT_BITS1 - 1;
    c_info.nbit.bit_len = NBIT_BITS1;
    aid1 = HCcreate(fid, NBIT_TAG1, ref1, COMP_MODEL_STDIO, &m_info,
                    COMP_CODE_NBIT, &c_info);
    CHECK(aid1, FAIL, "HCcreate");

    ret = Hwrite(aid1, NBIT_SIZE1, outbuf);
    if (ret != NBIT_SIZE1)
      {
          fprintf(stderr, "ERROR(%d): Hwrite returned the wrong length: %d\n", __LINE__, (int) ret);
          HEprint(stdout, 0);
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Verifying data\n");
        );
    ret = Hgetelement(fid, NBIT_TAG1, (uint16) ref1, inbuf);
    if (ret != NBIT_SIZE1)
      {
          HEprint(stderr, 0);
          fprintf(stderr, "ERROR: (%d) Hgetelement returned the wrong length: %d\n", __LINE__, (int) ret);
          errors++;
      }

    for (i = 0; i < NBIT_SIZE1; i++)
      {
          test_val = (uint8) (outbuf[i] & NBIT_MASK1);
          if ((uint8) inbuf[i] != (uint8) test_val)
            {
                printf("test_nbit1: Wrong data at %d, out (%d)%d in %d\n", i, outbuf[i], test_val, inbuf[i]);
                errors++;
            }
      }
    HDfree(outbuf);
    HDfree(inbuf);
    num_errs += errors;
}

static void
test_nbit2(int32 fid)
{
    int32       aid1;
    uint16      ref1;
    int i;
    int32       ret;
    intn        errors = 0;
    model_info  m_info;
    comp_info   c_info;
    int8       *outbuf, *inbuf;

    outbuf = (int8 *) HDmalloc(NBIT_SIZE2 * sizeof(int8));
    inbuf = (int8 *) HDmalloc(NBIT_SIZE2 * sizeof(int8));

    for (i = 0; i < NBIT_SIZE2; i++)    /* fill with pseudo-random data */
        outbuf[i] = (int8) (((i * 3) % 64) - 32);

    ref1 = Hnewref(fid);
    CHECK(ref1, 0, "Hnewref");

    MESSAGE(5, printf("Create a new element as a signed 8-bit n-bit element\n");
        );
    c_info.nbit.nt = DFNT_INT8;
    c_info.nbit.sign_ext = TRUE;
    c_info.nbit.fill_one = FALSE;
    c_info.nbit.start_bit = NBIT_BITS2 - 1;
    c_info.nbit.bit_len = NBIT_BITS2;
    aid1 = HCcreate(fid, NBIT_TAG2, ref1, COMP_MODEL_STDIO, &m_info,
                    COMP_CODE_NBIT, &c_info);
    CHECK(aid1, FAIL, "HCcreate");

    ret = Hwrite(aid1, NBIT_SIZE2, (uint8  *) outbuf);
    if (ret != NBIT_SIZE2)
      {
          fprintf(stderr, "ERROR(%d): Hwrite returned the wrong length: %d\n", __LINE__, (int) ret);
          HEprint(stdout, 0);
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Verifying data\n");
        );
    ret = Hgetelement(fid, NBIT_TAG2, (uint16) ref1, (uint8  *) inbuf);
    if (ret != NBIT_SIZE2)
      {
          HEprint(stderr, 0);
          fprintf(stderr, "ERROR: (%d) Hgetelement returned the wrong length: %d\n", __LINE__, (int) ret);
          errors++;
      }

    for (i = 0; i < ret; i++)
      {
          if ((int8) inbuf[i] != (int8) outbuf[i])
            {
                printf("test_nbit2: Wrong data at %d, out %d in %d\n", i, outbuf[i], inbuf[i]);
                errors++;
            }
      }
    HDfree(outbuf);
    HDfree(inbuf);
    num_errs += errors;
}

static void
test_nbit3(int32 fid)
{
    int32       aid1;
    uint16      ref1;
    int i;
    int32       ret;
    intn        errors = 0;
    model_info  m_info;
    comp_info   c_info;
    uint16     *outbuf, *inbuf;
    uint16      test_out, test_in;
    uint8      *convbuf;

    outbuf = (uint16 *) HDmalloc(NBIT_SIZE3 * sizeof(uint16));
    inbuf = (uint16 *) HDmalloc(NBIT_SIZE3 * sizeof(uint16));
    convbuf = (uint8 *) HDmalloc(NBIT_SIZE3 * (size_t)DFKNTsize(DFNT_UINT16));

    for (i = 0; i < NBIT_SIZE3; i++)    /* fill with pseudo-random data */
        outbuf[i] = (uint16) (i * 3);

    ref1 = Hnewref(fid);
    CHECK(ref1, 0, "Hnewref");

    MESSAGE(5, printf("Create a new element as a unsigned 16-bit n-bit element\n");
        );
    c_info.nbit.nt = DFNT_UINT16;
    c_info.nbit.sign_ext = FALSE;
    c_info.nbit.fill_one = FALSE;
    c_info.nbit.start_bit = NBIT_BITS3 - 1;
    c_info.nbit.bit_len = NBIT_BITS3;
    aid1 = HCcreate(fid, NBIT_TAG3, ref1, COMP_MODEL_STDIO, &m_info,
                    COMP_CODE_NBIT, &c_info);
    CHECK(aid1, FAIL, "HCcreate");

    ret = DFKconvert(outbuf, convbuf, DFNT_UINT16, NBIT_SIZE3, DFACC_WRITE, 0, 0);
    CHECK(ret, FAIL, "DFKconvert");
    ret = Hwrite(aid1, NBIT_SIZE3 * DFKNTsize(DFNT_UINT16), convbuf);
    if (ret != NBIT_SIZE3 * DFKNTsize(DFNT_UINT16))
      {
          fprintf(stderr, "ERROR(%d): Hwrite returned the wrong length: %d\n", __LINE__, (int) ret);
          HEprint(stdout, 0);
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Verifying data\n");
        );
    HDmemset(convbuf, 0, DFKNTsize(DFNT_UINT16) * NBIT_SIZE3);
    ret = Hgetelement(fid, NBIT_TAG3, (uint16) ref1, convbuf);
    if (ret != NBIT_SIZE3 * DFKNTsize(DFNT_UINT16))
      {
          HEprint(stderr, 0);
          fprintf(stderr, "ERROR: (%d) Hgetelement returned the wrong length: %d\n", __LINE__, (int) ret);
          errors++;
      }

    ret = DFKconvert(convbuf, inbuf, DFNT_UINT16, NBIT_SIZE3, DFACC_READ, 0, 0);
    CHECK(ret, FAIL, "DFKconvert");
    for (i = 0; i < NBIT_SIZE3; i++)
      {
          test_out = (uint16) (outbuf[i] & NBIT_MASK3A);
          test_in = (uint16) (inbuf[i] & NBIT_MASK3B);
#ifndef TESTING
          if ((int16) test_in != (int16) test_out)
            {
                printf("test_nbit3: Wrong data at %d, out (%d)%d in (%d)%d\n", i, outbuf[i], test_out, inbuf[i], test_in);
                errors++;
            }
#else
          printf("data at %d, out (%d)%d in (%d)%d\n", i, outbuf[i], test_out, inbuf[i], test_in);
#endif
      }
    HDfree(outbuf);
    HDfree(inbuf);
    HDfree(convbuf);
    num_errs += errors;
}

static void
test_nbit4(int32 fid)
{
    int32       aid1;
    uint16      ref1;
    int i;
    int32       ret;
    intn        errors = 0;
    model_info  m_info;
    comp_info   c_info;
    int16      *outbuf, *inbuf;
    int16       test_out, test_in;
    uint8      *convbuf;

    outbuf = (int16 *) HDmalloc(NBIT_SIZE4 * sizeof(int16));
    inbuf = (int16 *) HDmalloc(NBIT_SIZE4 * sizeof(int16));
    convbuf = (uint8 *) HDmalloc(NBIT_SIZE4 * (size_t)DFKNTsize(DFNT_INT16));

    for (i = 0; i < NBIT_SIZE4; i++)    /* fill with pseudo-random data */
        outbuf[i] = (int16) (((i * 3) % (64 * 256)) - (32 * 256));

    ref1 = Hnewref(fid);
    CHECK(ref1, 0, "Hnewref");

    MESSAGE(5, printf("Create a new element as a signed 16-bit n-bit element\n");
        );
    c_info.nbit.nt = DFNT_INT16;
    c_info.nbit.sign_ext = TRUE;
    c_info.nbit.fill_one = FALSE;
    c_info.nbit.start_bit = NBIT_BITS4 - 1;
    c_info.nbit.bit_len = NBIT_BITS4;
    aid1 = HCcreate(fid, NBIT_TAG4, ref1, COMP_MODEL_STDIO, &m_info,
                    COMP_CODE_NBIT, &c_info);
    CHECK(aid1, FAIL, "HCcreate");

    ret = DFKconvert(outbuf, convbuf, DFNT_INT16, NBIT_SIZE4, DFACC_WRITE, 0, 0);
    CHECK(ret, FAIL, "DFKconvert");

    ret = Hwrite(aid1, NBIT_SIZE4 * DFKNTsize(DFNT_INT16), convbuf);
    if (ret != NBIT_SIZE4 * DFKNTsize(DFNT_INT16))
      {
          fprintf(stderr, "ERROR(%d): Hwrite returned the wrong length: %d\n", __LINE__, (int) ret);
          HEprint(stdout, 0);
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Verifying data\n");
        );

    HDmemset(convbuf, 0, DFKNTsize(DFNT_INT16) * NBIT_SIZE4);

    ret = Hgetelement(fid, NBIT_TAG4, (uint16) ref1, convbuf);
    if (ret != NBIT_SIZE4 * DFKNTsize(DFNT_INT16))
      {
          HEprint(stderr, 0);
          fprintf(stderr, "ERROR: (%d) Hgetelement returned the wrong length: %d\n", __LINE__, (int) ret);
          errors++;
      }

    ret = DFKconvert(convbuf, inbuf, DFNT_INT16, NBIT_SIZE4, DFACC_READ, 0, 0);
    CHECK(ret, FAIL, "DFKconvert");
    for (i = 0; i < NBIT_SIZE4; i++)
      {
          test_out = (int16) (outbuf[i] & NBIT_MASK4A);
          test_in = (int16) (inbuf[i] & NBIT_MASK4B);
#ifndef TESTING
          if ((int16) test_in != (int16) test_out)
            {
                printf("test_nbit4: Wrong data at %d, out (%d)%d in (%d)%d\n", i, outbuf[i], test_out, inbuf[i], test_in);
                errors++;
            }
#else
          printf("data at %d, out (%d)%d in (%d)%d\n", i, outbuf[i], test_out, inbuf[i], test_in);
#endif
      }
    HDfree(outbuf);
    HDfree(inbuf);
    HDfree(convbuf);
    num_errs += errors;
}

static void
test_nbit5(int32 fid)
{
    int32       aid1;
    uint16      ref1;
    int i;
    int32       ret;
    intn        errors = 0;
    model_info  m_info;
    comp_info   c_info;
    uint32     *outbuf, *inbuf;
    uint32      test_out, test_in;
    uint8      *convbuf;

    outbuf = (uint32 *) HDmalloc(NBIT_SIZE5 * sizeof(uint32));
    inbuf = (uint32 *) HDmalloc(NBIT_SIZE5 * sizeof(uint32));
    convbuf = (uint8 *) HDmalloc(NBIT_SIZE5 * (size_t)DFKNTsize(DFNT_UINT32));

    for (i = 0; i < NBIT_SIZE5; i++)    /* fill with pseudo-random data */
        outbuf[i] = (uint32)(i * 300000);

    ref1 = Hnewref(fid);
    CHECK(ref1, 0, "Hnewref");

    MESSAGE(5, printf("Create a new element as a unsigned 32-bit n-bit element\n");
        );
    c_info.nbit.nt = DFNT_UINT32;
    c_info.nbit.sign_ext = FALSE;
    c_info.nbit.fill_one = FALSE;
    c_info.nbit.start_bit = NBIT_BITS5 - 1;
    c_info.nbit.bit_len = NBIT_BITS5;
    aid1 = HCcreate(fid, NBIT_TAG5, ref1, COMP_MODEL_STDIO, &m_info,
                    COMP_CODE_NBIT, &c_info);
    CHECK(aid1, FAIL, "HCcreate");

    ret = DFKconvert(outbuf, convbuf, DFNT_UINT32, NBIT_SIZE5, DFACC_WRITE, 0, 0);
    CHECK(ret, FAIL, "DFKconvert");

    ret = Hwrite(aid1, NBIT_SIZE5 * DFKNTsize(DFNT_UINT32), convbuf);
    if (ret != NBIT_SIZE5 * DFKNTsize(DFNT_UINT32))
      {
          fprintf(stderr, "ERROR(%d): Hwrite returned the wrong length: %d\n", __LINE__, (int) ret);
          HEprint(stdout, 0);
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Verifying data\n");
        );

    HDmemset(convbuf, 0, DFKNTsize(DFNT_UINT32) * NBIT_SIZE5);

    ret = Hgetelement(fid, NBIT_TAG5, (uint16) ref1, convbuf);
    if (ret != NBIT_SIZE5 * DFKNTsize(DFNT_UINT32))
      {
          HEprint(stderr, 0);
          fprintf(stderr, "ERROR: (%d) Hgetelement returned the wrong length: %d\n", __LINE__, (int) ret);
          errors++;
      }

    ret = DFKconvert(convbuf, inbuf, DFNT_UINT32, NBIT_SIZE5, DFACC_READ, 0, 0);
    CHECK(ret, FAIL, "DFKconvert");

    for (i = 0; i < NBIT_SIZE5; i++)
      {
          test_out = outbuf[i] & NBIT_MASK5A;
          test_in = (uintn)inbuf[i] & (uintn)NBIT_MASK5B;
#ifndef TESTING
          if ((uint32) test_in != (uint32) test_out)
            {
                printf("test_nbit5: Wrong data at %d, out (%lu)%lu in (%lu)%lu\n", i, (unsigned long) outbuf[i], (unsigned long) test_out, (unsigned long) inbuf[i], (unsigned long) test_in);
                errors++;
            }
#else
          printf("data at %d, out (%d)%d in (%d)%d\n", i, outbuf[i], test_out, inbuf[i], test_in);
#endif
      }
    HDfree(outbuf);
    HDfree(inbuf);
    HDfree(convbuf);
    num_errs += errors;
}

static void
test_nbit6(int32 fid)
{
    int32       aid1;
    uint16      ref1;
    int i;
    int32       ret;
    intn        errors = 0;
    model_info  m_info;
    comp_info   c_info;
    int32      *outbuf, *inbuf;
    int32       test_out, test_in;
    uint8      *convbuf;

    outbuf = (int32 *) HDmalloc(NBIT_SIZE6 * sizeof(int32));
    inbuf = (int32 *) HDmalloc(NBIT_SIZE6 * sizeof(int32));
    convbuf = (uint8 *) HDmalloc(NBIT_SIZE6 * (size_t)DFKNTsize(DFNT_INT32));

    for (i = 0; i < NBIT_SIZE6; i++)    /* fill with pseudo-random data */
        outbuf[i] = ((i * 300001) % ((int32) 16 * 256 * 256 * 256)) - ((int32) 8 * 256 * 256 * 256);

    ref1 = Hnewref(fid);
    CHECK(ref1, 0, "Hnewref");

    MESSAGE(5, printf("Create a new element as a signed 32-bit n-bit element\n");
        );
    c_info.nbit.nt = DFNT_INT32;
    c_info.nbit.sign_ext = TRUE;
    c_info.nbit.fill_one = FALSE;
    c_info.nbit.start_bit = NBIT_BITS6 - 1;
    c_info.nbit.bit_len = NBIT_BITS6;
    aid1 = HCcreate(fid, NBIT_TAG6, ref1, COMP_MODEL_STDIO, &m_info,
                    COMP_CODE_NBIT, &c_info);
    CHECK(aid1, FAIL, "HCcreate");

    ret = DFKconvert(outbuf, convbuf, DFNT_INT32, NBIT_SIZE6, DFACC_WRITE, 0, 0);
    CHECK(ret, FAIL, "DFKconvert");

    ret = Hwrite(aid1, NBIT_SIZE6 * DFKNTsize(DFNT_INT32), convbuf);
    if (ret != NBIT_SIZE6 * DFKNTsize(DFNT_INT32))
      {
          fprintf(stderr, "ERROR(%d): Hwrite returned the wrong length: %d\n", __LINE__, (int) ret);
          HEprint(stdout, 0);
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Verifying data\n");
        );

    HDmemset(convbuf, 0, DFKNTsize(DFNT_INT32) * NBIT_SIZE6);

    ret = Hgetelement(fid, NBIT_TAG6, (uint16) ref1, convbuf);
    if (ret != NBIT_SIZE6 * DFKNTsize(DFNT_INT32))
      {
          HEprint(stderr, 0);
          fprintf(stderr, "ERROR: (%d) Hgetelement returned the wrong length: %d\n", __LINE__, (int) ret);
          errors++;
      }

    ret = DFKconvert(convbuf, inbuf, DFNT_INT32, NBIT_SIZE6, DFACC_READ, 0, 0);
    CHECK(ret, FAIL, "DFKconvert");

    for (i = 0; i < NBIT_SIZE6; i++)
      {
          test_out = (int32)((uintn)outbuf[i] & (uintn)NBIT_MASK6A);
          test_in = (int32)((uintn)inbuf[i] & (uintn)NBIT_MASK6B);
#ifndef TESTING
          if ((int32) test_in != (int32) test_out)
            {
                printf("test_nbit6: Wrong data at %d, out (%ld)%ld in (%ld)%ld\n", i, (long) outbuf[i], (long) test_out, (long) inbuf[i], (long) test_in);
                errors++;
            }
#else
          printf("data at %d, out (%d)%d in (%d)%d\n", i, outbuf[i], test_out, inbuf[i], test_in);
#endif
      }
    HDfree(outbuf);
    HDfree(inbuf);
    HDfree(convbuf);
    num_errs += errors;
}

static void
test_nbit7(int32 fid)
{
    int32       aid1;
    uint16      ref1;
    int i;
    int32       ret;
    intn        errors = 0;
    model_info  m_info;
    comp_info   c_info;
    uint8      *outbuf, *inbuf;
    uint8       test_val;

    outbuf = (uint8 *) HDmalloc(NBIT_SIZE7 * sizeof(uint8));
    inbuf = (uint8 *) HDmalloc(NBIT_SIZE7 * sizeof(uint8));

    for (i = 0; i < NBIT_SIZE7; i++)    /* fill with pseudo-random data */
        outbuf[i] = (uint8) (i * 3);

    ref1 = Hnewref(fid);
    CHECK(ref1, 0, "Hnewref");

    MESSAGE(5, printf("Create a new element as a unsigned 8-bit n-bit element with filled ones\n");
        );
    c_info.nbit.nt = DFNT_UINT8;
    c_info.nbit.sign_ext = FALSE;
    c_info.nbit.fill_one = TRUE;
    c_info.nbit.start_bit = NBIT_OFF7;
    c_info.nbit.bit_len = NBIT_BITS7;
    aid1 = HCcreate(fid, NBIT_TAG7, ref1, COMP_MODEL_STDIO, &m_info,
                    COMP_CODE_NBIT, &c_info);
    CHECK(aid1, FAIL, "HCcreate");

    ret = Hwrite(aid1, NBIT_SIZE7, outbuf);
    if (ret != NBIT_SIZE7)
      {
          fprintf(stderr, "ERROR(%d): Hwrite returned the wrong length: %d\n", __LINE__, (int) ret);
          HEprint(stdout, 0);
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Verifying data\n");
        );
    ret = Hgetelement(fid, NBIT_TAG7, (uint16) ref1, inbuf);
    if (ret != NBIT_SIZE7)
      {
          HEprint(stderr, 0);
          fprintf(stderr, "ERROR: (%d) Hgetelement returned the wrong length: %d\n", __LINE__, (int) ret);
          errors++;
      }

    for (i = 0; i < ret; i++)
      {
          test_val = (uint8) ((outbuf[i] & NBIT_MASK7A) | NBIT_MASK7B);
          if ((uint8) inbuf[i] != (uint8) test_val)
            {
                printf("test_nbit7: Wrong data at %d, out (%d)%d in %d\n", i, outbuf[i], test_val, inbuf[i]);
                errors++;
            }
      }
    HDfree(outbuf);
    HDfree(inbuf);
    num_errs += errors;
}

static void
test_nbit8(int32 fid)
{
    int32       aid1;
    uint16      ref1;
    int i;
    int32       ret;
    intn        errors = 0;
    model_info  m_info;
    comp_info   c_info;
    int8       *outbuf, *inbuf;
    int8        test_val;

    outbuf = (int8 *) HDmalloc(NBIT_SIZE8 * sizeof(int8));
    inbuf = (int8 *) HDmalloc(NBIT_SIZE8 * sizeof(int8));

    for (i = 0; i < NBIT_SIZE8; i++)    /* fill with pseudo-random data */
        outbuf[i] = (int8) ((((i * 3) % 16) - 8) << 2);

    ref1 = Hnewref(fid);
    CHECK(ref1, 0, "Hnewref");

    MESSAGE(5, printf("Create a new element as a signed 8-bit n-bit element with filled ones\n");
        );
    c_info.nbit.nt = DFNT_INT8;
    c_info.nbit.sign_ext = TRUE;
    c_info.nbit.fill_one = TRUE;
    c_info.nbit.start_bit = NBIT_OFF8;
    c_info.nbit.bit_len = NBIT_BITS8;
    aid1 = HCcreate(fid, NBIT_TAG8, ref1, COMP_MODEL_STDIO, &m_info,
                    COMP_CODE_NBIT, &c_info);
    CHECK(aid1, FAIL, "HCcreate");

    ret = Hwrite(aid1, NBIT_SIZE8, (uint8  *) outbuf);
    if (ret != NBIT_SIZE8)
      {
          fprintf(stderr, "ERROR(%d): Hwrite returned the wrong length: %d\n", __LINE__, (int) ret);
          HEprint(stdout, 0);
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Verifying data\n");
        );
    ret = Hgetelement(fid, NBIT_TAG8, (uint16) ref1, (uint8  *) inbuf);
    if (ret != NBIT_SIZE8)
      {
          HEprint(stderr, 0);
          fprintf(stderr, "ERROR: (%d) Hgetelement returned the wrong length: %d\n", __LINE__, (int) ret);
          errors++;
      }

    for (i = 0; i < ret; i++)
      {
          test_val = (int8) (outbuf[i] | NBIT_MASK8);
          if ((int8) inbuf[i] != (int8) test_val)
            {
                printf("test_nbit8: Wrong data at %d, out (%d:%x)%d in %d\n", i, outbuf[i], outbuf[i], test_val, inbuf[i]);
                errors++;
            }
      }
    HDfree(outbuf);
    HDfree(inbuf);
    num_errs += errors;
}

static void
test_nbit9(int32 fid)
{
    int32       aid1;
    uint16      ref1;
    int i;
    int32       ret;
    intn        errors = 0;
    model_info  m_info;
    comp_info   c_info;
    uint16     *outbuf, *inbuf;
    uint16      test_out, test_in;
    uint8      *convbuf;

    outbuf = (uint16 *) HDmalloc(NBIT_SIZE9 * sizeof(uint16));
    inbuf = (uint16 *) HDmalloc(NBIT_SIZE9 * sizeof(uint16));
    convbuf = (uint8 *) HDmalloc(NBIT_SIZE9 * (size_t)DFKNTsize(DFNT_UINT16));

    for (i = 0; i < NBIT_SIZE9; i++)    /* fill with pseudo-random data */
        outbuf[i] = (uint16) (i * 3);

    ref1 = Hnewref(fid);
    CHECK(ref1, 0, "Hnewref");

    MESSAGE(5, printf("Create a new element as a unsigned 16-bit n-bit element\n");
        );
    c_info.nbit.nt = DFNT_UINT16;
    c_info.nbit.sign_ext = FALSE;
    c_info.nbit.fill_one = TRUE;
    c_info.nbit.start_bit = NBIT_OFF9;
    c_info.nbit.bit_len = NBIT_BITS9;
    aid1 = HCcreate(fid, NBIT_TAG9, ref1, COMP_MODEL_STDIO, &m_info,
                    COMP_CODE_NBIT, &c_info);
    CHECK(aid1, FAIL, "HCcreate");

    ret = DFKconvert(outbuf, convbuf, DFNT_UINT16, NBIT_SIZE9, DFACC_WRITE, 0, 0);
    CHECK(ret, FAIL, "DFKconvert");

    ret = Hwrite(aid1, NBIT_SIZE9 * DFKNTsize(DFNT_UINT16), convbuf);
    if (ret != NBIT_SIZE9 * DFKNTsize(DFNT_UINT16))
      {
          fprintf(stderr, "ERROR(%d): Hwrite returned the wrong length: %d\n", __LINE__, (int) ret);
          HEprint(stdout, 0);
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Verifying data\n");
        );

    HDmemset(convbuf, 0, DFKNTsize(DFNT_UINT16) * NBIT_SIZE9);

    ret = Hgetelement(fid, NBIT_TAG9, (uint16) ref1, convbuf);
    if (ret != NBIT_SIZE9 * DFKNTsize(DFNT_UINT16))
      {
          HEprint(stderr, 0);
          fprintf(stderr, "ERROR: (%d) Hgetelement returned the wrong length: %d\n", __LINE__, (int) ret);
          errors++;
      }

    ret = DFKconvert(convbuf, inbuf, DFNT_UINT16, NBIT_SIZE9, DFACC_READ, 0, 0);
    CHECK(ret, FAIL, "DFKconvert");

    for (i = 0; i < NBIT_SIZE9; i++)
      {
          test_out = (uint16) ((outbuf[i] | NBIT_MASK9A) & NBIT_MASK9B);
          test_in = (uint16) (inbuf[i] & NBIT_MASK9B);
#ifndef TESTING
          if ((uint16) test_in != (uint16) test_out)
            {
                printf("test_nbit9: Wrong data at %d, out (%d)%d in (%d)%d\n", i, outbuf[i], test_out, inbuf[i], test_in);
                errors++;
            }
#else
          printf("data at %d, out (%d)%d in (%d)%d\n", i, outbuf[i], test_val, inbuf[i], test_in);
#endif
      }
    HDfree(outbuf);
    HDfree(inbuf);
    HDfree(convbuf);
    num_errs += errors;
}

static void
test_nbit10(int32 fid)
{
    int32       aid1;
    uint16      ref1;
    int i;
    int32       ret;
    intn        errors = 0;
    model_info  m_info;
    comp_info   c_info;
    int16      *outbuf, *inbuf;
    int16       test_out, test_in;
    uint8      *convbuf;

    outbuf = (int16 *) HDmalloc(NBIT_SIZE10 * sizeof(int16));
    inbuf = (int16 *) HDmalloc(NBIT_SIZE10 * sizeof(int16));
    convbuf = (uint8 *) HDmalloc(NBIT_SIZE10 * (size_t)DFKNTsize(DFNT_UINT16));

    for (i = 0; i < NBIT_SIZE10; i++)   /* fill with pseudo-random data */
        outbuf[i] = (int16) ((((i * 3) % (2 * 256)) - (256)) << ((NBIT_OFF10 - NBIT_BITS10) + 1));

    ref1 = Hnewref(fid);
    CHECK(ref1, 0, "Hnewref");

    MESSAGE(5, printf("Create a new element as a signed 16-bit n-bit element\n");
        );
    c_info.nbit.nt = DFNT_INT16;
    c_info.nbit.sign_ext = TRUE;
    c_info.nbit.fill_one = TRUE;
    c_info.nbit.start_bit = NBIT_OFF10;
    c_info.nbit.bit_len = NBIT_BITS10;
    aid1 = HCcreate(fid, NBIT_TAG10, ref1, COMP_MODEL_STDIO, &m_info,
                    COMP_CODE_NBIT, &c_info);
    CHECK(aid1, FAIL, "HCcreate");

    ret = DFKconvert(outbuf, convbuf, DFNT_INT16, NBIT_SIZE10, DFACC_WRITE, 0, 0);
    CHECK(ret, FAIL, "DFKconvert");

    ret = Hwrite(aid1, NBIT_SIZE10 * DFKNTsize(DFNT_INT16), convbuf);
    if (ret != NBIT_SIZE10 * DFKNTsize(DFNT_INT16))
      {
          fprintf(stderr, "ERROR(%d): Hwrite returned the wrong length: %d\n", __LINE__, (int) ret);
          HEprint(stdout, 0);
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Verifying data\n");
        );

    HDmemset(convbuf, 0, DFKNTsize(DFNT_INT16) * NBIT_SIZE10);

    ret = Hgetelement(fid, NBIT_TAG10, (uint16) ref1, convbuf);
    if (ret != NBIT_SIZE10 * DFKNTsize(DFNT_INT16))
      {
          HEprint(stderr, 0);
          fprintf(stderr, "ERROR: (%d) Hgetelement returned the wrong length: %d\n", __LINE__, (int) ret);
          errors++;
      }

    ret = DFKconvert(convbuf, inbuf, DFNT_INT16, NBIT_SIZE10, DFACC_READ, 0, 0);
    CHECK(ret, FAIL, "DFKconvert");

    for (i = 0; i < NBIT_SIZE10; i++)
      {
/* intel windows c++ compiler has a bug when masking outbuf[i], it extends the
 * signed bit to all bits of higher 16 bit, so don't use MASK for intel windows
 * c++ compiler. Kent Yang 09/02/02
 * The same error occur in the Unix Intel Compiler. Albert Cheng 10/26/02
 */
/* Apparently Intel compiler bug was fixed and the fix below causes problems
 * when optimization is used. We decided to go to the original code that
 * works now with Intel 7.0 and Intel 7.1 compilers on both UNIX and Windows  
 * EIP 12/2/03
#if (defined __INTEL_COMPILER || defined __ICL)
     test_out = (int16)(outbuf[i] | NBIT_MASK10A);
#else
     test_out = (int16) ((outbuf[i] | NBIT_MASK10A) & NBIT_MASK10B);
#endif
*/
     test_out = (int16) ((outbuf[i] | NBIT_MASK10A) & NBIT_MASK10B);
          test_in = (int16) (inbuf[i] & NBIT_MASK10B);
/* The following ifdef block fixes a compiler bug on the */
/* HP9000, leave it in! -QAK */
#ifdef HP9000
          {
              char       *t;
              t = (char *) &test_out;
              t = (char *) &test_in;
          }
#endif /* HP9000 */
#ifndef TESTING
          if ((int16) test_in != (int16) test_out)
            {
                printf("test_nbit10: Wrong data at %d, out (%d)%d in (%d)%d\n", i, outbuf[i], test_out, inbuf[i], test_in);
                errors++;
            }
#else
          printf("data at %d, out (%d)%d in (%d)%d\n", i, outbuf[i], test_out, inbuf[i], test_in);
#endif
      }
    HDfree(outbuf);
    HDfree(inbuf);
    HDfree(convbuf);
    num_errs += errors;
}

static void
test_nbit11(int32 fid)
{
    int32       aid1;
    uint16      ref1;
    int i;
    int32       ret;
    intn        errors = 0;
    model_info  m_info;
    comp_info   c_info;
    uint32     *outbuf, *inbuf;
    uint32      test_out, test_in;
    uint8      *convbuf;

    outbuf = (uint32 *) HDmalloc(NBIT_SIZE11 * sizeof(uint32));
    inbuf = (uint32 *) HDmalloc(NBIT_SIZE11 * sizeof(uint32));
    convbuf = (uint8 *) HDmalloc(NBIT_SIZE11 * (size_t)DFKNTsize(DFNT_UINT32));

    for (i = 0; i < NBIT_SIZE11; i++)   /* fill with pseudo-random data */
        outbuf[i] = (uint32)(i * 304327);

    ref1 = Hnewref(fid);
    CHECK(ref1, 0, "Hnewref");

    MESSAGE(5, printf("Create a new element as a unsigned 32-bit n-bit element\n");
        );
    c_info.nbit.nt = DFNT_UINT32;
    c_info.nbit.sign_ext = FALSE;
    c_info.nbit.fill_one = TRUE;
    c_info.nbit.start_bit = NBIT_OFF11;
    c_info.nbit.bit_len = NBIT_BITS11;
    aid1 = HCcreate(fid, NBIT_TAG11, ref1, COMP_MODEL_STDIO, &m_info,
                    COMP_CODE_NBIT, &c_info);
    CHECK(aid1, FAIL, "HCcreate");

    ret = DFKconvert(outbuf, convbuf, DFNT_UINT32, NBIT_SIZE11, DFACC_WRITE, 0, 0);
    CHECK(ret, FAIL, "DFKconvert");

    ret = Hwrite(aid1, NBIT_SIZE11 * DFKNTsize(DFNT_UINT32), convbuf);
    if (ret != NBIT_SIZE11 * DFKNTsize(DFNT_UINT32))
      {
          fprintf(stderr, "ERROR(%d): Hwrite returned the wrong length: %d\n", __LINE__, (int) ret);
          HEprint(stdout, 0);
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Verifying data\n");
        );

    HDmemset(convbuf, 0, DFKNTsize(DFNT_UINT32) * NBIT_SIZE11);

    ret = Hgetelement(fid, NBIT_TAG11, (uint16) ref1, convbuf);
    if (ret != NBIT_SIZE11 * DFKNTsize(DFNT_UINT32))
      {
          HEprint(stderr, 0);
          fprintf(stderr, "ERROR: (%d) Hgetelement returned the wrong length: %d\n", __LINE__, (int) ret);
          errors++;
      }

    ret = DFKconvert(convbuf, inbuf, DFNT_UINT32, NBIT_SIZE11, DFACC_READ, 0, 0);
    CHECK(ret, FAIL, "DFKconvert");

    for (i = 0; i < NBIT_SIZE11; i++)
      {
          test_out = (outbuf[i] | (uintn)NBIT_MASK11A) & (uintn)NBIT_MASK11B;
          test_in = inbuf[i] & (uintn)NBIT_MASK11B;
#ifndef TESTING
          if ((uint32) test_in != (uint32) test_out)
            {
                printf("test_nbit11: Wrong data at %d, out (%lu)%lu in (%lu)%lu\n", i, (unsigned long) outbuf[i], (unsigned long) test_out, (unsigned long) inbuf[i], (unsigned long) test_in);
                errors++;
            }
#else
          printf("data at %d, out (%u)%u in (%u)%u\n", i, outbuf[i], test_out, inbuf[i], test_in);
#endif
      }
    HDfree(outbuf);
    HDfree(inbuf);
    HDfree(convbuf);
    num_errs += errors;
}

static void
test_nbit12(int32 fid)
{
    int32       aid1;
    uint16      ref1;
    int i;
    int32       ret;
    intn        errors = 0;
    model_info  m_info;
    comp_info   c_info;
    int32      *outbuf, *inbuf;
    int32       test_out, test_in;
    uint8      *convbuf;

    outbuf = (int32 *) HDmalloc(NBIT_SIZE12 * sizeof(int32));
    inbuf = (int32 *) HDmalloc(NBIT_SIZE12 * sizeof(int32));
    convbuf = (uint8 *) HDmalloc(NBIT_SIZE12 * (size_t)DFKNTsize(DFNT_INT32));

    for (i = 0; i < NBIT_SIZE12; i++)   /* fill with pseudo-random data */
        outbuf[i] = (((i * 300001) % ((int32) 4 * 256 * 256 * 256)) - ((int32) 2 * 256 * 256 * 256)) << ((NBIT_OFF10 - NBIT_BITS10) + 1);

    ref1 = Hnewref(fid);
    CHECK(ref1, 0, "Hnewref");

    MESSAGE(5, printf("Create a new element as a signed 32-bit n-bit element\n");
        );
    c_info.nbit.nt = DFNT_INT32;
    c_info.nbit.sign_ext = TRUE;
    c_info.nbit.fill_one = TRUE;
    c_info.nbit.start_bit = NBIT_OFF12;
    c_info.nbit.bit_len = NBIT_BITS12;
    aid1 = HCcreate(fid, NBIT_TAG12, ref1, COMP_MODEL_STDIO, &m_info,
                    COMP_CODE_NBIT, &c_info);
    CHECK(aid1, FAIL, "HCcreate");

    ret = DFKconvert(outbuf, convbuf, DFNT_INT32, NBIT_SIZE12, DFACC_WRITE, 0, 0);
    CHECK(ret, FAIL, "DFKconvert");

    ret = Hwrite(aid1, NBIT_SIZE12 * DFKNTsize(DFNT_INT32), convbuf);
    if (ret != NBIT_SIZE12 * DFKNTsize(DFNT_INT32))
      {
          fprintf(stderr, "ERROR(%d): Hwrite returned the wrong length: %d\n", __LINE__, (int) ret);
          HEprint(stdout, 0);
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Verifying data\n");
        );

    HDmemset(convbuf, 0, DFKNTsize(DFNT_INT32) * NBIT_SIZE12);

    ret = Hgetelement(fid, NBIT_TAG12, (uint16) ref1, convbuf);
    if (ret != NBIT_SIZE12 * DFKNTsize(DFNT_INT32))
      {
          HEprint(stderr, 0);
          fprintf(stderr, "ERROR: (%d) Hgetelement returned the wrong length: %d\n", __LINE__, (int) ret);
          errors++;
      }

    ret = DFKconvert(convbuf, inbuf, DFNT_INT32, NBIT_SIZE12, DFACC_READ, 0, 0);
    CHECK(ret, FAIL, "DFKconvert");

    for (i = 0; i < NBIT_SIZE12; i++)
      {
          test_out = (int32)(((uintn)outbuf[i] | (uintn)NBIT_MASK12A) & (uintn)NBIT_MASK12B);
          test_in = (int32)((uintn)inbuf[i] & (uintn)NBIT_MASK12B);
#ifndef TESTING
          if ((int32) test_in != (int32) test_out)
            {
                printf("test_nbit12: Wrong data at %d, out (%ld)%ld in (%ld)%ld\n", i, (long) outbuf[i], (long) test_out, (long) inbuf[i], (long) test_in);
                errors++;
            }
#else
          printf("data at %d, out (%d)%d in (%d)%d\n", i, outbuf[i], test_out, inbuf[i], test_in);
#endif
      }
    HDfree(outbuf);
    HDfree(inbuf);
    HDfree(convbuf);
    num_errs += errors;
}

void
test_nbit(void)
{
    int32       fid;
    int32       ret;

    MESSAGE(5, printf("Creating a file %s\n", TESTFILE_NAME);
        );
    fid = Hopen(TESTFILE_NAME, DFACC_CREATE, 0);
    CHECK(fid, FAIL, "Hopen");

    test_nbit1(fid);    /* basic uint8 test */
    test_nbit2(fid);    /* basic int8 test */
    test_nbit3(fid);    /* basic uint16 test */
    test_nbit4(fid);    /* basic int16 test */
    test_nbit5(fid);    /* basic uint32 test */
    test_nbit6(fid);    /* basic int32 test */

    test_nbit7(fid);    /* advanced uint8 with fill-ones test */
    test_nbit8(fid);    /* advanced int8 with fill-ones test */
    test_nbit9(fid);    /* advanced uint16 with fill-ones test */
    test_nbit10(fid);   /* advanced int16 with fill-ones test */
    test_nbit11(fid);   /* advanced uint32 with fill-ones test */
    test_nbit12(fid);   /* advanced int32 with fill-ones test */

    MESSAGE(5, printf("Closing the files\n");
        );
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");
}
