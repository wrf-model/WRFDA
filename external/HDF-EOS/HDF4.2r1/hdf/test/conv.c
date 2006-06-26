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

/* $Id: conv.c,v 1.23 1996/11/11 20:39:51 koziol Exp $ */

#ifdef RCSID
static char RcsId[] = "@(#)$Revision: 1.23 $";
#endif

/*
   FILE
   conv.c
   Test HDF Number-Type conversion routines

   REMARKS

   DESIGN

   BUGS/LIMITATIONS

   EXPORTED ROUTINES

   AUTHOR
   Quincey Koziol

   MODIFICATION HISTORY
   10/27/93 - Started coding.
 */

#ifdef UNICOS
#include <stdlib.h>
#endif
#include "tproto.h"
#include <time.h>
#ifdef I860
typedef int clock_t;
#define NO_TIMING
#define UINT_MAX USI_MAX
#endif /* I860 */

/* last ditch attempt do define this value... */
#ifndef UINT_MAX
#define UINT_MAX (unsigned)(-1)
#endif

/* Substitute bogus value if CLOCKS_PER_SEC is unavailable */
#ifndef CLOCKS_PER_SEC
#define CLOCKS_PER_SEC -1
#endif
#ifndef RAND_MAX
#define RAND_MAX (UINT_MAX)
#endif

#ifdef UNICOS
#define TEST_SIZE 1000001
#else
#if defined macintosh
#define TEST_SIZE 8001  /* so that 8*8000<=64K */
#else
#define TEST_SIZE 100001
#endif
#endif

#define SOURCE_STRIDE   2   /* space out the strides for source & dest. */
#define DEST_STRIDE     3

/* define aliases for random number generation */
#define RAND rand
#define SEED(a) srand((unsigned)(a))

#ifdef NO_TIMING
#define clock() (0)
#endif

extern int Verbocity;

/* Local variables */
static int32 test_type[] =
{0, DFNT_LITEND, DFNT_NATIVE};
static const char *test_name[] =
{"Big-Endian", "Little-Endian", "Native"};

/* for those machines with imprecise IEEE<-> conversions, this should be */
/* close enough */
#define EPS64          ((float64)1.0E-14)
#define EPS32          ((float32)1.0E-7)
void
test_conv(void)
{
    clock_t c1,c2,c3,c4;
    uint8 *src_uint8,
	  *dst_uint8,
	  *dst2_uint8;
    uint16 *src_uint16,
	  *dst_uint16,
	  *dst2_uint16;
    uint32 *src_uint32,
	  *dst_uint32,
	  *dst2_uint32;
    int8 *src_int8,
	  *dst_int8,
	  *dst2_int8;
    int16 *src_int16,
	  *dst_int16,
	  *dst2_int16;
    int32 *src_int32,
	  *dst_int32,
	  *dst2_int32;
    float32 *src_float32,
	  *dst_float32,
	  *dst2_float32;
    float64 *src_float64,
	  *dst_float64,
	  *dst2_float64;
    intn i,r;
    intn t;
    int32 ret;

    SEED((int)time(NULL));   /* seed with effectively random number */

    for(t=0; (size_t)t<sizeof(test_type)/sizeof(int32); t++) {
        MESSAGE(5,printf("Testing %s Number-Types\n",test_name[t]););
        MESSAGE(6,printf("seeding int8 array\n"););

        /* allocate arrays */
        src_int8=(int8 *)HDmalloc(TEST_SIZE*sizeof(int8));
        if(src_int8==NULL) {
        	CHECK(src_int8,NULL,"HDmalloc");
        	return;
          } /* end if */
        dst_int8=(int8 *)HDmalloc(TEST_SIZE*sizeof(int8));
        if(dst_int8==NULL) {
        	CHECK(dst_int8,NULL,"HDmalloc");
        	return;
          } /* end if */
        dst2_int8=(int8 *)HDmalloc(TEST_SIZE*sizeof(int8));
        if(dst2_int8==NULL) {
            CHECK(dst2_int8,NULL,"HDmalloc");
            return;
          } /* end if */

        /* Seed arrays with random values */
        for(i=0; i<TEST_SIZE; i++)
            src_int8[i]=(int8)(RAND()-RAND_MAX/2);

        MESSAGE(6,printf("converting %s int8 array\n",test_name[t]););
        c1=clock();
        ret=DFKconvert((VOIDP)src_int8,(VOIDP)dst_int8,test_type[t]|DFNT_INT8,TEST_SIZE,DFACC_WRITE,0,0);
        c2=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s int8 values\n",(int)(c2-c1),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t]););

        MESSAGE(6,printf("re-converting %s int8 array\n",test_name[t]););
        c3=clock();
        ret=DFKconvert((VOIDP)dst_int8,(VOIDP)dst2_int8,test_type[t]|DFNT_INT8,TEST_SIZE,DFACC_READ,0,0);
        c4=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s int8 values\n",(int)(c4-c3),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t]););
        if(HDmemcmp(src_int8,dst2_int8,TEST_SIZE*sizeof(int8))) {
            printf("Error converting %s int8 values!\n",test_name[t]);
            HEprint(stdout,0);
            num_errs++;
          } /* end if */

        /* clear arrays for next test */
        HDmemset(src_int8,0xae,TEST_SIZE*sizeof(int8));
        HDmemset(dst_int8,0xae,TEST_SIZE*sizeof(int8));
        HDmemset(dst2_int8,0xae,TEST_SIZE*sizeof(int8));
        /* Seed arrays with random values */
        for(i=0; i<TEST_SIZE; i+=SOURCE_STRIDE)
            src_int8[i]=(int8)(RAND()-RAND_MAX/2);

        MESSAGE(6,printf("converting %s int8 array with %d/%d stride\n",test_name[t],SOURCE_STRIDE,DEST_STRIDE););
        c1=clock();
        ret=DFKconvert((VOIDP)src_int8,(VOIDP)dst_int8,test_type[t]|DFNT_INT8,TEST_SIZE/4,DFACC_WRITE,SOURCE_STRIDE*sizeof(int8),DEST_STRIDE*sizeof(int8));
        c2=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s int8 values with %d/%d stride\n",(int)(c2-c1),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t],SOURCE_STRIDE,DEST_STRIDE););

        MESSAGE(6,printf("re-converting %s int8 array with %d/%d stride\n",test_name[t],SOURCE_STRIDE,DEST_STRIDE););
        c3=clock();
        ret=DFKconvert((VOIDP)dst_int8,(VOIDP)dst2_int8,test_type[t]|DFNT_INT8,TEST_SIZE/4,DFACC_READ,DEST_STRIDE*sizeof(int8),SOURCE_STRIDE*sizeof(int8));
        c4=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s int8 values with %d/%d stride\n",(int)(c4-c3),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t],DEST_STRIDE,SOURCE_STRIDE););
        if(HDmemcmp(src_int8,dst2_int8,(TEST_SIZE/2)*sizeof(int8))) {
            printf("Error converting %s int8 values with strides!\n",test_name[t]);
            HEprint(stdout,0);
            num_errs++;
          } /* end if */

        HDfree((VOIDP)src_int8);
        HDfree((VOIDP)dst_int8);
        HDfree((VOIDP)dst2_int8);

        MESSAGE(6,printf("seeding %s uint8 array\n",test_name[t]););
        src_uint8=(uint8 *)HDmalloc(TEST_SIZE*sizeof(uint8));
        if(src_uint8==NULL) {
            CHECK(src_uint8,NULL,"HDmalloc");
            return;
          } /* end if */
        dst_uint8=(uint8 *)HDmalloc(TEST_SIZE*sizeof(uint8));
        if(dst_uint8==NULL) {
            CHECK(dst_uint8,NULL,"HDmalloc");
            return;
          } /* end if */
        dst2_uint8=(uint8 *)HDmalloc(TEST_SIZE*sizeof(uint8));
        if(dst2_uint8==NULL) {
            CHECK(dst2_uint8,NULL,"HDmalloc");
            return;
          } /* end if */

        /* Seed arrays with random values */
        for(i=0; i<TEST_SIZE; i++)
            src_uint8[i]=(uint8)RAND();

        MESSAGE(6,printf("converting %s uint8 array\n",test_name[t]););
        c1=clock();
        ret=DFKconvert((VOIDP)src_uint8,(VOIDP)dst_uint8,test_type[t]|DFNT_UINT8,TEST_SIZE,DFACC_WRITE,0,0);
        c2=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s uint8 values\n",(int)(c2-c1),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t]););

        MESSAGE(6,printf("re-converting %s uint8 array\n",test_name[t]););
        c3=clock();
        ret=DFKconvert((VOIDP)dst_uint8,(VOIDP)dst2_uint8,test_type[t]|DFNT_UINT8,TEST_SIZE,DFACC_READ,0,0);
        c4=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s uint8 values\n",(int)(c4-c3),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t]););
        if(HDmemcmp(src_uint8,dst2_uint8,TEST_SIZE*sizeof(uint8))) {
            printf("Error converting %s uint8 values!\n",test_name[t]);
            HEprint(stdout,0);
            num_errs++;
          } /* end if */

        /* clear arrays for next test */
        HDmemset(src_uint8,0xae,TEST_SIZE*sizeof(uint8));
        HDmemset(dst_uint8,0xae,TEST_SIZE*sizeof(uint8));
        HDmemset(dst2_uint8,0xae,TEST_SIZE*sizeof(uint8));

        /* Seed arrays with random values */
        for(i=0; i<TEST_SIZE; i+=SOURCE_STRIDE)
            src_uint8[i]=(uint8)RAND();

        MESSAGE(6,printf("converting %s uint8 array with %d/%d stride\n",test_name[t],SOURCE_STRIDE,DEST_STRIDE););
        c1=clock();
        ret=DFKconvert((VOIDP)src_uint8,(VOIDP)dst_uint8,test_type[t]|DFNT_UINT8,TEST_SIZE/4,DFACC_WRITE,SOURCE_STRIDE*sizeof(uint8),DEST_STRIDE*sizeof(uint8));
        c2=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s uint8 values with %d/%d stride\n",(int)(c2-c1),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t],SOURCE_STRIDE,DEST_STRIDE););

        MESSAGE(6,printf("re-converting %s uint8 array with %d/%d stride\n",test_name[t],DEST_STRIDE,SOURCE_STRIDE););
        c3=clock();
        ret=DFKconvert((VOIDP)dst_uint8,(VOIDP)dst2_uint8,test_type[t]|DFNT_UINT8,TEST_SIZE/4,DFACC_READ,DEST_STRIDE*sizeof(uint8),SOURCE_STRIDE*sizeof(uint8));
        c4=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s uint8 values with %d/%d stride\n",(int)(c4-c3),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t],DEST_STRIDE,SOURCE_STRIDE););
        if(HDmemcmp(src_uint8,dst2_uint8,(TEST_SIZE/2)*sizeof(uint8))) {
            printf("Error converting %s uint8 values with strides!\n",test_name[t]);
            HEprint(stdout,0);
            num_errs++;
          } /* end if */

        HDfree((VOIDP)src_uint8);
        HDfree((VOIDP)dst_uint8);
        HDfree((VOIDP)dst2_uint8);

        MESSAGE(6,printf("seeding %s int16 array\n",test_name[t]););
        src_int16=(int16 *)HDmalloc(TEST_SIZE*sizeof(int16));
        if(src_int16==NULL) {
            CHECK(src_int16,NULL,"HDmalloc");
            return;
          } /* end if */
        dst_int16=(int16 *)HDmalloc(TEST_SIZE*sizeof(int16));
        if(dst_int16==NULL) {
            CHECK(dst_int16,NULL,"HDmalloc");
            return;
          } /* end if */
        dst2_int16=(int16 *)HDmalloc(TEST_SIZE*sizeof(int16));
        if(dst2_int16==NULL) {
            CHECK(dst2_int16,NULL,"HDmalloc");
            return;
          } /* end if */

        /* Seed arrays with random values */
        for(i=0; i<TEST_SIZE; i++)
            src_int16[i]=(int16)(RAND()-RAND_MAX/2);

        MESSAGE(6,printf("converting %s int16 array\n",test_name[t]););
        c1=clock();
        ret=DFKconvert((VOIDP)src_int16,(VOIDP)dst_int16,test_type[t]|DFNT_INT16,TEST_SIZE,DFACC_WRITE,0,0);
        c2=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s int16 values\n",(int)(c2-c1),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t]););

        MESSAGE(6,printf("re-converting %s int16 array\n",test_name[t]););
        c3=clock();
        ret=DFKconvert((VOIDP)dst_int16,(VOIDP)dst2_int16,test_type[t]|DFNT_INT16,TEST_SIZE,DFACC_READ,0,0);
        c4=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s int16 values\n",(int)(c4-c3),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t]););
        if(HDmemcmp(src_int16,dst2_int16,TEST_SIZE*sizeof(int16))) {
            printf("Error converting %s int16 values!\n",test_name[t]);
            HEprint(stdout,0);
            num_errs++;
          } /* end if */

        /* clear arrays for next test */
        HDmemset(src_int16,0xae,TEST_SIZE*sizeof(int16));
        HDmemset(dst_int16,0xae,TEST_SIZE*sizeof(int16));
        HDmemset(dst2_int16,0xae,TEST_SIZE*sizeof(int16));
        /* Seed arrays with random values */
        for(i=0; i<TEST_SIZE; i+=SOURCE_STRIDE)
            src_int16[i]=(int16)(RAND()-RAND_MAX/2);

        MESSAGE(6,printf("converting %s int16 array with %d/%d stride\n",test_name[t],SOURCE_STRIDE,DEST_STRIDE););
        c1=clock();
        ret=DFKconvert((VOIDP)src_int16,(VOIDP)dst_int16,test_type[t]|DFNT_INT16,TEST_SIZE/4,DFACC_WRITE,SOURCE_STRIDE*sizeof(int16),DEST_STRIDE*sizeof(int16));
        c2=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s int16 values with %d/%d stride\n",(int)(c2-c1),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t],SOURCE_STRIDE,DEST_STRIDE););

        MESSAGE(6,printf("re-converting %s int16 array with %d/%d stride\n",test_name[t],DEST_STRIDE,SOURCE_STRIDE););
        c3=clock();
        ret=DFKconvert((VOIDP)dst_int16,(VOIDP)dst2_int16,test_type[t]|DFNT_INT16,TEST_SIZE/4,DFACC_READ,DEST_STRIDE*sizeof(int16),SOURCE_STRIDE*sizeof(int16));
        c4=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s int16 values with %d/%d stride\n",(int)(c4-c3),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t],DEST_STRIDE,SOURCE_STRIDE););
        if(HDmemcmp(src_int16,dst2_int16,(TEST_SIZE/2)*sizeof(int16))) {
            printf("Error converting %s int16 values with strides!\n",test_name[t]);
            HEprint(stdout,0);
            num_errs++;
          } /* end if */

        HDfree((VOIDP)src_int16);
        HDfree((VOIDP)dst_int16);
        HDfree((VOIDP)dst2_int16);

        MESSAGE(6,printf("seeding %s uint16 array\n",test_name[t]););
        src_uint16=(uint16 *)HDmalloc(TEST_SIZE*sizeof(uint16));
        if(src_uint16==NULL) {
            CHECK(src_uint16,NULL,"HDmalloc");
            return;
          } /* end if */
        dst_uint16=(uint16 *)HDmalloc(TEST_SIZE*sizeof(uint16));
        if(dst_uint16==NULL) {
            CHECK(dst_uint16,NULL,"HDmalloc");
            return;
          } /* end if */
        dst2_uint16=(uint16 *)HDmalloc(TEST_SIZE*sizeof(uint16));
        if(dst2_uint16==NULL) {
            CHECK(dst2_uint16,NULL,"HDmalloc");
            return;
          } /* end if */

        /* Seed arrays with random values */
        for(i=0; i<TEST_SIZE; i++)
            src_uint16[i]=(uint16)RAND();

        MESSAGE(6,printf("converting %s uint16 array\n",test_name[t]););
        c1=clock();
        ret=DFKconvert((VOIDP)src_uint16,(VOIDP)dst_uint16,test_type[t]|DFNT_UINT16,TEST_SIZE,DFACC_WRITE,0,0);
        c2=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s uint16 values\n",(int)(c2-c1),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t]););

        MESSAGE(6,printf("re-converting %s uint16 array\n",test_name[t]););
        c3=clock();
        ret=DFKconvert((VOIDP)dst_uint16,(VOIDP)dst2_uint16,test_type[t]|DFNT_UINT16,TEST_SIZE,DFACC_READ,0,0);
        c4=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s uint16 values\n",(int)(c4-c3),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t]););
        if(HDmemcmp(src_uint16,dst2_uint16,TEST_SIZE*sizeof(uint16))) {
            printf("Error converting %s uint16 values!\n",test_name[t]);
            HEprint(stdout,0);
            num_errs++;
          } /* end if */

        /* clear arrays for next test */
        HDmemset(src_uint16,0xae,TEST_SIZE*sizeof(uint16));
        HDmemset(dst_uint16,0xae,TEST_SIZE*sizeof(uint16));
        HDmemset(dst2_uint16,0xae,TEST_SIZE*sizeof(uint16));
        /* Seed arrays with random values */
        for(i=0; i<TEST_SIZE; i+=SOURCE_STRIDE)
            src_uint16[i]=(uint16)RAND();

        MESSAGE(6,printf("converting %s uint16 array with %d/%d stride\n",test_name[t],SOURCE_STRIDE,DEST_STRIDE););
        c1=clock();
        ret=DFKconvert((VOIDP)src_uint16,(VOIDP)dst_uint16,test_type[t]|DFNT_UINT16,TEST_SIZE/4,DFACC_WRITE,SOURCE_STRIDE*sizeof(uint16),DEST_STRIDE*sizeof(uint16));
        c2=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s uint16 values with %d/%d stride\n",(int)(c2-c1),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t],SOURCE_STRIDE,DEST_STRIDE););

        MESSAGE(6,printf("re-converting %s uint16 array with %d/%d stride\n",test_name[t],DEST_STRIDE,SOURCE_STRIDE););
        c3=clock();
        ret=DFKconvert((VOIDP)dst_uint16,(VOIDP)dst2_uint16,test_type[t]|DFNT_UINT16,TEST_SIZE/4,DFACC_READ,DEST_STRIDE*sizeof(uint16),SOURCE_STRIDE*sizeof(uint16));
        c4=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s uint16 values with %d/%d stride\n",(int)(c4-c3),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t],DEST_STRIDE,SOURCE_STRIDE););
        if(HDmemcmp(src_uint16,dst2_uint16,(TEST_SIZE/2)*sizeof(uint16))) {
            printf("Error converting %s uint16 values with strides!\n",test_name[t]);
            HEprint(stdout,0);
            num_errs++;
          } /* end if */

        HDfree((VOIDP)src_uint16);
        HDfree((VOIDP)dst_uint16);
        HDfree((VOIDP)dst2_uint16);

        MESSAGE(6,printf("seeding %s int32 array\n",test_name[t]););
        src_int32=(int32 *)HDmalloc(TEST_SIZE*sizeof(int32));
        if(src_int32==NULL) {
            CHECK(src_int32,NULL,"HDmalloc");
            return;
          } /* end if */
        dst_int32=(int32 *)HDmalloc(TEST_SIZE*sizeof(int32));
        if(dst_int32==NULL) {
            CHECK(dst_int32,NULL,"HDmalloc");
            return;
          } /* end if */
        dst2_int32=(int32 *)HDmalloc(TEST_SIZE*sizeof(int32));
        if(dst2_int32==NULL) {
            CHECK(dst2_int32,NULL,"HDmalloc");
            return;
          } /* end if */

        /* Seed arrays with random values */
        for(i=0; i<TEST_SIZE; i++)
            src_int32[i]=(int32)(RAND()-RAND_MAX/2);

        MESSAGE(6,printf("converting %s int32 array\n",test_name[t]););
        c1=clock();
        ret=DFKconvert((VOIDP)src_int32,(VOIDP)dst_int32,test_type[t]|DFNT_INT32,TEST_SIZE,DFACC_WRITE,0,0);
        c2=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s int32 values\n",(int)(c2-c1),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t]););

        MESSAGE(6,printf("re-converting %s int32 array\n",test_name[t]););
        c3=clock();
        ret=DFKconvert((VOIDP)dst_int32,(VOIDP)dst2_int32,test_type[t]|DFNT_INT32,TEST_SIZE,DFACC_READ,0,0);
        c4=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s int32 values\n",(int)(c4-c3),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t]););
        if(HDmemcmp(src_int32,dst2_int32,TEST_SIZE*sizeof(int32))) {
            printf("Error converting %s int32 values!\n",test_name[t]);
            HEprint(stdout,0);
            num_errs++;
          } /* end if */

        /* clear arrays for next test */
        HDmemset(src_int32,0xae,TEST_SIZE*sizeof(int32));
        HDmemset(dst_int32,0xae,TEST_SIZE*sizeof(int32));
        HDmemset(dst2_int32,0xae,TEST_SIZE*sizeof(int32));
        /* Seed arrays with random values */
        for(i=0; i<TEST_SIZE; i+=SOURCE_STRIDE)
            src_int32[i]=(int32)(RAND()-RAND_MAX/2);

        MESSAGE(6,printf("converting %s int32 array with %d/%d stride\n",test_name[t],SOURCE_STRIDE,DEST_STRIDE););
        c1=clock();
        ret=DFKconvert((VOIDP)src_int32,(VOIDP)dst_int32,test_type[t]|DFNT_INT32,TEST_SIZE/4,DFACC_WRITE,SOURCE_STRIDE*sizeof(int32),DEST_STRIDE*sizeof(int32));
        c2=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s int32 values with %d/%d stride\n",(int)(c2-c1),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t],SOURCE_STRIDE,DEST_STRIDE););

        MESSAGE(6,printf("re-converting %s int32 array with %d/%d stride\n",test_name[t],DEST_STRIDE,SOURCE_STRIDE););
        c3=clock();
        ret=DFKconvert((VOIDP)dst_int32,(VOIDP)dst2_int32,test_type[t]|DFNT_INT32,TEST_SIZE/4,DFACC_READ,DEST_STRIDE*sizeof(int32),SOURCE_STRIDE*sizeof(int32));
        c4=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s int32 values with %d/%d stride\n",(int)(c4-c3),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t],DEST_STRIDE,SOURCE_STRIDE););
        if(HDmemcmp(src_int32,dst2_int32,(TEST_SIZE/2)*sizeof(int32))) {
            printf("Error converting %s int32 values with strides!\n",test_name[t]);
            HEprint(stdout,0);
            num_errs++;
          } /* end if */

        HDfree((VOIDP)src_int32);
        HDfree((VOIDP)dst_int32);
        HDfree((VOIDP)dst2_int32);

        MESSAGE(6,printf("seeding %s uint32 array\n",test_name[t]););
        src_uint32=(uint32 *)HDmalloc(TEST_SIZE*sizeof(uint32));
        if(src_uint32==NULL) {
            CHECK(src_uint32,NULL,"HDmalloc");
            return;
          } /* end if */
        dst_uint32=(uint32 *)HDmalloc(TEST_SIZE*sizeof(uint32));
        if(dst_uint32==NULL) {
            CHECK(dst_uint32,NULL,"HDmalloc");
            return;
          } /* end if */
        dst2_uint32=(uint32 *)HDmalloc(TEST_SIZE*sizeof(uint32));
        if(dst2_uint32==NULL) {
            CHECK(dst2_uint32,NULL,"HDmalloc");
            return;
          } /* end if */

        /* Seed arrays with random values */
        for(i=0; i<TEST_SIZE; i++)
            src_uint32[i]=(uint32)RAND();

        MESSAGE(6,printf("converting %s uint32 array\n",test_name[t]););
        c1=clock();
        ret=DFKconvert((VOIDP)src_uint32,(VOIDP)dst_uint32,test_type[t]|DFNT_UINT32,TEST_SIZE,DFACC_WRITE,0,0);
        c2=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s uint32 values\n",(int)(c2-c1),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t]););

        MESSAGE(6,printf("re-converting %s uint32 array\n",test_name[t]););
        c3=clock();
        ret=DFKconvert((VOIDP)dst_uint32,(VOIDP)dst2_uint32,test_type[t]|DFNT_UINT32,TEST_SIZE,DFACC_READ,0,0);
        c4=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s uint32 values\n",(int)(c4-c3),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t]););
        if(HDmemcmp(src_uint32,dst2_uint32,TEST_SIZE*sizeof(uint32))) {
            printf("Error converting %s uint32 values!\n",test_name[t]);
            HEprint(stdout,0);
            num_errs++;
          } /* end if */

        /* clear arrays for next test */
        HDmemset(src_uint32,0xae,TEST_SIZE*sizeof(uint32));
        HDmemset(dst_uint32,0xae,TEST_SIZE*sizeof(uint32));
        HDmemset(dst2_uint32,0xae,TEST_SIZE*sizeof(uint32));
        /* Seed arrays with random values */
        for(i=0; i<TEST_SIZE; i+=SOURCE_STRIDE)
            src_uint32[i]=(uint32)RAND();

        MESSAGE(6,printf("converting %s uint32 array with %d/%d stride\n",test_name[t],SOURCE_STRIDE,DEST_STRIDE););
        c1=clock();
        ret=DFKconvert((VOIDP)src_uint32,(VOIDP)dst_uint32,test_type[t]|DFNT_UINT32,TEST_SIZE/4,DFACC_WRITE,SOURCE_STRIDE*sizeof(uint32),DEST_STRIDE*sizeof(uint32));
        c2=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s uint32 values with %d/%d stride\n",(int)(c2-c1),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t],SOURCE_STRIDE,DEST_STRIDE););

        MESSAGE(6,printf("re-converting %s uint32 array with %d/%d stride\n",test_name[t],DEST_STRIDE,SOURCE_STRIDE););
        c3=clock();
        ret=DFKconvert((VOIDP)dst_uint32,(VOIDP)dst2_uint32,test_type[t]|DFNT_UINT32,TEST_SIZE/4,DFACC_READ,DEST_STRIDE*sizeof(uint32),SOURCE_STRIDE*sizeof(uint32));
        c4=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s uint32 values with %d/%d stride\n",(int)(c4-c3),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t],DEST_STRIDE,SOURCE_STRIDE););
        if(HDmemcmp(src_uint32,dst2_uint32,(TEST_SIZE/2)*sizeof(uint32))) {
            printf("Error converting %s uint32 values with strides!\n",test_name[t]);
            HEprint(stdout,0);
            num_errs++;
          } /* end if */

        HDfree((VOIDP)src_uint32);
        HDfree((VOIDP)dst_uint32);
        HDfree((VOIDP)dst2_uint32);

        MESSAGE(6,printf("seeding %s float32 array\n",test_name[t]););
        src_float32=(float32 *)HDmalloc(TEST_SIZE*sizeof(float32));
        if(src_float32==NULL) {
            CHECK(src_float32,NULL,"HDmalloc");
            return;
          } /* end if */
        dst_float32=(float32 *)HDmalloc(TEST_SIZE*sizeof(float32));
        if(dst_float32==NULL) {
            CHECK(dst_float32,NULL,"HDmalloc");
            return;
          } /* end if */
        dst2_float32=(float32 *)HDmalloc(TEST_SIZE*sizeof(float32));
        if(dst2_float32==NULL) {
            CHECK(dst2_float32,NULL,"HDmalloc");
            return;
          } /* end if */

        /* Seed arrays with random values */
        for(i=0; i<TEST_SIZE; i++) {
            src_float32[i]=(float32)(RAND()-RAND_MAX/2);
            while((r=RAND())==0) /* don't divide by zero */
                r=RAND();
            src_float32[i]/=(float32)r;
          } /* end for */

        MESSAGE(6,printf("converting %s float32 array\n",test_name[t]););
        c1=clock();
        ret=DFKconvert((VOIDP)src_float32,(VOIDP)dst_float32,test_type[t]|DFNT_FLOAT32,TEST_SIZE,DFACC_WRITE,0,0);
        c2=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s float32 values\n",(int)(c2-c1),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t]););

        MESSAGE(6,printf("re-converting %s float32 array\n",test_name[t]););
        c3=clock();
        ret=DFKconvert((VOIDP)dst_float32,(VOIDP)dst2_float32,test_type[t]|DFNT_FLOAT32,TEST_SIZE,DFACC_READ,0,0);
        c4=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s float32 values\n",(int)(c4-c3),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t]););

/* This amazing hack is because of the way the Cray converts numbers. */
/*  The converted number are going to have to be checked by hand... */
#if defined UNICOS | defined VP | (defined __alpha & !defined (__unix))
#ifdef OLD_WAY
        if(Verbocity>9) {
            intn i;
            uint8 *u8_s=(uint8 *)src_float32,
	        *u8_d=(uint8 *)dst_float32,
	        *u8_d2=(uint8 *)dst2_float32;

            printf("src_float32:  ");
            for(i=0; i<80; i++)
              printf("%.2x ",u8_s[i]);
            printf("\ndst_float32:  ");
            for(i=0; i<80; i++)
              printf("%.2x ",u8_d[i]);
            printf("\ndst2_float32: ");
            for(i=0; i<80; i++)
              printf("%.2x ",u8_d2[i]);
            printf("\n");
        }
#else /* OLD_WAY */
	for(i=0; i<TEST_SIZE; i++) {
	    if(abs(dst2_float32[i]-src_float32[i])> abs(src_float32[i]*EPS32)) {
              printf("Error converting %s float32 values!\n",test_name[t]);
printf("src[%d]=%lf, dst[%d]=%lf, dst2[%d]=%lf\n",i,src_float32[i],i,dst_float32[i],i,dst2_float32[i]);
{
            intn j;
            uint8 *u8_s=(uint8 *)&src_float32[i],
	        *u8_d=(uint8 *)&dst_float32[i],
	        *u8_d2=(uint8 *)&dst2_float32[i];

            printf("src_float32:  ");
            for(j=0; j<sizeof(float32); j++)
              printf("%.2x ",u8_s[j]);
            printf("\ndst_float32:  ");
            for(j=0; j<sizeof(float32); j++)
              printf("%.2x ",u8_d[j]);
            printf("\ndst2_float32: ");
            for(j=0; j<sizeof(float32); j++)
              printf("%.2x ",u8_d2[j]);
            printf("\n");
}
              HEprint(stdout,0);
              num_errs++;
            } /* end if */
	  } /* end for */
#endif /* OLD_WAY */
#else
        if(HDmemcmp(src_float32,dst2_float32,TEST_SIZE*sizeof(float32))) {
            printf("Error converting %s float32 values!\n",test_name[t]);
            HEprint(stdout,0);
            num_errs++;
          } /* end if */
#endif

        /* clear arrays for next test */
        HDmemset(src_float32,0xae,TEST_SIZE*sizeof(float32));
        HDmemset(dst_float32,0xae,TEST_SIZE*sizeof(float32));
        HDmemset(dst2_float32,0xae,TEST_SIZE*sizeof(float32));
        /* Seed arrays with random values */
        for(i=0; i<TEST_SIZE; i+=SOURCE_STRIDE) {
            src_float32[i]=(float32)(RAND()-RAND_MAX/2);
            while((r=RAND())==0) /* don't divide by zero */
                r=RAND();
            src_float32[i]/=(float32)r;
          } /* end for */

        MESSAGE(6,printf("converting %s float32 array with %d/%d stride\n",test_name[t],SOURCE_STRIDE,DEST_STRIDE););
        c1=clock();
        ret=DFKconvert((VOIDP)src_float32,(VOIDP)dst_float32,test_type[t]|DFNT_FLOAT32,TEST_SIZE/4,DFACC_WRITE,SOURCE_STRIDE*sizeof(float32),DEST_STRIDE*sizeof(float32));
        c2=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s float32 values with %d/%d stride\n",(int)(c2-c1),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t],SOURCE_STRIDE,DEST_STRIDE););

        MESSAGE(6,printf("re-converting %s float32 array with %d/%d stride\n",test_name[t],DEST_STRIDE,SOURCE_STRIDE););
        c3=clock();
        ret=DFKconvert((VOIDP)dst_float32,(VOIDP)dst2_float32,test_type[t]|DFNT_FLOAT32,TEST_SIZE/4,DFACC_READ,DEST_STRIDE*sizeof(float32),SOURCE_STRIDE*sizeof(float32));
        c4=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s float32 values with %d/%d stride\n",(int)(c4-c3),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t],DEST_STRIDE,SOURCE_STRIDE););

/* This amazing hack is because of the way the Cray converts numbers. */
/*  The converted number are going to have to be checked by hand... */
#if defined UNICOS | defined VP | defined CONVEXNATIVE
#ifdef OLD_WAY
        if(Verbocity>9) {
            intn i;
            uint8 *u8_s=(uint8 *)src_float32,
	        *u8_d=(uint8 *)dst_float32,
	        *u8_d2=(uint8 *)dst2_float32;

            printf("src_float32:  ");
            for(i=0; i<80; i++)
              printf("%.2x ",u8_s[i]);
            printf("\ndst_float32:  ");
            for(i=0; i<80; i++)
              printf("%.2x ",u8_d[i]);
            printf("\ndst2_float32: ");
            for(i=0; i<80; i++)
              printf("%.2x ",u8_d2[i]);
            printf("\n");
        }
#else /* OLD_WAY */
	for(i=0; i<(TEST_SIZE/2); i++) {
            if(abs(dst2_float32[i]-src_float32[i])>abs(src_float32[i]*EPS32)) {
              printf("Error converting %s float32 values!\n",test_name[t]);
printf("src[%d]=%lf, dst[%d]=%lf, dst2[%d]=%lf\n",i,src_float32[i],i,dst_float32[i],i,dst2_float32[i]);
{
            intn j;
            uint8 *u8_s=(uint8 *)&src_float32[i],
	        *u8_d=(uint8 *)&dst_float32[i],
	        *u8_d2=(uint8 *)&dst2_float32[i];

            printf("src_float32:  ");
            for(j=0; j<sizeof(float32); j++)
              printf("%.2x ",u8_s[j]);
            printf("\ndst_float32:  ");
            for(j=0; j<sizeof(float32); j++)
              printf("%.2x ",u8_d[j]);
            printf("\ndst2_float32: ");
            for(j=0; j<sizeof(float32); j++)
              printf("%.2x ",u8_d2[j]);
            printf("\n");
}
              HEprint(stdout,0);
              num_errs++;
            } /* end if */
	  } /* end for */
#endif /* OLD_WAY */
#else
        if(HDmemcmp(src_float32,dst2_float32,(TEST_SIZE/2)*sizeof(float32))) {
            printf("Error converting %s float32 values with strides!\n",test_name[t]);
            HEprint(stdout,0);
            num_errs++;
          } /* end if */
#endif

        HDfree((VOIDP)src_float32);
        HDfree((VOIDP)dst_float32);
        HDfree((VOIDP)dst2_float32);

        MESSAGE(6,printf("seeding %s float64 array\n",test_name[t]););
        src_float64=(float64 *)HDmalloc(TEST_SIZE*sizeof(float64));
        if(src_float64==NULL) {
            CHECK(src_float64,NULL,"HDmalloc");
            return;
          } /* end if */
        dst_float64=(float64 *)HDmalloc(TEST_SIZE*sizeof(float64));
        if(dst_float64==NULL) {
            CHECK(dst_float64,NULL,"HDmalloc");
            return;
          } /* end if */
        dst2_float64=(float64 *)HDmalloc(TEST_SIZE*sizeof(float64));
        if(dst2_float64==NULL) {
            CHECK(dst2_float64,NULL,"HDmalloc");
            return;
          } /* end if */

        /* Seed arrays with random values */
        for(i=0; i<TEST_SIZE; i++) {
            src_float64[i]=(float64)(RAND()-RAND_MAX/2);
            while((r=RAND())==0) /* don't divide by zero */
                r=RAND();
            src_float64[i]/=(float64)r;
          } /* end for */

        MESSAGE(6,printf("converting %s float64 array\n",test_name[t]););
        c1=clock();
        ret=DFKconvert((VOIDP)src_float64,(VOIDP)dst_float64,test_type[t]|DFNT_FLOAT64,TEST_SIZE,DFACC_WRITE,0,0);
        c2=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s float64 values\n",(int)(c2-c1),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t]););

        MESSAGE(6,printf("re-converting %s float64 array\n",test_name[t]););
        c3=clock();
        ret=DFKconvert((VOIDP)dst_float64,(VOIDP)dst2_float64,test_type[t]|DFNT_FLOAT64,TEST_SIZE,DFACC_READ,0,0);
        c4=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s float64 values\n",(int)(c4-c3),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t]););
/* This amazing hack is because of the way the VMS converts numbers. */
/*  The converted number are going to have to be checked by hand... */
#if defined VP | defined VMS | defined CONVEXNATIVE
#ifdef OLD_WAY
        if(Verbocity>9) {
            intn i;
            uint8 *u8_s=(uint8 *)src_float64,
	        *u8_d=(uint8 *)dst_float64,
	        *u8_d2=(uint8 *)dst2_float64;

            printf("src_float64:  ");
            for(i=0; i<80; i++)
              printf("%.2x ",u8_s[i]);
            printf("\ndst_float64:  ");
            for(i=0; i<80; i++)
              printf("%.2x ",u8_d[i]);
            printf("\ndst2_float64: ");
            for(i=0; i<80; i++)
              printf("%.2x ",u8_d2[i]);
            printf("\n");
        }
#else /* OLD_WAY */
	for(i=0; i<TEST_SIZE; i++) {
	    if(abs(dst2_float64[i]-src_float64[i]) >
		abs(src_float64[i]*EPS64)) {
             printf("Error converting %s float64 values!\n",test_name[t]);
printf("src[%d]=%lf, dst[%d]=%lf, dst2[%d]=%lf\n",i,src_float64[i],i,dst_float64[i],i,dst2_float64[i]);
{
            intn j;
            uint8 *u8_s=(uint8 *)&src_float64[i],
	        *u8_d=(uint8 *)&dst_float64[i],
	        *u8_d2=(uint8 *)&dst2_float64[i];

            printf("src_float64:  ");
            for(j=0; j<sizeof(float64); j++)
              printf("%.2x ",u8_s[j]);
            printf("\ndst_float64:  ");
            for(j=0; j<sizeof(float64); j++)
              printf("%.2x ",u8_d[j]);
            printf("\ndst2_float64: ");
            for(j=0; j<sizeof(float64); j++)
              printf("%.2x ",u8_d2[j]);
            printf("\n");
}
              HEprint(stdout,0);
              num_errs++;
            } /* end if */
	  } /* end for */
#endif /* OLD_WAY */
#else
        if(HDmemcmp(src_float64,dst2_float64,TEST_SIZE*sizeof(float64))) {
            printf("Error converting %s float64 values!\n",test_name[t]);
            HEprint(stdout,0);
            num_errs++;
          } /* end if */
#endif /* VMS */

        /* clear arrays for next test */
        HDmemset(src_float64,0xae,TEST_SIZE*sizeof(float64));
        HDmemset(dst_float64,0xae,TEST_SIZE*sizeof(float64));
        HDmemset(dst2_float64,0xae,TEST_SIZE*sizeof(float64));
        /* Seed arrays with random values */
        for(i=0; i<TEST_SIZE; i+=SOURCE_STRIDE) {
            src_float64[i]=(float64)(RAND()-RAND_MAX/2);
            while((r=RAND())==0) /* don't divide by zero */
                r=RAND();
            src_float64[i]/=(float64)r;
          } /* end for */

        MESSAGE(6,printf("converting %s float64 array with %d/%d stride\n",test_name[t],SOURCE_STRIDE,DEST_STRIDE););
        c1=clock();
        ret=DFKconvert((VOIDP)src_float64,(VOIDP)dst_float64,test_type[t]|DFNT_FLOAT64,TEST_SIZE/4,DFACC_WRITE,SOURCE_STRIDE*sizeof(float64),DEST_STRIDE*sizeof(float64));
        c2=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s float64 values with %d/%d stride\n",(int)(c2-c1),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t],SOURCE_STRIDE,DEST_STRIDE););

        MESSAGE(6,printf("re-converting %s float64 array with %d/%d stride\n",test_name[t],DEST_STRIDE,SOURCE_STRIDE););
        c3=clock();
        ret=DFKconvert((VOIDP)dst_float64,(VOIDP)dst2_float64,test_type[t]|DFNT_FLOAT64,TEST_SIZE/4,DFACC_READ,DEST_STRIDE*sizeof(float64),SOURCE_STRIDE*sizeof(float64));
        c4=clock();
        RESULT("DFKconvert");
        MESSAGE(6,printf("%d/%d seconds to convert %d %s float64 values with %d/%d stride\n",(int)(c4-c3),(int)CLOCKS_PER_SEC,(int)TEST_SIZE,test_name[t],DEST_STRIDE,SOURCE_STRIDE););
/* This amazing hack is because of the way the VMS converts numbers. */
/*  The converted number are going to have to be checked by hand... */
#if defined VP | defined VMS | defined CONVEXNATIVE
#ifdef OLD_WAY
        if(Verbocity>9) {
            intn i;
            uint8 *u8_s=(uint8 *)src_float64,
	        *u8_d=(uint8 *)dst_float64,
	        *u8_d2=(uint8 *)dst2_float64;

            printf("src_float64:  ");
            for(i=0; i<80; i++)
              printf("%.2x ",u8_s[i]);
            printf("\ndst_float64:  ");
            for(i=0; i<80; i++)
              printf("%.2x ",u8_d[i]);
            printf("\ndst2_float64: ");
            for(i=0; i<80; i++)
              printf("%.2x ",u8_d2[i]);
            printf("\n");
        }
#else /* OLD_WAY */
	for(i=0; i<(TEST_SIZE/2); i++) {
            if(abs(dst2_float64[i]-src_float64[i]) >
		 abs(src_float64[i]*EPS64)) {
              printf("Error converting %s float64 values!\n",test_name[t]);
printf("src[%d]=%lf, dst[%d]=%lf, dst2[%d]=%lf\n",i,src_float64[i],i,dst_float64[i],i,dst2_float64[i]);
{
            intn j;
            uint8 *u8_s=(uint8 *)&src_float64[i],
	        *u8_d=(uint8 *)&dst_float64[i],
	        *u8_d2=(uint8 *)&dst2_float64[i];

            printf("src_float64:  ");
            for(j=0; j<sizeof(float64); j++)
              printf("%.2x ",u8_s[j]);
            printf("\ndst_float64:  ");
            for(j=0; j<sizeof(float64); j++)
              printf("%.2x ",u8_d[j]);
            printf("\ndst2_float64: ");
            for(j=0; j<sizeof(float64); j++)
              printf("%.2x ",u8_d2[j]);
            printf("\n");
}
              HEprint(stdout,0);
              num_errs++;
            } /* end if */
	  } /* end for */
#endif /* OLD_WAY */
#else
        if(HDmemcmp(src_float64,dst2_float64,(TEST_SIZE/2)*sizeof(float64))) {
            printf("Error converting %s float64 values with strides!\n",test_name[t]);
            HEprint(stdout,0);
            num_errs++;
          } /* end if */
#endif

        HDfree((VOIDP)src_float64);
        HDfree((VOIDP)dst_float64);
        HDfree((VOIDP)dst2_float64);
      } /* end for */

}   /* end test_conv() */
