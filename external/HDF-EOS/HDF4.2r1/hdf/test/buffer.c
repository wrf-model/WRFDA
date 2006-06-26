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
    FILE - buffer.c
        Test HDF buffered data I/O routines

    DESIGN
        - Create a new data element and get a benchmark time for reading it in
            various ways.
        - Buffer the element and get times for the buffered element.
        - Make a new external element and get benchmark times
        - Buffer the external element and get times for the buffered element.
        - Make a new compressed element and get benchmark times
        - Buffer the compressed element and get times for the buffered element.
        - Make a new linked block element and get benchmark times
        - Buffer the linked block element and get times for the buffered element.
    
    BUGS/LIMITATIONS

    EXPORTED ROUTINES

    AUTHOR
        Quincey Koziol

    MODIFICATION HISTORY
        12/11/98 - Wrote tests
 */

/* $Id: buffer.c,v 1.6 2000/08/29 13:55:43 koziol Exp $ */

#ifdef RCSID
static char RcsId[] = "@(#)$Revision: 1.6 $";
#endif

#include <time.h>
#include "tproto.h"
#include "hfile.h"

/* Substitute bogus value if CLOCKS_PER_SEC is unavailable */
#ifndef CLOCKS_PER_SEC
#define CLOCKS_PER_SEC -1
#endif

#define TESTFILE_NAME "tbuffer.hdf"
#define EXTFILE_NAME "tbuffer.dat"

/* Size of data elements to create */
#define ELEMSIZE 16384

/* define aliases for random number generation */
#define RAND rand
#define SEED(a) srand((unsigned)(a))

/* Tag to use for creating the test elements */
#define BUFF_TAG 1000

/* Number of tests */
#define NUM_TESTS       4

/* Number of timing tests to run */
/* 0 - read/write entire buffer in one I/O operation */
/* 1 - read/write entire buffer one byte at a time forwards */
/* 2 - read/write entire buffer one byte at a time every other byte forwards */
/* 3 - read/write entire buffer one byte at a time backwards */
/* 4 - read/write entire buffer one byte at a time every other byte backwards */
#define NUM_TIMINGS     5
clock_t read_time[NUM_TESTS][2];    /* 0 is unbuffered, 1 is buffered */
clock_t write_time[NUM_TESTS][2];   /* 0 is unbuffered, 1 is buffered */

/* I/O buffers */
uint8 out_buf[ELEMSIZE];    /* Buffer for writing data */
uint8 in_buf[ELEMSIZE];     /* Buffer for reading data */

/* local function prototypes */
static void init_buffer(void);

/* Initialize output buffer */
static void
init_buffer(void)
{
    intn        j;

    SEED(time(NULL));
    for (j = 0; j < ELEMSIZE; j++)
      {
          out_buf[j] = (uint8) RAND();
      }     /* end for */
}   /* init_buffers() */

static clock_t
read_test(int32 aid)
{
    clock_t start_time, end_time, acc_time;     /* timing counts */
    int32       ret;
    intn i;             /* local counting index */
    intn timing;       /* Which timing test we are on */
    intn err_count;     /* number of incorrect array positions */

    acc_time=0;
    for(timing=0; timing<NUM_TIMINGS; timing++) {

        /* Seek to beginning of element */
        ret=Hseek(aid,0,DF_START);
        CHECK(ret, FAIL, "Hseek");

        switch(timing) {
            case 0:     /* Read entire buffer in one I/O operation */
                start_time=clock();
                ret=Hread(aid,ELEMSIZE,in_buf);
                VERIFY(ret, ELEMSIZE, "Hread");
                end_time=clock();
                break;

            case 1:     /* Read entire buffer one byte at a time forwards */
                start_time=clock();
                for(i=0; i<ELEMSIZE; i++) {
                    /* Seek to correct location within element */
                    ret=Hseek(aid,i,DF_START);
                    CHECK(ret, FAIL, "Hseek");

                    ret=Hread(aid,1,&in_buf[i]);
                    VERIFY(ret, 1, "Hread");
                } /* end for */
                end_time=clock();
                break;

            case 2:     /* Read entire buffer one byte at a time every one byte forwards */
                start_time=clock();
                for(i=0; i<ELEMSIZE; i+=2) {
                    /* Seek to correct location within element */
                    ret=Hseek(aid,i,DF_START);
                    CHECK(ret, FAIL, "Hseek");

                    ret=Hread(aid,1,&in_buf[i]);
                    VERIFY(ret, 1, "Hread");
                } /* end for */
                for(i=1; i<ELEMSIZE; i+=2) {
                    /* Seek to correct location within element */
                    ret=Hseek(aid,i,DF_START);
                    CHECK(ret, FAIL, "Hseek");

                    ret=Hread(aid,1,&in_buf[i]);
                    VERIFY(ret, 1, "Hread");
                } /* end for */
                end_time=clock();
                break;

            case 3:     /* Read entire buffer one byte at a time backwards */
                start_time=clock();
                for(i=ELEMSIZE-1; i>=0; i--) {
                    /* Seek to correct location within element */
                    ret=Hseek(aid,i,DF_START);
                    CHECK(ret, FAIL, "Hseek");

                    ret=Hread(aid,1,&in_buf[i]);
                    VERIFY(ret, 1, "Hread");
                } /* end for */
                end_time=clock();
                break;

            case 4:     /* Read entire buffer one byte at a time every one byte backwards */
                start_time=clock();
                for(i=ELEMSIZE-1; i>=0; i-=2) {
                    /* Seek to correct location within element */
                    ret=Hseek(aid,i,DF_START);
                    CHECK(ret, FAIL, "Hseek");

                    ret=Hread(aid,1,&in_buf[i]);
                    VERIFY(ret, 1, "Hread");
                } /* end for */
                for(i=ELEMSIZE-2; i>=0; i-=2) {
                    /* Seek to correct location within element */
                    ret=Hseek(aid,i,DF_START);
                    CHECK(ret, FAIL, "Hseek");

                    ret=Hread(aid,1,&in_buf[i]);
                    VERIFY(ret, 1, "Hread");
                } /* end for */
                end_time=clock();
                break;
        } /* end switch */

        /* Verify buffer contents */
        for(err_count=0,i=0; i<ELEMSIZE; i++) {
            if(out_buf[i]!=in_buf[i]) {
                printf("Position (%d) read in is (%d), should be (%d)\n",i,(int)in_buf[i],(int)out_buf[i]);
                num_errs++;
                err_count++;
                if(err_count>10)
                    break;
            } /* end if */
        } /* end for */

        /* Clear input buffer */
        HDmemset(in_buf,0,ELEMSIZE);

        /* Increment the total I/O time */
        acc_time+=(end_time-start_time);
    } /* end for */

    return(acc_time);
}   /* end read_test() */

static clock_t
write_test(int32 aid,intn num_timings)
{
    clock_t start_time, end_time, acc_time;     /* timing counts */
    int32       ret;
    intn i;             /* local counting index */
    intn timing;       /* Which timing test we are on */

    acc_time=0;
    for(timing=0; timing<num_timings; timing++) {

        /* Refresh output buffer with new values */
        init_buffer();

        /* Seek to beginning of element */
        ret=Hseek(aid,0,DF_START);
        CHECK(ret, FAIL, "Hseek");

        switch(timing) {
            case 0:     /* Write entire buffer in one I/O operation */
                start_time=clock();
                ret=Hwrite(aid,ELEMSIZE,out_buf);
                VERIFY(ret, ELEMSIZE, "Hwrite");
                end_time=clock();
                break;

            case 1:     /* Write entire buffer one byte at a time forwards */
                start_time=clock();
                for(i=0; i<ELEMSIZE; i++) {
                    /* Seek to correct location within element */
                    ret=Hseek(aid,i,DF_START);
                    CHECK(ret, FAIL, "Hseek");

                    ret=Hwrite(aid,1,&out_buf[i]);
                    VERIFY(ret, 1, "Hwrite");
                } /* end for */
                end_time=clock();
                break;

            case 2:     /* Write entire buffer one byte at a time every one byte forwards */
                start_time=clock();
                for(i=0; i<ELEMSIZE; i+=2) {
                    /* Seek to correct location within element */
                    ret=Hseek(aid,i,DF_START);
                    CHECK(ret, FAIL, "Hseek");

                    ret=Hwrite(aid,1,&out_buf[i]);
                    VERIFY(ret, 1, "Hwrite");
                } /* end for */
                for(i=1; i<ELEMSIZE; i+=2) {
                    /* Seek to correct location within element */
                    ret=Hseek(aid,i,DF_START);
                    CHECK(ret, FAIL, "Hseek");

                    ret=Hwrite(aid,1,&out_buf[i]);
                    VERIFY(ret, 1, "Hwrite");
                } /* end for */
                end_time=clock();
                break;

            case 3:     /* Write entire buffer one byte at a time backwards */
                start_time=clock();
                for(i=ELEMSIZE-1; i>=0; i--) {
                    /* Seek to correct location within element */
                    ret=Hseek(aid,i,DF_START);
                    CHECK(ret, FAIL, "Hseek");

                    ret=Hwrite(aid,1,&out_buf[i]);
                    VERIFY(ret, 1, "Hwrite");
                } /* end for */
                end_time=clock();
                break;

            case 4:     /* Write entire buffer one byte at a time every one byte backwards */
                start_time=clock();
                for(i=ELEMSIZE-1; i>=0; i-=2) {
                    /* Seek to correct location within element */
                    ret=Hseek(aid,i,DF_START);
                    CHECK(ret, FAIL, "Hseek");

                    ret=Hwrite(aid,1,&out_buf[i]);
                    VERIFY(ret, 1, "Hwrite");
                } /* end for */
                for(i=ELEMSIZE-2; i>=0; i-=2) {
                    /* Seek to correct location within element */
                    ret=Hseek(aid,i,DF_START);
                    CHECK(ret, FAIL, "Hseek");

                    ret=Hwrite(aid,1,&out_buf[i]);
                    VERIFY(ret, 1, "Hwrite");
                } /* end for */
                end_time=clock();
                break;
        } /* end switch */

        /* Seek to beginning of element */
        ret=Hseek(aid,0,DF_START);
        CHECK(ret, FAIL, "Hseek");

        /* Read buffer contents */
        ret=Hread(aid,ELEMSIZE,in_buf);
        VERIFY(ret, ELEMSIZE, "Hread");

        /* Verify buffer contents */
        for(i=0; i<ELEMSIZE; i++) {
            if(out_buf[i]!=in_buf[i]) {
                printf("Position (%d) read in is (%d), should be (%d)\n",i,(int)in_buf[i],(int)out_buf[i]);
                num_errs++;
                break;
            } /* end if */
        } /* end for */


        /* Clear input buffer */
        HDmemset(in_buf,0,ELEMSIZE);

        /* Increment the total I/O time */
        acc_time+=(end_time-start_time);
    } /* end for */

    return(acc_time);
}   /* end read_test() */

void
test_buffer(void)
{
    model_info  m_info;
    comp_info   c_info;
    uint16      ref_num;        /* reference number of the data written out */
    int32       fid;            /* file ID of HDF file for testing */
    int32       aid;            /* AID of element to test */
    intn        test_num;
    int32       ret;

    MESSAGE(6, printf("Starting buffered element test\n");
        )

    /* fill the buffer with interesting data to compress */
    init_buffer();

    /* open the HDF file */
    fid = Hopen(TESTFILE_NAME, DFACC_ALL, 0);
    CHECK(fid, FAIL, "Hopen");

    /* Cycle through the different testing element types */
    /* Performing timings on each type of buffer and record results for output */
    /* if verbosity level is high enough */
    for (test_num = 0; test_num < NUM_TESTS; test_num++)
      {
        /* Get a new reference number */
        ref_num=Htagnewref(fid,BUFF_TAG);
        CHECK(ref_num, 0, "Htagnewref");

        /* Create the data element to perform the tests on */
        switch(test_num) {
            case 0:     /* create plain data element */
                aid=Hstartaccess(fid,BUFF_TAG,ref_num,DFACC_RDWR);
                CHECK(aid, FAIL, "Hstartaccess");
                break;

            case 1:     /* create external data element */
                aid=HXcreate(fid,BUFF_TAG,ref_num,EXTFILE_NAME,0,ELEMSIZE);
                CHECK(aid, FAIL, "HXcreate");
                break;

            case 2:     /* create compressed data element */
                c_info.deflate.level=9;
                aid=HCcreate(fid,BUFF_TAG,ref_num,COMP_MODEL_STDIO,&m_info,COMP_CODE_DEFLATE,&c_info);
                CHECK(aid, FAIL, "HCcreate");
                break;

            case 3:     /* create linked-block data element */
                aid=HLcreate(fid,BUFF_TAG,ref_num,HDF_APPENDABLE_BLOCK_LEN,HDF_APPENDABLE_BLOCK_NUM);
                CHECK(aid, FAIL, "HLcreate");
                break;

        } /* end switch */

        /* Write the initial data to the data element */
        ret=Hwrite(aid,ELEMSIZE,out_buf);
        VERIFY(ret, ELEMSIZE, "Hwrite");

        /* Perform read timing tests on un-buffered data element */
        read_time[test_num][0]=read_test(aid);
        
        /* Perform write timing tests on un-buffered data element */
        /* Just write un-buffered compressed data in one block */
        write_time[test_num][0]=write_test(aid,(test_num==2 ? 1 : NUM_TIMINGS));

        /* Convert element to a buffered element */
        ret=HBconvert(aid);
        CHECK(ret, FAIL, "HBconvert");

        /* Perform read timing tests on buffered data element */
        read_time[test_num][1]=read_test(aid);

        /* Perform write timing tests on un-buffered data element */
        write_time[test_num][1]=write_test(aid,NUM_TIMINGS);

        /* Close data element */
        ret=Hendaccess(aid);
        CHECK(ret, FAIL, "Hendaccess");

        MESSAGE(6, {
            printf("Unbuffered read time=%f seconds\n",((float)read_time[test_num][0]/CLOCKS_PER_SEC));
            printf("Unbuffered write time=%f seconds\n",((float)write_time[test_num][0]/CLOCKS_PER_SEC));
            printf("Buffered read time=%f seconds\n",((float)read_time[test_num][1]/CLOCKS_PER_SEC));
            printf("Buffered write time=%f seconds\n",((float)write_time[test_num][1]/CLOCKS_PER_SEC));
        }
            )

      }     /* end for */

    /* close the HDF file */
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    /* Clean up files created */
    remove(EXTFILE_NAME);

    MESSAGE(6, printf("Finished buffered element test\n");
        )
}   /* end test_buffer() */
