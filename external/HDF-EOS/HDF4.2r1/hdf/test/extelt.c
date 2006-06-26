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
static char RcsId[] = "@(#)$Revision: 1.25 $";
#endif

/* $Id: extelt.c,v 1.25 2003/12/10 21:13:32 epourmal Exp $ */

/*
 * These is a first pass at rewriting how these tests for exteranl 
 * elements were done -GV
 */
#include "tproto.h"
#define TESTFILE_NAME "t.hdf"             /* file for first 4 series of tests */
#define TESTFILE_NAME1 "tx.hdf"           /* file for last test */
#define STRING         "element 1000 2"          /* 14 bytes */
#define STRING2        "element 1000 1   wrong"  /* 22 bytes */
#define STRING3        "element 1000 1 correct"  /* 22 bytes */

#define BUF_SIZE        4096

static uint8  outbuf[BUF_SIZE],  inbuf[BUF_SIZE];

void
test_hextelt(void)
{
    int32       fid, fid1;
    int32       aid1, aid2;
    int32       fileid, length, offset, posn;
    uint16      tag, ref;
    int16       acc_mode, special;
    int         i;
    int32       ret;
    intn        errflag  = 0;
    intn        errors = 0;

    /* Initialize buffer */
    for (i = 0; i < BUF_SIZE; i++)
        outbuf[i] = (char) (i % 256);

    /* Create header file */
    MESSAGE(5, printf("Creating base file %s\n", TESTFILE_NAME);
        );

    fid = Hopen(TESTFILE_NAME, DFACC_CREATE, 0);
    CHECK(fid, FAIL, "Hopen");

    /* Write first object to header file */
    MESSAGE(5, printf("Writing object(%lu bytes) into base file\n",
                      (unsigned long)HDstrlen(STRING2));
            );
    ret = Hputelement(fid, (uint16) 1000, (uint16) 1,
                      (const uint8 *) STRING2,
                      (int32)HDstrlen(STRING2) + 1);
    CHECK(ret, FAIL, "Hputelement");

    /* Promote the above object to an external object */
    MESSAGE(5, printf("Promoting above object to external element in file #1\n");
            );
    aid1 = HXcreate(fid, 1000, 1, "t1.hdf", (int32) 0, (int32) 0);
    CHECK(aid1, FAIL, "HXcreate");

    ret = Hseek(aid1, (int32)HDstrlen("element 1000 1") + 1, DF_START);
    CHECK(ret, FAIL, "Hseek");

    /* Now verify that the new promoted object can be written to */
    MESSAGE(5, printf("Writing to promoted object now in file #1 \n");
            );

    ret = Hwrite(aid1, (int32)HDstrlen("correct") + 1, "correct");
    if (ret != (int32) HDstrlen("correct") + 1)
      {
          fprintf(stderr, "Hwrite failed (code %d)\n", (int) ret);
          HEprint(stderr, 0);
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    /* Create a new external object of size 2000 bytes in a seperate file */
    MESSAGE(5, printf("Creating an external element in file #2\n");
        );
    aid1 = HXcreate(fid, 1000, 4, "t2.hdf", (int32) 0, (int32) 0);
    CHECK(aid1, FAIL, "HXcreate");

    MESSAGE(5, printf("Writing 2000 bytes to file #2\n");
        );
    ret = Hwrite(aid1, 2000, outbuf);
    CHECK(ret, FAIL, "Hwrite");

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    /* Create a new external string object  in a seperate file */
    MESSAGE(5, printf("Creating an external element in file #3\n");
        );
    aid1 = HXcreate(fid, 1000, 2, "t3.hdf", (int32) 0, (int32) 0);
    CHECK(aid1, FAIL, "HXcreate");

    MESSAGE(5, printf("Writing string '%s'(%lu bytes) to file #3\n", 
                      STRING, (unsigned long)HDstrlen(STRING));
        );
    ret = Hwrite(aid1, (int32)HDstrlen(STRING) + 1, STRING);
    if (ret != (int32) HDstrlen(STRING) + 1)
      {
          fprintf(stderr, "Hwrite failed (code %d)\n", (int) ret);
          HEprint(stderr, 0);
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    /* Create a new external object that points to part of an existing element */
    MESSAGE(5, printf("Creating an overlapping element that already exists in file #3\n");
        );
    aid2 = HXcreate(fid, 1001, 2, "t3.hdf", (int32) 8, (int32) 4);
    CHECK(aid2, FAIL, "HXcreate");

    ret = Hendaccess(aid2);
    CHECK(ret, FAIL, "Hendaccess");

    /* Create a new external object of size 4096 bytes */
    MESSAGE(5, printf("Creating an external element in file #4\n");
        );
    aid1 = HXcreate(fid, 1020, 2, "t4.hdf", (int32) 0, (int32) 0);
    CHECK(aid1, FAIL, "HXcreate");

    MESSAGE(5, printf("Writing %d bytes to file #4\n", BUF_SIZE);
        );
    ret = Hwrite(aid1, BUF_SIZE, outbuf);
    if (ret != BUF_SIZE)
      {
          fprintf(stderr, "Hwrite failed (code %d)\n", (int) ret);
          HEprint(stderr, 0);
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    /* Close the file */
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    /* Now re-open for reading and verifying the elements */
    MESSAGE(5, printf("Closing and re-opening base file %s\n", TESTFILE_NAME);
        );
    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(ret, FAIL, "Hopen");

    /* Verify element in file #1 */
    aid1 = Hstartread(fid, 1000, 1);
    CHECK(aid1, FAIL, "Hstartread");

    MESSAGE(5, printf("Inquiring about external element in file #1\n");
        );
    ret = Hinquire(aid1, &fileid, &tag, &ref, &length, &offset, &posn,
                   &acc_mode, &special);
    CHECK(ret, FAIL, "Hinquire");

    for (i = 0; i < BUF_SIZE; i++)
        inbuf[i] = '\0';

    MESSAGE(5, printf("Reading external element in file #1\n");
        );
    ret = Hread(aid1, length, inbuf);
    if (ret != length)
      {
          fprintf(stderr, "Hread failed (code %d)\n", (int) ret);
          HEprint(stderr, 0);
          errors++;
      }

    MESSAGE(5, printf("Verifying data(%d bytes) in external element in file #1\n", (int)ret);
        );
    if (HDstrcmp((const char *) inbuf, (const char *) STRING3))
      {
          fprintf(stderr, "Error: Object stored in file #1 is wrong\n");
          fprintf(stderr, "\t       Is: %s\n", (char *) inbuf);
          fprintf(stderr, "\tShould be: %s\n",STRING3);
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    /* Verify element in file #2 */
    aid1 = Hstartread(fid, 1000, 4);
    CHECK(aid1, FAIL, "Hstartread");

    MESSAGE(5, printf("Inquiring about external element in file #2\n");
        );
    ret = Hinquire(aid1, &fileid, &tag, &ref, &length, &offset, &posn,
                   &acc_mode, &special);
    CHECK(ret, FAIL, "Hinquire");

    for (i = 0; i < BUF_SIZE; i++)
        inbuf[i] = 0;

    ret = Hgetelement(fid, (uint16) tag, (uint16) ref, inbuf);
    if (ret != length)
      {
          fprintf(stderr, "Incorrect element size returned from Hgetelement: %d\n",
                  (int) ret);
          HEprint(stderr, 0);
          errors++;
      }

#if 0
    ret = Hread(aid1, length, inbuf);
    if (ret != 2000)
      {
          fprintf(stderr, "Incorrect element size returned from Hread: %d\n",
                  (int) ret);
          HEprint(stderr, 0);
          errors++;
      }
#endif
    MESSAGE(5, printf("Verifying data(%d bytes) that was stored to file #2\n",(int)ret);
        );

    errflag = 0;
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != outbuf[i])
            {
                errflag = 1;
                MESSAGE(8, printf("Wrong data at %d, out %d in %d\n", 
                                  i, outbuf[i], inbuf[i]);
                        );
                errors++;
            }
          inbuf[i] = '\0';
      }
    if (errflag)
        fprintf(stderr,"Error: Wrong data in inbuf[] from external elment in file #2\n");

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    /* Verify overlapping element in file #3 */
    aid1 = Hstartread(fid, 1001, 2);
    CHECK(aid1, FAIL, "Hstartread");

    MESSAGE(5, printf("Inquiring about overlaping external element in file #3\n");
        );
    ret = Hinquire(aid1, &fileid, &tag, &ref, &length, &offset, &posn,
                   &acc_mode, &special);
    CHECK(ret, FAIL, "Hinquire");

    for (i = 0; i < BUF_SIZE; i++)
        inbuf[i] = '\0';

    ret = Hgetelement(fid, (uint16) tag, (uint16) ref, inbuf);
    if (ret != length)
      {
          fprintf(stderr, "Incorrect element size returned from Hgetelement: %d\n",
                  (int) ret);
          HEprint(stderr, 0);
          errors++;
      }
#if 0
    ret = Hread(aid1, length, inbuf);
    if (ret != length)
      {
          fprintf(stderr, "Incorrect element size returned from Hread: %d\n",
                  (int) ret);
          HEprint(stderr, 0);
          errors++;
      }
#endif
    MESSAGE(5, printf("Verifying data(%d bytes) that was stored in overlapping element in file #3\n",(int)ret);
        );

    if (inbuf[0] != '1' ||
        inbuf[1] != '0' ||
        inbuf[2] != '0' ||
        inbuf[3] != '0')
      {
          fprintf(stderr,"Error: One or more errors in overlapping element in file #3\n");
          fprintf(stderr, "\t       is: %s\n", (char *) inbuf);
          fprintf(stderr, "\tShould be: %s\n","1000");
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    /* Verify the whole element in file #3 */
    aid1 = Hstartread(fid, 1000, 2);
    CHECK(aid1, FAIL, "Hstartread");

    MESSAGE(5, printf("Inquiring about external element in file #3\n");
        );
    ret = Hinquire(aid1, &fileid, &tag, &ref, &length, &offset, &posn,
                   &acc_mode, &special);
    CHECK(ret, FAIL, "Hinquire");

    for (i = 0; i < BUF_SIZE; i++)
        inbuf[i] = '\0';

    MESSAGE(5, printf("Reading whole external element in file #3\n");
        );
    ret = Hread(aid1, length, inbuf);
    if (ret != length)
      {
          fprintf(stderr, "Hread failed (code %d)\n", (int) ret);
          HEprint(stderr, 0);
          errors++;
      }

    MESSAGE(5, printf("Verifying data(%d bytes) in whole external element in file #3\n", (int)ret);
        );
    if (HDstrcmp((const char *) inbuf, (const char *) STRING))
      {
          fprintf(stderr, "Error: Object stored in file #3 is wrong\n");
          fprintf(stderr, "\t       is: %s\n", (char *) inbuf);
          fprintf(stderr, "\tShould be: %s\n",STRING);
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    /* Verify element in file #4 */
    aid1 = Hstartread(fid, 1020, 2);
    CHECK(aid1, FAIL, "Hstartread");

    MESSAGE(5, printf("Inquiring about access element in file #4\n");
        );
    ret = Hinquire(aid1, &fileid, &tag, &ref, &length, &offset, &posn,
                   &acc_mode, &special);
    CHECK(ret, FAIL, "Hinquire");

    for (i = 0; i < BUF_SIZE; i++)
        inbuf[i] = 0;


    ret = Hread(aid1, length, inbuf);
    if (ret != length)
      {
          fprintf(stderr, "Incorrect element size returned from Hread: %d\n",
                  (int) ret);
          HEprint(stderr, 0);
          errors++;
      }

    MESSAGE(5, printf("Verifying data(%d bytes) in external element in file #4\n",(int)ret);
        );

    errflag  = 0;
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != outbuf[i])
            {
                errflag = 1;
                MESSAGE(8, printf("Wrong data at %d, out %d in %d\n", 
                                  i, outbuf[i], inbuf[i]);
                        );
                errors++;
            }
          inbuf[i] = '\0';
      }
    if (errflag)
        fprintf(stderr,"Error: Wrong data in inbuf[]  from external elment in file #4\n");

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    /* Write to the first element in file #1 again */
    MESSAGE(5, printf("Now writing again to external element in file #1\n");
        );
    aid2 = Hstartwrite(fid, 1000, 1, 4);
    CHECK(aid2, FAIL, "Hstartwrite");

    ret = Hwrite(aid2, 4, "ABCD");
    if (ret != 4)
      {
          fprintf(stderr, "Hwrite failed (code %d)\n", (int) ret);
          HEprint(stderr, 0);
          errors++;
      }

    ret = Hendaccess(aid2);
    CHECK(ret, FAIL, "Hendaccess");

    /* Second file open rest for reading */
    fid1 = Hopen(TESTFILE_NAME, DFACC_READ, 0);
    CHECK(fid1, FAIL, "Hopen");

    ret = (int32)Hnewref(fid1);
    CHECK(ret, FAIL, "Hnewref");

    /* Close first open of file */
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    /* Close second open of file */
    ret = Hclose(fid1);
    CHECK(ret, FAIL, "Hclose");

    /*==============================*/
    /* Test External Path functions */
    /*==============================*/
    MESSAGE(5, printf("testing External Path functions\n");
        );

    /* start with a brand new file */
    MESSAGE(5, printf("Creating header file %s for external element \n",
                      TESTFILE_NAME1);
        );
    fid = Hopen(TESTFILE_NAME1, DFACC_CREATE, 0);
    CHECK(fid, FAIL, "Hopen");

    ret = HXsetcreatedir("testdir");
    CHECK(ret, FAIL, "HXsetcreatedir");
   
    MESSAGE(5, printf("Creating an external element in file testdir/t5.hdf\n");
        );
    aid1 = HXcreate(fid, 1000, 5, "t5.hdf", (int32) 0, (int32) 0);
    CHECK(aid1, FAIL, "HXcreate");

    MESSAGE(5, printf("Writing 2000 bytes to file t5.hdf\n");
        );
    ret = Hwrite(aid1, 2000, outbuf);
    CHECK(ret, FAIL, "Hwrite");

    MESSAGE(5, printf("Ending access to element and closing header file %s\n", 
                      TESTFILE_NAME1);
        );
    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    MESSAGE(5, printf("Re-open file and try read to external element.  Should fail the first time.\n");
        );

    fid = Hopen(TESTFILE_NAME1, DFACC_READ, 0);
    CHECK(fid, FAIL, "Hopen");

    ret = Hgetelement(fid, (uint16) 1000, (uint16) 5, inbuf);
    VERIFY(ret, FAIL, "Hgetelement");

    ret = HXsetdir("nosuchdir|testdir");
    CHECK(ret, FAIL, "HXsetdir");

    MESSAGE(5, printf("Try read it again.  Should not fail this time.\n");
        );

    ret = Hgetelement(fid, (uint16) 1000, (uint16) 5, inbuf);
    CHECK(ret, FAIL, "Hgetelement");

    errflag  = 0;
    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != outbuf[i])
            {
                errflag = 1;
                MESSAGE(8, printf("Wrong data at %d, out %d in %d\n",
                                  i, outbuf[i], inbuf[i]);
                        );
                errors++;
            }
          inbuf[i] = '\0';
      }
    if (errflag)
        fprintf(stderr,"Error: Wrong data in inbuf[]  from external elment in file #5\n" );

    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    /* unset the external paths directory variables */
    ret = HXsetcreatedir(NULL);
    CHECK(ret, FAIL, "HXsetcreatedir");
    ret = HXsetdir(NULL);
    CHECK(ret, FAIL, "HXsetdir");

    num_errs += errors;     /* increment global error count */
}

