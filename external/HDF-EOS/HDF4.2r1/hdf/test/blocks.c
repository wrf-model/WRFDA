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
static char RcsId[] = "@(#)$Revision: 1.16 $";
#endif

/* $Id: blocks.c,v 1.16 1997/10/25 00:56:10 koziol Exp $ */

#include "tproto.h"
#define TESTFILE_NAME "tblocks.hdf"

#define BUFSIZE 4096

#define HLCONVERT_TAG 1500

static uint8  outbuf[BUFSIZE],  inbuf[BUFSIZE];

void
test_hblocks(void)
{
    int32       fid, fid1;
    int32       aid, aid1, aid2;
    int32       fileid, length, offset, posn;
    uint16      tag, ref;
    int16       acc_mode, special;
    register int i;
    int32       ret;
    intn        errors = 0;

    for (i = 0; i < BUFSIZE; i++)
        outbuf[i] = (char) (i % 256);

    MESSAGE(5, printf("Creating a file %s\n", TESTFILE_NAME);
        );
    fid = Hopen(TESTFILE_NAME, DFACC_CREATE, 0);
    CHECK(fid, FAIL, "Hopen");

    ret = (int32)Hnewref(fid);
    CHECK(ret, FAIL, "Hnewref");

    MESSAGE(5, printf("Write an element and then promote to Linked Blocks\n");
        );
    ret = Hputelement(fid, (uint16) 1000, (uint16) 1,
                      (const uint8 *)"element 1000 1 wrong ",
                      (int32)HDstrlen("element 1000 1 wrong ") + 1);
    CHECK(ret, FAIL, "Hputelement");

    aid1 = HLcreate(fid, 1000, 1, 10, 10);
    CHECK(aid1, FAIL, "HLcreate");

    ret = Hseek(aid1, (int32)HDstrlen("element 1000 1 "), DF_START);
    CHECK(ret, FAIL, "Hseek");

    ret = Hwrite(aid1, (int32)HDstrlen("correct") + 1, "correct");
    if (ret != (int32) HDstrlen("correct") + 1)
      {
          fprintf(stderr, "ERROR: Hwrite returned the wrong length: %d\n", (int) ret);
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Create a new element as a Linked Block\n");
        );
    aid1 = HLcreate(fid, 1000, 4, 512, 2);
    CHECK(aid1, FAIL, "HLcreate");

    ret = Hwrite(aid1, BUFSIZE / 2, outbuf);
    if (ret != BUFSIZE / 2)
      {
          fprintf(stderr, "ERROR: Hwrite returned the wrong length: %d\n", (int) ret);
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    ret = (int32)Hnewref(fid);
    CHECK(ret, FAIL, "Hnewref");

    aid1 = HLcreate(fid, 1000, 2, 128, 16);
    CHECK(aid1, FAIL, "HLcreate");

    ret = Hwrite(aid1, (int32)HDstrlen("element 1000 2") + 1, "element 1000 2");
    if (ret != (int32) HDstrlen("element 1000 2") + 1)
      {
          fprintf(stderr, "ERROR: Hwrite returned the wrong length: %d\n", (int) ret);
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Verifying data\n");
        );
    ret = Hgetelement(fid, (uint16) 1000, (uint16) 4, inbuf);
    if (ret != BUFSIZE / 2)
      {
          fprintf(stderr, "ERROR(%d): Hgetelement returned the wrong length: %d\n", (int)__LINE__,(int) ret);
          errors++;
      }

    for (i = 0; i < ret; i++)
      {
          if (inbuf[i] != outbuf[i])
            {
                printf("Wrong data at %d, out %d in %d\n", i, outbuf[i], inbuf[i]);
                errors++;
            }
          inbuf[i] = '\0';
      }

    aid1 = HLcreate(fid, 1020, 2, 128, 4);
    CHECK(aid1, FAIL, "HLcreate");

    ret = Hwrite(aid1, BUFSIZE, outbuf);
    if (ret != BUFSIZE)
      {
          fprintf(stderr, "ERROR: Hwrite returned the wrong length: %d\n", (int) ret);
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Closing and re-opening file %s\n", TESTFILE_NAME);
        );
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    fid = Hopen(TESTFILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen");

    ret = (int32)Hnewref(fid);
    CHECK(ret, FAIL, "Hnewref");

    MESSAGE(5, printf("Verifying data\n");
        );

    aid1 = Hstartread(fid, 1000, 1);
    CHECK(aid1, FAIL, "Hstartread");

    ret = Hinquire(aid1, &fileid, &tag, &ref, &length, &offset, &posn,
                   &acc_mode, &special);
    CHECK(ret, FAIL, "Hinquire");
    if (!special)
      {
          fprintf(stderr, "ERROR: Hinquire does not think element is special line %d\n",
                  __LINE__);
          errors++;
      }

    ret = Hread(aid1, length, inbuf);
    if (ret != 23)
      {
          fprintf(stderr, "ERROR: Hread returned the wrong length: %d\n", (int) ret);
          errors++;
      }

    if (HDstrcmp((const char *) inbuf, (const char *) "element 1000 1 correct"))
      {
          fprintf(stderr, "ERROR: Hread returned the wrong data\n");
          fprintf(stderr, "\t       Is: %s\n", (char *) inbuf);
          fprintf(stderr, "\tShould be: element 1000 1 correct\n");
          errors++;
      }

    ret = (int32)Hnewref(fid);
    CHECK(ret, FAIL, "Hnewref");

    ret = Hnextread(aid1, 1000, DFREF_WILDCARD, DF_CURRENT);
    CHECK(ret, FAIL, "Hnextread");

    ret = Hinquire(aid1, &fileid, &tag, &ref, &length, &offset, &posn,
                   &acc_mode, &special);
    CHECK(ret, FAIL, "Hinquire");
    if (!special)
      {
          fprintf(stderr, "ERROR: Hinquire does not think element is special line %d\n",
                  __LINE__);
          errors++;
      }

    ret = Hnextread(aid1, DFTAG_WILDCARD, DFREF_WILDCARD, DF_START);
    CHECK(ret, FAIL, "Hnextread");

    ret = Hinquire(aid1, &fileid, &tag, &ref, &length, &offset, &posn,
                   &acc_mode, &special);
    CHECK(ret, FAIL, "Hinquire");
    if (special)
      {
          fprintf(stderr, "ERROR: Hinquire thinks a non-special element is special\n");
          errors++;
      }

    ret = Hnextread(aid1, DFTAG_WILDCARD, 1151, DF_CURRENT);
    if (ret != FAIL)
      {
          fprintf(stderr, "ERROR: Hnextread found object with bogus ref\n");
          errors++;
      }

    ret = Hnextread(aid1, 1020, DFREF_WILDCARD, DF_CURRENT);
    CHECK(ret, FAIL, "Hnextread");

    ret = Hinquire(aid1, &fileid, &tag, &ref, &length, &offset, &posn,
                   &acc_mode, &special);
    CHECK(ret, FAIL, "Hinquire");
    if (!special)
      {
          fprintf(stderr, "ERROR: Hinquire does not think element is special line %d\n",
                  __LINE__);
          errors++;
      }

    MESSAGE(5, printf("Writing to existing element\n");
        );
    aid2 = Hstartwrite(fid, 1000, 1, 4);
    CHECK(aid2, FAIL, "Hstartwrite");

    ret = Hwrite(aid2, 4, "ABCD");
    if (ret != 4)
      {
          fprintf(stderr, "ERROR: Hwrite returned the wrong length: %d\n", (int) ret);
          errors++;
      }

    /* let's try to write to a read element for fun */
    ret = Hwrite(aid1, 4, "ABCD");
    if (ret != FAIL)
      {
          fprintf(stderr, "ERROR: Hwrite allowed write to read access obj\n");
          errors++;
      }

    ret = Hendaccess(aid1);
    CHECK(ret, FAIL, "Hendaccess");

    ret = Hendaccess(aid2);
    CHECK(ret, FAIL, "Hendaccess");

    MESSAGE(5, printf("Open a second id for file %s\n", TESTFILE_NAME);
        );
    fid1 = Hopen(TESTFILE_NAME, DFACC_READ, 0);
    CHECK(fid1, FAIL, "Hopen");

    ret = (int32)Hnewref(fid1);
    CHECK(ret, FAIL, "Hnewref");

    MESSAGE(5, printf("Closing the files\n");
        );
    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    ret = Hclose(fid1);
    CHECK(ret, FAIL, "Hclose");

    MESSAGE(5, printf("Testing HLconvert function\n");
        );
    fid = Hopen(TESTFILE_NAME, DFACC_WRITE, 0);
    CHECK(fid, FAIL, "Hopen");

    ref = Hnewref(fid);
    CHECK(ret, FAIL, "Hnewref");

    aid = Hstartwrite(fid, HLCONVERT_TAG, ref, 5);
    CHECK(aid, FAIL, "Hstartwrite");

    ret = Hwrite(aid, 4, outbuf);
    CHECK(ret, FAIL, "Hwrite");

    ret = HLconvert(aid, 256, 10);
    CHECK(ret, FAIL, "HLconvert");

    ret = Hwrite(aid, 508, &outbuf[4]);
    CHECK(ret, FAIL, "Hwrite");

    ret = Hendaccess(aid);
    CHECK(ret, FAIL, "Hendaccess");

    aid = Hstartread(fid, HLCONVERT_TAG, ref);
    CHECK(aid, FAIL, "Hstartread");

    ret = Hread(aid, 512, inbuf);
    CHECK(ret, FAIL, "Hread");

    ret = Hendaccess(aid);
    CHECK(ret, FAIL, "Hendaccess");

    ret = Hclose(fid);
    CHECK(ret, FAIL, "Hclose");

    if (HDmemcmp(inbuf, outbuf, 512))
      {
          fprintf(stderr, "Error when reading data from HLconvert buffer\n");
          errors++;
      }

    num_errs += errors;     /* increment global error count */
}
