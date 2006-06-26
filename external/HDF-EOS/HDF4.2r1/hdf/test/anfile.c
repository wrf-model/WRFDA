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
static char RcsId[] = "@(#)$Revision: 1.12 $";
#endif

/* $Id: anfile.c,v 1.12 1997/10/02 18:53:05 georgev Exp $ */

#include "tproto.h"
#define TESTFILE "tdfan.hdf"

#define ISFIRST    (int)1
#define NOTFIRST   (int)0
#define MAXLEN_LAB     50
#define MAXLEN_DESC  1000

static int  checkannlen
            (int32 ret, const char *oldstr, const char *type, int32 testflag);

static int  checkann
            (const char *oldstr, const char *newstr, int32 ret, const char *type, int32 testflag);

void
test_anfile(void)
{
    char        lab1[MAXLEN_LAB], lab2[MAXLEN_LAB], desc1[MAXLEN_DESC], desc2[MAXLEN_DESC],
                tempstr[MAXLEN_DESC];
    int32       testflag = SUCCEED;
    int32       file_id, ret;

/* set up file labels and descriptions */

    HDstrcpy(lab1, "File label #1: aaa");
    HDstrcpy(lab2, "File label #2: bbbbbb");
    HDstrcpy(desc1, "File Descr #1: 1  2  3  4  5  6  7  8  9 10 11 12 13\n");
    HDstrcat(desc1, "              14 15 16 17 18 19 20 **END FILE DESCR**\n");
    HDstrcpy(desc2, "File Descr #2: A B C D E F G H I J K L\n");
    HDstrcat(desc2, "              M N O **END FILE DESCR**\n");

/********  Write file labels and descriptions *********/

    file_id = Hopen(TESTFILE, DFACC_CREATE, 0);
    if (file_id == FAIL)
        printf("\nUnable to open file %s for writing.\n\n", TESTFILE);

    MESSAGE(5, puts("Writing file labels.");
        );
    ret = DFANaddfid(file_id, lab1);
    RESULT("DFANaddfid");

    ret = DFANaddfid(file_id, lab2);
    RESULT("DFANaddfid");

    MESSAGE(5, puts("Writing file descriptions.");
        );
    ret = DFANaddfds(file_id, desc1, (int32)HDstrlen(desc1));
    RESULT("DFANaddfds");

    ret = DFANaddfds(file_id, desc2, (int32)HDstrlen(desc2));
    RESULT("DFANaddfds");

    if (FAIL == Hclose(file_id))
        printf("\nUnable to close file %s after writing.\n\n", TESTFILE);

/********  Read file labels *********/

    file_id = Hopen(TESTFILE, DFACC_READ, 0);
    if (file_id == FAIL)
        printf("\n\nUnable to open file %s for reading.\n\n", TESTFILE);

    MESSAGE(5, puts("Reading length of first file label, followed by label.");
        );
    ret = DFANgetfidlen(file_id, ISFIRST);
    RESULT("DFANgetfidlen");
    checkannlen(ret, lab1, "label", testflag);

    ret = DFANgetfid(file_id, tempstr, MAXLEN_LAB, ISFIRST);
    RESULT("DFANgetfid");
    checkann(lab1, tempstr, ret, "label", testflag);

    MESSAGE(5, puts("Reading length of second file label, followed by label.");
        );
    ret = DFANgetfidlen(file_id, NOTFIRST);
    RESULT("DFANgetfidlen");
    checkannlen(ret, lab2, "label", testflag);

    ret = DFANgetfid(file_id, tempstr, MAXLEN_LAB, NOTFIRST);
    RESULT("DFANgetfid");
    checkann(lab2, tempstr, ret, "label", testflag);

/********  Read file descriptions *********/

    MESSAGE(5, puts("Reading length of first file descr, followed by descr.");
        );
    ret = DFANgetfdslen(file_id, ISFIRST);
    RESULT("DFANgetfdslen");
    checkannlen(ret, desc1, "description", testflag);

    ret = DFANgetfds(file_id, tempstr, MAXLEN_DESC, ISFIRST);
    RESULT("DFANgetfds");
    checkann(desc1, tempstr, ret, "description", testflag);

    MESSAGE(5, puts("Reading length of second file descr, followed by descr.");
        );
    ret = DFANgetfdslen(file_id, NOTFIRST);
    RESULT("DFANgetfdslen");
    checkannlen(ret, desc2, "description", testflag);

    ret = DFANgetfds(file_id, tempstr, MAXLEN_DESC, NOTFIRST);
    RESULT("DFANgetfds");
    checkann(desc2, tempstr, ret, "description", testflag);

    if (FAIL == Hclose(file_id))
        printf("\n\nUnable to close file %s after reading.\n\n", TESTFILE);

}

static int
checkannlen(int32 ret, const char *oldstr, const char *type, int32 testflag)
{
    if ((ret >= 0) && (ret != (int32) HDstrlen(oldstr)))
      {
          printf("Length of %s is INCORRECT\n", type);
          printf("It is:  %d\n", (int) ret);
          printf("It should be: %d\n", (int) HDstrlen(oldstr));
          testflag = FAIL;
          return FAIL;
      }
    return SUCCEED;
}

static int
checkann(const char *oldstr, const char *newstr, int32 ret, const char *type,
         int32 testflag)
{
    if ((ret >= 0) && (0 != HDstrcmp(oldstr, newstr)))
      {
          printf("%s is INCORRECT.\n", type);
          printf("It is:  %s\n", newstr);
          printf("It should be: %s\n", oldstr);
          testflag = FAIL;
          return (FAIL);
      }
    return (SUCCEED);
}
