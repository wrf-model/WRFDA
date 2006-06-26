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

/****************************************************************************
 * tsd.c - tests SDstart for file with no write permission
****************************************************************************/

#include "mfhdf.h"

#ifdef HDF

#include "hdftest.h"
#include "hfile.h"

#define FILE_NAME     "sdtest.hdf"	/* data file to test ID types */

extern int
test_sd()
{
    int32     fid;
    intn      status;
#ifdef WIN32
    int mode;
#else
    mode_t mode;
#endif

    FILE *ff;
    intn      num_errs = 0;         /* number of errors so far */

    /* delete the file just to be sure */
    unlink(FILE_NAME);

    /* Create a file */
    fid = SDstart(FILE_NAME, DFACC_CREATE);
    CHECK(fid, FAIL, "SDstart");

    /* Close the file */
    status = SDend(fid);
    CHECK(status, FAIL, "SDend");
#ifdef WIN32
    mode = _S_IREAD;
#else
    mode =  S_IRUSR;
#endif

    status = chmod(FILE_NAME, mode);
    CHECK(status, FAIL, "chmod");

    /* Create a protected file */
    fid = SDstart(FILE_NAME, DFACC_CREATE);
    VERIFY(fid, FAIL, "second SDstart");

    ff = HI_OPEN(FILE_NAME, DFACC_READ);
    CHECK(ff, NULL, "fopen");
    
    if (ff != NULL) {
	    HI_CLOSE(ff);
    }

#ifdef WIN32
    mode = _S_IWRITE;
#else
    mode =  S_IWUSR;
#endif

    status = chmod(FILE_NAME, mode);
    CHECK(status, FAIL, "chmod");

    /* Return the number of errors that's been kept track of so far */
    return num_errs;
}   /* test_SDAPI_ids */
#endif /* HDF */
