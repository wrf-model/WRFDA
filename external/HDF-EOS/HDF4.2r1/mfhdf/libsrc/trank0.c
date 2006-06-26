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
 * trank0.c - tests that several APIs behave "correctly" when an SDS has 
 * 		rank = 0.
 * Structure of the file:
 *    test_rank0 - test routine, called in hdftest.c
****************************************************************************/

#include "mfhdf.h"

#ifdef HDF

#include "hdftest.h"

#define FILE_NAME     "trank0.hdf"	/* data file to test for rank=0 */
#define X_LENGTH      3
#define Y_LENGTH      3
#define RANK          0

/* Test routine for testing the several APIs with SDS having rank = 0. */
extern int
test_rank0()
{
    int32     fid, sds1_id, sds2_id, sds_id;
    int32     dim_sizes[2], array_rank, num_type, attributes;
    int32     start[2], edges[2];
    comp_coder_t comp_type;	/* to retrieve compression type into */
    comp_info c_info;		/* compression information structure */
    int32     comp_flag;	/* compression flag */
    HDF_CHUNK_DEF c_def;	/* Chunking definitions */
    int32     buf[Y_LENGTH][X_LENGTH], buf_dup[Y_LENGTH][X_LENGTH];
    intn      i, j, status;
    int32     status_32;
    intn      num_errs = 0;	/* number of errors so far */

    /* Create a file */
    fid = SDstart(FILE_NAME, DFACC_CREATE);
    CHECK(fid, FAIL, "test_rank0: SDstart");

    /**** Verify that SDsetcompress fails when dataset has rank 0 ****/

    /* Create an X_LENGTH by Y_LENGTH dataset, called DataSet_1 */
    dim_sizes[0] = X_LENGTH;
    dim_sizes[1] = Y_LENGTH;
    sds1_id = SDcreate(fid, "DataSet_1", DFNT_INT32, RANK, dim_sizes);
    CHECK(sds1_id, FAIL, "test_rank0: SDcreate");

    /* Attempt to set compress but should fail */
    comp_type = COMP_CODE_DEFLATE;
    c_info.deflate.level = 6;
    status = SDsetcompress(sds1_id, comp_type, &c_info);
    VERIFY(status, FAIL, "test_rank0: SDsetcompress");

    /**** Verify that SDsetchunk fails when dataset has rank 0 ****/

    /* Create an X_LENGTH by Y_LENGTH dataset, called DataSet_2 */
    sds2_id = SDcreate(fid, "DataSet_2", DFNT_INT32, RANK, dim_sizes);
    CHECK(sds2_id, FAIL, "test_rank0: SDcreate");

    /* Attempt to set chunk but should fail */
    comp_flag = HDF_CHUNK;
    status = SDsetchunk(sds_id, c_def, comp_flag);
    VERIFY(status, FAIL, "test_rank0: SDsetchunk");

     /* Close the SDSs */
    status = SDendaccess(sds1_id);
    CHECK(status, FAIL, "test_rank0: SDendaccess");
    status = SDendaccess(sds2_id);
    CHECK(status, FAIL, "test_rank0: SDendaccess");

    /**** Verify that SDwritedata fails when dataset has rank 0 ****/

    /* Initialize buffer and its copy, so later can verify that the buffer
       is not corrupted */
    for (j=0; j<Y_LENGTH; j++)
    {
        for (i=0; i<X_LENGTH; i++)
	{
	    buf[j][i] = i;
	    buf_dup[j][i] = i;
	}
    }
    /* Select the first dataset */
    sds_id = SDselect(fid, 0);
    CHECK(sds_id, FAIL, "test_rank0: SDselect");

    /* Define the location and size of the dataset to be written to the file */
    start[0] = 0;
    start[1] = 0;
    edges[0] = Y_LENGTH;
    edges[1] = X_LENGTH;

    /* Write the stored data to the dataset */
    status = SDwritedata(sds_id, start, NULL, edges, (VOIDP)buf);
    VERIFY(status, FAIL, "test_rank0:SDwritedata");

     /* Close the SDS */
    status = SDendaccess(sds_id);
    CHECK(status, FAIL, "test_rank0: SDendaccess");

    /**** Verify that SDreaddata doesn't corrupt user's buffer, when dataset 
	  has rank 0; at this time, it does (buf[0][0] changed), but EP said
    	  to just document it for now 2/8/05 - BMR ****/

    /* Select the first dataset */
    sds_id = SDselect(fid, 0);
    CHECK(sds_id, FAIL, "test_rank0: SDselect");

    /* Read the data set */
    start[0] = 0;
    start[1] = 0;
    edges[0] = Y_LENGTH;
    edges[1] = X_LENGTH;
    status = SDreaddata (sds_id, start, NULL, edges, (VOIDP)buf);
    CHECK(status, FAIL, "test_rank0: SDreaddata");

     /* Close the SDS */
    status = SDendaccess(sds_id);
    CHECK(status, FAIL, "test_rank0: SDendaccess");

    /**** Verify various functions ****/

    /* Select the second dataset */
    sds_id = SDselect(fid, 1);
    CHECK(sds_id, FAIL, "test_rank0: SDselect");

    /* Retrieve information of the data set */
    status = SDgetinfo(sds_id, NULL, &array_rank, dim_sizes, &num_type, &attributes);
    CHECK(status, FAIL, "test_rank0: SDgetinfo");
    VERIFY(array_rank, 0, "test_rank0: SDgetinfo");

    /* Verify that SDisrecord doesn't cause seg. fault */
    status_32 = SDisrecord(sds_id);
    VERIFY(status_32, TRUE, "test_rank0: SDisrecord");

     /* Close the SDSs */
    status = SDendaccess(sds_id);
    CHECK(status, FAIL, "test_rank0: SDendaccess");

    /* Close the SD interface */
    status = SDend(fid);
    CHECK(status, FAIL, "test_rank0: SDend");

    return num_errs;
}

#endif /* HDF */
