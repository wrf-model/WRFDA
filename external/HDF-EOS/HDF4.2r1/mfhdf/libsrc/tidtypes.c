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
 * tidtypes.c - tests the API SDcheckempty.
 * Structure of the file:
 *    test_idtype - test driver
 *	  test_SDAPI_ids    - tests SDidtype on SD API ids: sd, sds, dim ids
 *	  test_nonSDAPI_ids - tests SDidtype on non SD API ids and invalid id.
****************************************************************************/

#include "mfhdf.h"

#ifdef HDF

#include "hdftest.h"

#define FILE_NAME     "idtypes.hdf"	/* data file to test ID types */
#define X_LENGTH      10
#define Y_LENGTH      10
#define RANK          2

static intn
test_SDAPI_ids()
{
    int32     fid, dset1, dset2, dim_id;
    int32     dimsize[RANK];
    hdf_idtype_t id_type;
    intn      status;
    intn      num_errs = 0;         /* number of errors so far */

    /* Create a file */
    fid = SDstart(FILE_NAME, DFACC_CREATE);
    CHECK(fid, FAIL, "SDstart");

    /* Create an X_LENGTH by Y_LENGTH dataset, called DataSet_1 */
    dimsize[0] = X_LENGTH;
    dimsize[1] = Y_LENGTH;
    dset1 = SDcreate(fid, "DataSet_1", DFNT_INT32, RANK, dimsize);
    CHECK(dset1, FAIL, "SDcreate");

    /* Create another X_LENGTH by Y_LENGTH dataset, called DataSet_2 */
    dset2 = SDcreate(fid, "DataSet_2", DFNT_FLOAT64, RANK, dimsize);
    CHECK(dset2, FAIL, "SDcreate");

    /* Test SDidtype on the second dataset */
    id_type = SDidtype(dset2);
    VERIFY(id_type, SDS_ID, "SDidtype: id_type");

    /* Close the datasets */
    status = SDendaccess(dset1);
    CHECK(status, FAIL, "SDendaccess");
    status = SDendaccess(dset2);
    CHECK(status, FAIL, "SDendaccess");

    /* Close the file */
    status = SDend(fid);
    CHECK(status, FAIL, "SDend");

    /* Re-open the file to test SDidtype more */
    fid = SDstart(FILE_NAME, DFACC_RDWR);
    CHECK(fid, FAIL, "SDstart");

    /* Test SDidtype on the SD id */
    id_type = SDidtype(fid);
    VERIFY(id_type, SD_ID, "SDidtype: id_type");

    /* Get access to the first dataset and test SDidtype on the SDS id */
    dset1 = SDselect(fid, 0);
    CHECK(dset1, FAIL, "SDselect");
    id_type = SDidtype(dset1);
    VERIFY(id_type, SDS_ID, "SDidtype: id_type");

    /* Get access to the second dataset and test SDidtype on the SDS id */
    dset2 = SDselect(fid, 1);
    CHECK(dset2, FAIL, "SDselect");
    id_type = SDidtype(dset2);
    VERIFY(id_type, SDS_ID, "SDidtype: id_type");

    /* Get dimension handle for first dimension of DataSet_1 and test
       SDidtype on the dimension id */
    dim_id = SDgetdimid(dset1, 0);
    CHECK(dim_id, FAIL, "SDgetdimid");
    id_type = SDidtype(dim_id);
    VERIFY(id_type, DIM_ID, "SDidtype dim_id");

    /* Close the datasets */
    status = SDendaccess(dset1);
    CHECK(status, FAIL, "SDendaccess");
    status = SDendaccess(dset2);
    CHECK(status, FAIL, "SDendaccess");

    /* Close the file */
    status = SDend(fid);
    CHECK(status, FAIL, "SDend");

    /* Return the number of errors that's been kept track of so far */
    return num_errs;
}   /* test_SDAPI_ids */

static intn
test_nonSDAPI_ids ()
{
    int32     fid, gr_id, vdata_id, ri_id;
    int32     vdata_ref;
    intn      status;
    int32     dims[2]={4,5};    /* dimensions for the empty image */
    hdf_idtype_t id_type;
    intn      num_errs = 0;     /* number of errors so far */

    /* Open the HDF file */
    fid = Hopen(FILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen: tidtype.hdf");

    /* Initialize HDF for subsequent vdata access */
    status = Vstart(fid);
    CHECK(status, FAIL, "Vstart");

    /* Create a new vdata and give it a name */
    vdata_id = VSattach(fid, -1, "w");
    CHECK(vdata_id, FAIL, "VSattach");
    status = VSsetname(vdata_id, "Vdata_1");
    CHECK(status, FAIL, "VSsetname");

    /* Terminate access to the vdata */
    status = VSdetach(vdata_id);
    CHECK(status, FAIL, "VSdetach");

    /* Terminate access to the Vxxx interface and close the file */
    status = Vend(fid);
    CHECK(status, FAIL, "Vend");
    status = Hclose(fid);
    CHECK(status, FAIL, "Hclose");

    /* Open the HDF file again to test SDidtype */
    fid = Hopen(FILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen: tidtype.hdf");

    /* Initialize HDF for subsequent vdata accesses */
    status = Vstart(fid);
    CHECK(status, FAIL, "Vstart");

    /* Get access to the vdata "Vdata_1" via its reference number */
    vdata_ref = VSfind(fid, "Vdata_1");
    CHECK(vdata_ref, FAIL, "VSfind");
    vdata_id = VSattach(fid, vdata_ref, "w");
    CHECK(vdata_id, FAIL, "VSattach");

    /* Test SDidtype on the vdata id */
    id_type = SDidtype(vdata_id);
    VERIFY(id_type, NOT_SDAPI_ID, "SDidtype: id_type");

    /* Terminate access to the vdata */
    status = VSdetach(vdata_id);
    CHECK(status, FAIL, "VSdetach");

    /* Terminate access to the Vxxx interface and close the file */
    status = Vend(fid);
    CHECK(status, FAIL, "Vend");
    status = Hclose(fid);
    CHECK(status, FAIL, "Hclose");

    /* Test SDidtype on a GR raster image */

    /* Open file and initialize the GR interface */
    fid = Hopen(FILE_NAME, DFACC_RDWR, 0);
    CHECK(fid, FAIL, "Hopen: tidtype.hdf");
    gr_id = GRstart(fid);
    CHECK(gr_id, FAIL, "GRstart");

    /* Create an empty image with default fill value */
    ri_id = GRcreate(gr_id, "Empty Image", 3, DFNT_FLOAT32, 
		MFGR_INTERLACE_PIXEL, dims);
    CHECK(ri_id, FAIL, "GRcreate");

    /* Test SDidtype on the GR raster image id */
    id_type = SDidtype(ri_id);
    VERIFY(id_type, NOT_SDAPI_ID, "SDidtype: id_type");

    /* Close the image */
    status = GRendaccess(ri_id);
    CHECK(status, FAIL, "GRendaccess");

    /* Shut down the GR interface and close the file */
    status = GRend(gr_id);
    CHECK(status, FAIL, "GRend");
    status = Hclose(fid);
    CHECK(status, FAIL, "Hclose");

    /* Return the number of errors that's been kept track of so far */
    return num_errs;
}   /* test_nonSDAPI_ids */

/* Test driver for testing the API SDidtype. */
extern int
test_idtype()
{
    intn num_errs = 0;         /* number of errors */

    num_errs = num_errs + test_SDAPI_ids();
    num_errs = num_errs + test_nonSDAPI_ids();

    return num_errs;
}

#endif /* HDF */
