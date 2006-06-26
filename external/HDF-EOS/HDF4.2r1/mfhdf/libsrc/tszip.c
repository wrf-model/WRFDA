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

#include "mfhdf.h"
#include "hdftest.h"
#ifdef H4_HAVE_LIBSZ
#include "szlib.h"
#endif

#define FILE_NAME8	"SDS_8_sziped.hdf"
#define FILE_NAME16	"SDS_16_sziped.hdf"
#define FILE_NAME32	"SDS_32_sziped.hdf"
#define FILE_NAMEfl32	"SDS_fl32_sziped.hdf"
#define FILE_NAMEfl64	"SDS_fl64_sziped.hdf"
#define SDS_NAME	"SzipedData"

#define RANK		2
#define WIDTH		6
#define LENGTH		9

static intn 
test_szip_SDS8bit()
{
#ifdef H4_HAVE_LIBSZ
   /************************* Variable declaration **************************/

   int32	sd_id, sds_id;
   intn 	status;
   int32	dim_sizes[2], array_rank, num_type, attributes;
   char 	name[MAX_NC_NAME];
   comp_info 	c_info;
   int32        start[2], edges[2];
   int8         fill_value = 0; /* Fill value */
   int          i,j;
   int    	num_errs = 0;   /* number of errors so far */
   comp_coder_t comp_type;      /* to retrieve compression type into */
   comp_info    cinfo;          /* compression information structure */
   uint32       comp_config;
   int8         out_data[LENGTH][WIDTH];
   int8         in_data[LENGTH][WIDTH]={
	   			 1,1,2,2,3,4,
	   			 1,1,2,2,3,4,
				 1,1,2,2,3,4,
				 3,3,0,4,3,4,
				 3,3,0,4,3,4,
				 3,3,0,4,3,4,
				 0,0,6,6,3,4,
				 5,5,6,6,3,4,
				 0,0,6,6,3,4};

    /********************* End of variable declaration ***********************/

    HCget_config_info(COMP_CODE_SZIP,&comp_config);
    
    /* Create the file and initialize SD interface */
    sd_id = SDstart (FILE_NAME8, DFACC_CREATE);
    CHECK(sd_id, FAIL, "SDstart");

    /* Create the SDS */
    dim_sizes[0] = LENGTH;
    dim_sizes[1] = WIDTH;
    sds_id = SDcreate (sd_id, SDS_NAME, DFNT_INT8, RANK, dim_sizes);
    CHECK(sds_id, FAIL, "SDcreate:Failed to create a data set for szip compression testing");

    /* Define the location, pattern, and size of the data set */
    for (i = 0; i < RANK; i++) {
	start[i] = 0;
	edges[i] = dim_sizes[i];
	}

    /* Fill the SDS array with the fill value */
    status = SDsetfillvalue (sds_id, (VOIDP)&fill_value);
    CHECK(status, FAIL, "SDsetfillvalue");

    /* Initialization for SZIP */
    c_info.szip.pixels_per_block = 2;

    c_info.szip.options_mask = SZ_EC_OPTION_MASK;
    c_info.szip.options_mask |= SZ_RAW_OPTION_MASK;
    c_info.szip.bits_per_pixel = 0;
    c_info.szip.pixels = 0;
    c_info.szip.pixels_per_scanline = 0;

    /* Set the compression */
    status = SDsetcompress (sds_id, COMP_CODE_SZIP, &c_info);
    if ((comp_config & COMP_ENCODER_ENABLED) == COMP_ENCODER_ENABLED) {
	/* should work */
       CHECK(status, FAIL, "SDsetcompress");
    } else {
       /* skip rest of test?? */
        status = SDendaccess (sds_id);
        CHECK(status, FAIL, "SDendaccess");

        status = SDend (sd_id);
        CHECK(status, FAIL, "SDend");
        printf("szip_SD8: SKIPPED\n");
        return num_errs;
    }

    /* Write data to the SDS */
    status = SDwritedata(sds_id, start, NULL, edges, (VOIDP)in_data);
    CHECK(status, FAIL, "SDwritedata");

    /* Terminate access to the data set */
    status = SDendaccess (sds_id);
    CHECK(status, FAIL, "SDendaccess");

    /* Terminate access to the SD interface and close the file to 
       flush the compressed info to the file */
    status = SDend (sd_id);
    CHECK(status, FAIL, "SDend");

    /*
    * Verify the compressed data
    */

    /* Reopen the file and select the first SDS */
    sd_id = SDstart (FILE_NAME8, DFACC_READ);
    CHECK(sd_id, FAIL, "SDstart");

    sds_id = SDselect (sd_id, 0);
    CHECK(sds_id, FAIL, "SDselect:Failed to select a data set for szip compression testing");

    /* Retrieve information of the data set */
    status = SDgetinfo(sds_id, name, &array_rank, dim_sizes, &num_type, &attributes);
    CHECK(status, FAIL, "SDgetinfo");

    /* Retrieve compression informayion about the dataset */
    comp_type = COMP_CODE_INVALID;  /* reset variables before retrieving info */
    HDmemset(&cinfo, 0, sizeof(cinfo)) ;

    status = SDgetcompress(sds_id, &comp_type, &cinfo);
    CHECK(status, FAIL, "SDgetcompress");
    VERIFY(comp_type, COMP_CODE_SZIP, "SDgetcompress");

    /* Wipe out the output buffer */
    HDmemset(&out_data, 0, sizeof(out_data));

    /* Read the data set */
    start[0] = 0;
    start[1] = 0;
    edges[0] = LENGTH;
    edges[1] = WIDTH;
    status = SDreaddata (sds_id, start, NULL, edges, (VOIDP)out_data);
    CHECK(status, FAIL, "SDreaddata");

    /* Compare read data against input data */
    for (j=0; j<LENGTH; j++) 
    {
        for (i=0; i<WIDTH; i++)
	    if (out_data[j][i] != in_data[j][i])
	    {
		fprintf(stderr,"Bogus val in loc [%d][%d] in compressed dset, want %ld got %ld\n", j, i, (long)in_data[j][i], (long)out_data[j][i]);
		num_errs++;
	    }
    }

    /* Terminate access to the data set */
    status = SDendaccess (sds_id);
    CHECK(status, FAIL, "SDendaccess");

    /* Terminate access to the SD interface and close the file */
    status = SDend (sd_id);
    CHECK(status, FAIL, "SDend");

    /* Return the number of errors that's been kept track of so far */
    return num_errs;
#else
    return 0;
#endif
} /* test_szip_SDS8bit */

static intn 
test_szip_SDS16bit()
{
#ifdef H4_HAVE_LIBSZ
    /************************* Variable declaration **************************/

    int32	sd_id, sds_id;
    intn 	status;
    int32	dim_sizes[2], array_rank, num_type, attributes;
    char	name[MAX_NC_NAME];
    comp_info	c_info;
   uint32       comp_config;
    int32       start[2], edges[2];
    int16       fill_value = 0;   /* Fill value */
    int         i,j;
    int		num_errs = 0;    /* number of errors so far */
    int16	out_data[LENGTH][WIDTH];
    int16	in_data[LENGTH][WIDTH]={
	   			 100,100,200,200,300,400,
	   			 100,100,200,200,300,400,
				 100,100,200,200,300,400,
				 300,300,  0,400,300,400,
				 300,300,  0,400,300,400,
				 300,300,  0,400,300,400,
				   0,  0,600,600,300,400,
				 500,500,600,600,300,400,
				   0,  0,600,600,300,400};

    /********************* End of variable declaration ***********************/

    HCget_config_info(COMP_CODE_SZIP,&comp_config);
    /* Create the file and initialize SD interface */
    sd_id = SDstart (FILE_NAME16, DFACC_CREATE);
    CHECK(sd_id, FAIL, "SDstart");

    /* Create the SDS */
    dim_sizes[0] = LENGTH;
    dim_sizes[1] = WIDTH;
    sds_id = SDcreate (sd_id, SDS_NAME, DFNT_INT16, RANK, dim_sizes);
    CHECK(sds_id, FAIL, "SDcreate:Failed to create a data set for szip compression testing");

    /* Define the location, pattern, and size of the data set */
    for (i = 0; i < RANK; i++) {
	start[i] = 0;
	edges[i] = dim_sizes[i];
	}

    /* Fill the SDS array with the fill value */
    status = SDsetfillvalue (sds_id, (VOIDP)&fill_value);
    CHECK(status, FAIL, "SDsetfillvalue");

    /* Initialize for SZIP */
    c_info.szip.pixels_per_block = 2;

    c_info.szip.options_mask = SZ_EC_OPTION_MASK;
    c_info.szip.options_mask |= SZ_RAW_OPTION_MASK;
    c_info.szip.bits_per_pixel = 0;
    c_info.szip.pixels = 0;
    c_info.szip.pixels_per_scanline = 0;

    /* Set the compression */
    status = SDsetcompress (sds_id, COMP_CODE_SZIP, &c_info);
    if ((comp_config & COMP_ENCODER_ENABLED) == COMP_ENCODER_ENABLED) {
	/* should work */
       CHECK(status, FAIL, "SDsetcompress");
    } else {
       /* skip rest of test?? */
        status = SDendaccess (sds_id);
        CHECK(status, FAIL, "SDendaccess");

        status = SDend (sd_id);
        CHECK(status, FAIL, "SDend");
        printf("szip_SD16: SKIPPED\n");
        return num_errs;
    }

    /* Write data to the SDS */
    status = SDwritedata(sds_id, start, NULL, edges, (VOIDP)in_data);
    CHECK(status, FAIL, "SDwritedata");

    /* Terminate access to the data set */
    status = SDendaccess (sds_id);
    CHECK(status, FAIL, "SDendaccess");

    /* Terminate access to the SD interface and close the file to 
       flush the compressed info to the file */
    status = SDend (sd_id);
    CHECK(status, FAIL, "SDend");

    /*
    * Verify the compressed data
    */

    /* Reopen the file and select the first SDS */
    sd_id = SDstart (FILE_NAME16, DFACC_READ);
    CHECK(sd_id, FAIL, "SDstart");

    sds_id = SDselect (sd_id, 0);
    CHECK(sds_id, FAIL, "SDselect:Failed to select a data set for szip compression testing");

    /* Retrieve information of the data set */
    status = SDgetinfo(sds_id, name, &array_rank, dim_sizes, &num_type, &attributes);
    CHECK(status, FAIL, "SDgetinfo");

    /* Wipe out the output buffer */
    HDmemset(&out_data, 0, sizeof(out_data));

    /* Read the data set */
    start[0] = 0;
    start[1] = 0;
    edges[0] = LENGTH;
    edges[1] = WIDTH;
    status = SDreaddata (sds_id, start, NULL, edges, (VOIDP)out_data);
    CHECK(status, FAIL, "SDreaddata");

    /* Compare read data against input data */
    for (j=0; j<LENGTH; j++) 
    {
        for (i=0; i<WIDTH; i++)
	    if (out_data[j][i] != in_data[j][i])
	    {
		fprintf(stderr,"Bogus val in loc [%d][%d] in compressed dset, want %ld got %ld\n", j, i, (long)in_data[j][i], (long)out_data[j][i]);
		num_errs++;
	    }
    }

    /* Terminate access to the data set */
    status = SDendaccess (sds_id);
    CHECK(status, FAIL, "SDendaccess");

    /* Terminate access to the SD interface and close the file */
    status = SDend (sd_id);
    CHECK(status, FAIL, "SDend");

    /* Return the number of errors that's been kept track of so far */
    return num_errs;
#else
    return 0;
#endif
}  /* test_szip_SDS16bit */

static intn 
test_szip_SDS32bit()
{
#ifdef H4_HAVE_LIBSZ
    /************************* Variable declaration **************************/

    int32	sd_id, sds_id;
    intn 	status;
    int32	dim_sizes[2], array_rank, num_type, attributes;
    char	name[MAX_NC_NAME];
    comp_info	c_info;
   uint32       comp_config;
    int32       start[2], edges[2];
    int32       fill_value = 0;   /* Fill value */
    int         i,j;
    int		num_errs = 0;    /* number of errors so far */
    int32	out_data[LENGTH][WIDTH];
    int32	in_data[LENGTH][WIDTH]={
	   			 100,100,200,200,300,400,
	   			 100,100,200,200,300,400,
				 100,100,200,200,300,400,
				 300,300,  0,400,300,400,
				 300,300,  0,400,300,400,
				 300,300,  0,400,300,400,
				   0,  0,600,600,300,400,
				 500,500,600,600,300,400,
				   0,  0,600,600,300,400};

    /********************* End of variable declaration ***********************/

    HCget_config_info(COMP_CODE_SZIP,&comp_config);
    /* Create the file and initialize SD interface */
    sd_id = SDstart (FILE_NAME32, DFACC_CREATE);
    CHECK(sd_id, FAIL, "SDstart");

    /* Create the SDS */
    dim_sizes[0] = LENGTH;
    dim_sizes[1] = WIDTH;
    sds_id = SDcreate (sd_id, SDS_NAME, DFNT_INT32, RANK, dim_sizes);
    CHECK(sds_id, FAIL, "SDcreate:Failed to create a data set for szip compression testing");

    /* Define the location, pattern, and size of the data set */
    for (i = 0; i < RANK; i++) {
	start[i] = 0;
	edges[i] = dim_sizes[i];
	}

    /* Fill the SDS array with the fill value */
    status = SDsetfillvalue (sds_id, (VOIDP)&fill_value);
    CHECK(status, FAIL, "SDsetfillvalue");

    /* Initialize for SZIP */
    c_info.szip.pixels_per_block = 2;

    c_info.szip.options_mask = SZ_EC_OPTION_MASK;
    c_info.szip.options_mask |= SZ_RAW_OPTION_MASK;
    c_info.szip.bits_per_pixel = 0;
    c_info.szip.pixels = 0;
    c_info.szip.pixels_per_scanline = 0;

    /* Set the compression */
    status = SDsetcompress (sds_id, COMP_CODE_SZIP, &c_info);
    if ((comp_config & COMP_ENCODER_ENABLED) == COMP_ENCODER_ENABLED) {
	/* should work */
       CHECK(status, FAIL, "SDsetcompress");
    } else {
       /* skip rest of test?? */
        status = SDendaccess (sds_id);
        CHECK(status, FAIL, "SDendaccess");

        status = SDend (sd_id);
        CHECK(status, FAIL, "SDend");
        printf("szip_SD32: SKIPPED\n");
        return num_errs;
    }

    /* Write data to the SDS */
    status = SDwritedata(sds_id, start, NULL, edges, (VOIDP)in_data);
    CHECK(status, FAIL, "SDwritedata");

    /* Terminate access to the data set */
    status = SDendaccess (sds_id);
    CHECK(status, FAIL, "SDendaccess");

    /* Terminate access to the SD interface and close the file to 
       flush the compressed info to the file */
    status = SDend (sd_id);
    CHECK(status, FAIL, "SDend");

    /*
    * Verify the compressed data
    */

    /* Reopen the file and select the first SDS */
    sd_id = SDstart (FILE_NAME32, DFACC_READ);
    CHECK(sd_id, FAIL, "SDstart");

    sds_id = SDselect (sd_id, 0);
    CHECK(sds_id, FAIL, "SDselect:Failed to select a data set for szip compression testing");

    /* Retrieve information of the data set */
    status = SDgetinfo(sds_id, name, &array_rank, dim_sizes, &num_type, &attributes);
    CHECK(status, FAIL, "SDgetinfo");

    /* Wipe out the output buffer */
    HDmemset(&out_data, 0, sizeof(out_data));

    /* Read the data set */
    start[0] = 0;
    start[1] = 0;
    edges[0] = LENGTH;
    edges[1] = WIDTH;
    status = SDreaddata (sds_id, start, NULL, edges, (VOIDP)out_data);
    CHECK(status, FAIL, "SDreaddata");

    /* Compare read data against input data */
    for (j=0; j<LENGTH; j++) 
    {
        for (i=0; i<WIDTH; i++)
	    if (out_data[j][i] != in_data[j][i])
	    {
		fprintf(stderr,"Bogus val in loc [%d][%d] in compressed dset, want %ld got %ld\n", j, i, (long)in_data[j][i], (long)out_data[j][i]);
		num_errs++;
	    }
    }

    /* Terminate access to the data set */
    status = SDendaccess (sds_id);
    CHECK(status, FAIL, "SDendaccess");

    /* Terminate access to the SD interface and close the file */
    status = SDend (sd_id);
    CHECK(status, FAIL, "SDend");

    /* Return the number of errors that's been kept track of so far */
    return num_errs;
#else
    return 0;
#endif
}  /* test_szip_SDS32bit */

static intn 
test_szip_SDSfl32bit()
{
#ifdef H4_HAVE_LIBSZ
    /************************* Variable declaration **************************/

    int32	sd_id, sds_id;
    intn 	status;
    int32	dim_sizes[2], array_rank, num_type, attributes;
    char	name[MAX_NC_NAME];
    comp_info	c_info;
/*
    int32	pixels_per_scanline;
*/
    int32       start[2], edges[2];
    float32     fill_value = 0;   /* Fill value */
    int         i,j;
    int		num_errs = 0;    /* number of errors so far */
    float32	out_data[LENGTH][WIDTH];
    float32	in_data[LENGTH][WIDTH]={
	   			 100.0,100.0,200.0,200.0,300.0,400.0,
	   			 100.0,100.0,200.0,200.0,300.0,400.0,
				 100.0,100.0,200.0,200.0,300.0,400.0,
				 300.0,300.0,  0.0,400.0,300.0,400.0,
				 300.0,300.0,  0.0,400.0,300.0,400.0,
				 300.0,300.0,  0.0,400.0,300.0,400.0,
				   0.0,  0.0,600.0,600.0,300.0,400.0,
				 500.0,500.0,600.0,600.0,300.0,400.0,
				   0.0,  0.0,600.0,600.0,300.0,400.0};

    /********************* End of variable declaration ***********************/

    /* Create the file and initialize SD interface */
    sd_id = SDstart (FILE_NAMEfl32, DFACC_CREATE);
    CHECK(sd_id, FAIL, "SDstart");

    /* Create the SDS */
    dim_sizes[0] = LENGTH;
    dim_sizes[1] = WIDTH;
    sds_id = SDcreate (sd_id, SDS_NAME, DFNT_FLOAT32, RANK, dim_sizes);
    CHECK(sds_id, FAIL, "SDcreate:Failed to create a data set for szip compression testing");

    /* Define the location, pattern, and size of the data set */
    for (i = 0; i < RANK; i++) {
	start[i] = 0;
	edges[i] = dim_sizes[i];
	}

    /* Fill the SDS array with the fill value */
    status = SDsetfillvalue (sds_id, (VOIDP)&fill_value);
    CHECK(status, FAIL, "SDsetfillvalue");

    /* Initialize for SZIP */
    c_info.szip.pixels_per_block = 2;

    c_info.szip.options_mask = SZ_EC_OPTION_MASK;
    c_info.szip.options_mask |= SZ_RAW_OPTION_MASK;
    c_info.szip.bits_per_pixel = 0;
    c_info.szip.pixels = 0;
    c_info.szip.pixels_per_scanline = 0;

    /* Set the compression */
    status = SDsetcompress (sds_id, COMP_CODE_SZIP, &c_info);
    if (SZ_encoder_enabled()) {
	/* should pass */
        CHECK(status, FAIL, "SDsetcompress");
    } else {
        return num_errs;
    }

    /* Write data to the SDS */
    status = SDwritedata(sds_id, start, NULL, edges, (VOIDP)in_data);
    CHECK(status, FAIL, "SDwritedata");

    /* Terminate access to the data set */
    status = SDendaccess (sds_id);
    CHECK(status, FAIL, "SDendaccess");

    /* Terminate access to the SD interface and close the file to 
       flush the compressed info to the file */
    status = SDend (sd_id);
    CHECK(status, FAIL, "SDend");

    /*
    * Verify the compressed data
    */

    /* Reopen the file and select the first SDS */
    sd_id = SDstart (FILE_NAMEfl32, DFACC_READ);
    CHECK(sd_id, FAIL, "SDstart");

    sds_id = SDselect (sd_id, 0);
    CHECK(sds_id, FAIL, "SDselect:Failed to select a data set for szip compression testing");

    /* Retrieve information of the data set */
    status = SDgetinfo(sds_id, name, &array_rank, dim_sizes, &num_type, &attributes);
    CHECK(status, FAIL, "SDgetinfo");

    /* Wipe out the output buffer */
    HDmemset(&out_data, 0, sizeof(out_data));

    /* Read the data set */
    start[0] = 0;
    start[1] = 0;
    edges[0] = LENGTH;
    edges[1] = WIDTH;
    status = SDreaddata (sds_id, start, NULL, edges, (VOIDP)out_data);
    CHECK(status, FAIL, "SDreaddata");

    /* Compare read data against input data */
    for (j=0; j<LENGTH; j++) 
    {
        for (i=0; i<WIDTH; i++)
	    if (out_data[j][i] != in_data[j][i])
	    {
		fprintf(stderr,"Bogus val in loc [%d][%d] in compressed dset, want %ld got %ld\n", j, i, (long)in_data[j][i], (long)out_data[j][i]);
		num_errs++;
	    }
    }

    /* Terminate access to the data set */
    status = SDendaccess (sds_id);
    CHECK(status, FAIL, "SDendaccess");

    /* Terminate access to the SD interface and close the file */
    status = SDend (sd_id);
    CHECK(status, FAIL, "SDend");

    /* Return the number of errors that's been kept track of so far */
    return num_errs;
#else
    return 0;
#endif
}  /* test_szip_SDSfl32bit */

static intn 
test_szip_SDSfl64bit()
{
#ifdef H4_HAVE_LIBSZ
    /************************* Variable declaration **************************/

    int32	sd_id, sds_id;
    intn 	status;
    int32	dim_sizes[2], array_rank, num_type, attributes;
    char	name[MAX_NC_NAME];
    comp_info	c_info;
   uint32       comp_config;
    int32       start[2], edges[2];
    float64     fill_value = 0;   /* Fill value */
    int         i,j;
    int		num_errs = 0;    /* number of errors so far */
    float64	out_data[LENGTH][WIDTH];
    float64	in_data[LENGTH][WIDTH]={
	   			 100.0,100.0,200.0,200.0,300.0,400.0,
	   			 100.0,100.0,200.0,200.0,300.0,400.0,
				 100.0,100.0,200.0,200.0,300.0,400.0,
				 300.0,300.0,  0.0,400.0,300.0,400.0,
				 300.0,300.0,  0.0,400.0,300.0,400.0,
				 300.0,300.0,  0.0,400.0,300.0,400.0,
				   0.0,  0.0,600.0,600.0,300.0,400.0,
				 500.0,500.0,600.0,600.0,300.0,400.0,
				   0.0,  0.0,600.0,600.0,300.0,400.0};

    /********************* End of variable declaration ***********************/

    HCget_config_info(COMP_CODE_SZIP,&comp_config);
    /* Create the file and initialize SD interface */
    sd_id = SDstart (FILE_NAMEfl64, DFACC_CREATE);
    CHECK(sd_id, FAIL, "SDstart");

    /* Create the SDS */
    dim_sizes[0] = LENGTH;
    dim_sizes[1] = WIDTH;
    sds_id = SDcreate (sd_id, SDS_NAME, DFNT_FLOAT64, RANK, dim_sizes);
    CHECK(sds_id, FAIL, "SDcreate:Failed to create a data set for szip compression testing");

    /* Define the location, pattern, and size of the data set */
    for (i = 0; i < RANK; i++) {
	start[i] = 0;
	edges[i] = dim_sizes[i];
	}

    /* Fill the SDS array with the fill value */
    status = SDsetfillvalue (sds_id, (VOIDP)&fill_value);
    CHECK(status, FAIL, "SDsetfillvalue");

    /* Initialization for SZIP */
    c_info.szip.pixels_per_block = 2;

    c_info.szip.options_mask = SZ_EC_OPTION_MASK;
    c_info.szip.options_mask |= SZ_RAW_OPTION_MASK;
    c_info.szip.bits_per_pixel = 0;
    c_info.szip.pixels = 0;
    c_info.szip.pixels_per_scanline = 0;

    /* Set the compression */
    status = SDsetcompress (sds_id, COMP_CODE_SZIP, &c_info);
    if ((comp_config & COMP_ENCODER_ENABLED) == COMP_ENCODER_ENABLED) {
	/* should work */
       CHECK(status, FAIL, "SDsetcompress");
    } else {
       /* skip rest of test?? */
        status = SDendaccess (sds_id);
        CHECK(status, FAIL, "SDendaccess");

        status = SDend (sd_id);
        CHECK(status, FAIL, "SDend");
        printf("szip_SD64: SKIPPED\n");
        return num_errs;
    }

    /* Write data to the SDS */
    status = SDwritedata(sds_id, start, NULL, edges, (VOIDP)in_data);
    CHECK(status, FAIL, "SDwritedata");

    /* Terminate access to the data set */
    status = SDendaccess (sds_id);
    CHECK(status, FAIL, "SDendaccess");

    /* Terminate access to the SD interface and close the file to 
       flush the compressed info to the file */
    status = SDend (sd_id);
    CHECK(status, FAIL, "SDend");

    /*
     * Verify the compressed data
     */

    /* Reopen the file and select the first SDS */
    sd_id = SDstart (FILE_NAMEfl64, DFACC_READ);
    CHECK(sd_id, FAIL, "SDstart");

    sds_id = SDselect (sd_id, 0);
    CHECK(sds_id, FAIL, "SDselect:Failed to select a data set for szip compression testing");

    /* Retrieve information of the data set */
    status = SDgetinfo(sds_id, name, &array_rank, dim_sizes, &num_type, &attributes);
    CHECK(status, FAIL, "SDgetinfo");

    /* Wipe out the output buffer */
    HDmemset(&out_data, 0, sizeof(out_data));

    /* Read the data set */
    start[0] = 0;
    start[1] = 0;
    edges[0] = LENGTH;
    edges[1] = WIDTH;
    status = SDreaddata (sds_id, start, NULL, edges, (VOIDP)out_data);
    CHECK(status, FAIL, "SDreaddata");

    /* Compare read data against input data */
    for (j=0; j<LENGTH; j++) 
    {
        for (i=0; i<WIDTH; i++)
	    if (out_data[j][i] != in_data[j][i])
	    {
		fprintf(stderr,"Bogus val in loc [%d][%d] in compressed dset, want %ld got %ld\n", j, i, (long)in_data[j][i], (long)out_data[j][i]);
		num_errs++;
	    }
    }

    /* Terminate access to the data set */
    status = SDendaccess (sds_id);
    CHECK(status, FAIL, "SDendaccess");

    /* Terminate access to the SD interface and close the file */
    status = SDend (sd_id);
    CHECK(status, FAIL, "SDend");

    /* Return the number of errors that's been kept track of so far */
    return num_errs;
#else
    return 0;
#endif
}  /* test_szip_SDSfl64bit */


#define FILE_NAME	"SDSchunkedsziped.hdf"
#define SDS_NAME_CH	"ChunkedData"
#define RANK_CH         2	/* rank of the chunked data set */
#define WIDTH_CH 	4	/* width of the chunked data set */
#define LENGTH_CH	9	/* length of the chunked data set */
#define CWIDTH		2	/* width of the chunk */
#define CLENGTH		3	/* length of the chunk */

static intn 
test_szip_chunk()
{
#ifdef H4_HAVE_LIBSZ
   /************************* Variable declaration **************************/

   int32         sd_id, sds_id, sds_index;
   intn          status;
   int32         flag, maxcache, new_maxcache;
   int32         dim_sizes[2], origin[2];
   HDF_CHUNK_DEF c_def; /* Chunking definitions */ 
   uint32       comp_config;
   int32         comp_flag;
   int16         all_data[LENGTH_CH][WIDTH_CH];
   int32         start[2], edges[2];
   int16         chunk_out[CLENGTH][CWIDTH];
   int16         row[CWIDTH] = { 5, 5 };
   int16         column[CLENGTH] = { 4, 4, 4 };
   int16         fill_value = 0;   /* Fill value */
   comp_coder_t  comp_type;        /* to retrieve compression type into */
   comp_info     cinfo;            /* compression information structure */
   int    	 num_errs = 0;     /* number of errors so far */
   int           i,j;
   /*
   * Define all chunks.  Note that chunks 4 & 5 are not used to write,
   * only to verify the read data.  The 'row' and 'column' are used
   * to write in the place of these chunks.
   */
          int16 chunk1[CLENGTH][CWIDTH] = { 1, 1,
                                            1, 1,
                                            1, 1 }; 

          int16 chunk2[CLENGTH][CWIDTH] = { 2, 2,
                                            2, 2,
                                            2, 2 }; 

          int16 chunk3[CLENGTH][CWIDTH] = { 3, 3,
                                            3, 3,
                                            3, 3 }; 

          int16 chunk4[CLENGTH][CWIDTH] = { 0, 4,
                                            0, 4,
                                            0, 4 }; 

          int16 chunk5[CLENGTH][CWIDTH] = { 0, 0,
                                            5, 5,
                                            0, 0 }; 

          int16 chunk6[CLENGTH][CWIDTH] = { 6, 6,
                                            6, 6,
                                            6, 6 };

    /* Initialize chunk lengths. */
    c_def.comp.chunk_lengths[0] = CLENGTH;
    c_def.comp.chunk_lengths[1] = CWIDTH;

    HCget_config_info(COMP_CODE_SZIP,&comp_config);
    /* Create the file and initialize SD interface. */
    sd_id = SDstart (FILE_NAME, DFACC_CREATE);
    CHECK(sd_id, FAIL, "SDstart");

    /* Create LENGTH_CHxWIDTH_CH SDS. */
    dim_sizes[0] = LENGTH_CH;
    dim_sizes[1] = WIDTH_CH;
    sds_id = SDcreate (sd_id, SDS_NAME_CH,DFNT_INT16, RANK_CH, dim_sizes);
    CHECK(sds_id, FAIL, "SDcreate:Failed to create a data set for chunking/szip compression testing");

    /* Fill the SDS array with the fill value. */
    status = SDsetfillvalue (sds_id, (VOIDP)&fill_value);
    CHECK(status, FAIL, "SDsetfillvalue");

    /* Set parameters for Chunking/SZIP */
    c_def.comp.comp_type = COMP_CODE_SZIP;
    c_def.comp.cinfo.szip.pixels_per_block = 2;

    c_def.comp.cinfo.szip.options_mask = SZ_EC_OPTION_MASK;
    c_def.comp.cinfo.szip.options_mask |= SZ_MSB_OPTION_MASK;
    c_def.comp.cinfo.szip.bits_per_pixel = 0;
    c_def.comp.cinfo.szip.pixels = 0;
    c_def.comp.cinfo.szip.pixels_per_scanline = 0;
    comp_flag = HDF_CHUNK | HDF_COMP;
    status = SDsetchunk (sds_id, c_def, comp_flag);
    if ((comp_config & COMP_ENCODER_ENABLED) == COMP_ENCODER_ENABLED) {
	/* should work */
       CHECK(status, FAIL, "SDsetchunk");
    } else {
       /* skip rest of test?? */
        status = SDendaccess (sds_id);
        CHECK(status, FAIL, "SDendaccess");

        status = SDend (sd_id);
        CHECK(status, FAIL, "SDend");
        printf("szip chunk: SKIPPED\n");
        return num_errs;
    }

    /* Set chunk cache to hold maximum of 3 chunks. */
    maxcache = 3;
    flag = 0;
    new_maxcache = SDsetchunkcache (sds_id, maxcache, flag);
    CHECK(new_maxcache, FAIL, "SDsetchunkcache");

    /* 
     * Write chunks using SDwritechunk function.  Chunks can be written 
     * in any order. 
     */

    /* Write the chunk with the coordinates (0,0). */
    origin[0] = 0;
    origin[1] = 0;
    status = SDwritechunk (sds_id, origin, (VOIDP) chunk1);
    CHECK(status, FAIL, "SDwritechunk");

    /* Write the chunk with the coordinates (1,0). */
    origin[0] = 1;
    origin[1] = 0;
    status = SDwritechunk (sds_id, origin, (VOIDP) chunk3);
    CHECK(status, FAIL, "SDwritechunk");

    /* Write the chunk with the coordinates (0,1). */
    origin[0] = 0;
    origin[1] = 1;
    status = SDwritechunk (sds_id, origin, (VOIDP) chunk2);
    CHECK(status, FAIL, "SDwritechunk");

    /* Write chunk with the coordinates (1,2) using SDwritedata function. */
    start[0] = 6;
    start[1] = 2;
    edges[0] = 3;
    edges[1] = 2;
    status = SDwritedata (sds_id, start, NULL, edges, (VOIDP) chunk6); 
    CHECK(status, FAIL, "SDwritedata");

    /* Fill second column in the chunk with the coordinates (1,1) using 
     * SDwritedata function. */
    start[0] = 3;
    start[1] = 3;
    edges[0] = 3;
    edges[1] = 1;
    status = SDwritedata (sds_id, start, NULL, edges, (VOIDP) column); 
    CHECK(status, FAIL, "SDwritedata");

    /* Fill second row in the chunk with the coordinates (0,2) using 
     * SDwritedata function. */
    start[0] = 7;
    start[1] = 0;
    edges[0] = 1;
    edges[1] = 2;
    status = SDwritedata (sds_id, start, NULL, edges, (VOIDP) row); 
    CHECK(status, FAIL, "SDwritedata");
           
    /* Terminate access to the data set. */
    status = SDendaccess (sds_id);
    CHECK(status, FAIL, "SDendaccess");

    /* Terminate access to the SD interface and close the file. */
    status = SDend (sd_id);
    CHECK(status, FAIL, "SDend");

    /*
     * Verify the compressed data
     */

    /* Reopen the file and access the first data set. */
    sd_id = SDstart (FILE_NAME, DFACC_READ);
    sds_index = 0;
    sds_id = SDselect (sd_id, sds_index);
    CHECK(sds_id, FAIL, "SDselect:Failed to select a data set for chunking/szip compression testing");

    /* Retrieve compression information about the dataset */
    comp_type = COMP_CODE_INVALID;  /* reset variables before retrieving info */
    HDmemset(&cinfo, 0, sizeof(cinfo)) ;

    status = SDgetcompress(sds_id, &comp_type, &cinfo);
    CHECK(status, FAIL, "SDgetcompress");
    VERIFY(comp_type, COMP_CODE_SZIP, "SDgetcompress");

    /* Read the entire data set using SDreaddata function. */
    start[0] = 0;
    start[1] = 0;
    edges[0] = LENGTH_CH;
    edges[1] = WIDTH_CH;
    status = SDreaddata (sds_id, start, NULL, edges, (VOIDP)all_data);
    CHECK(status, FAIL, "SDreaddata");

    /* 
    * This is how the entire array should look like:
    *
    *          1 1 2 2
    *          1 1 2 2
    *          1 1 2 2
    *          3 3 0 4
    *          3 3 0 4
    *          3 3 0 4
    *          0 0 6 6
    *          5 5 6 6
    *          0 0 6 6
    */

    /* Read chunk #4 with the coordinates (1,1) and verify it. */
    origin[0] = 1;
    origin[1] = 1;    	
    status = SDreadchunk (sds_id, origin, chunk_out);
    CHECK(status, FAIL, "SDreadchunk");

    for (j=0; j<CLENGTH; j++) 
    {
	for (i=0; i<CWIDTH; i++) 
	{
	    if (chunk_out[j][i] != chunk4[j][i])
	    {
		fprintf(stderr,"Bogus val in loc [%d][%d] in chunk #4, want %ld got %ld\n", j, i, chunk4[j][i], chunk_out[j][i]);
		num_errs++;
	    }
	}
    }

    /* 
    * Read chunk #5 with the coordinates (2,0) and verify it.
    */
    origin[0] = 2;
    origin[1] = 0;    	
    status = SDreadchunk (sds_id, origin, chunk_out);
    CHECK(status, FAIL, "SDreadchunk");

    for (j=0; j<CLENGTH; j++) 
    {
	for (i=0; i<CWIDTH; i++) 
	    if (chunk_out[j][i] != chunk5[j][i])
	    {
		fprintf(stderr,"Bogus val in loc [%d][%d] in chunk #5, want %ld got %ld\n", j, i, chunk5[j][i], chunk_out[j][i]);
		num_errs++;
	    }
    }

    /* Terminate access to the data set. */
    status = SDendaccess (sds_id);
    CHECK(status, FAIL, "SDendaccess");

    /* Terminate access to the SD interface and close the file. */
    status = SDend (sd_id);
    CHECK(status, FAIL, "SDend");

    /* Return the number of errors that's been kept track of so far */
    return num_errs;

#else
    return 0;
#endif
}   /* test_szip_chunk */ 


#define FILE_NAME_3D	"SDSchunkedsziped3d.hdf"
#define RANK_CH3         3	/* rank of the chunked data set */
#define WIDTH_CH 	4	/* width of the chunked data set */
#define LENGTH_CH	9	/* length of the chunked data set */
#define Z_CH	11	/* length of the chunked data set */
#define X_CWIDTH		2	/* width of the chunk */
#define X_CLENGTH		9	/* length of the chunk */
#define CZ		2	/* length of the chunk */

   int16         all_data[LENGTH_CH][WIDTH_CH][Z_CH];
   int16         out_data[LENGTH_CH][WIDTH_CH][Z_CH];
static intn 
test_szip_chunk_3d()
{
#ifdef H4_HAVE_LIBSZ
   /************************* Variable declaration **************************/

   int32         sd_id, sds_id, sds_index;
   intn          status;
   int32         dim_sizes[3];
   HDF_CHUNK_DEF c_def; /* Chunking definitions */ 
   uint32       comp_config;
   int32         comp_flag;
   int32         start[3], edges[3];
   int16         fill_value = 0;   /* Fill value */
   comp_coder_t  comp_type;        /* to retrieve compression type into */
   comp_info     cinfo;            /* compression information structure */
   int    	 num_errs = 0;     /* number of errors so far */
   int           i,j,k;
for (i = 0; i < LENGTH_CH; i++) {
for (j = 0; j < WIDTH_CH; j++) {
for (k = 0; k < Z_CH; k++) {
   out_data[i][j][k] = i*100+j*10+k;
}}}

    /* Initialize chunk lengths. */
    c_def.comp.chunk_lengths[0] = X_CLENGTH;
    c_def.comp.chunk_lengths[1] = X_CWIDTH;
    c_def.comp.chunk_lengths[2] = CZ;

    HCget_config_info(COMP_CODE_SZIP,&comp_config);
    /* Create the file and initialize SD interface. */
    sd_id = SDstart (FILE_NAME_3D, DFACC_CREATE);
    CHECK(sd_id, FAIL, "SDstart");

    /* Create LENGTH_CHxWIDTH_CH SDS. */
    dim_sizes[0] = LENGTH_CH;
    dim_sizes[1] = WIDTH_CH;
    dim_sizes[2] = Z_CH;
    sds_id = SDcreate (sd_id, SDS_NAME_CH,DFNT_INT16, RANK_CH3, dim_sizes);
    CHECK(sds_id, FAIL, "SDcreate:Failed to create a data set for chunking/szip compression testing");

    /* Fill the SDS array with the fill value. */
    status = SDsetfillvalue (sds_id, (VOIDP)&fill_value);
    CHECK(status, FAIL, "SDsetfillvalue");

    /* Set parameters for Chunking/SZIP */
    c_def.comp.comp_type = COMP_CODE_SZIP;
    c_def.comp.cinfo.szip.pixels_per_block = 2;

    c_def.comp.cinfo.szip.options_mask = SZ_EC_OPTION_MASK;
    c_def.comp.cinfo.szip.options_mask |= SZ_MSB_OPTION_MASK;
    c_def.comp.cinfo.szip.bits_per_pixel = 0;
    c_def.comp.cinfo.szip.pixels = 0;
    c_def.comp.cinfo.szip.pixels_per_scanline = 0;
    comp_flag = HDF_CHUNK | HDF_COMP;
    status = SDsetchunk (sds_id, c_def, comp_flag);
    if ((comp_config & COMP_ENCODER_ENABLED) == COMP_ENCODER_ENABLED) {
	/* should work */
       CHECK(status, FAIL, "SDsetchunk");
    } else {
       /* skip rest of test?? */
        status = SDendaccess (sds_id);
        CHECK(status, FAIL, "SDendaccess");

        status = SDend (sd_id);
        CHECK(status, FAIL, "SDend");
        printf("szip chunk 3d: SKIPPED\n");
        return num_errs;
    }


    start[0] = 0;
    start[1] = 0;
    start[2] = 0;
    edges[0] = LENGTH_CH;
    edges[1] = WIDTH_CH;
    edges[2] = Z_CH;
    status = SDwritedata (sds_id, start, NULL, edges, (VOIDP) out_data); 
    CHECK(status, FAIL, "SDwritedata");

    /* Terminate access to the data set. */
    status = SDendaccess (sds_id);
    CHECK(status, FAIL, "SDendaccess");

    /* Terminate access to the SD interface and close the file. */
    status = SDend (sd_id);
    CHECK(status, FAIL, "SDend");

    /*
     * Verify the compressed data
     */

    /* Reopen the file and access the first data set. */
    sd_id = SDstart (FILE_NAME_3D, DFACC_READ);
    sds_index = 0;
    sds_id = SDselect (sd_id, sds_index);
    CHECK(sds_id, FAIL, "SDselect:Failed to select a data set for chunking/szip compression testing");

    /* Retrieve compression information about the dataset */
    comp_type = COMP_CODE_INVALID;  /* reset variables before retrieving info */
    HDmemset(&cinfo, 0, sizeof(cinfo)) ;

    status = SDgetcompress(sds_id, &comp_type, &cinfo);
    CHECK(status, FAIL, "SDgetcompress");
    VERIFY(comp_type, COMP_CODE_SZIP, "SDgetcompress");

    start[0] = 0;
    start[1] = 0;
    start[2] = 0;
    edges[0] = LENGTH_CH;
    edges[1] = WIDTH_CH;
    edges[2] = Z_CH;
    status = SDreaddata (sds_id, start, NULL, edges, (VOIDP)all_data);
    CHECK(status, FAIL, "SDreaddata");

for (i = 0; i < LENGTH_CH; i++) {
for (j = 0; j < WIDTH_CH; j++) {
for (k = 0; k < Z_CH; k++) {
	    if (out_data[i][j][k] != all_data[i][j][k])
	    {
		fprintf(stderr,"Bogus val in loc [%d][%d][%d] want %ld got %ld\n", i, j,k, out_data[i][j][k], all_data[i][j][k]);
		num_errs++;
	    }
    }
    }
    }

    /* Terminate access to the data set. */
    status = SDendaccess (sds_id);
    CHECK(status, FAIL, "SDendaccess");

    /* Terminate access to the SD interface and close the file. */
    status = SDend (sd_id);
    CHECK(status, FAIL, "SDend");

    /* Return the number of errors that's been kept track of so far */
    return num_errs;

#else
    return 0;
#endif
}   /* test_szip_chunk_3D */ 

/* 
 * At this time, the use of SZIP compression with unlimited dimension SDSs
 * is unavailable.  This test program is to verify that the feature is 
 * correctly disabled.
 */
#define FILE_NAME_UNLIM	"SDSunlimitedsziped.hdf"
#define SDS_NAME_UNLIM	"UnlimitedData"
static intn 
test_szip_unlimited()
{
#ifdef H4_HAVE_LIBSZ
    /************************* Variable declaration **************************/

    int32	sd_id, sds_id;
    intn 	status;
    int32	dim_sizes[2], array_rank, num_type, attributes;
    char	name[MAX_NC_NAME];
    comp_info	c_info;
   uint32       comp_config;
    int32       start[2], edges[2];
    int32       fill_value = 0;   /* Fill value */
    int         i,j;
    int		num_errs = 0;    /* number of errors so far */
    int32	out_data[LENGTH][WIDTH];
    int32	in_data[LENGTH][WIDTH]={
	   			 100,100,200,200,300,400,
	   			 100,100,200,200,300,400,
				 100,100,200,200,300,400,
				 300,300,  0,400,300,400,
				 300,300,  0,400,300,400,
				 300,300,  0,400,300,400,
				   0,  0,600,600,300,400,
				 500,500,600,600,300,400,
				   0,  0,600,600,300,400};

    /********************* End of variable declaration ***********************/

    HCget_config_info(COMP_CODE_SZIP,&comp_config);

    /* Create the file and initialize SD interface */
    sd_id = SDstart (FILE_NAME_UNLIM, DFACC_CREATE);
    CHECK(sd_id, FAIL, "SDstart");

    /* Create the SDS */
    dim_sizes[0] = SD_UNLIMITED;
    dim_sizes[1] = WIDTH;
    sds_id = SDcreate (sd_id, SDS_NAME, DFNT_INT32, RANK, dim_sizes);
    CHECK(sds_id, FAIL, "SDcreate:Failed to create a data set for szip compression testing");

    /* Define the location, pattern, and size of the data set */
    for (i = 0; i < RANK; i++) {
	start[i] = 0;
	}
    edges[0] = LENGTH;
    edges[1] = WIDTH;

    /* Fill the SDS array with the fill value */
    status = SDsetfillvalue (sds_id, (VOIDP)&fill_value);
    CHECK(status, FAIL, "SDsetfillvalue");

    /* Initialize for SZIP */
    c_info.szip.pixels_per_block = 2;

    c_info.szip.options_mask = SZ_EC_OPTION_MASK;
    c_info.szip.options_mask |= SZ_RAW_OPTION_MASK;
    c_info.szip.bits_per_pixel = 0;
    c_info.szip.pixels = 0;
    c_info.szip.pixels_per_scanline = 0;

    /* Attempting to set SZIP compression will fail because SZIP is 
     * not available with unlimited dimension yet */
    status = SDsetcompress (sds_id, COMP_CODE_SZIP, &c_info);
    if ((comp_config & COMP_ENCODER_ENABLED) == COMP_ENCODER_ENABLED) {
	/* should work */
       VERIFY(status, FAIL, "SDsetcompress");
    } else {
       /* skip rest of test?? */
        status = SDendaccess (sds_id);
        CHECK(status, FAIL, "SDendaccess");

        status = SDend (sd_id);
        CHECK(status, FAIL, "SDend");
        printf("szip_SD unlimited: SKIPPED\n");
        return num_errs;
    }

    /* Write data to the SDS; data will be uncompressed */
    status = SDwritedata(sds_id, start, NULL, edges, (VOIDP)in_data);
    CHECK(status, FAIL, "SDwritedata");

    /* Terminate access to the data set */
    status = SDendaccess (sds_id);
    CHECK(status, FAIL, "SDendaccess");

    /* Terminate access to the SD interface and close the file to 
       flush the compressed info to the file */
    status = SDend (sd_id);
    CHECK(status, FAIL, "SDend");

    /*
    * Verify the written data
    */

    /* Reopen the file and select the first SDS */
    sd_id = SDstart (FILE_NAME_UNLIM, DFACC_READ);
    CHECK(sd_id, FAIL, "SDstart");

    sds_id = SDselect (sd_id, 0);
    CHECK(sds_id, FAIL, "SDselect:Failed to select a data set for testing szip compression with unlimited dimension");

    /* Retrieve information of the data set */
    status = SDgetinfo(sds_id, name, &array_rank, dim_sizes, &num_type, &attributes);
    CHECK(status, FAIL, "SDgetinfo");

    /* Wipe out the output buffer */
    HDmemset(&out_data, 0, sizeof(out_data));

    /* Read the data set */
    start[0] = 0;
    start[1] = 0;
    edges[0] = LENGTH;
    edges[1] = WIDTH;
    status = SDreaddata (sds_id, start, NULL, edges, (VOIDP)out_data);
    CHECK(status, FAIL, "SDreaddata");

    /* Compare read data against input data */
    for (j=0; j<LENGTH; j++) 
    {
        for (i=0; i<WIDTH; i++)
	    if (out_data[j][i] != in_data[j][i])
	    {
		fprintf(stderr,"Bogus val in loc [%d][%d] in compressed dset, want %ld got %ld\n", j, i, (long)in_data[j][i], (long)out_data[j][i]);
		num_errs++;
	    }
    }

    /* Terminate access to the data set */
    status = SDendaccess (sds_id);
    CHECK(status, FAIL, "SDendaccess");

    /* Terminate access to the SD interface and close the file */
    status = SDend (sd_id);
    CHECK(status, FAIL, "SDend");

    /* Return the number of errors that's been kept track of so far */
    return num_errs;
#else
    return 0;
#endif
}  /* test_szip_unlimited */

/* 
 * Test drive for testing the szip compression feature with SD interface 
 */
extern int 
test_szip_compression ()
{
    int num_errs = 0;
#ifdef H4_HAVE_LIBSZ
    num_errs = num_errs + test_szip_SDS8bit();
    num_errs = num_errs + test_szip_SDS16bit();
    num_errs = num_errs + test_szip_SDS32bit();
    num_errs = num_errs + test_szip_SDSfl32bit();
    num_errs = num_errs + test_szip_SDSfl64bit();
    num_errs = num_errs + test_szip_chunk();
    num_errs = num_errs + test_szip_unlimited();
    num_errs = num_errs + test_szip_chunk_3d();
#else
    printf("SKIPPING SZIP compression tests\n");
#endif
    return num_errs;
}
