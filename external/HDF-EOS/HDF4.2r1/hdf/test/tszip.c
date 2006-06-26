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
 *  SZIP support eliminated for HDF4.2R1
 */
#ifdef H4_GR_SZIP
#include <hdf.h>
#ifdef H4_HAVE_LIBSZ
#include "szlib.h"
#endif
#include "tutils.h"


/*
 *  NOTE: these tests should be elaborated:
 *     - use NN and EC options
 *     - bigger datasets
 *     - more data types
 */     

#define  FILE_NAME8     "RI_8_sziped.hdf"
#define  FILE_NAME16    "RI_16_sziped.hdf"
#define  FILE_NAME32    "RI_32_sziped.hdf"
#define  FILE_NAMEfl32  "RI_fl32_sziped.hdf"
#define  FILE_NAMEfl64  "RI_fl64_sziped.hdf"
#define  WIDTH 		10    /* number of columns in the image */
#define  LENGTH		6     /* number of rows in the image */
#define  N_COMPS	3     /* number of components in the image */
#define  IMAGE_NAME 	"Sziped_Image"

/* 
 * Sub-tests for test_mgr_szip():
 *  test_szip_RI8bit()
 *  test_szip_RI16bit()
 *  test_szip_RI32bit()
 *  test_szip_RIfl32bit()
 *  test_szip_RIfl64bit()
 */

/* 
 * Write/Read szip compressed image with 8-bit integer data
 */
static void 
test_szip_RI8bit()
{
#ifdef H4_HAVE_LIBSZ
   /************************* Variable declaration **************************/

    intn  status;         /* status for functions returning an intn */
    int32 file_id,        /* HDF file identifier */
          gr_id,          /* GR interface identifier */
          ri_id,       	  /* raster image identifier */
          dim_sizes[2],   /* dimension sizes of the image array */
          interlace_mode, /* interlace mode of the image */
          data_type,      /* data type of the image data */
          index;
    int32 start[2],
          edges[2];
    uint32 comp_config;
    comp_info cinfo;    /* Compression parameters - union */
 
    comp_coder_t comp_type;
    int8 out_data[LENGTH][WIDTH][N_COMPS];
    int8 in_data[LENGTH][WIDTH][N_COMPS]    = {
		10, 11, 12, 13, 14, 15, 40, 41, 42, 43, 44, 45,  0,
                 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
		 0,  0,  0,  0, 20, 21, 22, 23, 24, 25, 50, 51, 52, 
		53, 54, 55,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0, 30, 31, 32, 33, 34, 
		35, 60, 61, 62, 63, 64, 65,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 70, 71, 72, 
		73, 74, 75,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0, 80, 81, 82, 83, 84, 85,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0, 90, 91, 92, 93, 94, 95};


    /********************** End of variable declaration **********************/

    HCget_config_info(COMP_CODE_SZIP, &comp_config);
    CHECK( (comp_config & COMP_DECODER_ENABLED|COMP_ENCODER_ENABLED),0, "SZIP Compression not available" );
    /* Create and open the file for sziped data */
    file_id = Hopen (FILE_NAME8, DFACC_CREATE, 0);
    CHECK(file_id, FAIL, "Hopen");

    /* Initialize the GR interface */
    gr_id = GRstart (file_id);
    CHECK(gr_id, FAIL, "GRstart");

    /* Set the data type, interlace mode, and dimensions of the image */
    data_type = DFNT_INT8;
    interlace_mode = MFGR_INTERLACE_PIXEL;
    dim_sizes[0] = WIDTH;
    dim_sizes[1] = LENGTH;

    /* Create the raster image array */
    ri_id = GRcreate (gr_id, IMAGE_NAME, N_COMPS, data_type, 
                     interlace_mode, dim_sizes);
    CHECK(ri_id, FAIL, "GRcreate:Failed to create a raster image for szip compression testing");

    /* Define the location, pattern, and size of the data set */
    start[0] = start[1] = 0;
    edges[0] = WIDTH;
    edges[1] = LENGTH;

    /* Initializate for SZIP */
    comp_type = COMP_CODE_SZIP;
    cinfo.szip.pixels_per_block = 2;
    cinfo.szip.options_mask = SZ_EC_OPTION_MASK;
    cinfo.szip.options_mask |= SZ_MSB_OPTION_MASK;
    cinfo.szip.options_mask |= SZ_RAW_OPTION_MASK;
    cinfo.szip.pixels = 0;
    cinfo.szip.pixels_per_scanline = 0;
    cinfo.szip.bits_per_pixel = 0;

    /* Set the compression */
    status = GRsetcompress(ri_id, comp_type, &cinfo);
    if ((comp_config & COMP_ENCODER_ENABLED) == COMP_ENCODER_ENABLED) {
	/* should work */
       CHECK(status, FAIL, "GRsetcompress");
    } else {
       /* skip rest of test?? */
        /* Terminate access to the raster image */
        status = GRendaccess (ri_id);
        CHECK(status, FAIL, "GRendaccess");

        /* Terminate access to the GR interface and close the HDF file */
        status = GRend (gr_id);
        CHECK(status, FAIL, "GRend");
        status = Hclose (file_id);
        CHECK(status, FAIL, "Hclose");
        MESSAGE(1,printf("test_szip_RI8bit(): %s\n",SKIP_STR););
       return;  
    }

    status = GRwriteimage(ri_id, start, NULL, edges, (VOIDP)in_data);
    CHECK(status, FAIL, "GRwriteimage");

    /* Terminate access to the raster image */
    status = GRendaccess (ri_id);
    CHECK(status, FAIL, "GRendaccess");

    /* Terminate access to the GR interface and close the file to
       flush the compressed info to the file */
    status = GRend (gr_id);
    CHECK(status, FAIL, "GRend");
    status = Hclose (file_id);
    CHECK(status, FAIL, "Hclose");

    /*
     * Verify the compressed data
     */

    /* Reopen the file */

    file_id = Hopen (FILE_NAME8, DFACC_WRITE, 0); 
    CHECK(file_id, FAIL, "Hopen");

    gr_id = GRstart (file_id);
    CHECK(gr_id, FAIL, "GRstart");

    /* Find the index of the specified image */
    index = GRnametoindex(gr_id, IMAGE_NAME);
    CHECK(index, FAIL, "GRnametoindex");
   
    /* Select the image */
    ri_id = GRselect(gr_id, index);
    CHECK(ri_id, FAIL, "GRselect");

    /* Get and verify the image's compression information */
    comp_type = COMP_CODE_INVALID;  /* reset variables before retrieving info */
    HDmemset(&cinfo,  0, sizeof(cinfo)) ;

    status = GRgetcompress(ri_id, &comp_type, &cinfo);
    CHECK(status, FAIL, "GRgetcompress");
    VERIFY(comp_type, COMP_CODE_SZIP, "GRgetcompress");

    /* Wipe out the output buffer */
    HDmemset(&out_data, 0, sizeof(out_data));

    /* Read the whole image */
    start[0] = start[1] = 0;
    edges[0] = WIDTH;
    edges[1] = LENGTH;
    status = GRreadimage(ri_id, start, NULL, edges, (VOIDP)out_data);
    CHECK(status, FAIL, "GRreadimage");

    /* Compare read data against input data */
    if (0!= HDmemcmp(out_data, in_data, sizeof(in_data)))
        printf("Error in reading the whole image \n" );

    /* Terminate access to the raster image */
    status = GRendaccess (ri_id);
    CHECK(status, FAIL, "GRendaccess");

    /* Terminate access to the GR interface and close the HDF file */
    status = GRend (gr_id);
    CHECK(status, FAIL, "GRend");
    status = Hclose (file_id);
    CHECK(status, FAIL, "Hclose");

#endif
}  /* end of test_szip_RI8bit */

/* 
 * Write/Read szip compressed image with 16-bit integer data
 */
static void 
test_szip_RI16bit()
{
#ifdef H4_HAVE_LIBSZ
   /************************* Variable declaration **************************/

    intn  status;         /* status for functions returning an intn */
    int32 file_id,        /* HDF file identifier */
          gr_id,          /* GR interface identifier */
          ri_id,       	  /* raster image identifier */
          dim_sizes[2],   /* dimension sizes of the image array */
          interlace_mode, /* interlace mode of the image */
          data_type,      /* data type of the image data */
          index;
    int32 start[2],
          edges[2];
    comp_info cinfo;    /* Compression parameters - union */
uint32 comp_config;

    comp_coder_t comp_type;
    int16 out_data[LENGTH][WIDTH][N_COMPS];
    int16 in_data[LENGTH][WIDTH][N_COMPS]    = {
		10, 11, 12, 13, 14, 15, 40, 41, 42, 43, 44, 45,  0,
                 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
		 0,  0,  0,  0, 20, 21, 22, 23, 24, 25, 50, 51, 52, 
		53, 54, 55,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0, 30, 31, 32, 33, 34, 
		35, 60, 61, 62, 63, 64, 65,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 70, 71, 72, 
		73, 74, 75,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0, 80, 81, 82, 83, 84, 85,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0, 90, 91, 92, 93, 94, 95};


    /********************** End of variable declaration **********************/

    HCget_config_info(COMP_CODE_SZIP, &comp_config);
    CHECK( (comp_config & COMP_DECODER_ENABLED|COMP_ENCODER_ENABLED),0, "SZIP Compression not available" );
    /* Create and open the file for sziped data */
    file_id = Hopen (FILE_NAME16, DFACC_CREATE, 0);
    CHECK(file_id, FAIL, "Hopen");

    /* Initialize the GR interface */
    gr_id = GRstart (file_id);
    CHECK(gr_id, FAIL, "GRstart");

    /* Set the data type, interlace mode, and dimensions of the image */
    data_type = DFNT_INT16;
    interlace_mode = MFGR_INTERLACE_PIXEL;
    dim_sizes[0] = WIDTH;
    dim_sizes[1] = LENGTH;

    /* Create the raster image array */
    ri_id = GRcreate (gr_id, IMAGE_NAME, N_COMPS, data_type, 
                     interlace_mode, dim_sizes);
    CHECK(ri_id, FAIL, "GRcreate:Failed to create a raster image for szip compression testing");

    /* Define the location, pattern, and size of the data set */
    start[0] = start[1] = 0;
    edges[0] = WIDTH;
    edges[1] = LENGTH;

    /* Initializate for SZIP */
    comp_type = COMP_CODE_SZIP;
    cinfo.szip.pixels_per_block = 2;
    cinfo.szip.options_mask = SZ_EC_OPTION_MASK;
    cinfo.szip.options_mask |= SZ_MSB_OPTION_MASK;
    cinfo.szip.options_mask |= SZ_RAW_OPTION_MASK;
    cinfo.szip.pixels = 0;
    cinfo.szip.pixels_per_scanline = 0;
    cinfo.szip.bits_per_pixel = 0;
 
    /* Set the compression */
    status = GRsetcompress(ri_id, comp_type, &cinfo);
    if ((comp_config & COMP_ENCODER_ENABLED) == COMP_ENCODER_ENABLED) {
	/* should work */
       CHECK(status, FAIL, "GRsetcompress");
    } else {
       /* skip rest of test?? */
        /* Terminate access to the raster image */
        status = GRendaccess (ri_id);
        CHECK(status, FAIL, "GRendaccess");

        /* Terminate access to the GR interface and close the HDF file */
        status = GRend (gr_id);
        CHECK(status, FAIL, "GRend");
        status = Hclose (file_id);
        CHECK(status, FAIL, "Hclose");
        MESSAGE(1,printf("szip_RI16: %s\n",SKIP_STR););
       return;  
    }

    status = GRwriteimage(ri_id, start, NULL, edges, (VOIDP)in_data);
    CHECK(status, FAIL, "SDwritedata");

    /* Terminate access to the raster image */
    status = GRendaccess (ri_id);
    CHECK(status, FAIL, "GRendaccess");

   /* Terminate access to the GR interface and close the file to
      flush the compressed info to the file */
    status = GRend (gr_id);
    CHECK(status, FAIL, "GRend");
    status = Hclose (file_id);
    CHECK(status, FAIL, "Hclose");

    /*
     * Verify the compressed data
     */

    /* Reopen the file */
    file_id = Hopen (FILE_NAME16, DFACC_WRITE, 0); 
    CHECK(file_id, FAIL, "Hopen");

    gr_id = GRstart (file_id);
    CHECK(gr_id, FAIL, "GRstart");

    /* Find the index of the specified image */
    index = GRnametoindex(gr_id, IMAGE_NAME);
    CHECK(index, FAIL, "GRnametoindex");
   
    /* Select the image */
    ri_id = GRselect(gr_id, index);
    CHECK(ri_id, FAIL, "GRselect");

    /* Get and verify the image's compression information */
    comp_type = COMP_CODE_INVALID;  /* reset variables before retrieving info */
    HDmemset(&cinfo,  0, sizeof(cinfo)) ;

    status = GRgetcompress(ri_id, &comp_type, &cinfo);
    CHECK(status, FAIL, "GRgetcompress");
    VERIFY(comp_type, COMP_CODE_SZIP, "GRgetcompress");

    /* Wipe out the output buffer */
    HDmemset(&out_data, 0, sizeof(out_data));

    /* Read the whole image */
    start[0] = start[1] = 0;
    edges[0] = WIDTH;
    edges[1] = LENGTH;
    status = GRreadimage(ri_id, start, NULL, edges, (VOIDP)out_data);
    CHECK(status, FAIL, "GRreadimage");

    /* Compare read data against input data */
    if (0!= HDmemcmp(out_data, in_data, sizeof(in_data)))
        printf("Error in reading the whole image \n" );

    /* Terminate access to the raster image */
    status = GRendaccess (ri_id);
    CHECK(status, FAIL, "GRendaccess");

    /* Terminate access to the GR interface and close the HDF file */
    status = GRend (gr_id);
    CHECK(status, FAIL, "GRend");
    status = Hclose (file_id);
    CHECK(status, FAIL, "Hclose");

#endif
}  /* end of test_szip_RI16bit */

/* 
 * Write/Read szip compressed image with 32-bit integer data
 */
static void 
test_szip_RI32bit()
{
#ifdef H4_HAVE_LIBSZ
   /************************* Variable declaration **************************/

    intn  status;         /* status for functions returning an intn */
    int32 file_id,        /* HDF file identifier */
          gr_id,          /* GR interface identifier */
          ri_id,       	  /* raster image identifier */
          dim_sizes[2],   /* dimension sizes of the image array */
          interlace_mode, /* interlace mode of the image */
          data_type,      /* data type of the image data */
          index;
    int32 start[2],
          edges[2];
    comp_info cinfo;    /* Compression parameters - union */
uint32 comp_config;

    comp_coder_t comp_type;
    int32 out_data[LENGTH][WIDTH][N_COMPS];
    int32 in_data[LENGTH][WIDTH][N_COMPS]    = {
		10, 11, 12, 13, 14, 15, 40, 41, 42, 43, 44, 45,  0,
                 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
		 0,  0,  0,  0, 20, 21, 22, 23, 24, 25, 50, 51, 52, 
		53, 54, 55,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0, 30, 31, 32, 33, 34, 
		35, 60, 61, 62, 63, 64, 65,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 70, 71, 72, 
		73, 74, 75,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0, 80, 81, 82, 83, 84, 85,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0, 90, 91, 92, 93, 94, 95};


   /********************** End of variable declaration **********************/

    HCget_config_info(COMP_CODE_SZIP, &comp_config);
    CHECK( (comp_config & COMP_DECODER_ENABLED|COMP_ENCODER_ENABLED),0, "SZIP Compression not available" );
    /* Create and open the file for sziped data */
    file_id = Hopen (FILE_NAME32, DFACC_CREATE, 0);
    CHECK(file_id, FAIL, "Hopen");

    /* Initialize the GR interface */
    gr_id = GRstart (file_id);
    CHECK(gr_id, FAIL, "GRstart");

    /* Set the data type, interlace mode, and dimensions of the image */
    data_type = DFNT_INT32;
    interlace_mode = MFGR_INTERLACE_PIXEL;
    dim_sizes[0] = WIDTH;
    dim_sizes[1] = LENGTH;
 
    /* Create the raster image array */
    ri_id = GRcreate (gr_id, IMAGE_NAME, N_COMPS, data_type, 
                     interlace_mode, dim_sizes);
    CHECK(ri_id, FAIL, "GRcreate:Failed to create a raster image for szip compression testing");

    /* Define the location, pattern, and size of the data set */
    start[0] = start[1] = 0;
    edges[0] = WIDTH;
    edges[1] = LENGTH;

    /* Initializate for SZIP */
    comp_type = COMP_CODE_SZIP;
    cinfo.szip.pixels_per_block = 2;
    cinfo.szip.options_mask = SZ_EC_OPTION_MASK;
    cinfo.szip.options_mask |= SZ_MSB_OPTION_MASK;
    cinfo.szip.options_mask |= SZ_RAW_OPTION_MASK;
    cinfo.szip.pixels = 0;
    cinfo.szip.pixels_per_scanline = 0;
    cinfo.szip.bits_per_pixel = 0;

    /* Set the compression */
    status = GRsetcompress(ri_id, comp_type, &cinfo);
    if ((comp_config & COMP_ENCODER_ENABLED) == COMP_ENCODER_ENABLED) {
	/* should work */
       CHECK(status, FAIL, "GRsetcompress");
    } else {
       /* skip rest of test?? */
        /* Terminate access to the raster image */
        status = GRendaccess (ri_id);
        CHECK(status, FAIL, "GRendaccess");

        /* Terminate access to the GR interface and close the HDF file */
        status = GRend (gr_id);
        CHECK(status, FAIL, "GRend");
        status = Hclose (file_id);
        CHECK(status, FAIL, "Hclose");
        MESSAGE(1,printf("szip_RI32: %s\n",SKIP_STR););
       return;  
    }

    status = GRwriteimage(ri_id, start, NULL, edges, (VOIDP)in_data);
    CHECK(status, FAIL, "GRwriteimage");

    /* Terminate access to the raster image */
    status = GRendaccess (ri_id);
    CHECK(status, FAIL, "GRendaccess");

   /* Terminate access to the GR interface and close the file to
      flush the compressed info to the file */
    status = GRend (gr_id);
    CHECK(status, FAIL, "GRend");
    status = Hclose (file_id);
    CHECK(status, FAIL, "Hclose");

    /*
     * Verify the compressed data
     */

    /* Reopen the file */

    file_id = Hopen (FILE_NAME32, DFACC_WRITE, 0); 
    CHECK(file_id, FAIL, "Hopen");

    gr_id = GRstart (file_id);
    CHECK(gr_id, FAIL, "GRstart");

    /* Find the index of the specified image */
    index = GRnametoindex(gr_id, IMAGE_NAME);
    CHECK(index, FAIL, "GRnametoindex");
   
    /* Select the image */
    ri_id = GRselect(gr_id, index);
    CHECK(ri_id, FAIL, "GRselect");

    /* Get and verify the image's compression information */
    comp_type = COMP_CODE_INVALID;  /* reset variables before retrieving info */
    HDmemset(&cinfo,  0, sizeof(cinfo)) ;

    status = GRgetcompress(ri_id, &comp_type, &cinfo);
    CHECK(status, FAIL, "GRgetcompress");
    VERIFY(comp_type, COMP_CODE_SZIP, "GRgetcompress");

    /* Wipe out the output buffer */
    HDmemset(&out_data, 0, sizeof(out_data));

    /* Read the whole image */
    start[0] = start[1] = 0;
    edges[0] = WIDTH;
    edges[1] = LENGTH;
    status = GRreadimage(ri_id, start, NULL, edges, (VOIDP)out_data);
    CHECK(status, FAIL, "GRreadimage");

    /* Compare read data against input data */
    if (0!= HDmemcmp(out_data, in_data, sizeof(in_data)))
        printf("Error in reading the whole image \n" );

    /* Terminate access to the raster image */
    status = GRendaccess (ri_id);
    CHECK(status, FAIL, "GRendaccess");

    /* Terminate access to the GR interface and close the HDF file */
    status = GRend (gr_id);
    CHECK(status, FAIL, "GRend");
    status = Hclose (file_id);
    CHECK(status, FAIL, "Hclose");

#endif
}  /* end of test_szip_RI32bit */

/* 
 * Write/Read szip compressed image with 32-bit floating point data
 */
static void 
test_szip_RIfl32bit()
{
#ifdef H4_HAVE_LIBSZ
   /************************* Variable declaration **************************/

    intn  status;         /* status for functions returning an intn */
    int32 file_id,        /* HDF file identifier */
          gr_id,          /* GR interface identifier */
          ri_id,       	  /* raster image identifier */
          dim_sizes[2],   /* dimension sizes of the image array */
          interlace_mode, /* interlace mode of the image */
          data_type,      /* data type of the image data */
          index;
    int32 start[2],
          edges[2];
    comp_info cinfo;    /* Compression parameters - union */
    uint32 comp_config;

    comp_coder_t comp_type;
    float32 out_data[LENGTH][WIDTH][N_COMPS];
    float32 in_data[LENGTH][WIDTH][N_COMPS]    = {
		10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 40.0, 41.0, 42.0, 43.0, 44.0, 45.0,  0.0,
                 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, 
		 0.0,  0.0,  0.0,  0.0, 20.0, 21.0, 22.0, 23.0, 24.0, 25.0, 50.0, 51.0, 52.0, 
		53.0, 54.0, 55.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  
		 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, 30.0, 31.0, 32.0, 33.0, 34.0, 
		35.0, 60.0, 61.0, 62.0, 63.0, 64.0, 65.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  
		 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  
		 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  
		 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, 70.0, 71.0, 72.0, 
		73.0, 74.0, 75.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  
		 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  
		 0.0, 80.0, 81.0, 82.0, 83.0, 84.0, 85.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  
		 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  
		 0.0,  0.0,  0.0,  0.0,  0.0, 90.0, 91.0, 92.0, 93.0, 94.0, 95.0};


   /********************** End of variable declaration **********************/

    HCget_config_info(COMP_CODE_SZIP, &comp_config);
    CHECK( (comp_config & COMP_DECODER_ENABLED|COMP_ENCODER_ENABLED),0, "SZIP Compression not available" );

    /* Create and open the file for sziped data */
    /* Create and open the file for sziped data */
    file_id = Hopen (FILE_NAMEfl32, DFACC_CREATE, 0);
    CHECK(file_id, FAIL, "Hopen");

    /* Initialize the GR interface */
    gr_id = GRstart (file_id);
    CHECK(gr_id, FAIL, "GRstart");

    /* Set the data type, interlace mode, and dimensions of the image */
    data_type = DFNT_FLOAT32;
    interlace_mode = MFGR_INTERLACE_PIXEL;
    dim_sizes[0] = WIDTH;
    dim_sizes[1] = LENGTH;

    /* Create the raster image array */
    ri_id = GRcreate (gr_id, IMAGE_NAME, N_COMPS, data_type, 
                     interlace_mode, dim_sizes);
    CHECK(ri_id, FAIL, "GRcreate:Failed to create a raster image for szip compression testing");

    /* Define the location, pattern, and size of the data set */
    start[0] = start[1] = 0;
    edges[0] = WIDTH;
    edges[1] = LENGTH;

    /* Initializate for SZIP */
    comp_type = COMP_CODE_SZIP;
    cinfo.szip.pixels_per_block = 2;

    cinfo.szip.options_mask = SZ_EC_OPTION_MASK;
    cinfo.szip.options_mask |= SZ_MSB_OPTION_MASK;
    cinfo.szip.options_mask |= SZ_RAW_OPTION_MASK;
    cinfo.szip.pixels = 0;
    cinfo.szip.pixels_per_scanline = 0;
    cinfo.szip.bits_per_pixel = 0;
 
    /* Set the compression */
    status = GRsetcompress(ri_id, comp_type, &cinfo);
    if ((comp_config & COMP_ENCODER_ENABLED) == COMP_ENCODER_ENABLED) {
	/* should work */
       CHECK(status, FAIL, "GRsetcompress");
    } else {
       /* skip rest of test?? */
        /* Terminate access to the raster image */
        status = GRendaccess (ri_id);
        CHECK(status, FAIL, "GRendaccess");

        /* Terminate access to the GR interface and close the HDF file */
        status = GRend (gr_id);
        CHECK(status, FAIL, "GRend");
        status = Hclose (file_id);
        CHECK(status, FAIL, "Hclose");
        MESSAGE(1,printf("szip_RIflt32: %s\n",SKIP_STR););
       return;  
    }

    status = GRwriteimage(ri_id, start, NULL, edges, (VOIDP)in_data);
    CHECK(status, FAIL, "GRwriteimage");

    /* Terminate access to the raster image */
    status = GRendaccess (ri_id);
    CHECK(status, FAIL, "GRendaccess");

   /* Terminate access to the GR interface and close the file to
      flush the compressed info to the file */
    status = GRend (gr_id);
    CHECK(status, FAIL, "GRend");
    status = Hclose (file_id);
    CHECK(status, FAIL, "Hclose");

    /*
     * Verify the compressed data
     */

    /* Reopen the file */
    file_id = Hopen (FILE_NAMEfl32, DFACC_WRITE, 0); 
    CHECK(file_id, FAIL, "Hopen");

    gr_id = GRstart (file_id);
    CHECK(gr_id, FAIL, "GRstart");

    /* Find the index of the specified image */
    index = GRnametoindex(gr_id, IMAGE_NAME);
    CHECK(index, FAIL, "GRnametoindex");
   
    /* Select the image */
    ri_id = GRselect(gr_id, index);
    CHECK(ri_id, FAIL, "GRselect");

    /* Get and verify the image's compression information */
    comp_type = COMP_CODE_INVALID;  /* reset variables before retrieving info */
    HDmemset(&cinfo,  0, sizeof(cinfo)) ;

    status = GRgetcompress(ri_id, &comp_type, &cinfo);
    CHECK(status, FAIL, "GRgetcompress");
    VERIFY(comp_type, COMP_CODE_SZIP, "GRgetcompress");

    /* Wipe out the output buffer */
    HDmemset(&out_data, 0, sizeof(out_data));

    /* Read the whole image */
    start[0] = start[1] = 0;
    edges[0] = WIDTH;
    edges[1] = LENGTH;
    status = GRreadimage(ri_id, start, NULL, edges, (VOIDP)out_data);
    CHECK(status, FAIL, "GRreadimage");

    /* Compare read data against input data */
    if (0!= HDmemcmp(out_data, in_data, sizeof(in_data)))
        printf("Error in reading the whole image \n" );

    /* Terminate access to the raster image */
    status = GRendaccess (ri_id);
    CHECK(status, FAIL, "GRendaccess");

    /* Terminate access to the GR interface and close the HDF file */
    status = GRend (gr_id);
    CHECK(status, FAIL, "GRend");
    status = Hclose (file_id);
    CHECK(status, FAIL, "Hclose");

#endif
}  /* end of test_szip_RIfl32bit */

/* 
 * Write/Read szip compressed image with 64-bit floating point data
 */
static void 
test_szip_RIfl64bit()
{
#ifdef H4_HAVE_LIBSZ
   /************************* Variable declaration **************************/

    intn  status;         /* status for functions returning an intn */
    int32 file_id,        /* HDF file identifier */
          gr_id,          /* GR interface identifier */
          ri_id,       	  /* raster image identifier */
          dim_sizes[2],   /* dimension sizes of the image array */
          interlace_mode, /* interlace mode of the image */
          data_type,      /* data type of the image data */
          index;
    int32 start[2],
          edges[2];
    comp_info cinfo;    /* Compression parameters - union */
    uint32 comp_config;

    comp_coder_t comp_type;
    float64 out_data[LENGTH][WIDTH][N_COMPS];
    float64 in_data[LENGTH][WIDTH][N_COMPS]    = {
		10.0, 11.0, 12.0, 13.0, 14.0, 15.0, 40.0, 41.0, 42.0, 43.0, 44.0, 45.0,  0.0,
                 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, 
		 0.0,  0.0,  0.0,  0.0, 20.0, 21.0, 22.0, 23.0, 24.0, 25.0, 50.0, 51.0, 52.0, 
		53.0, 54.0, 55.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  
		 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, 30.0, 31.0, 32.0, 33.0, 34.0, 
		35.0, 60.0, 61.0, 62.0, 63.0, 64.0, 65.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  
		 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  
		 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  
		 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0, 70.0, 71.0, 72.0, 
		73.0, 74.0, 75.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  
		 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  
		 0.0, 80.0, 81.0, 82.0, 83.0, 84.0, 85.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  
		 0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  
		 0.0,  0.0,  0.0,  0.0,  0.0, 90.0, 91.0, 92.0, 93.0, 94.0, 95.0};


    /********************** End of variable declaration **********************/

    HCget_config_info(COMP_CODE_SZIP, &comp_config);
    CHECK( (comp_config & COMP_DECODER_ENABLED|COMP_ENCODER_ENABLED),0, "SZIP Compression not available" );

    /* Create and open the file for sziped data */
    /* Create and open the file for sziped data */
    file_id = Hopen (FILE_NAMEfl64, DFACC_CREATE, 0);
    CHECK(file_id, FAIL, "Hopen");

    /* Initialize the GR interface */
    gr_id = GRstart (file_id);
    CHECK(gr_id, FAIL, "GRstart");

    /* Set the data type, interlace mode, and dimensions of the image */
    data_type = DFNT_FLOAT64;
    interlace_mode = MFGR_INTERLACE_PIXEL;
    dim_sizes[0] = WIDTH;
    dim_sizes[1] = LENGTH;
 
    /* Create the raster image array */
    ri_id = GRcreate (gr_id, IMAGE_NAME, N_COMPS, data_type, 
                     interlace_mode, dim_sizes);
    CHECK(ri_id, FAIL, "GRcreate:Failed to create a raster image for szip compression testing");

    /* Define the location, pattern, and size of the data set */
    start[0] = start[1] = 0;
    edges[0] = WIDTH;
    edges[1] = LENGTH;
 
    /* Initializate for SZIP */
    comp_type = COMP_CODE_SZIP;
    cinfo.szip.pixels_per_block = 2;
 
    cinfo.szip.options_mask = SZ_EC_OPTION_MASK;
    cinfo.szip.options_mask |= SZ_MSB_OPTION_MASK;
    cinfo.szip.options_mask |= SZ_RAW_OPTION_MASK;
    cinfo.szip.pixels = 0;
    cinfo.szip.pixels_per_scanline = 0;
    cinfo.szip.bits_per_pixel = 0;

    /* Set the compression */
    status = GRsetcompress(ri_id, comp_type, &cinfo);
    if ((comp_config & COMP_ENCODER_ENABLED) == COMP_ENCODER_ENABLED) {
	/* should work */
       CHECK(status, FAIL, "GRsetcompress");
    } else {
       /* skip rest of test?? */
        /* Terminate access to the raster image */
        status = GRendaccess (ri_id);
        CHECK(status, FAIL, "GRendaccess");

        /* Terminate access to the GR interface and close the HDF file */
        status = GRend (gr_id);
        CHECK(status, FAIL, "GRend");
        status = Hclose (file_id);
        CHECK(status, FAIL, "Hclose");
        MESSAGE(1,printf("szip_RIflt 64: %s\n",SKIP_STR););
       return;  
    }

    status = GRwriteimage(ri_id, start, NULL, edges, (VOIDP)in_data);
    CHECK(status, FAIL, "GRwriteimage");

    /* Terminate access to the raster image */
    status = GRendaccess (ri_id);
    CHECK(status, FAIL, "GRendaccess");

    /* Terminate access to the GR interface and close the file to
      flush the compressed info to the file */
    status = GRend (gr_id);
    CHECK(status, FAIL, "GRend");
    status = Hclose (file_id);
    CHECK(status, FAIL, "Hclose");

    /*
     * Verify the compressed data
     */

    /* Reopen the file */
    file_id = Hopen (FILE_NAMEfl64, DFACC_WRITE, 0); 
    CHECK(file_id, FAIL, "Hopen");

    gr_id = GRstart (file_id);
    CHECK(gr_id, FAIL, "GRstart");

    /* Find the index of the specified image */
    index = GRnametoindex(gr_id, IMAGE_NAME);
    CHECK(index, FAIL, "GRnametoindex");
   
    /* Select the image */
    ri_id = GRselect(gr_id, index);
    CHECK(ri_id, FAIL, "GRselect");

    /* Get and verify the image's compression information */
    comp_type = COMP_CODE_INVALID;  /* reset variables before retrieving info */
    HDmemset(&cinfo,  0, sizeof(cinfo)) ;

    status = GRgetcompress(ri_id, &comp_type, &cinfo);
    CHECK(status, FAIL, "GRgetcompress");
    VERIFY(comp_type, COMP_CODE_SZIP, "GRgetcompress");

    /* Wipe out the output buffer */
    HDmemset(&out_data, 0, sizeof(out_data));

    /* Read the whole image */
    start[0] = start[1] = 0;
    edges[0] = WIDTH;
    edges[1] = LENGTH;
    status = GRreadimage(ri_id, start, NULL, edges, (VOIDP)out_data);
    CHECK(status, FAIL, "GRreadimage");

    /* Compare read data against input data */
    if (0!= HDmemcmp(out_data, in_data, sizeof(in_data)))
        printf("Error in reading the whole image \n" );

    /* Terminate access to the raster image */
    status = GRendaccess (ri_id);
    CHECK(status, FAIL, "GRendaccess");

    /* Terminate access to the GR interface and close the HDF file */
    status = GRend (gr_id);
    CHECK(status, FAIL, "GRend");
    status = Hclose (file_id);
    CHECK(status, FAIL, "Hclose");

#endif
}  /* end of test_szip_RIfl64bit */

/*
* This function tests GR chunking write/read operations for the
* szip compressions
*/                    
#define  CHKSZIPFILE	"RIchunkedsziped.hdf"
#define  WIDTH_CH	10    /* number of columns in the image */
#define  LENGTH_CH	 6    /* number of rows in the image */

static void 
test_szip_chunk()
{
#ifdef H4_HAVE_LIBSZ

    /************************* Variable declaration **************************/

    intn  status;         /* status for functions returning an intn */
    int32 file_id,        /* HDF file identifier */
          gr_id,          /* GR interface identifier */
          ri_id,       /* raster image identifier */
          origin[2],      /* start position to write for each dimension */
          dim_sizes[2],   /* dimension sizes of the image array */
          interlace_mode, /* interlace mode of the image */
          data_type,      /* data type of the image data */
          comp_flag,      /* compression flag */
          index;
    int32 start[2],
          stride[2],
          edge[2];
    comp_info cinfo_out;    /* Compression parameters - union */
    uint32 comp_config;
    comp_coder_t comp_type;
    int8 data_out[N_COMPS*LENGTH_CH*WIDTH_CH];
    char *image_name = "Image_chunked_sziped";
    HDF_CHUNK_DEF chunk_def;
    int8 chunk_buf[18];

    /* 
     * Initialize data for RI
     */
    int8 chunk00[] = {10, 11, 12, 13, 14, 15,
                      20, 21, 22, 23, 24, 25,
                      30, 31, 32, 33, 34, 35 };
 
 
    int8 chunk01[] = {40, 41, 42, 43, 44, 45,
                      50, 51, 52, 53, 54, 55,
                      60, 61, 62, 63, 64, 65};
 
    int8 chunk14[] = {70, 71, 72, 73, 74, 75,
                      80, 81, 82, 83, 84, 85,
                      90, 91, 92, 93, 94, 95};

    int8 data[]    = {
		10, 11, 12, 13, 14, 15, 40, 41, 42, 43, 44, 45,  0,
                 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 
		 0,  0,  0,  0, 20, 21, 22, 23, 24, 25, 50, 51, 52, 
		53, 54, 55,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0, 30, 31, 32, 33, 34, 
		35, 60, 61, 62, 63, 64, 65,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0,  0,  0, 70, 71, 72, 
		73, 74, 75,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0, 80, 81, 82, 83, 84, 85,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  
		 0,  0,  0,  0,  0, 90, 91, 92, 93, 94, 95};


    /********************** End of variable declaration **********************/

    HCget_config_info(COMP_CODE_SZIP, &comp_config);
    CHECK( (comp_config & COMP_DECODER_ENABLED|COMP_ENCODER_ENABLED),0, "SZIP Compression not available" );
    /* Create and open the file for sziped data */
    /* Create and open the file for chunked and sziped data. */
    file_id = Hopen (CHKSZIPFILE, DFACC_CREATE, 0);
    CHECK(file_id, FAIL, "Hopen");

    /* Initialize the GR interface. */
    gr_id = GRstart (file_id);
    CHECK(gr_id, FAIL, "GRstart");

    /* Set the data type, interlace mode, and dimensions of the image. */
    data_type = DFNT_INT8;
    interlace_mode = MFGR_INTERLACE_PIXEL;
    dim_sizes[0] = LENGTH_CH;
    dim_sizes[1] = WIDTH_CH;
 
    /* Create the raster image array. */
    ri_id = GRcreate (gr_id, image_name, N_COMPS, data_type, 
                      interlace_mode, dim_sizes);
    CHECK(ri_id, FAIL, "GRcreate");
 
    /* Create chunked image array. */
    comp_flag = HDF_CHUNK | HDF_COMP;
    chunk_def.comp.chunk_lengths[0] = 3;
    chunk_def.comp.chunk_lengths[1] = 2;
    chunk_def.comp.comp_type = COMP_CODE_SZIP;
    chunk_def.comp.cinfo.szip.pixels_per_block = 2;

    chunk_def.comp.cinfo.szip.options_mask = SZ_EC_OPTION_MASK;
    chunk_def.comp.cinfo.szip.options_mask |= SZ_MSB_OPTION_MASK;
    chunk_def.comp.cinfo.szip.options_mask |= SZ_RAW_OPTION_MASK;
    chunk_def.comp.cinfo.szip.pixels = 0;
    chunk_def.comp.cinfo.szip.pixels_per_scanline = 0;
    chunk_def.comp.cinfo.szip.bits_per_pixel = 0;
 
    status = GRsetchunk(ri_id, chunk_def, comp_flag);
    if ((comp_config & COMP_ENCODER_ENABLED) == COMP_ENCODER_ENABLED) {
	/* should work */
       CHECK(status, FAIL, "GRsetchunk");
    } else {
       /* skip rest of test?? */
        /* Terminate access to the raster image */
        status = GRendaccess (ri_id);
        CHECK(status, FAIL, "GRendaccess");

        /* Terminate access to the GR interface and close the HDF file */
        status = GRend (gr_id);
        CHECK(status, FAIL, "GRend");
        status = Hclose (file_id);
        CHECK(status, FAIL, "Hclose");
        MESSAGE(1,printf("szip RI chunk: %s\n",SKIP_STR););
       return;  
    }

    /* Write first data chunk ( 0, 0 ). */
    origin[0] = origin[1] = 0;
    status = GRwritechunk(ri_id, origin, (VOIDP)chunk00);
    CHECK(status, FAIL, "GRwritechunk");
 
    /* Write second data chunk ( 0, 1 ). */
    origin[0] = 0; origin[1] = 1;
    status = GRwritechunk(ri_id, origin, (VOIDP)chunk01);
    CHECK(status, FAIL, "GRwritechunk");

    /* Write third data chunk ( 1, 4 ). */
    origin[0] = 1; origin[1] = 4;
    status = GRwritechunk(ri_id, origin, (VOIDP)chunk14);
    CHECK(status, FAIL, "GRwritechunk");

    /* Terminate accesses and close the HDF file. */
    status = GRendaccess (ri_id);
    CHECK(status, FAIL, "GRendaccess");
    status = GRend (gr_id);
    CHECK(status, FAIL, "GRend");
    status = Hclose (file_id);
    CHECK(status, FAIL, "Hclose");

    /*
     * Verify the compressed data
     */

    /* Reopen the file.  */
    file_id = Hopen (CHKSZIPFILE, DFACC_WRITE, 0); 
    CHECK(file_id, FAIL, "Hopen");

    /* Initialize the GR interface. */
    gr_id = GRstart (file_id);
    CHECK(gr_id, FAIL, "GRstart");
 
    /* Find the index of the specified image. */
    index = GRnametoindex(gr_id, image_name);
    CHECK(index, FAIL, "GRnametoindex");
   
    /* Select the image. */
    ri_id = GRselect(gr_id, index);
    CHECK(ri_id, FAIL, "GRselect");

    /* Get and verify the image's compression information. */
    comp_type = COMP_CODE_INVALID;  /* reset variables before retrieving info */
    HDmemset(&cinfo_out,  0, sizeof(cinfo_out)) ;

    status = GRgetcompress(ri_id, &comp_type, &cinfo_out);
    CHECK(status, FAIL, "GRgetcompress");
    VERIFY(comp_type, COMP_CODE_SZIP, "GRgetcompress");

    /* Read first chunk back and compare with input chunk. */
    origin[0] = 0; origin[1] = 0;
    status = GRreadchunk(ri_id, origin, (VOIDP)chunk_buf);
    CHECK(status, FAIL, "GRreadchunk");
    if (0 != HDmemcmp(chunk_buf, chunk00 , sizeof(chunk00)))
    {
	printf("Error in reading chunk 00\n" );
        num_errs++;
    }

    /* Read second chunk back and compare with input chunk. */
    origin[0] = 0; origin[1] = 1;
    status = GRreadchunk(ri_id, origin, (VOIDP)chunk_buf);
    CHECK(status, FAIL, "GRreadchunk");
    if (0 != HDmemcmp(chunk_buf, chunk01 , sizeof(chunk01)))
    {
        printf("Error in reading chunk 01\n" );
        num_errs++;
    }

    /* Read third chunk back and compare with input chunk. */
    origin[0] = 1; origin[1] = 4;
    status = GRreadchunk(ri_id, origin, (VOIDP)chunk_buf);
    CHECK(status, FAIL, "GRreadchunk");
    if (0 != HDmemcmp(chunk_buf, chunk14 , sizeof(chunk14)))
    {
        printf("Error in reading chunk 14\n" );
        num_errs++;
    }

    /* Read the whole image. */
    start[0] = start[1] = 0;
    stride[0] = stride[1] = 1;
    edge[0] = LENGTH_CH;
    edge[1] = WIDTH_CH;
    status = GRreadimage(ri_id, start, stride, edge, (VOIDP)data_out);
    CHECK(status, FAIL, "GRreadimage");
    if (0!= HDmemcmp(data_out, data, sizeof(data)))
    {
        printf("Error in reading the whole image \n" );
        num_errs++;
    }

    /* Terminate accesses and close the HDF file. */
    status = GRendaccess (ri_id);
    CHECK(status, FAIL, "GRendaccess");
    status = GRend (gr_id);
    CHECK(status, FAIL, "GRend");
    status = Hclose (file_id);
    CHECK(status, FAIL, "Hclose");
#endif
}  /* end of test_szip_chunk */

/****************************************************************
 * 
 *   test_mgr_szip(): SZIP Compression tests
 * 
 *   XIV. GR write/read szip compression tests with different data types
 *        and with chunked data
 *       A. Write/Read szip compressed image with 8-bit integer data type
 *       B. Write/Read szip compressed image with 16-bit integer data type
 *       C. Write/Read szip compressed image with 32-bit integer data type
 *       D. Write/Read szip compressed image with 32-bit floating point data type
 *       E. Write/Read szip compressed image with 64-bit floating point data type
 *       F. Write/Read image with chunked and sziped data
 * 
 * ****************************************************************/
extern void
test_mgr_szip()
{
#ifdef H4_HAVE_LIBSZ
    /* Output message about test being performed */
    MESSAGE(6, printf("Testing GR szip compression WRITE/READ\n"););

    test_szip_RI8bit();
    test_szip_RI16bit();
    test_szip_RI32bit();
    test_szip_RIfl32bit();
    test_szip_RIfl64bit();
    test_szip_chunk();
#else
    /* Output message about test being performed */
    MESSAGE(6, printf("Skipping GR szip compression WRITE/READ\n"););
#endif
} 
#endif
