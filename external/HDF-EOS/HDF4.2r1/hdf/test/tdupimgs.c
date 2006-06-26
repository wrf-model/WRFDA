#include "hdf.h"
#include <stdio.h>
#include "tutils.h"

#define FILE_NAME     "gr_double_test.hdf"
#define IMAGE_COMP    "Compressed Image"
#define IMAGE_CHUNK   "Chunked Image"
#define IMAGE_CMP_CHK "Comp_Chunked Image"
#define  N_COMPS  3  /* number of components in the image */
#define N_IMAGES  3  /* currently, 3 images in the file, must be updated as needed */
#define X_LENGTH 10
#define Y_LENGTH 10

extern void test_mgr_dup_images()
{
    int32 fid, grid, riid, index, il = MFGR_INTERLACE_PIXEL;
    int32 ncomp=1; 
    int32 start[2], stride[2], edges[2],dims[2];
    uint8 image_data[X_LENGTH][Y_LENGTH]; 
    intn i, j; 
    intn status; 
    int32 n_datasets;       /* number of datasets */
    int32 n_attrs;          /* number of attributes */

    /* Output message about test being performed */
    MESSAGE(6, printf("Testing eliminating duplicate images\n"););

    MESSAGE(8, printf("Try creating a new file and checking it out\n"););

    /* Create the file. */
    fid = Hopen(FILE_NAME, DFACC_CREATE, 0); 
    CHECK(fid, FAIL,"Hopen");

    /* Initiate the GR interface. */
    grid = GRstart(fid); 
    CHECK(grid, FAIL, "GRstart");

    /* Define the location, pattern, and size of the data set */
    dims[0] = X_LENGTH;
    dims[1] = Y_LENGTH;
    for (i = 0; i < 2; i++) 
    {
	start[i] = 0; 
	edges[i] = dims[i]; 
    }

    /* Initialize data we are going to write out, same for all images */
    for (i = 0; i < Y_LENGTH; i++)
	for (j = 0; j < X_LENGTH; j++)
	    image_data[i][j] = (uint8) (i + j);

    /**** Make an image with compressed data ****/
    {
	HDF_CHUNK_DEF c_def; 
	comp_info cinfo;
	int32 comp_type;

        /* Create the first image in this file */
        riid=GRcreate(grid, IMAGE_COMP, 1, DFNT_UINT8, il, dims);
        CHECK(riid, FAIL, "GRcreate");

        /* Set the compression method for the image */
        comp_type=COMP_CODE_DEFLATE;
        cinfo.deflate.level=7;
        status = GRsetcompress(riid, comp_type, &cinfo);
        CHECK(status, FAIL, "GRsetcompress");

        /* Write the whole image out */
        start[0]=start[1]=0;
        stride[0]=stride[1]=1;
        status = GRwriteimage(riid, start, stride, dims, image_data);
        CHECK(status, FAIL, "GRreadimage");

        /* Close the image */
        status = GRendaccess(riid);
	CHECK(status, FAIL, "GRendaccess");
    }

    /**** Make an image with chunked  data ****/
    {
	int32 comp_flag;
	HDF_CHUNK_DEF c_def; 

	/* Create the second image */
	riid=GRcreate(grid, IMAGE_CHUNK, N_COMPS, DFNT_UINT8, il, dims);
	CHECK(riid, FAIL, "GRcreate");

	/* Make it chunked image */
	comp_flag = HDF_CHUNK;
	c_def.chunk_lengths[0] = 3;
	c_def.chunk_lengths[1] = 2;

	status = GRsetchunk(riid, c_def, comp_flag);
	CHECK(status, FAIL, "GRsetchunk");

	/* Write data to the entire image */
	start[0]=start[1]=0;
	stride[0]=stride[1]=1;
	status = GRwriteimage(riid, start, stride, dims, image_data);
	CHECK(status, FAIL, "GRwriteimage");

	/* Terminate access to this image */
	status = GRendaccess (riid);
	CHECK(status, FAIL, "GRendaccess");
    } /* finish creating an image with chunked data */

    /**** Make a GR image with chunked and DEFLATE compressed data */
    {
	int32 comp_type, comp_flag;
	HDF_CHUNK_DEF c_def; 
	comp_info cinfo;

	/* Create the image */
	riid = GRcreate(grid, IMAGE_CMP_CHK, 3, DFNT_UINT8, il, dims); 
	CHECK(riid, FAIL, "GRcreate");

	/* Define the location, pattern, and size of the data set */
	for (i = 0; i < 2; i++) 
	{ 
	    c_def.comp.chunk_lengths[i]=dims[i]/2; 
	} 

	/* Set info for compression/chunk */
	c_def.comp.comp_type = COMP_CODE_DEFLATE;
	comp_flag = HDF_CHUNK|HDF_COMP;
	c_def.comp.cinfo.deflate.level = 9;
	status = GRsetchunk(riid,c_def,comp_flag);
	CHECK(status, FAIL, "GRsetchunk");

	/* Write the stored data to the image array */
	status = GRwriteimage(riid, start, NULL, edges, (VOIDP)image_data);
	CHECK(status, FAIL, "GRwriteimage");

	/* Terminate access to the image */
	status = GRendaccess(riid);
	CHECK(status, FAIL, "GRendaccess");
    }

    /* Terminate access to the file */
    status = GRend(grid);
    CHECK(status, FAIL, "GRend");
    status = Hclose(fid);
    CHECK(status, FAIL, "Hclose");

    /* Open the file again and check its info */
    fid = Hopen(FILE_NAME, DFACC_READ, 0); 
    CHECK(fid, FAIL, "Hopen");

    /* Initiate the GR interface. */
    grid = GRstart(fid); 
    CHECK(grid, FAIL, "GRstart");

    /* There currently should only be 3 images in the file */
    status = GRfileinfo(grid,&n_datasets,&n_attrs);
    CHECK(status, FAIL, "GRfileinfo");
    VERIFY(n_datasets, N_IMAGES, "GRfileinfo");

    /* Terminate access to the file */
    status = GRend(grid);
    CHECK(status, FAIL, "GRend");
    status = Hclose(fid);
    CHECK(status, FAIL, "Hclose");
}

