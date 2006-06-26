#include "mfhdf.h"
 
#define X_LENGTH      2
#define Y_LENGTH      3
#define RANK          2  /* Number of dimensions of the SDS */
#define FILE_ATTR_NAME "File_contents"
#define SDS_ATTR_NAME  "Valid_range"


#define  CLASS1_NAME    "5x1 Array"
#define  CLASS2_NAME    "6x4 Array"
#define  VDATA1_NAME    "vdata1"
#define  VDATA2_NAME    "vdata2"
#define  FIELD1_NAME    "Single-component Field"
#define  FIELD2_NAME    "Multi-component Field"
#define  N_RECORDS_1    5    /* number of records the first vdata contains  */
#define  N_RECORDS_2    2    /* number of records the second vdata contains */
#define  ORDER_2        4    /* order of the field in the second vdata      */

#define  CLASS3_NAME       "Particle Data"
#define  VDATA3_NAME       "vdata3"
#define  FIELD3_NAME1      "Position"      /* contains x, y, z values */
#define  FIELD3_NAME2      "Mass"          /* contains weight values */
#define  FIELD3_NAME3      "Temperature"   /* contains min and max values */
#define  FIELDNAME3_LIST   "Position,Mass,Temperature" /* No spaces b/w names */
#define  ORDER3_1          3         /* order of first field */
#define  ORDER3_2          1         /* order of second field */
#define  ORDER3_3          2         /* order of third field */
#define  N_VALS_PER_REC   (ORDER3_1 + ORDER3_2 + ORDER3_3)  /* number of values per record */


int
main( ) 
{
 int32 sd1_id, sd2_id;       /* SD interface identifier */
 int32 sds1_id, sds2_id;     /* data set identifiers */
 int32 dim_sizes[2];         /* sizes of the SDS dimensions */
 int32 start[2], edges[2];
 intn  status;
 int32 status_32;
 int32 n_values;
 int32 buf1a[Y_LENGTH][X_LENGTH] = {{1,1},{1,1},{5,6}};
 int32 buf1b[Y_LENGTH][X_LENGTH] = {{1,2},{3,4},{5,6}};
 char8 bufga1[] = "Storm_track_data1"; 
 char8 bufga2[] = "Storm_track_data2"; 
 float32 bufa1[2] = {1., 1.};
 float32 bufa2[2] = {1., 2.};
                                                                     

 /*vdata*/
 int32 file1_id; 
 int32 file2_id; 
 int32 vdata1_id, vdata2_id;
 char8 vdata1_buf1 [N_RECORDS_1] = {'V', 'D', 'A', 'T', 'A'};
 char8 vdata1_buf2 [N_RECORDS_1] = {'X', 'D', 'A', 'T', 'A'};
 int32 vdata2_buf1  [N_RECORDS_2][ORDER_2] = {{1, 2, 3, 4}, {5, 6, 7, 8}};
 int32 vdata2_buf2  [N_RECORDS_2][ORDER_2] = {{1, 1, 1, 1}, {5, 6, 7, 8}};
 float32 vdata3_buf1[N_RECORDS_2][N_VALS_PER_REC]={{1,2,3,4,5,6},{7,8,9,10,11,12}}; 
 float32 vdata3_buf2[N_RECORDS_2][N_VALS_PER_REC]={{1,1,1,1,1,1},{7,8,9,10,11,12}}; 
 
/* Define the location and size of the data to be written to the data set*/
 start[0] = 0;
 start[1] = 0;
 edges[0] = Y_LENGTH;
 edges[1] = X_LENGTH;

/* Define the dimensions of the array to be created */
 dim_sizes[0] = Y_LENGTH;
 dim_sizes[1] = X_LENGTH;

/*-------------------------------------------------------------------------
 * SD data
 *-------------------------------------------------------------------------
 */
 
/* Create the files and initialize the SD interface */
 sd1_id = SDstart ("hdifftst1.hdf", DFACC_CREATE);
 sd2_id = SDstart ("hdifftst2.hdf", DFACC_CREATE);

/* Set a global attribute */
 n_values  = sizeof(bufga1);
 status = SDsetattr (sd1_id, FILE_ATTR_NAME, DFNT_CHAR8, n_values, (VOIDP)bufga1);
 status = SDsetattr (sd2_id, FILE_ATTR_NAME, DFNT_CHAR8, n_values, (VOIDP)bufga2);
  
/* Create the data set */ 
 sds1_id = SDcreate (sd1_id, "dset1", DFNT_INT32, RANK, dim_sizes);
 sds2_id = SDcreate (sd2_id, "dset1", DFNT_INT32, RANK, dim_sizes);

/* Assign attribute */
 n_values  = 2;
 status = SDsetattr (sds1_id, SDS_ATTR_NAME, DFNT_FLOAT32, n_values, (VOIDP)bufa1);
 status = SDsetattr (sds2_id, SDS_ATTR_NAME, DFNT_FLOAT32, n_values, (VOIDP)bufa2);

 
/* Write the stored data to the data set */
 status = SDwritedata (sds1_id, start, NULL, edges, (VOIDP)buf1a);
 status = SDwritedata (sds2_id, start, NULL, edges, (VOIDP)buf1b);
 
/* Terminate access to the data set */
 status = SDendaccess (sds1_id);
 status = SDendaccess (sds2_id);

 /* Create another data set */ 
 sds1_id = SDcreate (sd1_id, "dset2", DFNT_INT32, RANK, dim_sizes);
 sds2_id = SDcreate (sd2_id, "dset2", DFNT_INT32, RANK, dim_sizes);
 status = SDwritedata (sds1_id, start, NULL, edges, (VOIDP)buf1a);
 status = SDwritedata (sds2_id, start, NULL, edges, (VOIDP)buf1b);
 status = SDendaccess (sds1_id);
 status = SDendaccess (sds2_id);


/*-------------------------------------------------------------------------
 * end SD 
 *-------------------------------------------------------------------------
 */
 
/* Terminate access to the SD interface and close the file */
 status = SDend (sd1_id);
 status = SDend (sd2_id);



/*-------------------------------------------------------------------------
 * VD data 
 *-------------------------------------------------------------------------
 */

/* Open the HDF file for writing */
 file1_id = Hopen ("hdifftst1.hdf", DFACC_WRITE, 0);
 file2_id = Hopen ("hdifftst2.hdf", DFACC_WRITE, 0);
 
/* Initialize the VS interface */
 status = Vstart (file1_id);
 status = Vstart (file2_id);

/*-------------------------------------------------------------------------
 * VD data one field
 *-------------------------------------------------------------------------
 */
 
/* Create the first vdata and populate it with data from vdata1_buf */
 VHstoredata (file1_id, FIELD1_NAME, (uint8 *)vdata1_buf1, 
  N_RECORDS_1, DFNT_CHAR8, VDATA1_NAME, CLASS1_NAME); 
 VHstoredata (file2_id, FIELD1_NAME, (uint8 *)vdata1_buf2, 
  N_RECORDS_1, DFNT_CHAR8, VDATA1_NAME, CLASS1_NAME); 

/*-------------------------------------------------------------------------
 * VD data one field, order 4
 *-------------------------------------------------------------------------
 */
 
/* Create the second vdata and populate it with data from vdata2_buf */
 VHstoredatam (file1_id, FIELD2_NAME, (uint8 *)vdata2_buf1, 
  N_RECORDS_2, DFNT_INT32, VDATA2_NAME, CLASS2_NAME, ORDER_2); 
 VHstoredatam (file2_id, FIELD2_NAME, (uint8 *)vdata2_buf2, 
  N_RECORDS_2, DFNT_INT32, VDATA2_NAME, CLASS2_NAME, ORDER_2); 

/*-------------------------------------------------------------------------
 * VD data several fields
 *-------------------------------------------------------------------------
 */

/* Create a new vdata */
 vdata1_id = VSattach (file1_id, -1, "w");
 vdata2_id = VSattach (file2_id, -1, "w");

/* Set name and class name of the vdata */
 status_32 = VSsetname (vdata1_id, VDATA3_NAME);
 status_32 = VSsetclass (vdata1_id, CLASS3_NAME);
 status_32 = VSsetname (vdata2_id, VDATA3_NAME);
 status_32 = VSsetclass (vdata2_id, CLASS3_NAME);

/* Define fields */
 status = VSfdefine (vdata1_id, FIELD3_NAME1, DFNT_FLOAT32, ORDER3_1 );
 status = VSfdefine (vdata1_id, FIELD3_NAME2, DFNT_FLOAT32, ORDER3_2 );
 status = VSfdefine (vdata1_id, FIELD3_NAME3, DFNT_FLOAT32, ORDER3_3 );
 status = VSsetfields (vdata1_id, FIELDNAME3_LIST);
 status = VSfdefine (vdata2_id, FIELD3_NAME1, DFNT_FLOAT32, ORDER3_1 );
 status = VSfdefine (vdata2_id, FIELD3_NAME2, DFNT_FLOAT32, ORDER3_2 );
 status = VSfdefine (vdata2_id, FIELD3_NAME3, DFNT_FLOAT32, ORDER3_3 );
 status = VSsetfields (vdata2_id, FIELDNAME3_LIST);

/* Write the data with full interlacing mode */
 VSwrite (vdata1_id, (uint8 *)vdata3_buf1, N_RECORDS_2, FULL_INTERLACE);
 VSwrite (vdata2_id, (uint8 *)vdata3_buf2, N_RECORDS_2, FULL_INTERLACE);

 status_32 = VSdetach (vdata1_id);
 status_32 = VSdetach (vdata2_id);

/*-------------------------------------------------------------------------
 * end VD data 
 *-------------------------------------------------------------------------
 */
 
/* Terminate access to the VS interface and close the HDF file */
 status = Vend (file1_id);
 status = Vend (file2_id);
 status_32 = Hclose (file1_id);
 status_32 = Hclose (file2_id);

 /* shut compiler */
 status=status;
 status_32=status_32;

 return 0;

}


