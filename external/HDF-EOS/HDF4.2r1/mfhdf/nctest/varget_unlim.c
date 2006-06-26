/*
 * This tests uses HDF NetCDF APIs to read the NetCDF file test_unlim.nc
 * generated with the NetCDF Library v3.5 from test_unlim.cdl
 */
    
#include <stdio.h>
#include <stdlib.h>
#include "netcdf.h"
#include "testcdf.h"            /* defines in-memory test cdf structure */
#include "error.h"
#include "tests.h"
#include "alloc.h"
#include "emalloc.h"
#ifdef HDF  
#include "hdf.h" 
#endif  

float a_val[2][3] = {
                      {1.0, 2.0, 3.0}, 
                      {4.0, 5.0, 6.0}
                    };
int   date_val[12] = {840116, 840214, 840316, 840415, 840516, 840615, 840716, 840816,
                      840915, 841016, 841115, 841216 };
int   time_val[12] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 };
short b_val[][3][2] = {
                      {{1, 1}, {2, 2}, {3, 3}},
                      {{4, 4}, {5, 5}, {6, 6}},
                      {{7, 7}, {8, 8}, {9, 9}},
                      {{10, 10}, {11, 11}, {12, 12}},
                      {{13, 13}, {14, 14}, {15, 15}},
                      {{16, 16}, {17, 17}, {18, 18}},
                      {{19, 19}, {20, 20}, {21, 22}},
                      {{23, 23}, {24, 24}, {25, 25}},
                      {{26, 26}, {27, 27}, {28, 28}},
                      {{29, 29}, {30, 30}, {31, 31}},
                      {{32, 32}, {33, 33}, {34, 34}},
                      {{35, 35}, {36, 36}, {37, 37}}
                                                    };
/*
 * Test ncvarget for variables with unlimited dimensions (bug #897)
 */
void
test_ncvarget_unlim(path)
     char *path;		/* name of writable netcdf file to open */
{
    int nerrs = 0;
    static char pname[] = "test_ncvarget_unlim";
    
    int status;
    int ncid;
    int var_id;
    float a[2][3];
    int date[12];
    int time[12];
    short val[12][3][2];
    long start[3], count[3];
    int i, j, n;

    (void) fprintf(stderr, "*** Testing %s ...\t", &pname[5]);
	if ((ncid = ncopen(path, NC_NOWRITE)) == -1) {
             error("%s: ncopen failed", pname);
             return;
        }

/* Reading 3D array with unlimited dimension */

	var_id = ncvarid( ncid, "b");
        start[0] = 0;
        start[1] = 0;
        start[2] = 0;
        count[0] = 12;
        count[1] = 3;
        count[2] = 2;
	
        if(status = ncvarget (ncid, var_id, start, count, val) == -1) {
           error("%s: ncvarget failed for variable b in ", pname);
           ncclose(ncid);
           return;
        }
           
        for (n=0; n <12 ; n++) {
         for (i=0; i <3; i++)   {
          for (j=0; j<2 ; j++)   {
             if (val[n][i][j] != b_val[n][i][j]) {
             nerrs++;
             printf(" Wrong value of variable b at index %d,%d,%d\n", n,i,j);
             }
          }
         }
        }

/* Reading 2D array */

	var_id = ncvarid( ncid, "a");
        start[0] = 0;
        start[1] = 0;
        count[0] = 2;
        count[1] = 3;
	
        if(status = ncvarget (ncid, var_id, start, count, a) == -1) {
           error("%s: ncvarget failed for variable a in ", pname);
           ncclose(ncid);
           return;
        }
           
         for (i=0; i <2; i++)   {
          for (j=0; j<3 ; j++)   {
             if (a[i][j] != a_val[i][j]) {
             nerrs++;
             printf(" Wrong value of variable a at index %d,%d\n", i,j);
             }
          }
         }
        

/* Reading 1D array with unlimited dimension */

	var_id = ncvarid( ncid, "date");
        start[0] = 0;
        count[0] = 12;
	
        if(status = ncvarget (ncid, var_id, start, count, date) == -1) {
           error("%s: ncvarget failed for variable date in ", pname);
           ncclose(ncid);
           return;
        }
           
        for (n=0; n <12 ; n++) {
             if (date[n] != date_val[n]) {
             nerrs++;
             printf(" Wrong value of variable date at index %d\n", n);
             }
        }

/* Reading 1D array with unlimited dimension */

	var_id = ncvarid( ncid, "time");
        start[0] = 0;
        count[0] = 12;
	
        if(status = ncvarget (ncid, var_id, start, count, time) == -1) {
           error("%s: ncvarget failed varaible time in ", pname);
           ncclose(ncid);
           return;
        }
           
        for (n=0; n <12 ; n++) {
             if (time[n] != time_val[n]) {
             nerrs++;
             printf(" Wrong value of variable time at index %d\n", n);
             }
        }

	status = ncclose(ncid);


    if (nerrs > 0)
      (void) fprintf(stderr,"FAILED! ***\n");
    else
      (void) fprintf(stderr,"ok ***\n");
}
