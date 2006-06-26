#include "netcdf.h"

int
main() {			/* create ctest0.nc */

   int  ncid;			/* netCDF id */

   /* dimension ids */
   int  i_dim, j_dim, k_dim, l_dim;

   /* variable ids */
   int  bears_id, order_id, shot_id, aloan_id, cross_id, i_id, j_id, l_id;

   /* variable shapes */
   int dims[3];

   /* containers for scalar attributes */
   short  short_val;
   nclong  nclong_val;

   /* attribute vectors */
   float  bears_acf[3];
   double  bears_acd[2];

   /* enter define mode */
   ncid = nccreate("ctest0.nc", NC_CLOBBER);

   /* define dimensions */
   i_dim = ncdimdef(ncid, "i", 2L);
   j_dim = ncdimdef(ncid, "j", 3L);
   k_dim = ncdimdef(ncid, "k", NC_UNLIMITED);
   l_dim = ncdimdef(ncid, "l", 3L);

   /* define variables */

   dims[0] = i_dim;
   dims[1] = j_dim;
   dims[2] = l_dim;
   bears_id = ncvardef (ncid, "bears", NC_CHAR, 3, dims);

   dims[0] = i_dim;
   dims[1] = j_dim;
   order_id = ncvardef (ncid, "order", NC_SHORT, 2, dims);

   dims[0] = i_dim;
   dims[1] = j_dim;
   shot_id = ncvardef (ncid, "shot", NC_LONG, 2, dims);

   dims[0] = i_dim;
   dims[1] = j_dim;
   aloan_id = ncvardef (ncid, "aloan", NC_FLOAT, 2, dims);

   dims[0] = i_dim;
   dims[1] = j_dim;
   cross_id = ncvardef (ncid, "cross", NC_DOUBLE, 2, dims);

   dims[0] = i_dim;
   i_id = ncvardef (ncid, "i", NC_LONG, 1, dims);

   dims[0] = j_dim;
   j_id = ncvardef (ncid, "j", NC_FLOAT, 1, dims);

   dims[0] = l_dim;
   l_id = ncvardef (ncid, "l", NC_SHORT, 1, dims);

   /* assign attributes */
   ncattput (ncid, bears_id, "act", NC_CHAR, 16, (void *)"text string\n\t123");
   short_val = -40;
   ncattput (ncid, bears_id, "acs", NC_SHORT, 1,(void *) &short_val);
   nclong_val = 17000;
   ncattput (ncid, bears_id, "acl", NC_LONG, 1,(void *) &nclong_val);
   bears_acf[0] = -2;
   bears_acf[1] = 1;
   bears_acf[2] = 0;
   ncattput (ncid, bears_id, "acf", NC_FLOAT, 3, (void *) bears_acf);
   bears_acd[0] = -1;
   bears_acd[1] = 0.75;
   ncattput (ncid, bears_id, "acd", NC_DOUBLE, 2, (void *) bears_acd);
   ncattput (ncid, NC_GLOBAL, "history", NC_CHAR, 136, (void *)"This is an example of a multi-line global\nattribute.  It could be used for representing the\nprocessing history of the data, for example.");

   /* leave define mode */
   ncendef (ncid);

   {			/* store bears */
    static long bears_start[] = {0, 0, 0};
    static long bears_edges[] = {2, 3, 3};
    static char bears[] = {"indistinguishable"};
    ncvarput(ncid, bears_id, bears_start, bears_edges, (void *)bears);
   }

   {			/* store order */
    static long order_start[] = {0, 0};
    static long order_edges[] = {2, 3};
    static short order[] = {1, 2, 3, 4, 5, 6};
    ncvarput(ncid, order_id, order_start, order_edges, (void *)order);
   }

   {			/* store shot */
    static long shot_start[] = {0, 0};
    static long shot_edges[] = {2, 3};
    static nclong shot[] = {2, 3, 4, 5, 6, 7};
    ncvarput(ncid, shot_id, shot_start, shot_edges, (void *)shot);
   }

   {			/* store aloan */
    static long aloan_start[] = {0, 0};
    static long aloan_edges[] = {2, 3};
    static float aloan[] = {3, 4, 5, 6, 7, 1e+12};
    ncvarput(ncid, aloan_id, aloan_start, aloan_edges, (void *)aloan);
   }

   {			/* store cross */
    static long cross_start[] = {0, 0};
    static long cross_edges[] = {2, 3};
    static double cross[] = {4., 5., 0.000244140625, 7., 8., 10000000000.};
    ncvarput(ncid, cross_id, cross_start, cross_edges, (void *)cross);
   }

   {			/* store i */
    static long i_start[] = {0};
    static long i_edges[] = {2};
    static nclong i[] = {10, 20};
    ncvarput(ncid, i_id, i_start, i_edges, (void *)i);
   }

   {			/* store j */
    static long j_start[] = {0};
    static long j_edges[] = {3};
    static float j[] = {2, 4, 6};
    ncvarput(ncid, j_id, j_start, j_edges, (void *)j);
   }

   {			/* store l */
    static long l_start[] = {0};
    static long l_edges[] = {3};
    static short l[] = {10, 9, 8};
    ncvarput(ncid, l_id, l_start, l_edges, (void *)l);
   }
   ncclose (ncid);
   return 0;
}
