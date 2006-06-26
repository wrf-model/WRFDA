/*******************************************************************************
NAME                            Cylinderical Equal Area

PURPOSE:	Transforms input longitude and latitude to Easting and
		Northing for the Cylinderical Equal Area projection.
		The longitude and latitude must be in radians.  The Easting
		and Northing values will be returned in meters.


PROGRAMMER                               DATE          REASON
----------                               ----          ------
Abe Taaheri/Emergent Info. Tech., Inc.   6/15/00       Initial version.
Abe Taaheri/L3 Comm. Analyrics Corp.     1/15/03       Modified to support both
                                                       spherical and ellipsoid
                                                       models of earth for 
                                                       Cylinderical Equal Area
                                                       projection.

ALGORITHM REFERENCES

1.  Snyder, John P., "Map Projections--A Working Manual", U.S. Geological
    Survey Professional Paper 1395 (Supersedes USGS Bulletin 1532), United
    State Government Printing Office, Washington D.C., 1987.

2.  Snyder, John P. and Voxland, Philip M., "An Album of Map Projections",
    U.S. Geological Survey Professional Paper 1453 , United State Government
    Printing Office, Washington D.C., 1989.
*******************************************************************************/
#include <stdio.h>
#include "cproj.h"

/* Variables common to all subroutines in this code file
  -----------------------------------------------------*/
static double r_major;		/* major axis 				*/
static double r_minor;	        /* minor axis    			*/
static double e;		/* eccentricity		        	*/
static double es;		/* eccentricity squared	        	*/
static double lon_center;	/* Center longitude (projection center) */
static double lat_truesc;	/* Latitude of true scale      		*/
static double false_northing;	/* y offset in meters			*/
static double false_easting;	/* x offset in meters			*/
static double cosphi1;		/* cos of latitude of true scale	*/
static double sinphi1;		/* sin of latitude of true scale       	*/
static double kz;               /* K_0 for Cylinderical Equal Area proj.*/
static long ind;		/* spherical flag       		*/

/* Initialize the Cylinderical Equal Area projection
  -------------------------------------------------*/
int ceaforint(
double r_maj,			/* major axis			*/
double r_min,			/* minor axis			*/
double center_lon,		/* center longitude		*/
double center_lat,		/* center latitude		*/
double false_east,		/* x offset in meters		*/
double false_north)		/* y offset in meters		*/
{
double temp;			/* temporary variable		*/

/* Place parameters in static storage for common use
  -------------------------------------------------*/
r_major = r_maj;
r_minor = r_min;
lon_center = center_lon;
lat_truesc = center_lat;
false_northing = false_north;
false_easting = false_east;

temp = r_minor / r_major;
es = 1.0 - SQUARE(temp);
e = sqrt(es);

 if(es < 0.00001)
   {
     ind = 1;
   }
 else
   {
     ind = 0;
   }

 cosphi1 = cos(lat_truesc);
 sinphi1 = sin(lat_truesc);
 kz = cosphi1/(sqrt(1.0 - (es*sinphi1*sinphi1)));

/* Report parameters to the user
  -----------------------------*/
ptitle("Cylinderical Equal Area"); 
radius2(r_major, r_minor);
cenlonmer(lon_center);
true_scale(lat_truesc);
offsetp(false_easting,false_northing);
return(OK);
}


/* Cylinderical Equal Area forward equations--mapping lat,long to x,y
  --------------------------------------------------*/
int ceafor(
double lon,			/* (I) Longitude 		*/
double lat,			/* (I) Latitude 		*/
double *x,			/* (O) X projection coordinate 	*/
double *y)			/* (O) Y projection coordinate 	*/
{
double dlon;		        /* delta longitude value	*/
double sinphi;		        /* sin value	       		*/
double q;

/* Forward equations
  -----------------*/
 dlon = adjust_lon(lon - lon_center);
 sinphi = sin(lat);

 if( ind != 0) /* sphere */
   {
     *x = false_easting + r_major * dlon * cosphi1;
     *y = false_northing + r_major * sinphi / cosphi1;
   }
 else /* ellipsoid */
   {
     q = (1.0 - es) * ((sinphi / (1.0 - es * sinphi * sinphi))
		       - (1.0 / (2.0 * e)) * 
		       log((1.0 - e * sinphi)/(1.0 + e * sinphi)));
     *x = false_easting + (r_major * kz * dlon);
     *y = false_northing + (r_major * q) / (2.0 * kz);
   }

return(OK);
}
