/*******************************************************************************
NAME                            Cylinderical Equal Area

PURPOSE:	Transforms input Easting and Northing to longitude and
		latitude for the Cylinderical Equal Area projection.
		The Easting and Northing must be in meters.  The longitude
		and latitude values will be returned in radians.


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
static double r_minor;		/* minor axis 				*/
static double e;		/* eccentricity		        	*/
static double es;		/* eccentricity squared	        	*/
static double e_p4;                    /* eccentricity's 4th power    	*/
static double e_p6;                    /* eccentricity's 6th power    	*/
static double lon_center;	/* Center longitude (projection center) */
static double lat_truesc;	/* latitude of true scale	      	*/
static double false_northing;	/* y offset in meters			*/
static double false_easting;	/* x offset in meters			*/
static double cosphi1;		/* cos of latitude of true scale	*/
static double sinphi1;		/* sin of latitude of true scale       	*/
static double kz;               /* K_0 for Cylinderical Equal Area proj.*/
static double qp;               /* qp  for Cylinderical Equal Area proj.*/
static long ind;		/* spherical flag        		*/

/* Initialize the Cylinderical Equal Area projection
  -----------------------------------*/
int ceainvint(
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
e_p4 = es * es;
e_p6 = e_p4 *es;

 if(e < 0.00001)
   {
     ind = 1;
     qp = 2.0;
   }
 else
   {
     ind = 0;
     qp = (1.0 - es)*((1.0/(1.0 - es))-(1.0/(2.0*e))*log((1.0 - e)/(1.0 + e)));
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


/* Cylinderical Equal Area inverse equations--mapping x,y to lat/long
  --------------------------------------------------*/
int ceainv(
double x,			/* (O) X projection coordinate 	*/
double y,			/* (O) Y projection coordinate 	*/
double *lon,			/* (I) Longitude 		*/
double *lat)			/* (I) Latitude 		*/
{
  double beta;

/* Inverse equations
  -----------------*/
x -= false_easting;
y -= false_northing;

 if( ind != 0) /* sphere */
   {
     *lat = asin( y * cosphi1 / r_major); /* we may need to use asinz dfined in
					     cproj.c instead of asin */
     *lon = adjust_lon(lon_center + x/(r_major * cosphi1));
   }
 else /* ellipsoid */
   {
     beta = asin(2.0 * y * kz/(r_major * qp));
  
     *lat = beta +(((es / 3.0) + ((31.0/180.0) * e_p4)+
		((517.0/5040.0) * e_p6)) * sin(2.0*beta))+
		  ((((23.0/360.0) * e_p4)+
		    ((251.0/3780.0) * e_p6)) * sin(4.0*beta))+
		      (((761.0/45360.0) * e_p6) * sin(6.0*beta));
     *lon = adjust_lon(lon_center + x/(r_major * kz));
   }

return(OK);
}
