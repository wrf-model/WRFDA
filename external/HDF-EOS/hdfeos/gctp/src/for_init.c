/*******************************************************************************
NAME                           FOR_INIT 

PURPOSE:	Initializes forward projection transformation parameters

PROGRAMMER                DATE		REASON
----------                ----		------
T. Mittan		  3-09-93	Initial Development
S. Nelson		  11-94		Added Clarke spheroid default to UTM
Raj Gejjagaraguppe(ARC)   8-30-96       Landsat Ratio is removed as hard
                                        coded value.  Now this ratio can be
                                        an input from the user through the
                                        projection parameter array element
                                        number 9.
Raj Gejjagaraguppe(ARC)   1-07-97       Added a new projection type called
                                        Integerized Sinusoidal Grid to 
                                        support MODIS level 3 datasets.
D. Wynne(ARC)		  3-24-97	Added Support for Power Challenge
					(R10000 Processor Chip Revision: 2.5)
					Long is 8 bytes, on all other currently
					supported platforms Long is 4 bytes.
Abe Taaheri               06-27-00      Added a new projection type called
                                        Behrmann Cylinderical Equal Area to
                                        support EASE grid.
Abe Taaheri               10-23-00      Updated for ISINUS projection, so 
                                        that both codes 31 and 99 (i.e.
                                        ISINUS and ISINUS1) can be used 
                                        for this projection.
Abe Taaheri                1-15-03      Modified to support both
                                        spherical and ellipsoid
                                        models of earth for 
                                        Normal Cylinderical Equal Area
                                        projection.
Abe Taaheri                8-15-03      Added CEA projection. This is used
                                        by generalized EASE grid, when the
                                        EASE grid corners are specified in
                                        meters (Note that BCEA is similar
                                        projection and used when EASE grid
                                        corners are set in packed DMS degrees.
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
#include "proj.h"

int for_init(
int outsys,		/* output system code				*/
int outzone,		/* output zone number				*/
double *outparm,	/* output array of projection parameters	*/
int outdatum,		/* output datum					*/
char *fn27,		/* NAD 1927 parameter file			*/
char *fn83,		/* NAD 1983 parameter file			*/
int *iflg,		/* status flag					*/
int (*for_trans[])(double, double, double *, double *))	/* forward function pointer			*/
{
long zone;		/* zone number					*/
double azimuth;		/* azimuth					*/
double alf;		/* SOM angle					*/
double angle;		/* rotation anlge				*/
double lon1;		/* longitude point in utm scene			*/
double lon2;		/* 2nd longitude 				*/
double lat1;		/* 1st standard parallel			*/
double lat2;		/* 2nd standard parallel			*/
double center_long;	/* center longitude				*/
double center_lat;	/* center latitude				*/
double h;		/* height above sphere				*/
double lon_origin;	/* longitude at origin				*/
double lat_origin;	/* latitude at origin				*/
double r_major;		/* major axis in meters				*/
double r_minor;		/* minor axis in meters				*/
double scale_factor;	/* scale factor					*/
double false_easting;	/* false easting in meters			*/
double false_northing;	/* false northing in meters			*/
double shape_m;		/* constant used for Oblated Equal Area		*/
double shape_n;		/* constant used for Oblated Equal Area		*/
long   start;		/* where SOM starts beginning or end		*/
double time;		/* SOM time					*/
double radius;		/* radius of sphere				*/
long tmpdatum;		/* temporary datum for UTM			*/
long path;		/* SOM path number				*/
long satnum;		/* SOM satellite number				*/
long mode;		/* which initialization method  to use A or B	*/
double sat_ratio;       /* satellite ratio which specify the start point*/
double dzone;           /* number of longitudinal zones in ISG          */
double djustify;        /* justify flag in ISG projection               */

long thing;		/* used to initialize 8 byte pointer, added	*/
			/* for Power Challenge		 		*/
long *iflg64;		/* 8 byte status flag, for Power Challenge	*/

thing = 0;			/* These lines are to initialize the 	*/
iflg64 = &thing;		/* the 8-byte pointer address           */

/* Initialize forward transformations
-----------------------------------*/
  /* find the correct major and minor axis
  --------------------------------------*/
 if(outsys == CEA)
   {
     if(outparm[0] > 0.0 || outparm[0] < 0.0 || 
	outparm[1] > 0.0 || outparm[1] < 0.0)
       {
	 outdatum = -20;
       }

     sphdz(outdatum,outparm,&r_major,&r_minor,&radius);
   }
 else if(outsys == BCEA)
   {
     if(outparm[0] > 0.0 || outparm[0] < 0.0 || 
	outparm[1] > 0.0 || outparm[1] < 0.0)
       {
	 outdatum = -20;
       }
     else /* for BCEA use 6371228.0 m as default for r_maj and r_min, i.e.
	     use spherical earth model with radius 6371228.0 m instead of 
	     Clarke 1866 spheroid */
       {
	 outdatum = 20;
       }

     sphdz(outdatum,outparm,&r_major,&r_minor,&radius);
   }
 else
   {
     sphdz(outdatum,outparm,&r_major,&r_minor,&radius);
   }

  false_easting  = outparm[6];
  false_northing = outparm[7];

 if (outsys == CEA)/* Cylindrical Equal-Area, used for EASE grid wghen
                      grid corners are specified in meters */
    {
    /* this is the call to initialize CEA
    ----------------------------------------*/
    center_long  = paksz(outparm[4],iflg64)* 3600 * S2R;
    *iflg = *iflg64;
    if (*iflg64 != 0)
      return ERROR;
    
    lat1   = paksz(outparm[5],iflg64)* 3600 * S2R;
    *iflg = *iflg64;
    if (*iflg64 != 0)
      return ERROR;;

    *iflg64 = ceaforint(r_major,r_minor,center_long,lat1,false_easting,
                     false_northing);
    *iflg = *iflg64;
    for_trans[outsys] = ceafor;
    }
 else 
 if (outsys == BCEA)/* Cylindrical Equal-Area, used for EASE grid wghen
                      grid corners are specified in DMS degrees */
   {
    /* this is the call to initialize BCEA
    ----------------------------------------*/
    center_long  = paksz(outparm[4],iflg64)* 3600 * S2R;
    *iflg = *iflg64;
    if (*iflg64 != 0)
      return ERROR;
    
    lat1   = paksz(outparm[5],iflg64)* 3600 * S2R;
    *iflg = *iflg64;
    if (*iflg64 != 0)
      return ERROR;;

    *iflg64 = bceaforint(r_major,r_minor,center_long,lat1,false_easting,
                     false_northing);
    *iflg = *iflg64;
    for_trans[outsys] = bceafor;
    }
  else
  if (outsys == UTM)
    {
    /* this is the call to initialize U T M
    -------------------------------------*/
     /* set Clarke 1866 spheroid if negative datum code  
        ----------------------------------------------*/
     if (outdatum < 0)
        {
        tmpdatum = 0;
	sphdz(tmpdatum,outparm,&r_major,&r_minor,&radius);
        }
    zone = outzone;
    if (zone == 0)
      {
      lon1 = paksz(outparm[0],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
      if (*iflg64 != 0)
        return ERROR;
      lat1 = paksz(outparm[1],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
      if (*iflg64 != 0)
        return ERROR;
      zone = calc_utm_zone(lon1 * R2D);
      if (lat1 < 0)
         zone = -zone;
      }
    scale_factor = .9996;
    *iflg64 = utmforint(r_major,r_minor,scale_factor,zone);
    *iflg = *iflg64;
    for_trans[outsys] = utmfor;
    }
  else
  if (outsys == SPCS)
    {
    /* this is the call to initialize STATE PLANE 
    -------------------------------------------*/
    *iflg64 = stplnforint(outzone,outdatum,fn27,fn83);
    *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    for_trans[outsys] = stplnfor;
    }
  else
  if (outsys == ALBERS)
    {
    /* this is the call to initialize ALBERS CONICAL EQUAL AREA 
    ----------------------------------------------------------*/
    lat1 = paksz(outparm[2],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    lat2 = paksz(outparm[3],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    lat_origin = paksz(outparm[5],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    center_long = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = alberforint(r_major,r_minor,lat1,lat2,center_long,lat_origin,
		       false_easting, false_northing);
      *iflg = *iflg64;
    for_trans[outsys] = alberfor;
    }
  else
  if (outsys == LAMCC)
    {
    /* this is the call to initialize LAMBERT CONFORMAL CONIC 
    --------------------------------------------------------*/
    lat1 = paksz(outparm[2],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    lat2 = paksz(outparm[3],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    center_long = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    lat_origin  = paksz(outparm[5],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = lamccforint(r_major,r_minor,lat1,lat2,center_long,lat_origin,
	   	       false_easting, false_northing);
      *iflg = *iflg64;
    for_trans[outsys] = lamccfor;
    }
  else
  if (outsys == MERCAT)
    {
    /* this is the call to initialize MERCATOR
    ----------------------------------------*/
    center_long  = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    lat1   = paksz(outparm[5],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = merforint(r_major,r_minor,center_long,lat1,false_easting,
		     false_northing);
      *iflg = *iflg64;
    for_trans[outsys] = merfor;
    }
  else
  if (outsys == PS)
    {
    /* this is the call to initialize POLAR STEREOGRAPHIC 
    ----------------------------------------------------*/
    center_long = paksz(outparm[4],iflg64)* 3600 * S2R;
       *iflg = (int) *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    lat1  = paksz(outparm[5],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;

    *iflg64 = psforint(r_major,r_minor,center_long,lat1,false_easting,
		    false_northing);
      *iflg = *iflg64;

    for_trans[outsys] = psfor;
    }
  else
  if (outsys == POLYC)
    {
    /* this is the call to initialize POLYCONIC
    -----------------------------------------*/
    center_long  = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    lat_origin   = paksz(outparm[5],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = polyforint(r_major,r_minor,center_long,lat_origin,false_easting,
		      false_northing); 
      *iflg = *iflg64;
    for_trans[outsys] = polyfor;
    }
  else
  if (outsys == EQUIDC)
    {
    /* this is the call to initialize EQUIDISTANT CONIC 
    -------------------------------------------------*/
    lat1 = paksz(outparm[2],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    lat2 = paksz(outparm[3],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    center_long  = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    lat_origin   = paksz(outparm[5],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    if (outparm[8] == 0)
       mode = 0;
    else 
       mode = 1;
    *iflg64 = eqconforint(r_major,r_minor,lat1,lat2,center_long,lat_origin,
		false_easting,false_northing,mode);
      *iflg = *iflg64;
    for_trans[outsys] = eqconfor;
    }
  else
  if (outsys == TM)
    {
    /* this is the call to initialize TRANSVERSE MECTAR
    -------------------------------------------------*/
    scale_factor = outparm[2];
    center_long  = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    lat_origin   = paksz(outparm[5],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = tmforint(r_major,r_minor,scale_factor,center_long,lat_origin,
		    false_easting,false_northing);
      *iflg = *iflg64;
    for_trans[outsys] = tmfor;
    }
  else
  if (outsys == STEREO)
    {
    /* this is the call to initialize STEREOGRAPHIC
    ---------------------------------------------*/
    center_long  = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    center_lat   = paksz(outparm[5],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = sterforint(radius,center_long,center_lat,false_easting,
		      false_northing); 
      *iflg = *iflg64;
    for_trans[outsys] = sterfor;
    }
  else
  if (outsys == LAMAZ)
    {
    /* this is the call to initialize LAMBERT AZIMUTHAL
    -------------------------------------------------*/
    center_long = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    center_lat  = paksz(outparm[5],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = lamazforint(radius,center_long, center_lat,false_easting,
		       false_northing);
      *iflg = *iflg64;
    for_trans[outsys] = lamazfor;
    }
  else
  if (outsys == AZMEQD)
    {
    /* this is the call to initialize AZIMUTHAL EQUIDISTANT
    -----------------------------------------------------*/
    center_long  = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    center_lat   = paksz(outparm[5],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = azimforint(radius,center_long,center_lat,false_easting,
		      false_northing); 
      *iflg = *iflg64;
    for_trans[outsys] = azimfor;
    }
  else
  if (outsys == GNOMON)
    {
    /* this is the call to initialize GNOMONIC 
    ----------------------------------------*/
    center_long  = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    center_lat   = paksz(outparm[5],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = gnomforint(radius,center_long,center_lat,false_easting,
		      false_northing);
      *iflg = *iflg64;
    for_trans[outsys] = gnomfor;
    }
  else
  if (outsys == ORTHO)
    {
    /* this is the call to initalize ORTHOGRAPHIC
    -------------------------------------------*/
    center_long  = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    center_lat   = paksz(outparm[5],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = orthforint(radius,center_long,center_lat,false_easting,
		      false_northing); 
      *iflg = *iflg64;
    for_trans[outsys] = orthfor;
    }
  else
  if (outsys == GVNSP)
    {
    /* this is the call to initalize GENERAL VERTICAL NEAR-SIDE PERSPECTIVE
    ----------------------------------------------------------------------*/
    center_long  = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    center_lat   = paksz(outparm[5],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    h = outparm[2];
    *iflg64 = gvnspforint(radius,h,center_long,center_lat,false_easting,
		       false_northing);
      *iflg = *iflg64;
    for_trans[outsys] = gvnspfor;
    }
  else
  if (outsys == SNSOID)
    {
    /* this is the call to initialize SINUSOIDAL 
    -------------------------------------------*/
    center_long = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = sinforint(radius, center_long,false_easting,false_northing);
      *iflg = *iflg64;
    for_trans[outsys] = sinfor;
    }
  else
  if (outsys == EQRECT)
    {
    /* this is the call to initialize EQUIRECTANGULAR
    -----------------------------------------------*/
    center_long  = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    lat1   = paksz(outparm[5],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = equiforint(radius,center_long,lat1,false_easting,false_northing); 
      *iflg = *iflg64;
    for_trans[outsys] = equifor;
    }
  else
  if (outsys == MILLER)
    {
    /* this is the call to initialize MILLER CYLINDRICAL 
    --------------------------------------------------*/
    center_long  = paksz(outparm[4],iflg64) * 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = millforint(radius, center_long,false_easting,false_northing); 
      *iflg = *iflg64;
    for_trans[outsys] = millfor;
    }
  else
  if (outsys == VGRINT)
    {
    /* this is the call to initialize VAN DER GRINTEN 
    -----------------------------------------------*/
    center_long  = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = vandgforint(radius, center_long,false_easting,false_northing); 
      *iflg = *iflg64;
    for_trans[outsys] = vandgfor;
    }
  else
  if (outsys == HOM)
     {
     /* this is the call to initialize HOTLINE OBLIQUE MERCATOR
     ---------------------------------------------------------*/
     scale_factor = outparm[2];
     lat_origin = paksz(outparm[5],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     if (outparm[12] != 0)
        {
        mode = 1;
        azimuth = paksz(outparm[3],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
        if (*iflg64 != 0)
           return ERROR;
        lon_origin = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
        if (*iflg64 != 0)
           return ERROR;
        }
     else
        {
        mode = 0;
        lon1 = paksz(outparm[8],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
        if (*iflg64 != 0)
           return ERROR;
        lat1 = paksz(outparm[9],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
        if (*iflg64 != 0)
           return ERROR;
        lon2 = paksz(outparm[10],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
        if (*iflg64 != 0)
           return ERROR;
        lat2 = paksz(outparm[11],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
        if (*iflg64 != 0)
           return ERROR;
        }
     *iflg64 = omerforint(r_major,r_minor,scale_factor,azimuth,lon_origin,
                        lat_origin,false_easting, false_northing,lon1,lat1,
                        lon2,lat2,mode);
      *iflg = *iflg64;
     for_trans[outsys] = omerfor;
     }
  else
  if (outsys == SOM)
    {
    /* this is the call to initialize SOM 
    -----------------------------------*/
    path = (long) outparm[3];
    satnum = (long) outparm[2];
    if (outparm[12] == 0)
       {
       mode = 1;
       alf = paksz(outparm[3],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
       if (*iflg64 != 0)
          return ERROR;
       lon1 = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
       if (*iflg64 != 0)
          return ERROR;
       time = outparm[8];
       sat_ratio = outparm[9];
       start = (long) outparm[10];
       }
    else
       mode = 0;
/*
    *iflg64 = somforint(r_major,r_minor,satnum,path,false_easting,false_northing);
      *iflg = *iflg64;
*/
    *iflg64 = somforint(r_major,r_minor,satnum,path,alf,lon1,false_easting,
		      false_northing,time,start,mode,sat_ratio);
      *iflg = *iflg64;
    for_trans[outsys] = somfor;
    }
  else
  if (outsys == HAMMER)
    {
    /* this is the call to initialize HAMMER 
    --------------------------------------*/
    center_long  = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = hamforint(radius,center_long,false_easting,false_northing); 
      *iflg = *iflg64;
    for_trans[outsys] = hamfor;
    }
  else
  if (outsys == ROBIN)
    {
    /* this is the call to initialize ROBINSON 
    ----------------------------------------*/
    center_long  = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = robforint(radius,center_long,false_easting,false_northing); 
      *iflg = *iflg64;
    for_trans[outsys] = robfor;
    }
  else
  if (outsys == GOODE)
    {
    /* this is the call to initialize GOODE'S HOMOLOSINE
    ---------------------------------------------------*/
    *iflg64 = goodforint(radius);
      *iflg = *iflg64;
    for_trans[outsys] = goodfor;
    }
  else
  if (outsys == MOLL)
    {
    /* this is the call to initialize MOLLWEIDE
    ------------------------------------------*/
    center_long = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = molwforint(radius, center_long,false_easting,false_northing);
      *iflg = *iflg64;
    for_trans[outsys] = molwfor;
    }
  else
  if (outsys == IMOLL)
    {
    /* this is the call to initialize INTERRUPTED MOLLWEIDE
    -----------------------------------------------------*/
    *iflg64 = imolwforint(radius);
      *iflg = *iflg64;
    for_trans[outsys] = imolwfor;
    }
  else
  if (outsys == ALASKA)
    {
    /* this is the call to initialize ALASKA CONFORMAL 
    ------------------------------------------------*/
    *iflg64 = alconforint(r_major,r_minor,false_easting,false_northing);
      *iflg = *iflg64;
    for_trans[outsys] = alconfor;
    }
  else
  if (outsys == WAGIV)
    {
    /* this is the call to initialize WAGNER IV 
    -----------------------------------------*/
    center_long = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = wivforint(radius, center_long,false_easting,false_northing);
      *iflg = *iflg64;
    for_trans[outsys] = wivfor;
    }
  else
  if (outsys == WAGVII)
    {
    /* this is the call to initialize WAGNER VII 
    ------------------------------------------*/
    center_long = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = wviiforint(radius, center_long,false_easting,false_northing);
      *iflg = *iflg64;
    for_trans[outsys] = wviifor;
    }
  else
  if (outsys == OBEQA)
    {
    /* this is the call to initialize OBLATED EQUAL AREA 
    ---------------------------------------------------*/
    center_long = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    center_lat  = paksz(outparm[5],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    shape_m = outparm[2];
    shape_n = outparm[3];
    angle = paksz(outparm[8],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = obleqforint(radius,center_long,center_lat,shape_m, shape_n, 
		angle,false_easting,false_northing);
      *iflg = *iflg64;
    for_trans[outsys] = obleqfor;
    }
  else
  if ((outsys == ISINUS) || (outsys == ISINUS1))
    {
    /* this is the call to initialize INTEGERIZED SINUSOIDAL GRID
    ------------------------------------------------------------*/

    center_long = paksz(outparm[4],iflg64)* 3600 * S2R;
      *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    dzone = outparm[8];
    djustify = outparm[10];

    *iflg64 = isinusforinit(radius, center_long, false_easting, false_northing,
                    dzone, djustify);
      *iflg = *iflg64;

    for_trans[outsys] = isinusfor;
    }
       
return OK;
}
