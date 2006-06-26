/*******************************************************************************
NAME                           INV_INIT 

PURPOSE:	Initializes inverse projection transformation parameters

PROGRAMMER              DATE		REASON
----------              ----		------
T. Mittan		3-09-93		Initial Development
S. Nelson		11-94		Added Clarke spheroid default to UTM
Raj Gejjagaraguppe(ARC)   08-30-96      Landsat Ratio is removed as hard
                                        coded value.  Now this ratio can be
                                        an input from the user through the
                                        projection parameter array element
                                        number 9.
Raj Gejjagaraguppe(ARC)   01-07-97      Added a new projection type called
                                        Integerized Sinusoidal Grid to 
                                        support MODIS level 3 datasets.
D. Wynne(ARC)             3-24-97       Added Support for Power Challenge
                                        (R10000 Processor Chip Revision: 2.5)
                                        Long is 8 bytes,on all other currently 
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

int inv_init(
int insys,		/* input system code				*/
int inzone,		/* input zone number				*/
double *inparm,	/* input array of projection parameters	*/
int indatum,	/* input datum code				    */
char *fn27,		/* NAD 1927 parameter file			*/
char *fn83,		/* NAD 1983 parameter file			*/
int *iflg,		/* status flag					*/
int (*inv_trans[])(double, double, double*, double*))	
                /* inverse function pointer			*/
{
long zone;		/* zone number					*/
double azimuth;		/* azimuth					*/
double angle;		/* rotation anlge				*/
double alf;		/* SOM angle					*/
double lon1;		/* longitude point in utm scene			*/
double lon2;		/* 2nd longitude point 				*/
double lat1;		/* 1st standard parallel			*/
double lat2;		/* 2nd standard parallel			*/
double center_long;	/* center longitude				*/
double center_lat;	/* center latitude				*/
double h;		/* height above sphere				*/
double lat_origin;	/* latitude at origin				*/
double lon_origin;	/* longitude at origin				*/
double r_major;		/* major axis in meters				*/
double r_minor;		/* minor axis in meters				*/
double scale_factor;	/* scale factor					*/
double false_easting;	/* false easting in meters			*/
double false_northing;	/* false northing in meters			*/
double radius;		/* radius of sphere				*/
double shape_m;		/* constant used for Oblated Equal Area		*/
double shape_n;		/* constant used for Oblated Equal Area		*/
/*long   start;	*/	/* start of SOM Beginning or end		*/
double time;		/* SOM time					*/
long path;		/* SOM path number				*/
long satnum;		/* SOM satellite number				*/
long mode;		/* which format is used	A or B			*/
long tmpdatum;		/* temporary datum for UTM			*/
double sat_ratio;       /* satellite ratio which specify the start point*/
double dzone;           /* number of longitudinal zones in ISG          */
double djustify;        /* justify flag in ISG projection               */

long thing;             /* used to initialize 8 byte pointer, added     */
                        /* for Power Challenge                          */
long *iflg64;		/* 8 byte status flag for Power Challenge	*/

thing = 0;                      /* These lines are to initialize the    */
iflg64 = &thing;                /* the 8-byte pointer address           */

/* Initialize inverse transformations
-----------------------------------*/
  /* find the correct major and minor axis
  --------------------------------------*/
 if(insys == CEA)
   {
     if(inparm[0] > 0.0 || inparm[0] < 0.0 || 
	inparm[1] > 0.0 || inparm[1] < 0.0)
       {
	 indatum = -20;
       }

     sphdz(indatum,inparm,&r_major,&r_minor,&radius);
   }
 else if(insys == BCEA)
   {
     if(inparm[0] > 0.0 || inparm[0] < 0.0 || 
	inparm[1] > 0.0 || inparm[1] < 0.0)
       {
	 indatum = -20;
       }
     else /* for BCEA use 6371228.0 m as default for r_maj and r_min, i.e.
	     use spherical earth model with radius 6371228.0 m instead of 
	     Clarke 1866 spheroid */
       {
	 indatum = 20;
       }

     sphdz(indatum,inparm,&r_major,&r_minor,&radius);
   }
 else
   {
     sphdz(indatum,inparm,&r_major,&r_minor,&radius);
   }
  false_easting  = inparm[6];
  false_northing = inparm[7];

  if (insys == CEA)/* Cylindrical Equal-Area, used for EASE grid wghen
                      grid corners are specified in meters */
    {
    /* this is the call to initialize CEA
    ----------------------------------------*/
    center_long  = paksz(inparm[4],iflg64)* 3600 * S2R;
    *iflg = *iflg64;
    if (*iflg64 != 0)
      return ERROR;
    
    lat1   = paksz(inparm[5],iflg64)* 3600 * S2R;
    *iflg = *iflg64;
    if (*iflg64 != 0)
      return ERROR;
    
    *iflg64 = ceainvint(r_major,r_minor,center_long,lat1,false_easting,
                     false_northing);
    *iflg = *iflg64;
    inv_trans[insys] = ceainv;
    }
  else 
  if (insys == BCEA)/* Cylindrical Equal-Area, used for EASE grid wghen
                      grid corners are specified in DMS degrees */
    {
    /* this is the call to initialize BCEA
    ----------------------------------------*/
    center_long  = paksz(inparm[4],iflg64)* 3600 * S2R;
    *iflg = *iflg64;
    if (*iflg64 != 0)
      return ERROR;
    
    lat1   = paksz(inparm[5],iflg64)* 3600 * S2R;
    *iflg = *iflg64;
    if (*iflg64 != 0)
      return ERROR;
    
    *iflg64 = bceainvint(r_major,r_minor,center_long,lat1,false_easting,
                     false_northing);
    *iflg = *iflg64;
    inv_trans[insys] = bceainv;
    }
  else
  if (insys == UTM)
     {
     /* this is the call to initialize U T M
     -------------------------------------*/
     /* set Clarke 1866 spheroid if negative datum code
        ----------------------------------------------*/
     if (indatum < 0)
        {
        tmpdatum = 0;
        sphdz(tmpdatum,inparm,&r_major,&r_minor,&radius);
        }
     zone = inzone;
     if (zone == 0)
        {
        lon1 = paksz(inparm[0],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
        if (*iflg64 != 0)
           return ERROR;
        lat1 = paksz(inparm[1],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
        if (*iflg64 != 0)
           return ERROR;
        zone = calc_utm_zone(lon1 * R2D);
        if (lat1 < 0)
           zone = -zone;
        }
     scale_factor = .9996;
     *iflg64 = utminvint(r_major,r_minor,scale_factor,zone);
        *iflg = *iflg64;
     inv_trans[insys] = utminv;
     }
  else
  if (insys == SPCS)
     {
     /* this is the call to initialize STATE PLANE 
     --------------------------------------------*/
     *iflg64 = stplninvint( inzone,indatum,fn27,fn83);
        *iflg = *iflg64;
        if (*iflg64 != 0)
           return ERROR;
     inv_trans[insys] = stplninv;
     }
  else
  if (insys == ALBERS)
     {
     /* this is the call to initialize ALBERS 
     ---------------------------------------*/
     lat1 = paksz(inparm[2],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
         return ERROR;
     lat2 = paksz(inparm[3],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
         return ERROR;
     center_long = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     lat_origin  = paksz(inparm[5],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     *iflg64 = alberinvint(r_major,r_minor,lat1,lat2,center_long,lat_origin,
			false_easting, false_northing);
        *iflg = *iflg64;
     inv_trans[insys] = alberinv;
     }
  else
  if (insys == LAMCC)
     {
     /* this is the call to initialize LAMBERT CONFORMAL CONIC 
     --------------------------------------------------------*/
     lat1 = paksz(inparm[2],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     lat2 = paksz(inparm[3],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     center_long = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     lat_origin  = paksz(inparm[5],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     *iflg64 = lamccinvint(r_major,r_minor,lat1,lat2,center_long,lat_origin,
			false_easting, false_northing);
        *iflg = *iflg64;
     inv_trans[insys] = lamccinv;
     }
  else
  if (insys == MERCAT)
     {
     /* this is the call to initialize MERCATOR
     ----------------------------------------*/
     center_long  = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     lat1   = paksz(inparm[5],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     *iflg64 = merinvint(r_major,r_minor,center_long,lat1,false_easting,
                     false_northing);
        *iflg = *iflg64;
     inv_trans[insys] = merinv;
     }
  else
  if (insys == PS)
     {
     /* this is the call to initialize POLAR STEREOGRAPHIC 
     ----------------------------------------------------*/
     center_long = paksz(inparm[4],iflg64) * 3600 * S2R;
     *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     lat1  =  paksz(inparm[5],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     *iflg64 = psinvint(r_major,r_minor,center_long,lat1,false_easting,
	      false_northing);
        *iflg = *iflg64;
     inv_trans[insys] = psinv;
     }
  else
  if (insys == POLYC)
     {
     /* this is the call to initialize POLYCONIC
     -----------------------------------------*/
     center_long  = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     lat_origin   = paksz(inparm[5],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     *iflg64 = polyinvint(r_major,r_minor,center_long,lat_origin,false_easting,
		       false_northing); 
        *iflg = *iflg64;
     inv_trans[insys] = polyinv;
     }
  else
    if (insys == EQUIDC)
    {
     /* this is the call to initialize EQUIDISTANT CONIC
     ---------------------------------------------------*/
    lat1 = paksz(inparm[2],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    lat2 = paksz(inparm[3],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    center_long  = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    lat_origin   = paksz(inparm[5],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    if (inparm[8] == 0)
       mode = 0;
    else
       mode = 1;
    *iflg64 = eqconinvint(r_major,r_minor,lat1,lat2,center_long,lat_origin,
		false_easting,false_northing,mode);
        *iflg = *iflg64;
    inv_trans[insys] = eqconinv;
    }
  else
  if (insys == TM)
     {
     /* this is the call to initialize TRANSVERSE MERCATOR
     -------------------------------------------------*/
     scale_factor = inparm[2];
     center_long  = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     lat_origin   = paksz(inparm[5],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     *iflg64 = tminvint(r_major,r_minor,scale_factor,center_long,lat_origin,
		     false_easting, false_northing);
        *iflg = *iflg64;
     inv_trans[insys] = tminv;
     }
  else
  if (insys == STEREO)
     {
     /* this is the call to initialize STEREOGRAPHIC
     ---------------------------------------------*/
     center_long  = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     center_lat   = paksz(inparm[5],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     *iflg64 = sterinvint(radius,center_long,center_lat,false_easting, 
		       false_northing); 
        *iflg = *iflg64;
     inv_trans[insys] = sterinv;
     }
  else
  if (insys == LAMAZ)
     {
     /* this is the call to initialize LAMBERT AZIMUTHAL EQUAL-AREA 
     -------------------------------------------------------------*/
     center_long = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     center_lat  = paksz(inparm[5],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     *iflg64 = lamazinvint(radius, center_long, center_lat,false_easting,
			false_northing);
        *iflg = *iflg64;
     inv_trans[insys] = lamazinv;
     }
  else
  if (insys == AZMEQD)
     {
     /* this is the call to initialize AZIMUTHAL EQUIDISTANT
     ------------------------------------------------------*/
     center_long  = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     center_lat   = paksz(inparm[5],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     *iflg64 = aziminvint(radius,center_long,center_lat,false_easting,
		       false_northing); 
        *iflg = *iflg64;
     inv_trans[insys] = aziminv;
     }
  else
  if (insys == GNOMON)
     {
     /* this is the call to initialize GNOMONIC 
     ----------------------------------------*/
     center_long  = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     center_lat   = paksz(inparm[5],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     *iflg64 = gnominvint(radius,center_long,center_lat,false_easting,
                      false_northing);
        *iflg = *iflg64;
     inv_trans[insys] = gnominv;
     }
  else
  if (insys == ORTHO)
     {
     /* this is the call to initialize ORTHOGRAPHIC
     --------------------------------------------*/
     center_long  = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     center_lat   = paksz(inparm[5],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     *iflg64 = orthinvint(radius,center_long,center_lat,false_easting,
		       false_northing); 
        *iflg = *iflg64;
     inv_trans[insys] = orthinv;
     }
  else
  if (insys == GVNSP)
     {
     /* this is the call to initialize GENERAL VERTICAL NEAR SIDED PERSPECTIVE 
     -----------------------------------------------------------------------*/
     center_long  = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     center_lat   = paksz(inparm[5],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     h = inparm[2];
     *iflg64 = gvnspinvint(radius,h,center_long,center_lat,false_easting,
			false_northing);
        *iflg = *iflg64;
     inv_trans[insys] = gvnspinv;
     }
  else
  if (insys == SNSOID)
     {
     /* this is the call to initialize SINUSOIDAL 
     --------------------------------------------*/
     center_long    = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     *iflg64 = sininvint(radius, center_long,false_easting,false_northing);
        *iflg = *iflg64;
     inv_trans[insys] = sininv;
     }
  else
  if (insys == EQRECT)
     {
     /* this is the call to initialize EQUIRECTANGULAR
     -----------------------------------------------*/
     center_long  = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     lat1   = paksz(inparm[5],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     *iflg64 = equiinvint(radius,center_long,lat1,false_easting,
		       false_northing); 
        *iflg = *iflg64;
     inv_trans[insys] = equiinv;
     }
  else
  if (insys == MILLER)
    {
    /* this is the call to initialize MILLER CYLINDRICAL
    --------------------------------------------------*/
    center_long  = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = millinvint(radius, center_long,false_easting,false_northing);
        *iflg = *iflg64;
    inv_trans[insys] = millinv;
    }
  else
  if (insys == VGRINT)
    {
    /* this is the call to initialize VAN DER GRINTEN 
    -----------------------------------------------*/
    center_long  = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = vandginvint(radius, center_long,false_easting,false_northing);
        *iflg = *iflg64;
    inv_trans[insys] = vandginv;
    }
  else
  if (insys == HOM)
     {
     /* this is the call to initialize HOTLINE OBLIQUE MERCATOR 
     ---------------------------------------------------------*/
     scale_factor = inparm[2];
     lat_origin = paksz(inparm[5],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     if (inparm[12] != 0)
        {
        mode = 1;
        azimuth = paksz(inparm[3],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
        if (*iflg64 != 0)
           return ERROR;
        lon_origin = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
        if (*iflg64 != 0)
           return ERROR;
        }
     else
        {
        mode = 0;
        lon1 = paksz(inparm[8],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
        if (*iflg64 != 0)
           return ERROR;
        lat1 = paksz(inparm[9],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
        if (*iflg64 != 0)
           return ERROR;
        lon2 = paksz(inparm[10],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
        if (*iflg64 != 0)
           return ERROR;
        lat2 = paksz(inparm[11],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
        if (*iflg64 != 0)
           return ERROR;
        
        }
     *iflg64 = omerinvint(r_major,r_minor,scale_factor,azimuth,lon_origin,
			lat_origin,false_easting, false_northing,lon1,lat1,
			lon2,lat2,mode);
        *iflg = *iflg64;
     inv_trans[insys] = omerinv;
     }
  else
  if (insys == SOM)
     {
     /* this is the call to initialize SOM 
     -----------------------------------*/
     path = (long) inparm[3];
     satnum = (long) inparm[2];
    if (inparm[12] == 0)
       {
       mode = 1;
       alf = paksz(inparm[3],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
       if (*iflg64 != 0)
          return ERROR;
       lon1 = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
       if (*iflg64 != 0)
          return ERROR;
       time = inparm[8];
       sat_ratio = inparm[9];
       /* start = (long) inparm[10];*/
       }
    else
       mode = 0;
/*
     *iflg64 = sominvint(r_major,r_minor,satnum,path,false_easting,
			false_northing);
        *iflg = *iflg64;
*/
     *iflg64 = sominvint(r_major,r_minor,satnum,path,alf,lon1,false_easting,
                      false_northing,time,mode,sat_ratio);
        *iflg = *iflg64;
     inv_trans[insys] = sominv;
     }
  else
  if (insys == HAMMER)
    {
    /* this is the call to initialize HAMMER 
    --------------------------------------*/
    center_long  = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = haminvint(radius, center_long,false_easting,false_northing);
        *iflg = *iflg64;
    inv_trans[insys] = haminv;
    }
  else
  if (insys == ROBIN)
    {
    /* this is the call to initialize ROBINSON 
    ----------------------------------------*/
    center_long  = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = robinvint(radius, center_long,false_easting,false_northing);
        *iflg = *iflg64;
    inv_trans[insys] = robinv;
    }
  else
  if (insys == GOODE)
     {
     /* this is the call to initialize GOODE'S HOMOLOSINE
     ---------------------------------------------------*/
     *iflg64 = goodinvint(radius);
        *iflg = *iflg64;
     inv_trans[insys] = goodinv;
     }
  else
  if (insys == MOLL)
     {
     /* this is the call to initialize MOLLWEIDE
     -------------------------------------------*/
     center_long = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     *iflg64 = molwinvint(radius, center_long,false_easting,false_northing);
        *iflg = *iflg64;
     inv_trans[insys] = molwinv;
     }
  else
  if (insys == IMOLL)
     {
     /* this is the call to initialize INTERRUPTED MOLLWEIDE 
     -----------------------------------------------------*/
     *iflg64 = imolwinvint(radius);
        *iflg = *iflg64;
     inv_trans[insys] = imolwinv;
     }
  else
  if (insys == ALASKA)
     {
     /* this is the call to initialize ALASKA CONFORMAL 
     ------------------------------------------------*/
     *iflg64 = alconinvint(r_major,r_minor,false_easting,false_northing);
        *iflg = *iflg64;
     inv_trans[insys] = alconinv;
     }
  else
  if (insys == WAGIV)
     {
     /* this is the call to initialize WAGNER IV 
     -----------------------------------------*/
     center_long = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     *iflg64 = wivinvint(radius, center_long,false_easting,false_northing);
        *iflg = *iflg64;
     inv_trans[insys] = wivinv;
     }
  else
  if (insys == WAGVII)
     {
     /* this is the call to initialize WAGNER VII 
     ------------------------------------------*/
     center_long = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
     if (*iflg64 != 0)
        return ERROR;
     *iflg64 = wviiinvint(radius, center_long,false_easting,false_northing);
        *iflg = *iflg64;
     inv_trans[insys] = wviiinv;
     }
  else
  if (insys == OBEQA)
    {
    /* this is the call to initialize OBLATED EQUAL AREA
    ---------------------------------------------------*/
    center_long = paksz(inparm[4],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    center_lat  = paksz(inparm[5],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    shape_m = inparm[2];
    shape_n = inparm[3];
    angle = paksz(inparm[8],iflg64) * 3600 * S2R;
        *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    *iflg64 = obleqinvint(radius,center_long,center_lat,shape_m, shape_n,
                angle,false_easting,false_northing);
        *iflg = *iflg64;
    inv_trans[insys] = obleqinv;
    }
  else
    if ((insys == ISINUS) || (insys == ISINUS1))
    {
    /* this is the call to initialize INTEGERIZED SINUSOIDAL GRID
    ------------------------------------------------------------*/
 
    center_long = paksz(inparm[4],iflg64)* 3600 * S2R;
        *iflg = *iflg64;
    if (*iflg64 != 0)
       return ERROR;
    dzone = inparm[8];
    djustify = inparm[10];
 
    *iflg64 = isinusinvinit(radius, center_long, false_easting, false_northing,
                    dzone, djustify);
        *iflg = *iflg64;
 
    inv_trans[insys] = isinusinv;
    }

return OK;
}
