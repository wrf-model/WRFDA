/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: generic.h,v 1.3 1996/03/26 22:39:34 georgev Exp $
 *********************************************************************/

union generic {			/* used to hold any kind of fill_value */
    double doublev;
    float floatv;
    nclong longv;
    short shortv;
    char charv;
};
