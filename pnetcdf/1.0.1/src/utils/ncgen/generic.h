/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Header: /homes/gropp/cvsMaster_z/parallel-netcdf/src/utils/ncgen/generic.h,v 1.1 2003/12/11 23:28:38 robl Exp $
 *********************************************************************/

#ifndef UD_GENERIC_H
#define UD_GENERIC_H

union generic {			/* used to hold any kind of fill_value */
    float floatv;
    double doublev;
    int intv;
    short shortv;
    char charv;
};

#endif
