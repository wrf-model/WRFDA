/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: getfill.c,v 1.6 2000/08/29 13:57:00 koziol Exp $
 *********************************************************************/

#include "mfhdf.h"
#include "generic.h"
#include "genlib.h"

/*
 * Given netCDF type, return a default fill_value appropriate for
 * that type.
 */
void
nc_getfill(type, gval)
     nc_type type;
     union generic *gval;
{
    switch(type) {
      case NC_CHAR:
	gval->charv = FILL_CHAR;
	return;
      case NC_BYTE:
	gval->charv = FILL_BYTE;
	return;
      case NC_SHORT:
	gval->shortv = FILL_SHORT;
	return;
      case NC_LONG:
	gval->longv = FILL_LONG;
	return;
      case NC_FLOAT:
	gval->floatv = FILL_FLOAT;
	return;
      case NC_DOUBLE:
	gval->doublev = FILL_DOUBLE;
	return;
      default:
	derror("nc_getfill: unrecognized type");
    }
}

void
nc_fill(type, num, datp, fill_val)
     nc_type type;			/* netcdf type code  */
     long num;			/* number of values to fill */
     void *datp;		/* where to start filling */
     union generic fill_val;	/* value to use */
{
    char *char_valp=NULL;		/* pointers used to accumulate data values */
    short *short_valp=NULL;
    nclong *long_valp=NULL;
    float *float_valp=NULL;
    double *double_valp=NULL;

    switch (type) {
      case NC_CHAR:
      case NC_BYTE:
	char_valp = (char *) datp;
	break;
      case NC_SHORT:
	short_valp = (short *) datp;
	break;
      case NC_LONG:
	long_valp = (nclong *) datp;
	break;
      case NC_FLOAT:
	float_valp = (float *) datp;
	break;
      case NC_DOUBLE:
	double_valp = (double *) datp;
	break;
      default:
        break;
    }
    while (num--) {
	switch (type) {
	  case NC_CHAR:
	  case NC_BYTE:
	    *char_valp++ = fill_val.charv;
	    break;
	  case NC_SHORT:
	    *short_valp++ = fill_val.shortv;
	    break;
	  case NC_LONG:
	    *long_valp++ = fill_val.longv;
	    break;
	  case NC_FLOAT:
	    *float_valp++ = fill_val.floatv;
	    break;
	  case NC_DOUBLE:
	    *double_valp++ = fill_val.doublev;
	    break;
      default:
        break;
	}
    }
}


/*
 * Given netCDF type, put a value of that type into a fill_value
 */
void
nc_putfill(type, val, gval)
     nc_type type;
     void *val;			/* value of type to be put */
     union generic *gval;	/* where the value is to be put */
{
    switch(type) {
      case NC_CHAR:
      case NC_BYTE:
	gval->charv = *(char *)val;
	return;
      case NC_SHORT:
	gval->shortv = *(short *)val;
	return;
      case NC_LONG:
	gval->longv = *(nclong *)val;
	return;
      case NC_FLOAT:
	gval->floatv = *(float *)val;
	return;
      case NC_DOUBLE:
	gval->doublev = *(double *)val;
	return;
      default:
	derror("nc_putfill: unrecognized type");
    }
}
