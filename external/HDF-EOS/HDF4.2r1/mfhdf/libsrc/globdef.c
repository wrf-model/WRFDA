/*
 *	Copyright 1993, University Corporation for Atmospheric Research
 *
 *  Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose without fee is hereby granted, provided
 * that the above copyright notice appear in all copies, that both that
 * copyright notice and this permission notice appear in supporting
 * documentation, and that the name of UCAR/Unidata not be used in
 * advertising or publicity pertaining to distribution of the software
 * without specific, written prior permission.  UCAR makes no
 * representations about the suitability of this software for any purpose.
 * It is provided "as is" without express or implied warranty.  It is
 * provided with no support and without obligation on the part of UCAR
 * Unidata, to assist in its use, correction, modification, or enhancement.
 *
 *
 * $Id: globdef.c,v 1.1 1993/04/21 21:50:03 chouck Exp $
 *
 * This file initializes all global variables.  It's a separate file in order
 * to allow the creation of SunOS sharable-libraries.
 */

#include "netcdf.h"


int ncerr = NC_NOERR ;


/*
 * russ's last minute whistles
 *	The error(3) subroutines emit no messages unless NC_VERBOSE bit is on.
 *	The error(3) subroutines call exit() when NC_FATAL bit is on.
 */
int ncopts = (NC_FATAL | NC_VERBOSE) ;


/*
 *	Set to the the name of the current interface routine by the
 * interface routine.
 */
char *cdf_routine_name = "netcdf";
