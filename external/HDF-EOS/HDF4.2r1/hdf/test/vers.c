/****************************************************************************
 * NCSA HDF                                                                 *
 * Software Development Group                                               *
 * National Center for Supercomputing Applications                          *
 * University of Illinois at Urbana-Champaign                               *
 * 605 E. Springfield, Champaign IL 61820                                   *
 *                                                                          *
 * For conditions of distribution and use, see the accompanying             *
 * hdf/COPYING file.                                                        *
 *                                                                          *
 ****************************************************************************/

#ifdef RCSID
static char RcsId[] = "@(#)$Revision: 1.8 $";
#endif

/* $Id: vers.c,v 1.8 1994/06/29 13:10:53 koziol Exp $ */

/*
   ***********************************************************************
   ** get version string from an HDF file
   ***********************************************************************
 */

#include <stdio.h>
#include "tproto.h"

void
test_vers(void)
{
    int         ret;
    uint32      lmajor, lminor, lrelease;
    char        lstring[81];

    ret = Hgetlibversion(&lmajor, &lminor, &lrelease, lstring);
    RESULT("Hgetlibversion");
    MESSAGE(5, printf("Library Version\n");
        );
    MESSAGE(5, printf("---------------\n");
        );
    MESSAGE(5, printf("Major:\t\t%u\nMinor:\t\t%u\nRelease:\t%u\nString:\t\t\"%s\"\n", (unsigned) lmajor, (unsigned) lminor, (unsigned) lrelease, lstring);
        );

}
