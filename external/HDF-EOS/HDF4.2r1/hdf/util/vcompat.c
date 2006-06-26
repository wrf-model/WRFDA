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
static char RcsId[] = "@(#)$Revision: 1.14 $";
#endif

/* $Id: vcompat.c,v 1.14 1998/12/08 21:37:57 koziol Exp $ */
#if defined __MWERKS__
#include <console.h>
#endif

#include <stdio.h>
#include "hdf.h"

int
main(int argc, char *argv[])
{

#if defined __MWERKS__
    argc = ccommand(&argv);
#endif

    if (argc != 2)
      {
          fprintf(stderr, "%s: converts HDF vset v1.0 files to v2.0\n", argv[0]);
          fprintf(stderr, "Usage: %s hdf-file\n", argv[0]);
          exit(0);
      }

    if (0 == vcheckcompat(argv[1]))
      {
          vmakecompat(argv[1]);
          fprintf(stderr, "file [%s] is now compatible\n", argv[1]);
      }
    else
        fprintf(stderr, "file [%s] already compatible with r2.0\n", argv[1]);

    return (0);
}   /* main */

/* ------------------------------------------------------------------ */
