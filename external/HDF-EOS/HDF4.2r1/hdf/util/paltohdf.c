
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
static char RcsId[] = "@(#) $Revision: 1.11 $"
#endif

/* $Id: paltohdf.c,v 1.11 1996/04/09 20:09:44 georgev Exp $ */

/*
   *  paltohdf.c
   *       Version: 1.0   date: August 1, 1989
   *       This utility converts a raw palette to hdf format
   *       The incoming palette is assumed to have 768 bytes:
   *          256 red values, 256 greens, and 256 blues.
   *          The palette in the HDF file will have the RGB values
   *          interlaced: RGB RGB ... (This is standard HDF format.)
   *
   *  by Mike Folk
   *  first version of paltohdf:   8/01/89
   *
   *  This program is in the public domain
 */
#if defined __MWERKS__
#include <console.h>
#endif

#include "hdf.h"

int main(int argc, char *argv[]);
int         palconv(char *palfile, char *outfile);

int
main(int argc, char *argv[])
{
#if defined __MWERKS__
    argc = ccommand(&argv);
#endif

    if (argc != 3)
      {
          printf("Usage:\n");
          printf("   %s rawpalfile hdffile \n\n", argv[0]);
          printf("%s,  version: 1.1   date: July 1, 1992\n\n", argv[0]);
          printf("\tThis utility converts a raw palette to hdf format \n\n");
          printf("\tThe incoming palette is assumed to have 768 bytes:\n");
          printf("\t256 red values, 256 greens, and 256 blues.\n\n");
          printf("\tThe palette in the HDF file will have the RGB values\n");
          printf("\tinterlaced: RGB RGB ... (standard HDF format).\n\n");
          exit(1);
      }

    palconv(argv[1], argv[2]);
    return (0);
}

/*
 *    palconv(palfile, outfile) sets the palette
 */

int
palconv(char *palfile, char *outfile)
{
    unsigned char palspace[1024], reds[256], greens[256], blues[256], *p;
    FILE       *fp;
    int         j, ret;

    fp = fopen(palfile, "r");
    if (fp == NULL)
      {
          printf(" Error opening palette file %s\n", palfile);
          exit(1);
      }
    fread(reds, 1, 256, fp);
    fread(greens, 1, 256, fp);
    fread(blues, 1, 256, fp);
    fclose(fp);

    p = palspace;
    for (j = 0; j < 256; j++)
      {
          *p++ = reds[j];
          *p++ = greens[j];
          *p++ = blues[j];
      }

    ret = DFPaddpal(outfile, (VOIDP) palspace);
    if (ret < 0)
      {
          printf(" Error: %d, in writing palette %s\n", ret, palfile);
          exit(1);
      }
    return (0);
}
