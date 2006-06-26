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
static char RcsId[] = "@(#)$Revision: 1.16 $";
#endif

/* $Id: r8tohdf.c,v 1.16 1996/11/11 20:40:26 koziol Exp $ */

/*
   *  r8tohdf.c
   *  Encoding of raster images in HDF files
 */

/* The intrepretation of arguments has changed a little.  A -p introduces a
   palette which will be used for subsequent images, till another -p.
   -i and -c introduce a series of images/compressed images */
#if defined __MWERKS__
#include <console.h>
#endif

#include "hdf.h"

int32       xdim, ydim;

int         main(int argc, char *argv[]);
int         palconv(char *palfile);
int         imconv(char *outfile, char *imfile, uint16 compress);

int
main(int argc, char *argv[])
{
    int         i, is_pal = 0, image = 1;
    char       *outfile;
    uint16      compress = (uint16) 0;

#if defined __MWERKS__
    argc = ccommand(&argv);
#endif

    if (argc < 5)
      {
          printf("%s,  version: 1.1   date: July 1, 1992\n", argv[0]);
          printf("   This utility converts one or more raw raster-8 images to\n");
          printf("   HDF RIS8 format and writes them to an HDF file.\n\n");
          printf("Usage:\n");
          printf("  %s xdim ydim outfile [-p palfile] ", argv[0]);
          printf("{[-r],[-c],[-i]} imagefile\n");
          printf("\t\t\t\t... [-p palfile] {[-r],[-c],[-i]} imagefile ...\n");
          printf("  -r: Store without compression (default)\n");
          printf("  -c: Store using RLE compression\n");
          printf("  -i: Store using IMCOMP compression\n\n");
          printf("* r8tohdf can take any number of images and palettes\n");
          printf("* Compression, palette, apply to all subsequent images.\n");
          printf("* All images are assumed to be the same dimensions.\n\n");
          exit(1);
      }

    xdim = atoi(argv[1]);
    ydim = atoi(argv[2]);

    if (xdim < 1 || ydim < 1)
      {
          printf("Must specify xdim and ydim\n");
          exit(1);
      }

    outfile = argv[3];

    for (i = 4; i < argc; i++)
      {
          if (*argv[i] == '-')
            {
                switch (argv[i][1])
                  {
                      case 'p': /* palette */
                          is_pal = 1;
                          image = 0;
                          break;
                      case 'r': /* raster */
                          image = 1;
                          compress = (uint16) 0;
                          break;
                      case 'c': /* RLE */
                          image = 1;
                          compress = DFTAG_RLE;
                          break;
                      case 'i': /* IMCOMP */
                          image = 1;
                          compress = DFTAG_IMC;
                          break;
                      default:
                          printf("Illegal option: %s, skipping....\n", argv[i]);
                          break;
                  }
            }
          else
            {   /* file name */
                if (image)
                  {
                      if (compress == DFTAG_IMC && is_pal == 0)
                        {
                            printf("Illegal options.  If imcomp compression (-i) ");
                            printf("chosen, you must supply a palette.\n");
                            printf("Program aborted.\n");
                            exit(1);
                        }
                      imconv(outfile, argv[i], compress);
                  }
                else
                  {
                      palconv(argv[i]);
                      image = 1;
                  }
            }
      }
    return (0);
}

/*
 *  palconv(file) sets the palette
 */

int
palconv(char *palfile)
{
    uint8       palspace[1024], reds[256], greens[256], blues[256];
    uint8      *p;
    FILE       *fp;
    int         j, ret;

    fp = fopen(palfile, "rb");
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

    ret = DFR8setpalette(palspace);
    if (ret < 0)
      {
          printf(" Error: %d, in writing palette %s\n", ret, palfile);
          exit(1);
      }
    return (0);
}

int
imconv(char *outfile, char *imfile, uint16 compress)
{
    int         ret;
    char       *space;
    FILE       *fp;

    if ((fp = fopen(imfile, "rb")) == NULL)
      {
          printf("Error opening image file\n");
          exit(1);
      }

    if ((space = (char *) HDmalloc((size_t) (xdim * ydim))) == NULL)
      {
          printf("Not enough memory to convert image\n");
          exit(1);
      }

    if ((ret = (int)fread(space, (size_t) xdim, (size_t) ydim, fp)) <= 0)
      {
          printf("Cannot read image file\n");
          fclose(fp);
          exit(1);
      }

    ret = DFR8addimage(outfile, space, xdim, ydim, compress);

    if (ret < 0)
      {
          printf(" Error: %d, in writing image %s\n", HEvalue(1), outfile);
          exit(1);
      }

    HDfree(space);
    fclose(fp);
    return (0);
}
