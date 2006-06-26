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

/* $Id: hdf24to8.c,v 1.14 1996/11/11 20:40:14 koziol Exp $ */

/**************************************************************************
* hdf24hdf8 Quantizes a HDF RGB 24 bit "pixel" image into a 8 bit image
*               with RGB palette and stores it as a HDF 8-bit raster image
*               file.
*
*  usage:        hdf24hdf8 r24_file hdf8_file
*
*               On Input:
*               --------
*
*               hdf24_file  - File containing the HDF RGB 24-bit
*                             raster image.
*
*               On Output:
*               ---------
*
*               hdf8_file   - HDF file with one 8-bit raster image set,
*                             i.e. 8-bit image, dimensions, and
*                             RGB palette.
*
* by:           NCSA
* date(s):        May 89, Jun 89, Aug 89, May 90
*
****************************************************************************/

typedef unsigned char UCHAR;
typedef unsigned int UINT;

#define     NCOLORS     256
#define     PALSIZE     3 * NCOLORS
#define     COMPRESSION 0   /* no compression */

#include <stdio.h>

#ifdef UNIX
#include <sys/file.h>
#endif

#if defined __MWERKS__
#include <console.h>
#endif

#include "hdf.h"

#define USAGE   fprintf (stderr, "usage: hdf24hdf8 hdf24_file hdf8_file\n")

int         main
            (int, char *a[]);
int         r24r8
            (int32, int32, unsigned char *, unsigned char *, int, unsigned char *);

int
main(int argc, char *argv[])
{
    int         i;
    int32       x_dim, y_dim, size;
    int         interlace;
    uint8      *r24, *r8, *pal;
    uint8       hdfpal[PALSIZE], *p;

#if defined __MWERKS__
    argc = ccommand(&argv);
#endif

    if (argc != 3)
      {
          USAGE;
          exit(1);
      }

    /* Get the HDF R24 image */

    if (DF24getdims(argv[1], &x_dim, &y_dim, &interlace) < 0)
      {
          fprintf(stderr, "error: %s is not an HDF file or ",
                  argv[1]);
          fprintf(stderr, "it does not contain a R24 image\n");
          exit(-1);
      }

    size = x_dim * y_dim;

    if ((r24 = (UCHAR *) HDmalloc(size * 3)) == NULL)
      {
          fprintf(stderr, "error: malloc to hold r24 image failed\n");
          exit(-1);
      }

    if (DF24getimage(argv[1], (VOIDP) r24, x_dim, y_dim) < 0)
      {
          fprintf(stderr, "error: DF24getimage failed\n");
          exit(-1);
      }

    if ((r8 = (UCHAR *) HDmalloc(size)) == NULL)
      {
          fprintf(stderr, "error: malloc to hold r8 image failed\n");
          exit(-1);
      }
    if ((pal = (UCHAR *) HDmalloc(PALSIZE)) == NULL)
      {
          fprintf(stderr, "error: malloc to hold palette failed\n");
          exit(-1);
      }

    if (r24r8(x_dim, y_dim, r24, r8, NCOLORS, pal) == -1)
      {
          fprintf(stderr, "error: quantization failed\n");
          exit(-1);
      }

    /* rearrange palette to conform to HDF requirements */
    p = hdfpal;
    for (i = 0; i < NCOLORS; i++)
      {
          *p++ = pal[i];
          *p++ = pal[i + NCOLORS];
          *p++ = pal[i + NCOLORS * 2];
      }
    if (DFR8setpalette(hdfpal) == -1)
      {
          HEprint(stderr, 0);
          exit(-1);
      }
    if (DFR8putimage(argv[2], (VOIDP) r8, x_dim, y_dim, COMPRESSION) == -1)
      {
          HEprint(stderr, 0);
          exit(-1);
      }

    HDfree((char *) r24);
    HDfree((char *) r8);
    HDfree((char *) pal);

    return 0;
}

int
r24r8(int32 xres, int32 yres, UCHAR * dat24, UCHAR * dat8, int cres, UCHAR * cdat)
{
    int         ct, xct, yct;
    int         rres, rd, rr, rn, rct;
    int         gres, gd, gr, gn, gct;
    int         bres, bd, br, bn, bct;
    int         coff;
    UINT       *idat[2];
    UINT       *cp, *np;
    UCHAR      *dip, *dop, *rp, *gp, *bp;

    if ((idat[0] = (UINT *) HDmalloc((size_t)(6 * xres) * sizeof(UINT))) == NULL)
      {
          fprintf(stderr, "error: Memory allocation fault\n");
          return -1;
      }
    idat[1] = idat[0] + (3 * xres);

    rres = 6;
    gres = 7;
    bres = 6;
    coff = 2;

    rr = gr = br = 255;
    rn = rres - 1;
    gn = gres - 1;
    bn = bres - 1;

    rp = cdat + coff;
    gp = rp + cres;
    bp = gp + cres;

    for (rct = 0; rct < rres; rct++)
      {
          for (gct = 0; gct < gres; gct++)
            {
                for (bct = 0; bct < bres; bct++)
                  {
                      *rp++ = (UCHAR) (rr * rct / rn);
                      *gp++ = (UCHAR) (gr * gct / gn);
                      *bp++ = (UCHAR) (br * bct / bn);
                  }
            }
      }

    rp = cdat;
    gp = rp + cres;
    bp = gp + cres;
    cp = idat[0];
    np = idat[1];
    dip = dat24;
    dop = dat8;

    for (xct = (intn) (3 * xres); --xct >= 0;)
        *cp++ = (UINT)*dip++;

    for (yct = 0; yct < (yres - 1); yct++)
      {
          np = idat[(yct + 1) % 2];
          for (xct = (intn) (3 * xres); --xct >= 0;)
              *np++ = (UINT)*dip++;

          cp = idat[yct % 2];
          np = idat[(yct + 1) % 2];

          if ((rct = ((int)cp[0] * rn / rr)) > rn)
              rct = rn;
          if ((gct = ((int)cp[1] * gn / gr)) > gn)
              gct = gn;
          if ((bct = ((int)cp[2] * bn / br)) > bn)
              bct = bn;

          *dop++ = (UCHAR) (ct = (rct * gres + gct) * bres + bct + coff);

          rd = (int)cp[0] - rp[ct];
          gd = (int)cp[1] - gp[ct];
          bd = (int)cp[2] - bp[ct];

          cp += 3;
          np += 3;

          cp[0] += (UINT)(rd * 7 / 16);
          cp[1] += (UINT)(gd * 7 / 16);
          cp[2] += (UINT)(bd * 7 / 16);
          np[-3] += (UINT)(rd * 5 / 16);
          np[-2] += (UINT)(gd * 5 / 16);
          np[-1] += (UINT)(bd * 5 / 16);
          np[0] += (UINT)(rd / 16);
          np[1] += (UINT)(gd / 16);
          np[2] += (UINT)(bd / 16);

          for (xct = 2; xct < xres; xct++)
            {
                if ((rct = ((int)cp[0] * rn / rr)) > rn)
                    rct = rn;
                if ((gct = ((int)cp[1] * gn / gr)) > gn)
                    gct = gn;
                if ((bct = ((int)cp[2] * bn / br)) > bn)
                    bct = bn;

                *dop++ = (UCHAR) (ct = (rct * gres + gct) * bres + bct + coff);

                rd = (int)cp[0] - rp[ct];
                gd = (int)cp[1] - gp[ct];
                bd = (int)cp[2] - bp[ct];

                cp += 3;
                np += 3;

                cp[0] += (UINT)(rd * 7 / 16);
                cp[1] += (UINT)(gd * 7 / 16);
                cp[2] += (UINT)(bd * 7 / 16);
                np[-6] += (UINT)(rd * 3 / 16);
                np[-5] += (UINT)(gd * 3 / 16);
                np[-4] += (UINT)(bd * 3 / 16);
                np[-3] += (UINT)(rd * 5 / 16);
                np[-2] += (UINT)(gd * 5 / 16);
                np[-1] += (UINT)(bd * 5 / 16);
                np[0] += (UINT)(rd / 16);
                np[1] += (UINT)(gd / 16);
                np[2] += (UINT)(bd / 16);

            }

          if ((rct = ((int)cp[0] * rn / rr)) > rn)
              rct = rn;
          if ((gct = ((int)cp[1] * gn / gr)) > gn)
              gct = gn;
          if ((bct = ((int)cp[2] * bn / br)) > bn)
              bct = bn;

          *dop++ = (UCHAR) (ct = (rct * gres + gct) * bres + bct + coff);

          rd = (int)cp[0] - rp[ct];
          gd = (int)cp[1] - gp[ct];
          bd = (int)cp[2] - bp[ct];

          cp += 3;
          np += 3;

          np[-6] += (UINT)(rd * 3 / 16);
          np[-5] += (UINT)(gd * 3 / 16);
          np[-4] += (UINT)(bd * 3 / 16);
          np[-3] += (UINT)(rd * 5 / 16);
          np[-2] += (UINT)(gd * 5 / 16);
          np[-1] += (UINT)(bd * 5 / 16);
      }

    cp = idat[yct % 2];

    if ((rct = ((int)cp[0] * rn / rr)) > rn)
        rct = rn;
    if ((gct = ((int)cp[1] * gn / gr)) > gn)
        gct = gn;
    if ((bct = ((int)cp[2] * bn / br)) > bn)
        bct = bn;

    *dop++ = (UCHAR) (ct = (rct * gres + gct) * bres + bct + coff);

    rd = (int)cp[0] - rp[ct];
    gd = (int)cp[1] - gp[ct];
    bd = (int)cp[2] - bp[ct];

    cp += 3;

    cp[0] += (UINT)(rd * 7 / 16);
    cp[1] += (UINT)(gd * 7 / 16);
    cp[2] += (UINT)(bd * 7 / 16);

    for (xct = 2; xct < xres; xct++)
      {
          if ((rct = ((int)cp[0] * rn / rr)) > rn)
              rct = rn;
          if ((gct = ((int)cp[1] * gn / gr)) > gn)
              gct = gn;
          if ((bct = ((int)cp[2] * bn / br)) > bn)
              bct = bn;

          *dop++ = (UCHAR) (ct = (rct * gres + gct) * bres + bct + coff);

          rd = (int)cp[0] - rp[ct];
          gd = (int)cp[1] - gp[ct];
          bd = (int)cp[2] - bp[ct];

          cp += 3;

          cp[0] += (UINT)(rd * 7 / 16);
          cp[1] += (UINT)(gd * 7 / 16);
          cp[2] += (UINT)(bd * 7 / 16);
      }

    if ((rct = ((int)cp[0] * rn / rr)) > rn)
        rct = rn;
    if ((gct = ((int)cp[1] * gn / gr)) > gn)
        gct = gn;
    if ((bct = ((int)cp[2] * bn / br)) > bn)
        bct = bn;

    *dop++ = (UCHAR) ((rct * gres + gct) * bres + bct + coff);

    HDfree(idat[0]);
    return 0;
}
