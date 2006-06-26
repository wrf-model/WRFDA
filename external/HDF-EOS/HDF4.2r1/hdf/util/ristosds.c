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
static char RcsId[] = "@(#)$Revision: 1.17 $";
#endif

/* $Id: ristosds.c,v 1.17 1996/11/11 20:40:26 koziol Exp $ */

/* This program converts a series raster image hdf files into   */
/* a single 3D sds hdf file. Each ris hdf file contains one     */
/* or more raster images. All images should be of the same      */
/* dimension sizes. And, if there is a palette associated to    */
/* the images, the palette should be included in the first      */
/* ris file. Any subsequent palettes will be ignored.           */
/* Sept. 23, 1991                                               */

/*   USAGE: ristosds infile{ infile} -o outfile                 */
#if defined __MWERKS__
#include <console.h>
#endif

#include "hdf.h"

#define r_imgae 2;
#define r_sds   3;
#define USAGE "ristosds infile{ infile} -o outfile"

int         main
            (int, char *a[]);
int         cntimage
            (char *filename, int32 *p_w, int32 *p_h, int *n_images);
VOID        finishing
            (void);

int
main(int argc, char *argv[])
{
    int         i, j;
    int         nimg, nimg0;    /* nimg, nimg0 -- number of images        */
    int32       w, h;           /* w, h -- width and height of images */
    int         ret, n_infile, getoutfile, ispal;
    int32       dimsizes[3];
    char       *infile, *outfile, **argv_infile;
    uint8      *indata, *indata0, palette[768];
    uint8      *outdata, *outdata0;

#if defined __MWERKS__
    argc = ccommand(&argv);
#endif

    if (argc < 4)
      {
          printf("Usage %s.\n", USAGE);
          finishing();
      }
    /* initialization   */

    nimg = 0;
    w = h = 0;
    n_infile = 0;   /* count number of input files   */
    getoutfile = 0;
    argv_infile = &argv[1];     /* save start address of input file names. */

    /* Count total number of images to be converted.   */

    while (--argc > 1)
      {     /* reach -o. go to get output file name */
          infile = *++argv;
          if (infile[0] == '-')
            {
                getoutfile = 1;
                break;
            }
          else
            {
                n_infile++;
                ret = cntimage(infile, &w, &h, &nimg);
                if (ret != 0)
                    finishing();
            }
      }

    nimg0 = nimg;
    printf("Total number of images: %d.\n", nimg);

    /* get output file name     */

    argc--;

    if (getoutfile == 0 || argc < 1)
      {     /* is -o or outfile missing?    */
          printf("Bad command line. \n\n\t\t %s\n", USAGE);
          finishing();
      }

    outfile = *++argv;

    /* read in images from all input files.     */

    outdata0 = outdata = (uint8 *) HDmalloc((size_t)(nimg * w * h) * (sizeof(uint8)));
    indata0 = indata = (uint8 *) HDmalloc((size_t)(w * h) * sizeof(char));
    infile = *argv_infile;
    ret = DFR8getdims(infile, &w, &h, &ispal);
    if (ispal)
      {
          DFPgetpal(infile, (char *) palette);
          DFR8restart();    /* in case the palette is not at the  */
          /* beginning of the first input file  */
          DFPputpal(outfile, (char *) palette, 0, "a");
      }
    while (nimg > 0)
      {
          indata = indata0;     /* Restart from the beginning of the buf */
          ret = DFR8getimage(infile, indata, w, h, palette);
          if (ret != 0)
            {   /* end of file. open next one   */
                n_infile--;
                if (n_infile < 1)
                  {
                      printf("Inconsistent number of files and images\n");
                      finishing();
                  }
                infile = *++argv_infile;
                ret = DFR8getimage(infile, indata, w, h, palette);
                if (ret != 0)
                    finishing();
            }

          /* convert image data into floating point and store in the array  */

          for (i = 0; i < w; i++)
              for (j = 0; j < h; j++)
                  *outdata++ = *indata++;
          nimg--;
      }

    dimsizes[0] = nimg0;
    dimsizes[1] = h;
    dimsizes[2] = w;
    if (DFSDsetNT(DFNT_UINT8) == FAIL)
        finishing();
    ret = DFSDadddata(outfile, 3, dimsizes, (VOIDP) outdata0);
    if (ret != 0)
        finishing();
    return (0);
}

VOID
finishing(void)
{
    printf("end of ristosds.\n");
    exit(1);
}

/* count # of images  */

int
cntimage(char *filename, int32 *p_w, int32 *p_h, int *n_images)

{
    int32       ret, width, height;
    int         ispal, dimerror;

    dimerror = 0;

    ret = DFR8getdims(filename, &width, &height, &ispal);
    if (ret == FAIL)
      {
          HEprint(stderr, 0);
          finishing();
      }
    if (*p_w == 0 && *p_h == 0)
      {     /* the first time  */
          *p_w = width;
          *p_h = height;
      }
    while (ret == 0)
      {     /* count # of images and check dims    */
          if ((width != *p_w) || (height != *p_h))
            {
                printf("Inconsistent dims: %s .\n", filename);
                dimerror = -1;
                break;
            }
          *n_images = *n_images + 1;
          ret = DFR8getdims(filename, &width, &height, &ispal);
      }

    /* ready to return   */

    DFR8restart();
    if (dimerror == -1)
        return (-1);
    else
        return (0);
}
