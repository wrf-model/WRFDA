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
static char RcsId[] = "@(#)$Revision: 1.18 $";
#endif

/* $Id: hdf8to24.c,v 1.18 1998/12/08 21:37:55 koziol Exp $ */

#if defined __MWERKS__
#include <console.h>
#endif

#include "hdf.h"
#ifndef I860
#include <stdlib.h>
#include <string.h>
#endif /* I860 */

/* Exception checking macro */
#define EXCHECK(a,b)    if(a) goto b

/* Static variables */
uint8       red_comp[256], green_comp[256], blue_comp[256];

comp_info   cinfo;              /* compression structure */

static intn
magnify(uint8 *from_buffer, uint8 *to_buffer, int32 from_x0,
        int32 from_y0, int32 from_x1, int32 from_y1, int32 from_width,
        int32 from_height, int32 to_width, int32 to_height);

static intn
convert8to24(uint8 *img8_buf, uint8 *img24_buf, int32 img_xdim,
             int32 img_ydim);

static VOID usage(void);

/**********************************************************************
*  Function :   magnify
*  Purpose  :   Magnify an image by independant X and Y magnification
*                   factors.  Note that the coordinates which specify
*                   the area in the "from" image are _inclusive_, i.e.
*                   if you would like to magnify the entire image, the
*                   "from" coordinates should be specified as:
*                   (0,0) and ((width-1),(height-1)), where "width"
*                   and "height" are the width and height of the image
*                   (respectively) in pixels.
*  Parameters   :
*           from_buffer - pointer to the buffer which contains the image
*                           to magnify
*           to_buffer - pointer to the buffer in which to place the
*                           magnified image
*           from_x0, from_y0 - Upper Left Corner (ULC) of the rectangular
*                           area in the image to magnify
*           from_x1, from_y1 - Lower Right Corner (LRC) of the rectangular
*                           area in the image to magnify
*           from_width, from_height - the width and height of the image
*                           to magnify (or "from" image)
*           to_width, to_height - the width and height of the magnified
*                           image (or "to" image)
*  Returns  :   (TRUE) for success, (FALSE) for error
*  Calls    :
*  Called by    :
**********************************************************************/
static intn
magnify(uint8 *from_buffer, uint8 *to_buffer, int32 from_x0,
        int32 from_y0, int32 from_x1, int32 from_y1, int32 from_width,
        int32 from_height, int32 to_width, int32 to_height)
{
    uint8      *buf_off,        /* the current offset into the magnified data */
               *last_buf,       /* pointer to the last useful magnified line */
               *y_off,          /* the y offset into the data block */
               *last_y_coor,    /* pointer to the last line copied */
              **y_coor;         /* pointers to image data */
    float64     temp_val,       /* temporary value for holding double results */
                wind_width,     /* the width of the window to magnify */
                wind_height;    /* the height of the window to magnify */
    int32       u, v;           /* local unsigned counting variables */
    int32      *x_coor,         /* the X coor. lookup table */
               *x_coor_temp;    /* temporary pointer to the x lookup table */

    if (from_width == 0 || from_height == 0)    /* check for bad image dimensions */
        return (FALSE);
    if (to_width == 0 || to_height == 0)    /* check for bad image dimensions */
        return (FALSE);
    if (from_x0 > from_x1 || from_y0 > from_y1)     /* check for an invalid window */
        return (FALSE);

/* find width and height of the window to magnify */
    wind_width = (float64)((from_x1 - from_x0) + 1);
    wind_height = (float64)((from_y1 - from_y0) + 1);

/* allocate room for the x coordinate lookup table */
    x_coor = (int32 *) HDmalloc((int32) ((size_t)to_width * sizeof(int32)));
    EXCHECK(x_coor == NULL, XCoorFailed);   /* check if malloc() failed */
    temp_val = wind_width / (float64) to_width;
    for (u = 0; u < to_width; u++)  /* calculate the x coordinate lookup table */
        x_coor[u] = ((uint16) ((float64) u * temp_val) + from_x0);

/* allocate room for the array of pointers */
    y_coor = (uint8 **) HDmalloc((int32) ((size_t)to_height * sizeof(uint8 *)));
    EXCHECK(y_coor == NULL, YCoorFailed);   /* check if malloc() failed */
    temp_val = wind_height / (float64) to_height;
    for (u = 0; u < to_height; u++)     /* calculate the y coordinates */
        y_coor[u] = from_buffer + ((uint32) ((float64) u * temp_val) + (uint32)from_y0) * (uint32)from_width;

    last_buf = to_buffer;   /* set the previous line pointer */
    buf_off = to_buffer;    /* set the pointer to the "to" image */
    last_y_coor = NULL;     /* force to calculate the first line */

    for (u = 0; u < to_height; u++)
      {     /* go through each magnified line */
/* if this line is not the same as the previous one, then make it again */
          if (y_coor[u] != last_y_coor)
            {
                last_y_coor = y_off = y_coor[u];
                x_coor_temp = x_coor;   /* assign the temporary pointer */
                last_buf = buf_off;     /* set the pointer to the previous line */
                for (v = 0; v < to_width; v++)  /* go through each line magnifying it */
                    *buf_off++ = y_off[*x_coor_temp++];
            }   /* end if */
/* this line is the same as the previous one, just copy it */
          else
            {
                HDmemcpy(buf_off, last_buf, to_width);  /* copy the previous line */
                buf_off += to_width;    /* advance the buffer offset pointer */
            }   /* end else */
      }     /* end for */
    HDfree((char *) y_coor);
    HDfree((char *) x_coor);
    return (TRUE);

  YCoorFailed:      /* Failed to allocate memory for the Y coor. lookup table */
    HDfree((VOIDP) x_coor);
  XCoorFailed:      /* Failed to allocate memory for the X coor. lookup table */
    return (FALSE);
}   /* end magnify() */

/**********************************************************************
*  Function :   convert8to24
*  Purpose  :   Convert an 8-bit image to a 24-bit image, using the
*                   palette components already set up.
*  Parameters   :
*           img_8_buf - pointer to the buffer which contains the 8-bit image
*           img24_buf - pointer to the buffer in which to place the
*                           24-bit image
*           img_xdim, img_ydim - the width and height of the images
*  Returns  :   (TRUE) for success, (FALSE) for error
*  Calls    :
*  Called by    :
**********************************************************************/
static intn
convert8to24(uint8 *img8_buf, uint8 *img24_buf, int32 img_xdim,
             int32 img_ydim)
{
    uint32      pixels;         /* local counting variable */

    if (img_xdim <= 0 || img_ydim <= 0)     /* check for bad image dimensions */
        return (FALSE);
    if (img8_buf == NULL || img24_buf == NULL)  /* check for invalid images */
        return (FALSE);

    pixels = (uint32)(img_xdim * img_ydim);   /* get the number of pixels to process */
    while (pixels > 0)
      {     /* do all the pixels */
          *img24_buf++ = red_comp[*img8_buf];
          *img24_buf++ = green_comp[*img8_buf];
          *img24_buf++ = blue_comp[*img8_buf];
          img8_buf++;
          pixels--;
      }     /* end while */
    return (TRUE);
}   /* end convert8to24() */

static VOID
usage(void)
{
    printf("USAGE: make24 [-s<scale>] [-j] <input HDF file> <output HDF file>\n");
    printf("    -s<scale> : set scale for magnifying the 8-bit input file.\n");
    printf("                scales between 0 and 1 shrink the image, scales\n");
    printf("                greater than 1 expand the image.  Defaults to 1\n");
    printf("    -j[quality] : use JPEG compression to store the 24-bit image\n");
    printf("                Defaults to no compression, or quality level\n");
    printf("                75, if no quality is specified\n");
    printf("    <input HDF file>  : HDF file which contains an 8-bit image\n");
    printf("    <output HDF file> : HDF file to store the 24-bit image\n");
    exit(1);
}   /* end usage() */

int
main(int argc, char *argv[])
{
    intn        do_jpeg = FALSE;    /* flag to indicate JPEG compression */
    intn        jpeg_qual = 75; /* JPEG quality factor */
    intn        do_scale = FALSE;   /* flag to indicate whether to scale images */
    float32     img_scale = (float32) 1.0;  /* scaling factor */
    int32       xdim, ydim;     /* dimensions of the image to convert */
    intn        ispal;          /* whether there's a palette with the image */
    uint8      *img_buf;        /* buffer to store the image in */
    uint8      *img24_buf;      /* buffer to store the 24-bit image in */
    uint8      *pal_buf = NULL; /* buffer to store the palette in */
    intn        file = 1;       /* the arguement the files start at */
    intn        i;              /* local counting variable */

#if defined __MWERKS__
    argc = ccommand(&argv);
#endif

    if (argc < 3)
        usage();

    if (argv[1][0] == '-' || argv[1][0] == '/')
      {     /* check command line */
          if ((argv[1][1] != 's' && argv[1][1] != 'j') || argc < 4)
              usage();

          while (argv[file][0] == '-' || argv[file][0] == '/')
            {
                switch (argv[file][1])
                  {
                      case 's':
                          if ((img_scale = (float32) atof(&argv[file][2])) <= (float32)0.0)
                            {   /* check for valid scale */
                                printf("Bad scale, must be greater than 0\n");
                                return (1);
                            }   /* end if */
                          do_scale = TRUE;
                          break;

                      case 'j':
                          if ((jpeg_qual = atoi(&argv[file][2])) <= 0 || jpeg_qual > 100)
                            {
                                printf("Bad JPEG quality setting, should be between\n");
                                printf("1 and 100, using default value of 75\n");
                                jpeg_qual = 75;
                            }   /* end if */
                          do_jpeg = TRUE;
                          break;

                      default:
                          usage();

                  }     /* end switch */
                file++;
            }   /* end while */
      }     /* end if */

    /* get the image dimensions */
    if (DFR8getdims(argv[file], &xdim, &ydim, &ispal) == FAIL)
      {
          printf("Error, bad dimensions in file: %s\n", argv[file]);
          HEprint(stdout, 0);
          return (1);
      }     /* end if */

    if ((img_buf = (uint8 *) HDmalloc((size_t)(xdim * ydim))) == NULL)
      {
          printf("Error, cannot allocate space for %dx%d image\n", (int)xdim, (int)ydim);
          return (1);
      }     /* end if */

    if (ispal)
      {
          if ((pal_buf = (uint8 *) HDmalloc(768)) == NULL)
            {
                printf("Error, cannot allocate space for image palette\n");
                return (1);
            }   /* end if */
      }     /* end if */
    else
        printf("No palette associated with image, using default grey scale converion\n");

    if (DFR8getimage(argv[file], img_buf, xdim, ydim, (ispal ? pal_buf : NULL)) == FAIL)
      {
          printf("Error reading image\n");
          HEprint(stdout, 0);
          return (1);
      }     /* end if */

    if (do_scale)
      {     /* check whether we should scale the image */
          uint8      *scaled_image;     /* storage for the scaled image */
          int32       new_xdim, new_ydim;   /* the new image's x and y dim. */

          new_xdim = (int32) (img_scale * (float32)xdim);    /* calc. new image's dimensions */
          new_ydim = (int32) (img_scale * (float32)ydim);
          if ((scaled_image = (uint8 *) HDmalloc((size_t)(new_xdim * new_ydim))) == NULL)
            {
                printf("Error, cannot allocate space for %dx%d scaled image\n", (int)new_xdim, (int)new_ydim);
                return (1);
            }   /* end if */
          if (!magnify(img_buf, scaled_image, 0, 0, xdim - 1, ydim - 1, xdim, ydim, new_xdim, new_ydim))
            {
                printf("Error scaling image, out of memory or bad dimensions\n");
                return (1);
            }   /* end if */
          HDfree((VOIDP) img_buf);     /* free the old image */

          img_buf = scaled_image;   /* use the new image for further processing */
          xdim = new_xdim;
          ydim = new_ydim;
      }     /* end if */

    /* Generate the RGB components for the 8 -> 24 bit converter */
    if (ispal)
      {
          uint8      *pal_ptr = pal_buf;    /* temporary pointer into the palette */

          for (i = 0; i < 256; i++)
            {
                red_comp[i] = *pal_ptr++;
                green_comp[i] = *pal_ptr++;
                blue_comp[i] = *pal_ptr++;
            }   /* end for */
      }     /* end if */
    else
      {     /* no palette, use a greyscale palette */
          for (i = 0; i < 256; i++)
              red_comp[i] = green_comp[i] = blue_comp[i] = (uint8) i;
      }     /* end else */

    /* allocate space for the 24-bit image */
    if ((img24_buf = (uint8 *) HDmalloc((size_t)(xdim * ydim * 3))) == NULL)
      {
          printf("Error, cannot allocate space for %dx%d 24-bit image\n", (int)xdim, (int)ydim);
          return (1);
      }     /* end if */

    /* convert the image */
    if (!convert8to24(img_buf, img24_buf, xdim, ydim))
      {
          printf("Error converting 8-bit image to 24-bit image\n");
          return (1);
      }     /* end if */

    if (do_jpeg)
      {     /* set up JPEG compression if necessary */
          cinfo.jpeg.quality = jpeg_qual;   /* set JPEG comp. parameters */
          cinfo.jpeg.force_baseline = TRUE;
          DF24setcompress(COMP_JPEG, &cinfo);   /* set compression parameters */
      }     /* end if */

    /* store 24-bit image */
    if (DF24putimage(argv[file + 1], (VOIDP) img24_buf, xdim, ydim) == FAIL)
      {
          printf("Error storing 24-bit image\n");
          HEprint(stdout, 0);
          return (1);
      }     /* end if */

    return (0);
}   /* end make24 */
