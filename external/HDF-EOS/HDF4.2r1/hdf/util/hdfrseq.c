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

/* $Id: hdfrseq.c,v 1.14 1996/04/09 20:09:38 georgev Exp $ */

/*    hdfrseq
   *  Sequencer for NCSA Hierarchical Data Format files
   *
   *  Display images on a remote ICR terminal
   *  in sequence, suitable for onscreen animation.  Can expand the image size
   *  on the fly.
   *
   *  This version now supports displaying an image in a window on a Silicon
   *  Graphics Iris 4D workstation.  There are bugs in this code (possibly due
   *  to evolving system software combined with old code).
   *
   *  National Center for Supercomputing Applications
   *  University of Illinois, Urbana-Champaign
   *
   *  by Tim Krauskopf
   *  first version of seq 7/11/86
   *  modifications        2/20/87
   *  re-write             6/21/88
   *  Iris 4D support      6/24/89  Mike Krogh
   *  Line padding              9/25/89  Chin-Chau Low
   *  In-between versions re-written by Gaige B. Paulsen and Swami Natarajan
   *  ANSI-fication & relegation to remote-only use
   *                    6/9/92   Doug Ilg
   *
   *  This program is in the public domain
   *
 */
#if defined __MWERKS__
#include <console.h>
#endif

#include "hdf.h"    /* HDF includes */

#ifdef IRIS4
#include <gl.h>
#include <device.h>
#endif

#ifndef IRIS4
#define  SCRX    1152
#define  SCRY    900
#endif

#ifdef IRIS4
#define  SCRX    1280
#define  SCRY    1024
#endif

#define  LUSAGE   printf("\nUsage: %s [-s] [-l] [-e exp] [-p xwhere ywhere] file1 file2 . . .\n",argv[0])
#define  RUSAGE   printf("\nUsage: %s [-s] [-l] [-e exp] file1 file2 . . .\n",argv[0])
#define USAGE   if (remote) { RUSAGE; } else { LUSAGE; }

int
            oldcf = 0,          /* old value of compression flag */
            oldx = 0, oldy = 0, /* old values of xdim and ydim */
            coldx = 0, coldy = 0,   /* old values of xdim and ydim for CI8s */
            xwhere = 0, ywhere = 0,     /* where to put it on the screen */
            step = 0,           /* single step? */
            remote = 0,         /* should use ICR for remote display */
            large = 0,          /* should make images as large as possible */
            center = 1,         /* should center the images */
            startpic = 1,       /* for parameter counts */
            oldxs = 0, oldys = 0,   /* old sizes */
            xsize = 0, ysize = 0,   /* what final size on screen, after blow-up */
            xfact = 1, yfact = 1;   /* what factor for blowing up the picture */

int32       xdim = 0, ydim = 0; /* size of image on disk */

#ifdef NEEDPAD
int         xpad;
#endif

#ifdef IRIS4
int         GID;
unsigned short *img = NULL;
int         WINDOW_OPEN = 0;
int16       idev, qvalue;
int         attached = 1;
#endif

char
            rgb[768],           /* storage for a palette */
           *wherebig = NULL,    /* where to store small image */
           *wheresmall = NULL;  /* where to store image-related stuff */

int         main(int, char *a[]);
int         getspace(void);
int         getpix(void);
int         largeset(void);
int         showpic(char *);
int         piximage(int);
int         rimage(int);
int         bigimg(unsigned char *, unsigned char *);
int         rleit(char *, char *, int);

/*************************************************************************/
/*  hdfseq
   *   Load the images from the specified files.
   *   Create the internal storage if needed.
   *
   *   Search pattern for files:
   *       RIG images in file
   *       RI8 and CI8 images in file
 */
int
main(int argc, char *argv[])
{
    int         i, filearg;

#if defined __MWERKS__
    argc = ccommand(&argv);
#endif

/*
   *  Check to see if we are displaying on a local console or on a
   *  remote ICR.  This program does double duty.
 */
    i = HDstrlen(argv[0]);
    if (HDstrncmp("hdfseq", argv[0] + i - 6, 6))
        remote = 1;

/*
   *  Are there enough parameters?  Give user information on calling.
 */
    if (argc < 2)
      {
          printf("%s,  version: 1.0   date: December 1, 1988\n", argv[0]);
          if (remote)
            {
                printf(" This utility displays sequences of raster-8 images\n");
                printf(" from HDF files on a remote device using NCSA's\n");
                printf(" Interactive Color Raster protocol.\n\n");
            }
          if (!remote)
            {
#ifdef SUN
                printf(" This utility displays sequences of raster-8 images\n");
                printf(" from HDF files on the Sun console running Sunview.\n");
#endif
#ifdef IRIS4
                printf(" This utility displays sequences of raster-8 images\n");
                printf(" from HDF files on the Iris console running 4Sight.\n");
#endif
                printf("\n");
            }
          USAGE;
          puts(" -l              make image as large as possible");
          puts(" -e expansion    expand image by a certain factor");
          if (!remote)
              puts(" -p xloc yloc    position on screen");
          puts(" -s              step through images");
          exit(1);
      }

/*
   *  work on parms
 */
    for (i = 1; i < argc; i++)
      {     /* look at each parm */
          if (*argv[i] == '-')
              switch (*(argv[i] + 1))
                {
                    case 'p':   /* special position on screen */
                        center = 0;     /* don't center */
                        xwhere = atoi(argv[++i]);
                        ywhere = atoi(argv[++i]);
                        if (xwhere > SCRX || ywhere > SCRY)
                          {
                              puts("\n Invalid position ");
                              USAGE;
                              exit(1);
                          }
                        startpic += 3;
                        break;
                    case 'e':
                        xfact = yfact = atoi(argv[++i]);
                        if (xfact < 1)
                          {
                              xfact = 1;
                              yfact = 1;
                          }
                        startpic += 2;
                        break;
                    case 'l':   /* large pic */
                        large = 1;
                        startpic++;
                        break;
                    case 's':   /* step through pics */
                        step = 1;
                        startpic++;
                        break;
                    default:
                        USAGE;
                        exit(1);
                }
      }

/*
   *  Process each of the files in the command string.
   *
 */
    for (filearg = startpic; filearg < argc; filearg++)
        showpic(argv[filearg]);

#ifdef IRIS4
    if ((!remote) && (img))     /* make sure we have an image */
        while (1)
          {
              idev = qread(&qvalue);
              if (idev == REDRAW)
                {
                    reshapeviewport();
                    if (img != NULL)
                      {
                          rectwrite(0, 0, (xdim - 1), (ydim - 1), img);
                          swapbuffers();
                      }
                }
          }
#endif

    exit(0);
}

/*************************************************************************/
/*  getspace
   *  Provide enough space in the space pointers for the operations
   *  to all work.
 */
int
getspace(void)
{

/*
   *  Don't allocate anything if the image is the same size as before.
 */
    if (oldx != xdim || oldy != ydim)
      {
          oldx = xdim;
          oldy = ydim;

          if (wheresmall)
              HDfree(wheresmall);

          if (NULL == (wheresmall = (char *) HDmalloc(xdim * ydim)))
            {
                puts(" Cannot allocate memory, fatal error");
                exit(1);
            }

      }
    return (0);
}

#ifdef IRIS4
/*
   *  Allocate the space for the image if displaying locally
 */
getpix(void)
{
/*
   *  Allocation will take place because xsize and ysize will
   *  be set before calling this routine.
   *
 */
    if (!remote && (oldxs != xsize || oldys != ysize))
      {
          oldxs = xsize;
          oldys = ysize;
          if (img)
              HDfree(img);
          if ((img = (unsigned short *) HDmalloc(xdim * ydim * sizeof(short))) == NULL)
            {
                puts(" Cannot allocate memory, fatal error");
                exit(1);
            }

      }
    return (0);
}
#endif

/*************************************************************************/
/*  largeset
   *  Set up the xfact, yfact, xsize and ysize for expanding the image
   *  locally.
   *
 */
int
largeset(void)
{

    if (large)
      {
          xfact = SCRX / xdim;  /* how much blow-up can we do? */
          yfact = SCRY / ydim;  /* calculate expansion factor  */
          if (xfact > yfact)
              xfact = yfact;
          else
              yfact = xfact;
      }

    xsize = xfact * xdim;   /* re-calculate actual pixel dimensions */
    ysize = yfact * ydim;

#ifdef NEEDPAD  /* add padding for byte boundary */
    xpad = BYTEBOUND - xsize % BYTEBOUND;
    if (xpad == BYTEBOUND)
        xpad = 0;
    else
        xsize += xpad;
#ifdef DEBUG
    printf("xpad %d\n", xpad);
#endif /*DEBUG */
    return (xfact > 1 || yfact > 1 || xpad > 0);    /* is expansion necessary? */
#endif /*NEEDPAD */

#ifndef NEEDPAD     /* make sure there is only 1 return stmt */
    return (xfact > 1 || yfact > 1);    /* is expansion necessary? */
#endif /*NEEDPAD */
}

/*************************************************************************/
/*  showpic
   *  For each file, search for the components that we can display.
   *  Display them according to the remote flag on or off.
   *
 */
int
showpic(char *filename)
{
    int         ispal, r8_exists;

    oldx = xdim;
    oldy = ydim;    /* save old values */

    if (-1 == Hishdf(filename))
      {
          printf("\'%s\' is not an HDF Format Data File.\n", filename);
          return (0);
      }
/*
   *  Search for all RIGs in this file.
   *  Process each RIG and display.
 */
    r8_exists = FALSE;
    while (1)
      {
          if (DFR8getdims(filename, &xdim, &ydim, &ispal) < 0)
            {
                if (!r8_exists)
                    printf("There are no 8-bit images in the file %s\n", filename);
                break;  /* all RIGs processed */
            }
          r8_exists = TRUE;     /* at least one 8-bit image found */

#ifdef DEBUG
          printf("xdim %d ydim %d\n", xdim, ydim);
#endif /*DEBUG */
#ifdef IRIS4
          if (!remote)
            {
                largeset();     /* set expansion needs */
                getspace();     /* get local space for pre-expansion */
                getpix();   /* allocate memory */
            }
#endif

          if (remote)
              getspace();   /* get space for image in mem */

/*
   *  Try to successfully load the palette and image from the file
 */
          if (!DFR8getimage(filename, (uint8 *) wheresmall, xdim, ydim, (uint8 *) rgb))
            {
                if (remote)
                    rimage(ispal);  /* display remote image with [palette] */
#ifdef IRIS4
                else
                    piximage(ispal);    /* display image on Iris with [palette] */
#endif
            }
          else
              puts(" Error loading image");

      }

    return (0);

}

#ifdef IRIS4
/***********************************************************************/
/*  piximage
   *  On the Iris console, display the image as the parameters specify.
   *  Handles centering (center)
   *  Uses xwhere and ywhere, xsize and ysize.
   *  Performs expansion if xfact or yfact > 1
   *  Takes the palette from the rgb[] if asked to.
   *  Will pause if step=1.
 */
piximage(int usepal)
{
    char       *pp;
    int         r, g, b;
    int         j, j1, j2, k;

/*
   *  compute centering values, based on new size
 */
    if (center)
      {
          xwhere = (SCRX - xsize) / 2;
          ywhere = (SCRY - ysize) / 2;
          if (xwhere < 0)
              xwhere = 0;
          if (ywhere < 0)
              ywhere = 0;
      }

    if (!WINDOW_OPEN)
      {
          WINDOW_OPEN = 1;
          if (step)
            {
                printf("Press <enter> to step through images or");
                printf(" 'Q' to quit.\n");
            }
          prefposition(xwhere, (xwhere + xsize), ywhere, (ywhere + ysize));
          GID = winopen("hdfseq");  /* open the window */
          shademodel(FLAT);     /* don't worry about shading */
          doublebuffer();
          multimap();
          gconfig();
          setmap(4);
          color(BLACK);
          clear();
          qdevice(REDRAW);
          if (step)
            {
                qdevice(QKEY);
                qdevice(RETKEY);
                qdevice(INPUTCHANGE);
            }
      }

/*
   * Process window movement events.
 */
    while (qtest())
      {
          idev = qread(&qvalue);
          if (idev == REDRAW)
            {
                reshapeviewport();
                if (img != NULL)
                  {
                      rectwrite(0, 0, (xdim - 1), (ydim - 1), img);
                      swapbuffers();
                  }
            }
          if (idev == INPUTCHANGE)
              attached = qvalue;
      }

/*
   *  Do the image expansion, if called for.
 */
    if (xfact > 1 || yfact > 1)
        rectzoom((float32) xfact, (float) yfact);   /* let the iris scale it */

/*
   *  Set the display palette to the new palette.
 */

    if (usepal)
      {
          pp = rgb;
          for (j = 0; j < 256; j++)
            {
                r = (int) (*pp++);
                g = (int) (*pp++);
                b = (int) (*pp++);
                mapcolor(j, r, g, b);   /* change the system palette */
            }
      }

/*
   * Convert image data to iris pixel format
   * flip it upside down and convert to short integers.
 */

    for (j = 0; j < ydim; j++)
      {
          j1 = (ydim - j - 1) * xdim;
          j2 = j * xdim;
          for (k = 0; k < xdim; k++)
              *(img + j1 + k) = (short) (*(wheresmall + j2 + k));
      }

    rectwrite(0, 0, (xdim - 1), (ydim - 1), img);
    swapbuffers();

    if (step)
      {
          while (1)
            {
                idev = qread(&qvalue);
                if (idev == REDRAW)
                  {
                      reshapeviewport();
                      if (img != NULL)
                        {
                            rectwrite(0, 0, (xdim - 1), (ydim - 1), img);
                            swapbuffers();
                        }
                  }
                if (idev == QKEY)
                    exit(0);
                if (idev == RETKEY)
                    break;
                if (idev == INPUTCHANGE)
                    attached = qvalue;
            }
      }

}
#endif

/*****************************************************************************/
/*  rimage
   *  Remote display of the image using the ICR.
   *  Just print the codes to stdout using the protocol.
 */
int
rimage(int usepal)
{
    int         i, j, newxsize;
    char       *space, *thisline, *thischar;
    unsigned char c;

/*
   *  Open the window with the W command
 */

    (void) printf("\033^W;%d;%d;%ld;%ld;0;rseq^", xwhere, ywhere, (long) (xdim * xfact), (long) (ydim * yfact));

/*
   *  If a palette should be used, send it with the M command.
 */
    if (usepal)
      {
          (void) printf("\033^M;0;256;768;rseq^");  /* start map */

          thischar = rgb;
          for (j = 0; j < 768; j++)
            {
                c = *thischar++;
                if (c > 31 && c < 123)
                  {
                      putchar(c);
                  }
                else
                  {
                      putchar((c >> 6) + 123);
                      putchar((c & 0x3f) + 32);
                  }
            }
      }

/*
   *  Send the data for the image with RLE encoding for efficiency.
   *  Encode each line and send it.
 */
    space = (char *) HDmalloc(ydim + 100);
    thisline = wheresmall;

    for (i = 0; i < ydim; i++)
      {
          newxsize = rleit(thisline, space, xdim);
          thisline += xdim;     /* increment to next line */

          (void) printf("\033^R;0;%d;%d;%d;rseq^", i * xfact, xfact, newxsize);

          thischar = space;
          for (j = 0; j < newxsize; j++)
            {

/***********************************************************************/
/*  Encoding of bytes:
   *
   *  123 precedes #'s 0-63
   *  124 precedes #'s 64-127
   *  125 precedes #'s 128-191
   *  126 precedes #'s 192-255
   *  overall:  realchar = (specialchar - 123)*64 + (char-32)
   *            specialchar = r div 64 + 123
   *            char = r mod 64 + 32
 */
/***********************************************************************/

                c = *thischar++;    /* get byte to send */

                if (c > 31 && c < 123)
                  {
                      putchar(c);
                  }
                else
                  {
                      putchar((c >> 6) + 123);
                      putchar((c & 0x3f) + 32);
                  }
            }
      }

/*
   *  pause for the user
 */
    if (step)
      {
          printf("Press return to continue, 'q' return to quit");
          if ('q' == getchar())
              exit(0);
      }

    HDfree(space);
    return (0);
}

/********************************************************************/
/*  rleit
   *  compress the data to go out with a simple run-length encoded scheme.
   *
 */
int
rleit(char *buf, char *bufto, int len)
{
    char *p, *q, *cfoll, *clead;
    char       *begp;
    int         i;

    p = buf;
    cfoll = bufto;  /* place to copy to */
    clead = cfoll + 1;

    begp = p;
    while (len > 0)
      {     /* encode stuff until gone */

          q = p + 1;
          i = len - 1;
          while (*p == *q && i + 120 > len && i)
            {
                q++;
                i--;
            }

          if (q > p + 2)
            {   /* three in a row */
                if (p > begp)
                  {
                      *cfoll = p - begp;
                      cfoll = clead;
                  }
                *cfoll++ = 128 | (q - p);   /* len of seq */
                *cfoll++ = *p;  /* char of seq */
                len -= q - p;   /* subtract len of seq */
                p = q;
                clead = cfoll + 1;
                begp = p;
            }
          else
            {
                *clead++ = *p++;    /* copy one char */
                len--;
                if (p > begp + 120)
                  {
                      *cfoll = p - begp;
                      cfoll = clead++;
                      begp = p;
                  }
            }

      }
/*
   *  fill in last bytecount
 */
    if (p > begp)
        *cfoll = (p - begp);
    else
        clead--;    /* don't need count position */

    return ((int) (clead - bufto));     /* how many stored as encoded */
}
