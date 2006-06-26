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
static char RcsId[] = "@(#)$Revision: 1.19 $";
#endif

/* $Id: he_disp.c,v 1.19 1996/11/11 20:40:22 koziol Exp $ */

/* display.c -- contains code for displaying an image using ICR
 * this code is plucked from hdfrseq.c
 */
#include "he.h"

#define SCRX 1152
#define SCRY 900

/* HEdisplay -- stub function for displaying an image using ICR */
int
HEdisplay(HE_CMD * cmd)
{
#ifndef IBM6000
    int i;
    int         center = 1;
    int         xwhere = 0;
    int         ywhere = 0;
    int         factor = 1;
    int         large = 0;

    for (i = 1; i < cmd->argc; i++)
        if (cmd->argv[i][0] == '-')
            switch (findOpt(cmd->argv[i] + 1))
              {
                  case HE_HELP:
                      printf("display [-position <xpos> <ypos>] [-expansion <exp>] [-large]\n");
                      printf("\t-position\tImage position on console screen\n");
                      printf("\t-expansion\tImage expansion factor\n");
                      printf("\t-large\t\tMake image as large as posible\n");
                      return HE_OK;
                  case HE_POSITION:
                      center = 0;
                      xwhere = atoi(cmd->argv[++i]);
                      ywhere = atoi(cmd->argv[++i]);
                      if ((xwhere < 0) || (xwhere > SCRX) ||
                          (ywhere < 0) || (ywhere > SCRY))
                        {
                            fprintf(stderr, "Invalid position.\n");
                            return HE_FAIL;
                        }
                      break;

                  case HE_EXPANSION:
                      factor = atoi(cmd->argv[++i]);
                      if (factor < 1)
                          factor = 1;
                      break;

                  case HE_LARGE:
                      large = 1;
                      break;
                  case HE_NOTFOUND:
                      unkOpt(cmd->argv[i]);
                      return HE_FAIL;
                  case HE_AMBIG:
                      ambigOpt(cmd->argv[i]);
                      return HE_FAIL;
                  default:
                      irrOpt(cmd->argv[i]);
                      return HE_FAIL;
              }
        else
          {
              unkArg(cmd->argv[i]);
              return HE_FAIL;
          }
    return display(center, xwhere, ywhere, factor, large);
#else
    printf("Display routines do not work on this platform.\n");
    return 1;
#endif
}

void
goTo(int desc)
{
    /* goto element of he_desc[desc] */
    /* ask swami */

    /* right now this only works for r8 */
    DFR8readref(he_file, he_desc[desc].ref);
}

#ifndef IBM6000     /* Skip it all */

int         oldcf = 0;          /* old value of compression flag */
int32       oldx = 0, oldy = 0; /* old values of xdim and ydim */
int         coldx = 0, coldy = 0;   /* old values of xdim and ydim for CI8s */
int32       xdim = 0, ydim = 0; /* size of image on disk */
int         xwhere, ywhere;     /* where to put it on the screen */
int         ispal;
int         large;              /* should make images as large as possible */
int         center;             /* should center the images */
int         oldxs = 0, oldys = 0;   /* old sizes */
int         xsize = 0, ysize = 0;   /* what final size on screen, after blow-up */
int         factor;

unsigned char rgb[768];         /* storage for a palette */
char       *wherebig = NULL;    /* where to store small image */
uint8      *wheresmall = NULL;  /* where to store image-related stuff */

int
getSpace(void)
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

          if (NULL == (wheresmall = (uint8 *) HDmalloc((size_t)(xdim * ydim))))
            {
                printf(" Cannot allocate memory, fatal error\n");
                exit(1);
            }

      }
    return (0);
}

/*************************************************************************/
/*  largeset
 *  Set up the xfact, yfact, xsize and ysize for expanding the image
 *  locally.
 *
 */
int
largeSet(void)
{
    int
                tmp;

    if (large)
      {
          factor = (int) (SCRX / xdim);     /* how much blow-up can we do? */
          /* calculate expansion factor  */
          tmp = (int) (SCRY / ydim);

          /* take minimum expansion factor */
          if (factor > tmp)
              factor = tmp;
      }

    xsize = (int) (factor * xdim);  /* re-calculate actual pixel dimensions */
    ysize = (int) (factor * ydim);

    return (factor > 1);    /* is expansion necessary? */
}

int
display(int c, int x, int y, int f, int l)
{

    center = c;
    xwhere = x;
    ywhere = y;
    factor = f;
    large = l;

    if (!isRig(he_desc[he_currDesc].tag))
      {

          fprintf(stderr, "Current element not a image group.\n");
          return HE_FAIL;
      }

    goTo(he_currDesc);

    if (DFR8getdims(he_file, &xdim, &ydim, &ispal) < 0)
      {
          fprintf(stderr, "Error getting image group.\n");
          return HE_FAIL;
      }

    if (he_remote)
        getSpace();     /* get space for image in mem */

/*
   *  Try to successfully load the palette and image from the file
 */
    if (DFR8getimage(he_file, wheresmall, xdim, ydim, rgb) < 0)
      {
          fprintf(stderr, "Error getting image group.\n");
          return HE_FAIL;
      }

    if (he_remote)
        rImage(ispal);  /* display remote image with [palette] */

    return HE_OK;

}

/*****************************************************************************/
/*  rimage
   *  Remote display of the image using the ICR.
   *  Just print the codes to stdout using the protocol.
 */
int
rImage(int usepal)
{
    int         i, j, newxsize;
    int8       *thisline, *space, *thischar;
    unsigned char c;

/*
   *  Open the window with the W command
 */

    printf("\033^W;%d;%d;%ld;%ld;0;rseq^", xwhere, ywhere, (long) (xdim * factor), (long) (ydim * factor));

/*
   *  If a palette should be used, send it with the M command.
 */
    if (usepal)
      {
          (void) printf("\033^M;0;256;768;rseq^");  /* start map */

          thischar = (int8 *) rgb;
          for (j = 0; j < 768; j++)
            {
                c = (unsigned char)*thischar++;
                if ((unsigned)c > (unsigned)31 && (unsigned)c < (unsigned)123)
                  {
                      putchar((int)c);
                  }
                else
                  {
                      putchar((int)(((unsigned)c >> 6) + (unsigned)123));
                      putchar((int)((c & 0x3f) + 32));
                  }
            }
      }

/*
   *  Send the data for the image with RLE encoding for efficiency.
   *  Encode each line and send it.
 */
    space = (int8 *) HDmalloc(ydim + 128);
    thisline = (int8 *) wheresmall;

    for (i = 0; i < ydim; i++)
      {
          newxsize = rleIt((char *) thisline, (char *) space, (int) xdim);
          thisline += xdim;     /* increment to next line */

          (void) printf("\033^R;0;%d;%d;%d;rseq^", i * factor, factor, newxsize);

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

                c = (unsigned char)*thischar++;    /* get byte to send */

                if ((unsigned)c > (unsigned)31 && (unsigned)c < (unsigned)123)
                  {
                      putchar((int)c);
                  }
                else
                  {
                      putchar((int)((unsigned)(c >> 6) + (unsigned)123));
                      putchar((int)((c & 0x3f) + 32));
                  }
            }
      }

/*
   *  pause for the user
 */

    HDfree(space);
    return HE_OK;
}

/*****************************************************************************/
/* expandimg
   *  copy an image memory to memory, expanding byte by byte to get a larger image.
   *  no aliasing, just byte replication
 */
int
bigImg(unsigned char *targ, unsigned char *src)
{
    int i, j, line;
    unsigned char *p, *q, *oldq;

    for (line = 0; line < ydim; line++)
      {
          p = src + line * xdim;
          oldq = q = targ + line * xsize * factor;

          for (i = 0; i < xdim; i++, p++)
              for (j = 0; j < factor; j++)
                  *q++ = *p;

          for (i = 1; i < factor; i++)
            {
                HDmemcpy(q, oldq, xsize);   /* make one copy of the line */
                q += xsize;
            }

      }
    return HE_OK;
}

/********************************************************************/
/*  rleit
   *  compress the data to go out with a simple run-length encoded scheme.
   *
 */
int
rleIt(char *buf, char *bufto, int len)
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
                      *cfoll = (char) (p - begp);
                      cfoll = clead;
                  }
                *cfoll++ = (char) (128 | (q - p));  /* len of seq */
                *cfoll++ = *p;  /* char of seq */
                len -= (int) (q - p);   /* subtract len of seq */
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
                      *cfoll = (char) (p - begp);
                      cfoll = clead++;
                      begp = p;
                  }
            }

      }
/*
   *  fill in last bytecount
 */
    if (p > begp)
        *cfoll = (char) (p - begp);
    else
        clead--;    /* don't need count position */

    return ((int) (clead - bufto));     /* how many stored as encoded */
}

#endif /* IBM6000 */

/* end of display.c */
