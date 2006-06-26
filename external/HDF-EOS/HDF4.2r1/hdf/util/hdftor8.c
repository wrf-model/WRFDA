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
static char RcsId[] = "@(#)$Revision: 1.22 $";
#endif

/* $Id: hdftor8.c,v 1.22 1998/12/08 21:37:57 koziol Exp $ */

/*
 * hdftor8.c
 * Extract images from HDF file to raster files
 */

#if defined __MWERKS__
#include <console.h>
#endif

#include "hdf.h"

#define PALETTE_SIZE    768     /* size of palette array */
#define COLOR_SIZE  256     /* size of palette color array */
                /* COLOR_SIZE == PALETTE_SIZE / 3 */

#define INTERACTIVE 'i'     /* interactive option */
#define RASTER_FILE 'r'     /* raster filename template */
#define PALETTE_FILE    'p'     /* palette filename template */
#define VERBOSE     'v'     /* verbose option */

#define TEMPLATE_NUMBER '#'     /* image or palette number positions */
#define TEMPLATE_XDIM   '@'     /* image x dim positions */
#define TEMPLATE_YDIM   '%'     /* image y dim positions */

const char  D_RASTER_TEM[] = "img#-@.%";    /* default raster file name template */
const char  D_PALETTE_TEM[] = "pal#";   /*  default palette file name template */

int         interactive;        /* interactive option */
int         verbose;            /* verbose option */

int         main
            (int argc, char *argv[]);
void        putRaster
            (const char *template, int32 xdim, int32 ydim, int imageNumber, uint8 *image);
void        putPalette
            (const char *template, int imageNumber, uint8 *palette);
void        convert
            (const char *template, int imageNumber, int32 xdim, int32 ydim, char *stringOut);
void        fillStr
            (const char **template, char **stringOut, char *string, char specialChar);
char       *newSpace
            (int32 size);
const char       *getTemplate
            (const char *type, int imageNumber);

int
main(int argc, char *argv[])
{
    int         i, imageNumber, ispal, err_val;
    int32       xdim, ydim;
    char       *hdfFile;
    uint8      *image, palette[PALETTE_SIZE];
    const char *rasterTemplate = NULL, *paletteTemplate = NULL;

#if defined __MWERKS__
    argc = ccommand(&argv);
#endif

    if (argc < 2)
      {
          printf("%s,  version: 1.1   date: July 1, 1992\n", argv[0]);
          printf("\tThis utility extracts all raster-8 images and/or\n");
          printf("\tpalettes from an HDF file and stores them in two sets of\n");
          printf("\tfiles containing only images and palettes, respectively.\n");
          printf("Usage:\n");
          printf("hdftor8 hdf_file [-i] [-v] [-r image_file] [-p pal_file]\n");
          printf("\t-i: interactive (specify filenames interactively)\n");
          printf("\t-v: verbose (provide descriptive messages)\n");
          printf("\tImages and palettes are placed in the specified files\n");
          printf("\tThe names of these files may contain special characters\n");
          printf("\t\twhich will be replaced by numbers:\n");
          printf("\t #    replace with image or palette number\n");
          printf("\t @    replace with x dim of image\n");
          printf("\t %%    replace with y dim of image\n");
          printf("\tIf not specified, image filename defaults to img#-@.%%\n");
          printf("\tIf not specified, palette filename defaults to pal.#\n\n");
          exit(1);
      }

    hdfFile = argv[1];

    for (i = 2; i < argc; i++)
      {
          if (*argv[i] == '-')
            {
                switch (argv[i][1])
                  {
                      case INTERACTIVE:
                          interactive = TRUE;
                          break;
                      case RASTER_FILE:
                          rasterTemplate = argv[++i];
                          break;
                      case PALETTE_FILE:
                          paletteTemplate = argv[++i];
                          break;
                      case VERBOSE:
                          verbose = TRUE;
                          break;
                      default:
                          printf("Illegal option: %s, skipping...\n", argv[i]);
                          break;
                  }
            }
          else
              printf("Illegal option: %s, skipping...\n", argv[i]);
      }

    if (!rasterTemplate && !interactive)
        rasterTemplate = D_RASTER_TEM;
    if (!paletteTemplate && !interactive)
        paletteTemplate = D_PALETTE_TEM;

    for (imageNumber = 1; !DFR8getdims(hdfFile, &xdim, &ydim, &ispal);)
      {
          image = (uint8 *) newSpace(xdim * ydim);
          if (verbose)
            {
                if (ispal)
                    printf("Getting image and palette %d.\n", imageNumber);
                else
                    printf("Getting image %d.\n", imageNumber);
                printf("Image dimensions : %d * %d\n", (int)xdim, (int)ydim);
            }
          if (!DFR8getimage(hdfFile, image, xdim, ydim, palette))
            {
                putRaster(rasterTemplate, xdim, ydim, imageNumber, image);
                if (ispal)
                    putPalette(paletteTemplate, imageNumber, palette);
                imageNumber++;
            }
          else
              break;
      }

    err_val = (int)HEvalue(1);
    if ((err_val != DFE_NOMATCH) && (err_val != DFE_NONE))
      {
          if (verbose)
              HEprint(stderr, 0);
          exit(1);
      }
    return (0);
}

/*
 * putRaster
 *
 * Write the image to a raster image file.
 *
 * INPUT:
 *        template : pointer to template string
 *        xdim : x dimension of image
 *        ydim : y dimension of image
 *        imageNumber : (need I say more?)
 *        image : pointer to image array
 */
void
putRaster(const char *template, int32 xdim, int32 ydim, int imageNumber,
          uint8 *image)
{
    FILE       *fd;
    char        fileName[DF_MAXFNLEN];

    if (!template)  /* can assume interactive (see main) */
        template = getTemplate("image", imageNumber);

    convert(template, imageNumber, xdim, ydim, fileName);

    if (verbose)
        printf("Writing into image file : %s\n", fileName);

    if ((fd = fopen(fileName, "wb")) == NULL)
      {
          puts("Unable to open file. Exiting...");
          exit(1);
      }
    if (fwrite(image, (size_t) xdim, (size_t) ydim, fd) != (unsigned) ydim)
      {
          puts("Unable to write to file. Exiting...");
          exit(1);
      }
    if (fclose(fd))
      {
          puts("Unable to close file. Exiting...");
          exit(1);
      }
}

/*
 * putPalette
 *
 * Write palette array out to palette file.
 *
 * INPUT:
 *        template : palette filename template
 *        imageNumber : Yes, the number of the image
 *        palette : pointer to the palette array
 */
void
putPalette(const char *template, int imageNumber, uint8 *palette)
{
    int         i;
    FILE       *fd;
    char        fileName[DF_MAXFNLEN], reds[COLOR_SIZE];
    char        greens[COLOR_SIZE], blues[COLOR_SIZE];

    if (!template)  /* can assume interactive (see main) */
        template = getTemplate("palette", imageNumber);

    convert(template, imageNumber, (int32) 1, (int32) 768, fileName);

    if (verbose)
        printf("Writing into palette file : %s\n", fileName);

    if ((fd = fopen(fileName, "wb")) == NULL)
      {
          puts("Unable to open file. Exiting...");
          exit(1);
      }

    for (i = 0; i < COLOR_SIZE; i++)
      {
          reds[i] = *palette++;
          greens[i] = *palette++;
          blues[i] = *palette++;
      }
    if (fwrite(reds, 1, COLOR_SIZE, fd) != COLOR_SIZE)
      {
          printf("Unable to write to file. Exiting...");
          exit(1);
      }
    if (fwrite(greens, 1, COLOR_SIZE, fd) != COLOR_SIZE)
      {
          printf("Unable to write to file. Exiting...");
          exit(1);
      }
    if (fwrite(blues, 1, COLOR_SIZE, fd) != COLOR_SIZE)
      {
          printf("Unable to write to file. Exiting...");
          exit(1);
      }
    if (fclose(fd))
      {
          printf("Unable to close file. Exiting...");
          exit(1);
      }
}

/*
 * convert
 *
 * Determine the file name given the template, imageNumber, x dimension
 * and y dimension. Replaces template special characters with the
 * corresponding numbers.
 *
 * INPUT:
 *        template : file name template
 *        imageNumber :
 *        xdim : x dimension of image
 *        ydim : y dimension of image
 * OUTPUT:
 *        stringOut : the concocted file name
 */
void
convert(const char *template, int imageNumber, int32 xdim, int32 ydim,
        char *stringOut)
{
    char        numStr[20], xStr[20], yStr[20];

    sprintf(numStr, "%03d", imageNumber);
    sprintf(xStr, "%03d", (int)xdim);
    sprintf(yStr, "%03d", (int)ydim);

    for (; (*template);)
      {
          switch (*template)
            {
                case TEMPLATE_NUMBER:
                    fillStr(&template, &stringOut, numStr, TEMPLATE_NUMBER);
                    break;
                case TEMPLATE_XDIM:
                    fillStr(&template, &stringOut, xStr, TEMPLATE_XDIM);
                    break;
                case TEMPLATE_YDIM:
                    fillStr(&template, &stringOut, yStr, TEMPLATE_YDIM);
                    break;
                default:
                    *stringOut++ = *template++;
            }
      }
    *stringOut = '\0';
}

/*
 * fillStr
 *
 * Fill a string of special characters with a number string.
 * If the number string is shorter than the number of special characters
 * then the string is padded with '0' on the left. Else the number of the
 * special characters is ignored.
 *
 * INPUT:
 *        template : pointer to pointer of string template
 *        string : pointer to the number string
 *        specialChar : the special character we are replacing
 * OUTPUT:
 *        stringOut : pointer to pointer of converted string (not really)
 * BUG: Both the pointer to the template string and the pointer to the
 *        comverted string are moved to after the position of the conversion.
 */
void
fillStr(const char **template, char **stringOut, char *string, char specialChar)
{
    int         templateLen, stringLen, i;

    for (templateLen = 1; *(++(*template)) == specialChar; templateLen++) ;
    stringLen = (int)HDstrlen(string);

    for (i = templateLen - stringLen; i > 0; i--)
        *(*stringOut)++ = '0';

    for (; (*string);)
        *(*stringOut)++ = *string++;
}

/*
 * newSpace
 *
 * Allocate a space with little wastage
 *
 * INPUT:
 *        size : size of space request
 * RETURN:
 *        pointer to the space allocated
 *
 * BUG: This routine can only handle one request at any time,
 *        a second call cannot be made while the space is still
 *        in use (somewhere else).
 */
char       *
newSpace(int32 size)
{
    static int32 oldSize = 0;   /* must be static */
    static char *oldSpace = NULL;   /* must be static */

    if (size >= oldSize)
      {
          if (oldSpace != NULL)
              HDfree(oldSpace);
          if ((oldSpace = (char *) HDmalloc((uint32) size)) == NULL)
            {
                puts("Out of memory. Abort.");
                exit(1);
            }
          oldSize = size;
      }

    return oldSpace;
}

/*
 * getTemplate
 *
 * Ask the user for a file name template string.
 *
 * INPUT:
 *        type : a description string of the type of file,
 *                i.e. image or palette.
 *        imageNumber :
 * RETURN:
 *        pointer to template string
 * BUG: This routine can only handle one request at any time,
 *        a second call cannot be made while the template is still
 *        in use (somewhere else).
 */
const char       *
getTemplate(const char *type, int imageNumber)
{
    static char template[DF_MAXFNLEN];

    printf("This is %s %d.\nWhat template would you like?\n",
           type, imageNumber);
    scanf("%s", template);
    return template;
}
