/*
 * Name:
 *      fp2hdf
 *
 * Purpose:
 *      To convert floating point data to HDF Scientific Data Set (SDS)
 *      and/or 8-bit Raster Image Set (RIS8) format, storing the results
 *      in an HDF file.  The image data can be scaled about the mean value.
 *
 *                                  -----------
 *      floating point data        |           | ----------> RIS8
 *      (SDS, ASCII text, or  ---> |  fp2hdf   |   and/or
 *      native floating point)     |           | ----------> SDS
 *                                  -----------
 *
 * Synopsis:
 *      fp2hdf -h[elp], OR
 *      fp2hdf <infile> [<infile>...] -o[utfile] <outfile>
 *             [-r[aster] [ras_opts ...]] [-f[loat]]
 *
 *      -h[elp]:
 *              Print a helpful summary of usage, and exit.
 *
 *      <infile(s)>:
 *              Input file(s), containing a single two-dimensional or
 *              three-dimensional floating point array in either ASCII
 *              text, native floating point, or HDF SDS format.  If an
 *              HDF file is used for input, it must contain an SDS.
 *              The SDS need only contain a dimension record and the
 *              data, but if it also contains maximum and minimum values
 *              and/or scales for each axis, these will be used.  If the
 *              input format is ASCII text or native floating point, see
 *              "Notes" below on how it must be organized.
 *
 *      -o[utfile] <outfile>:
 *              Data from one or more input files are stored as one or
 *              more data sets and/or images in one HDF output file,
 *              "outfile".
 *
 *      -r[aster]:
 *              Store output as a raster image set in the output file.
 *
 *      -f[loat]:
 *              Store output as a scientific data set in the output file.
 *              This is the default if the "-r" option is not specified.
 *
 *      ras_opts ...
 *
 *      -e[xpand] <horiz> <vert> [<depth>]:
 *              Expand float data via pixel replication to produce the
 *              image(s).  "horiz" and "vert" give the horizontal and
 *              vertical resolution of the image(s) to be produced; and
 *              optionally, "depth" gives the number of images or depth
 *              planes (for 3D input data).
 *
 *      -i[nterp] <horiz> <vert> [<depth>]:
 *              Apply bilinear, or trilinear, interpolation to the float
 *              data to produce the image(s).  "horiz", "vert", and "depth"
 *              must be greater than or equal to the dimensions of the
 *              original dataset.
 *      If max and min are supplied in input file, this option clips
 *      values that are greater than max or less then min, setting
 *      them to the max and min, respectively.
 *
 *      -p[alfile] <palfile>:
 *              Store the palette with the image.  Get the palette from
 *              "palfile"; which may be an HDF file containing a palette,
 *              or a file containing a raw palette.
 *
 *      -m[ean] <mean>:
 *              If a floating point mean value is given, the image will be
 *              scaled about the mean.  The new extremes (newmax and newmin),
 *              as given by:
 *
 *                 newmax = mean + max(abs(max-mean), abs(mean-min))
 *                 newmin = mean - max(abs(max-mean), abs(mean-min))
 *
 *              will be equidistant from the mean value.  If no mean value
 *              is given, then the mean will be:  0.5 * (max + min)
 *
 * Notes:
 *      If the input file format is ASCII text or native floating point, it
 *      must have the following input fields:
 *
 *              format
 *              nplanes
 *              nrows
 *              ncols
 *              max_value
 *              min_value
 *              [plane1 plane2 plane3 ...]
 *              row1 row2 row3 ...
 *              col1 col2 col3 ...
 *              data1 data2 data3 ...
 *              ...
 *
 *      Where:
 *              format:
 *                      Format designator ("TEXT", "FP32" or "FP64").
 *              nplanes:
 *                      Dimension of the depth axis ("1" for 2D input).
 *              nrows:
 *                      Dimension of the vertical axis.
 *              ncols:
 *                      Dimension of the horizontal axis.
 *              max_value:
 *                      Maximum data value.
 *              min_value:
 *                      Minimum data value.
 *              plane1, plane2, plane3, ...:
 *                      Scales for depth axis.
 *              row1, row2, row3, ...:
 *                      Scales for the vertical axis.
 *              col1, col2, col3, ...:
 *                      Scales for the horizontal axis.
 *              data1, data2, data3, ...:
 *                      The data ordered by rows, left to right and top
 *                      to bottom; then optionally, ordered by planes,
 *                      front to back.
 *
 *      For FP32 and FP64 input format, "format", "nplanes", "nrows", "ncols",
 *      and "nplanes" are native integers; where "format" is the integer
 *      representation of the appropriate 4-character string (0x46503332 for
 *      "FP32" and 0x46503634 for "FP64").  The remaining input fields are
 *      composed of native 32-bit floating point values for FP32 input format,
 *      or native 64-bit floating point values for FP64 input format.
 *
 * Source Availability:
 *      This program is in the public domain, and was developed and made
 *      available by the National Center for Supercomputing Applications,
 *      University of Illinois, Urbana-Champaign (ftp.ncsa.uiuc.edu).
 *
 * History:
 *      Beta version:                                           17-May-89
 *              (by Mike Folk mfolk@ncsa.uiuc.edu)
 *      Revision to put in the mean option:                     15-Sep-89
 *              (by Glen Mortensen gam@inel.gov)
 *      Officially released:                                    01-Dec-89
 *              (by NCSA ftp.ncsa.uiuc.edu)
 *      Revision to fix some bugs:                              16-Mar-90
 *              (by Mike Folk mfolk@ncsa.uiuc.edu)
 *      Revision to support 3D and native fp input:             15-May-90
 *              (by Bob Weaver baw@inel.gov)
 *      Revision to fix bug in interp() :                    17-Oct-90
 *              (by Fred Walsteijn nwalstyn@fys.ruu.n)
 *      Revision to fix bug in interp() :                    23-Nov-90
 *              Now it clips values outside of max and min.
 *              (by Fred Walsteijn nwalstyn@fys.ruu.n)
 *      Revision to start to use HDF 3.2 (and 3.3) library:  22-Jun-93
 *              Still lots to do to support other number types.
 *              (by Chris Houck chouck@ncsa.uiuc.edu)
 *
 */

#include "hdf.h"
#include <stdio.h>
#ifndef MIPSEL
#include <math.h>
#endif /* MIPSEL */
#include <string.h>
#include <ctype.h>

#ifdef WIN32
#include <sys/stat.h>
#include <fcntl.h>
#endif

/*
 * global macros
 */
#define EXPAND      1   /* -e: expand image with pixel replication */
#define INTERP      2   /* -i: expand image with interpolation */

/*
 * structure definition for command line options
 */
struct Options
  {
      char      **infiles;      /* pointer to list of input file names */
      char        outfile[32];  /* output file name */
      char        palfile[32];  /* palette file name, if any */
      int         fcount;       /* number of input files */
      int         to_float;     /* float output is desired */
      int         to_image;     /* image output is desired */
      int         pal;          /* output palette with image */
      int         ctm;          /* color transform method: EXPAND or INTERP */
      int         exh;          /* horizontal expansion factor */
      int         exv;          /* vertical expansion factor */
      int         exd;          /* depth expansion factor */
      int         hres;         /* horizontal resolution of output image */
      int         vres;         /* vertical resolution of output image */
      int         dres;         /* depth resolution of output image */
      int         mean;         /* scale image around a mean */
      float32     meanval;      /* mean value to scale the image around */
  };

/*
 * structure definition for the input data
 */
struct Input
  {
      int         is_hdf;       /* HDF file format flag */
      int         is_text;      /* ASCII text format flag */
      int         is_fp32;      /* 32-bit native floating point format flag */
      int         is_fp64;      /* 64-bit native floating point format flag */
      int         rank;         /* number of input data dimensions */
      int         dims[3];      /* input dimensions - ncols, nrows, nplanes */
      int         is_vscale;    /* vertical axis scales in the input */
      int         is_hscale;    /* horizontal axis scales in the input */
      int         is_dscale;    /* depth axis scales in the input */
      float32     max;          /* maximum value of the data */
      float32     min;          /* minimum value of the data */
      float32    *hscale;       /* horizontal scales */
      float32    *vscale;       /* vertical scales */
      float32    *dscale;       /* depth scales */
      VOIDP       data;         /* input data */
  };

/*
 * structure definition for the output raster images
 */
struct Raster
  {
      int         hres;         /* horizontal resolution of the image */
      int         vres;         /* vertical resolution of the image */
      int         dres;         /* depth resolution of the image */
      unsigned char *image;
  };

/*
 * state table tokens
 */
#define FILNAME 0   /* filename */
#define OPT_o   1   /* output filename */
#define OPT_r   2   /* convert to image */
#define OPT_e   3   /* expand image via pixel replication */
#define OPT_i   4   /* make interpolated image */
#define NUMBR   5   /* resolution of enlarged image */
#define OPT_p   6   /* palette filename */
#define OPT_f   7   /* convert to float (default) */
#define OPT_h   8   /* request for explanation */
#define OPT_m   9   /* mean to scale around */
#define ERR 20  /* invalid token */

/*
 * state table for parsing the command line.
 */
static int  state_table[17][10] =
{

    /* token ordering:
       FILNAME      OPT_o   OPT_r   OPT_e   OPT_i   NUMBR   OPT_p   OPT_f
       OPT_h        OPT_m   */

    /* state 0: start */
    {1, ERR, ERR, ERR, ERR, ERR, ERR, ERR,
    14, ERR},

    /* state 1: input files */
    {1, 2, ERR, ERR, ERR, ERR, ERR, ERR,
    ERR, ERR},

    /* state 2: -o[utfile] */
    {3, ERR, ERR, ERR, ERR, ERR, ERR, ERR,
    ERR, ERR},

    /* state 3: outfile */
    {ERR, ERR, 4, ERR, ERR, ERR, ERR, 13,
    ERR, ERR},

    /* state 4: -r[aster] */
    {ERR, ERR, ERR, 5, 9, ERR, 10, 12,
    ERR, 15},

    /* state 5: -e[xpand] */
    {ERR, ERR, ERR, ERR, ERR, 6, ERR, ERR,
    ERR, ERR},

    /* state 6: -e[xpand] or -i[nterp] option argument */
    {ERR, ERR, ERR, ERR, ERR, 7, ERR, ERR,
    ERR, ERR},

    /* state 7: -e[xpand] or -i[nterp] option argument */
    {ERR, ERR, ERR, ERR, ERR, 8, 10, 12,
    ERR, 15},

    /* state 8: -e[xpand] or -i[nterp] option argument */
    {ERR, ERR, ERR, ERR, ERR, ERR, 10, 12,
    ERR, 15},

    /* state 9: -i[nterp] */
    {ERR, ERR, ERR, ERR, ERR, 6, ERR, ERR,
    ERR, ERR},

    /* state 10: -p[alfile] */
    {11, ERR, ERR, ERR, ERR, ERR, ERR, ERR,
    ERR, ERR},

    /* state 11: palfile */
    {ERR, ERR, ERR, 5, 9, ERR, ERR, 12,
    ERR, 15},

    /* state 12: -f[loat] (after -r[aster]) */
    {ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR,
    ERR, ERR},

    /* state 13: -f[loat] */
    {ERR, ERR, 4, ERR, ERR, ERR, ERR, ERR,
    ERR, ERR},

    /* state 14: -h[elp] */
    {ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR,
    ERR, ERR},

    /* state 15: -m[ean] */
    {ERR, ERR, ERR, ERR, ERR, 16, ERR, ERR,
    ERR, ERR},

    /* state 16: mean */
    {ERR, ERR, ERR, 5, 9, ERR, 10, 12,
    ERR, ERR}
};

/* static local functions */
static int  gtoken(char *s);
static int  process(struct Options *opt);
static int  gfloat(char *infile, FILE * strm, float32 *fp32, struct Input *in);
static int  gint(char *infile, FILE * strm, int *ival, struct Input *in);
static int  isnum(char *s);
static int  gdata(char *infile, struct Input *in, FILE *strm, int *is_maxmin);
static int  gdimen(char *infile, struct Input *inp, FILE *strm);
static int  gmaxmin(char *infile, struct Input *in, FILE *strm, int *is_maxmin);
static int  gscale(char *infile, struct Input *in, FILE *strm, int *is_scale);
static int  gtype(char *infile, struct Input *in, FILE **strm);
static int  indexes(float32 *scale, int dim, int *idx, int res);
static int  interp(struct Input *in, struct Raster *im);
static int  palette(char *palfile);
static int  pixrep(struct Input *in, struct Raster *im);

/*
 * functions with non-integer return types
 */
void        help(char *);
void        mean(struct Input *, struct Options *);
void        usage(char *);

/*
 * Name:
 *      main
 *
 * Purpose:
 *      The driver for "fp2hdf".
 */
int
main(int argc, char *argv[])
{
    struct Options opt;
    int         i;
    int         outfile_named = FALSE;
    int         token;
    int         state = 0;

    
    const char *err1 = "Invalid number of arguments:  %d.\n";
    const char *err2 = "Error in state table.\n";
    const char *err3 = "No output file given.\n";
    const char *err4 = "Program aborted.\n";

#ifdef WIN32
	_fmode = _O_BINARY;
#endif

    /*
     * set 'stdout' and 'stderr' to line-buffering mode
     */
    (void) setvbuf(stderr, (char *) NULL, _IOLBF, 0);
    (void) setvbuf(stdout, (char *) NULL, _IOLBF, 0);

#if defined __MWERKS__
    argc = ccommand(&argv);
#endif

    /*
     * validate the number of command line arguments
     */
    if (argc < 2)
      {
          (void) fprintf(stderr, err1, argc);
          usage(argv[0]);
          goto err;
      }

    opt.to_image = FALSE;   /* default: no image */
    opt.to_float = FALSE;   /* default: make float if no image */
			    /* Set FALSE here.  Will be set TRUE */
			    /* after confirming image option is not set.  */
    opt.ctm = EXPAND;   /* default: pixel replication */
    opt.hres = 0;   /* default: no expansion values */
    opt.vres = 0;
    opt.dres = 0;
    opt.pal = FALSE;    /* default: no palette */
    opt.mean = FALSE;   /* default: no mean given */
    opt.fcount = 0;     /* to count number of input files */

    /*
     * parse the command line
     */
    for (i = 1; i < argc; i++)
      {
          if ((token = gtoken(argv[i])) == ERR)
            {
                usage(argv[0]);
                goto err;
            }
          state = state_table[state][token];
          switch (state)
            {
                case 1: /* counting input files */
                    opt.fcount++;
                    break;
                case 2: /* -o found; look for outfile */
                    break;
                case 3: /* get outfile name */
                    (void) HDstrcpy(opt.outfile, argv[i]);
                    outfile_named = TRUE;
                    break;
                case 4: /* -r found */
                    opt.to_image = TRUE;
                    break;
                case 5: /* -e found */
                    opt.ctm = EXPAND;
                    break;
                case 6: /* horizontal resolution */
                    opt.hres = atoi(argv[i]);
                    break;
                case 7: /* vertical resolution */
                    opt.vres = atoi(argv[i]);
                    break;
                case 8: /* depth resolution */
                    opt.dres = atoi(argv[i]);
                    break;
                case 9: /* -i found */
                    opt.ctm = INTERP;
                    break;
                case 10:    /* -p found */
                    opt.pal = TRUE;
                    break;
                case 11:    /* get pal filename */
                    (void) HDstrcpy(opt.palfile, argv[i]);
                    break;
                case 12:    /* -f found (after a -r) */
                case 13:    /* -f found (no -r yet) */
                    opt.to_float = TRUE;
                    break;
                case 14:    /* -h found; help, then exit */
                    help(argv[0]);
                    exit(0);
                case 15:    /* -m found */
                    opt.mean = TRUE;
                    break;
                case 16:    /* mean value */
                    opt.meanval = (float32)atof(argv[i]);
                    break;
                case ERR:   /* command syntax error */
                default:
                    (void) fprintf(stderr, err2);
                    usage(argv[0]);
                    goto err;
            }
      }

    /*
     * make sure an output file was specified
     */
    if (!outfile_named)
      {
          (void) fprintf(stderr, err3);
          usage(argv[0]);
          goto err;
      }

    if (!opt.to_image)
        opt.to_float = TRUE;
    opt.infiles = argv + 1;

    /*
     * process the input files
     */
    if (process(&opt))
        goto err;

    return(0);

  err:
    (void) fprintf(stderr, err4);
    return(1);
}

/*
 * Name:
 *      gdata
 *
 * Purpose:
 *      Get the input data.
 */
static int
gdata(char *infile, struct Input *in, FILE *strm, int *is_maxmin)
{
    int32       i, j, k;
    float32    *fp32;
    int32       hdfdims[3];     /* order: ZYX or YX */
    int32       len = in->dims[0] * in->dims[1] * in->dims[2];

    const char *err1 = "Unable to get input data from file: %s.\n";

    /*
     * extract the input data from the input file
     */
    if (in->is_hdf == TRUE)
      {
          /*
           * hdfdims is ordered: ZYX or YX
           * in->dims is ordered: XYZ
           */
          if (in->rank == 2)
            {
                hdfdims[0] = in->dims[1];
                hdfdims[1] = in->dims[0];
            }
          else
            {
                hdfdims[0] = in->dims[2];
                hdfdims[1] = in->dims[1];
                hdfdims[2] = in->dims[0];
            }

          if (DFSDgetdata(infile, in->rank, hdfdims, in->data))
            {
                (void) fprintf(stderr, err1, infile);
                goto err;
            }
      }
    else
      {
          for (k = 0, fp32 = (float32 *) in->data; k < in->dims[2]; k++)
            {
                for (j = 0; j < in->dims[1]; j++)
                  {
                      for (i = 0; i < in->dims[0]; i++, fp32++)
                        {
                            if (gfloat(infile, strm, fp32, in))
                              {
                                  (void) fprintf(stderr, err1, infile);
                                  goto err;
                              }
                        }
                  }
            }
          (void) fclose(strm);
      }

    /*
     * derive the max/min values, if needed
     */
    if (*is_maxmin == FALSE)
      {
          in->min = in->max = *(float32 *) in->data;
          for (i = 1; i < len; i++)
            {
                if (((float32 *) in->data)[i] > in->max)
                    in->max = ((float32 *) in->data)[i];
                if (((float32 *) in->data)[i] < in->min)
                    in->min = ((float32 *) in->data)[i];
            }
          *is_maxmin = TRUE;
      }

#ifdef  DEBUG
    (void) printf("\tdata:");
    for (k = 0, fp32 = in->data; k < in->dims[2]; k++)
      {
          (void) printf("\n");
          for (j = 0; j < in->dims[1]; j++)
            {
                (void) printf("\n\t");
                for (i = 0; i < in->dims[0]; i++, fp32++)
                    (void) printf("%E ", *fp32);
            }
      }
    (void) printf("\n\n\n");
#endif /* DEBUG */

    return (0);

  err:
    return (1);
}

/*
 * Name:
 *      gdimen
 *
 * Purpose:
 *      Determine the input data dimensions.
 */
static int
gdimen(char *infile, struct Input *inp, FILE *strm)
{
    int32       hdfdims[3];     /* order: ZYX or YX */
    int32       nt;             /* number type of input file */

    const char *err1 = "Unable to get data dimensions from file: %s.\n";
    const char *err2 = "Invalid data rank of %d in file: %s.\n";
    const char *err3 = "Dimension(s) is less than '2' in file: %s.\n";
    const char *err4 = "Unexpected number type from file: %s.\n";

    /*
     * extract the rank and dimensions of the HDF input file
     */
    if (inp->is_hdf == TRUE)
      {
          if (DFSDgetdims(infile, &inp->rank, hdfdims, 3) == FAIL)
            {
                (void) fprintf(stderr, err1, infile);
                goto err;
            }

          /* don't know how to deal with other numbers yet */
          if (DFSDgetNT(&nt) == FAIL || nt != DFNT_FLOAT32)
            {
                (void) fprintf(stderr, err4, infile);
                goto err;
            }

          /*
           * hdfdims is ordered: ZYX or YX
           * inp->dims is ordered: XYZ
           */
          if (inp->rank == 2)
            {
                inp->dims[0] = hdfdims[1];
                inp->dims[1] = hdfdims[0];
                inp->dims[2] = 1;
            }
          else if (inp->rank == 3)
            {
                inp->dims[0] = hdfdims[2];
                inp->dims[1] = hdfdims[1];
                inp->dims[2] = hdfdims[0];
            }
          else
            {
                (void) fprintf(stderr, err2, inp->rank, infile);
                goto err;
            }

          /*
           * get the rank and dimensions from a TEXT or native floating point
           * format input file
           */
      }
    else
      {
          if (gint(infile, strm, &inp->dims[2], inp))
            {
                (void) fprintf(stderr, err1, infile);
                goto err;
            }
          if (inp->dims[2] > 1)
              inp->rank = 3;
          else
              inp->rank = 2;
          if (gint(infile, strm, &inp->dims[1], inp))
            {
                (void) fprintf(stderr, err1, infile);
                goto err;
            }
          if (gint(infile, strm, &inp->dims[0], inp))
            {
                (void) fprintf(stderr, err1, infile);
                goto err;
            }
      }

    /*
     * validate dimension sizes
     */
    if ((inp->dims[0] < 2) || (inp->dims[1] < 2))
      {
          (void) fprintf(stderr, err3, infile);
          goto err;
      }

#ifdef  DEBUG
    (void) printf("\nInput Information ...\n\n");
    (void) printf("\trank:\n\n\t%d\n\n", inp->rank);
    (void) printf("\tdimensions (nplanes,nrows,ncols):\n\n");
    (void) printf("\t%d %d %d\n\n", inp->dims[2], inp->dims[1], inp->dims[0]);
#endif /* DEBUG */

    return (0);

  err:
    return (1);
}

/*
 * Name:
 *      gfloat
 *
 * Purpose:
 *      Read in a single floating point value from the input stream.  The
 *      input format may either be ASCII text , 32-bit native floating point,
 *      or 64-bit native floating point.
 */
static int
gfloat(char *infile, FILE * strm, float32 *fp32, struct Input *in)
{
    float64     fp64=0.0;

    const char *err1 = "Unable to get 'float' value from file: %s.\n";

    if (in->is_text == TRUE)
      {

		if (fscanf(strm, "%e", fp32) != 1)
            {
                (void) fprintf(stderr, err1, infile);
                goto err;
            }
      }
    else if (in->is_fp32 == TRUE)
      {
		
          if (fread((char *) fp32, sizeof(float32), 1, strm) != 1)
            {
                (void) fprintf(stderr, err1, infile);
                goto err;
            }
      }
    else
      {
          if (fread((char *) &fp64, sizeof(float64), 1, strm) != 1)
            {
                (void) fprintf(stderr, err1, infile);
                goto err;
            }
          *fp32 = (float32) fp64;
      }

    return (0);

  err:
    return (1);
}

/*
 * Name:
 *      gint
 *
 * Purpose:
 *      Read in a single integer value from the input stream.  The input
 *      format may either be ASCII text or a native BCD of type integer.
 */
static int
gint(char *infile, FILE * strm, int *ival, struct Input *in)
{
    const char *err1 = "Unable to get 'int' value from file: %s.\n";

    /*
     * process TEXT-formatted input
     */
    if (in->is_text == TRUE)
      {
          if (fscanf(strm, "%d", ival) != 1)
            {
                (void) fprintf(stderr, err1, infile);
                goto err;
            }

          /*
           * process BCD-formatted input
           */
      }
    else
      {
          if (fread((char *) ival, sizeof(int), 1, strm) != 1)
            {
                (void) fprintf(stderr, err1, infile);
                goto err;
            }
      }

    return (0);

  err:
    return (1);
}

/*
 * Name:
 *      gmaxmin
 *
 * Purpose:
 *      Extract the maximum and minimum data values from the input file.
 */
static int
gmaxmin(char *infile, struct Input *in, FILE *strm, int *is_maxmin)
{
    const char *err1 = "Unable to get max/min values from file: %s.\n";

    /*
     * extract the max/min values from the input file
     */
    if (in->is_hdf == TRUE)
      {
          if (!DFSDgetrange(&in->max, &in->min))
              if (in->max > in->min)
                  *is_maxmin = TRUE;
      }
    else
      {
          if (gfloat(infile, strm, &in->max, in))
            {
                (void) fprintf(stderr, err1, infile);
                goto err;
            }
          if (gfloat(infile, strm, &in->min, in))
            {
                (void) fprintf(stderr, err1, infile);
                goto err;
            }
          if (in->max > in->min)
              *is_maxmin = TRUE;
      }

#ifdef  DEBUG
    (void) printf("\tinput maximum/minimum values:\n\n");
    (void) printf("\t%E %E\n\n", in->max, in->min);
#endif /* DEBUG */

    return (0);

  err:
    return (1);
}

/*
 * Name:
 *      gscale
 *
 * Purpose:
 *      Determine the scale for each axis.
 */
static int
gscale(char *infile, struct Input *in, FILE *strm, int *is_scale)
{
    int         i;
    int32       hdfdims[3];     /* order: ZYX or YX */

    const char *err1 = "Unable to get axis scale from file: %s.\n";

    *is_scale = TRUE;

    /*
     * hdfdims is ordered: ZYX or YX
     * in->dims is ordered: XYZ
     */
    if (in->rank == 2)
      {
          hdfdims[0] = in->dims[1];
          hdfdims[1] = in->dims[0];
      }
    else
      {
          hdfdims[0] = in->dims[2];
          hdfdims[1] = in->dims[1];
          hdfdims[2] = in->dims[0];
      }

    /*
     * extract the scale values from the input file
     */
    if (in->is_hdf == TRUE)
      {
          if (in->rank == 2)
            {
                if (DFSDgetdimscale(1, hdfdims[0], in->vscale))
                  {
                      *is_scale = FALSE;
                      for (i = 0; i <= hdfdims[0]; i++)
                          in->vscale[i] = (float32) i;
                  }
                if (DFSDgetdimscale(2, hdfdims[1], in->hscale))
                  {
                      *is_scale = FALSE;
                      for (i = 0; i <= hdfdims[1]; i++)
                          in->hscale[i] = (float32) i;
                  }
            }
          else
            {
                if (DFSDgetdimscale(1, hdfdims[0], in->dscale))
                  {
                      *is_scale = FALSE;
                      for (i = 0; i <= hdfdims[0]; i++)
                          in->dscale[i] = (float32) i;
                  }
                if (DFSDgetdimscale(2, hdfdims[1], in->vscale))
                  {
                      *is_scale = FALSE;
                      for (i = 0; i <= hdfdims[1]; i++)
                          in->vscale[i] = (float32) i;
                  }
                if (DFSDgetdimscale(3, hdfdims[2], in->hscale))
                  {
                      *is_scale = FALSE;
                      for (i = 0; i <= hdfdims[2]; i++)
                          in->hscale[i] = (float32) i;
                  }
            }
      }
    else
      {
          if (in->rank == 2)
            {
                for (i = 0; i < hdfdims[0]; i++)
                  {
                      if (gfloat(infile, strm, &in->vscale[i], in))
                        {
                            (void) fprintf(stderr, err1, infile);
                            goto err;
                        }
                  }
                in->vscale[i] = in->vscale[i - 1];
                for (i = 0; i < hdfdims[1]; i++)
                  {
                      if (gfloat(infile, strm, &in->hscale[i], in))
                        {
                            (void) fprintf(stderr, err1, infile);
                            goto err;
                        }
                  }
                in->hscale[i] = in->hscale[i - 1];
            }
          else
            {
                for (i = 0; i < hdfdims[0]; i++)
                  {
                      if (gfloat(infile, strm, &in->dscale[i], in))
                        {
                            (void) fprintf(stderr, err1, infile);
                            goto err;
                        }
                  }
                in->dscale[i] = in->dscale[i - 1];
                for (i = 0; i < hdfdims[1]; i++)
                  {
                      if (gfloat(infile, strm, &in->vscale[i], in))
                        {
                            (void) fprintf(stderr, err1, infile);
                            goto err;
                        }
                  }
                in->vscale[i] = in->vscale[i - 1];
                for (i = 0; i < hdfdims[2]; i++)
                  {
                      if (gfloat(infile, strm, &in->hscale[i], in))
                        {
                            (void) fprintf(stderr, err1, infile);
                            goto err;
                        }
                  }
                in->hscale[i] = in->hscale[i - 1];
            }
      }

#ifdef  DEBUG
    if (in->rank == 2)
      {
          (void) printf("\tscales of the axes (vert,horiz):\n\n\t");
          for (i = 0; i < hdfdims[0]; i++)
              (void) printf("%E ", in->vscale[i]);
          (void) printf("\n\t");
          for (i = 0; i < hdfdims[1]; i++)
              (void) printf("%E ", in->hscale[i]);
      }
    else
      {
          (void) printf("\tscales of the axes (depth,vert,horiz):\n\n\t");
          for (i = 0; i < hdfdims[0]; i++)
              (void) printf("%E ", in->dscale[i]);
          (void) printf("\n\t");
          for (i = 0; i < hdfdims[1]; i++)
              (void) printf("%E ", in->vscale[i]);
          (void) printf("\n\t");
          for (i = 0; i < hdfdims[2]; i++)
              (void) printf("%E ", in->hscale[i]);
      }
    (void) printf("\n\n\n");
#endif /* DEBUG */

    return (0);

  err:
    return (1);
}

/*
 * Name:
 *      gtoken
 *
 * Purpose:
 *      Return the token identifier associated with the command line
 *      argument.
 */
static int
gtoken(char *s)
{
    size_t      len;
    int         token;

    const char *err1 = "Illegal argument: %s.\n";

    /*
     * identify the token type
     */
    if (s[0] == '-')
      {     /* option name (or negative number) */
          token = ERR;
          len = HDstrlen(&s[1]);
          switch (s[1])
            {
                case 'o':
                    if (!HDstrncmp("outfile", &s[1], len))
                        token = OPT_o;
                    break;
                case 'r':
                    if (!HDstrncmp("raster", &s[1], len))
                        token = OPT_r;
                    break;
                case 'e':
                    if (!HDstrncmp("expand", &s[1], len))
                        token = OPT_e;
                    break;
                case 'i':
                    if (!HDstrncmp("interp", &s[1], len))
                        token = OPT_i;
                    break;
                case 'p':
                    if (!HDstrncmp("palfile", &s[1], len))
                        token = OPT_p;
                    break;
                case 'f':
                    if (!HDstrncmp("float", &s[1], len))
                        token = OPT_f;
                    break;
                case 'h':
                    if (!HDstrncmp("help", &s[1], len))
                        token = OPT_h;
                    break;
                case 'm':
                    if (!HDstrncmp("mean", &s[1], len))
                        token = OPT_m;
                    break;
                default:
                    if (isnum(s))   /* negative number? */
                        token = NUMBR;
            }
          if (token == ERR)
              (void) fprintf(stderr, err1, s);

      }
    else if (isnum(s))  /* positive number */
        token = NUMBR;
    else    /* filename */
        token = FILNAME;

    return (token);
}

/*
 * Name:
 *      gtype
 *
 * Purpose:
 *      Determine the type of the input file (HDF, TEXT, FP32 or FP64).
 */
static int
gtype(char *infile, struct Input *in, FILE **strm)
{
    char        buf[8];

    const char *err1 = "Unable to open file: %s.\n";
    const char *err2 = "Unable to get format tag from file: %s.\n";
    const char *err3 = "Invalid file format in file: %s.\n";

    /*
     * determine the input file format
     */
    if (Hishdf(infile))
        in->is_hdf = TRUE;
    else
      {
          if ((*strm = fopen(infile, "r")) == NULL)
            {
                (void) fprintf(stderr, err1, infile);
                goto err;
            }
          if (fread(buf, 4, 1, *strm) != 1)
            {
                (void) fprintf(stderr, err2, infile);
                goto err;
            }
          if (!HDmemcmp("TEXT", buf, 4) || !HDmemcmp("text", buf, 4)) {
#ifdef WIN32
			  _fmode = _O_TEXT;
#endif
              in->is_text = TRUE;
		  }
          else
            {
                rewind(*strm);
                if (fread(buf, sizeof(int), 1, *strm) != 1)
                  {
                      (void) fprintf(stderr, err2, infile);
                      goto err;
                  }
                if (!HDmemcmp("FP32", buf, 4) || !HDmemcmp("fp32", buf, 4))
                    in->is_fp32 = TRUE;
                else if (!HDmemcmp("FP64", buf, 4) ||
                         !HDmemcmp("fp64", buf, 4))
                    in->is_fp64 = TRUE;
                else
                  {
                      (void) fprintf(stderr, err3, infile);
                      goto err;
                  }
		
            }
      }

    return (0);

  err:
    return (1);
}

/*
 * Name:
 *      help
 *
 * Purpose:
 *      Print a helpful summary of command usage and features.
 */
void
help(char *name)
{
    (void) printf("Name:\n");
    (void) printf("\t%s\n\n", name);
    (void) printf("Purpose:\n");
    (void) printf("\tTo convert floating point data to HDF Scientific ");
    (void) printf("Data Set (SDS)\n");
    (void) printf("\tand/or 8-bit Raster Image Set (RIS8) format, ");
    (void) printf("storing the results\n");
    (void) printf("\tin an HDF file.  The image data can be scaled ");
    (void) printf("about a mean value.\n\n");
    (void) printf("Version:\n");
    (void) printf("\tv1.1 (Apr 30, 1990)\n\n");
    (void) printf("Synopsis:\n");
    (void) printf("\t%s -h[elp], OR\n", name);
    (void) printf("\t%s <infile> [<infile>...] -o[utfile] ", name);
    (void) printf("<outfile>\n");
    (void) printf("\t\t[-r[aster] [ras_opts ...]] [-f[loat]]\n\n");
    (void) printf("\t-h[elp]:\n");
    (void) printf("\t\tPrint a helpful summary of usage, and exit.\n\n");
    (void) printf("\tinfile(s):\n");
    (void) printf("\t\tInput file(s), containing a single ");
    (void) printf("two-dimensional or\n");
    (void) printf("\t\tthree-dimensional floating point array in ");
    (void) printf("either ASCII\n");
    (void) printf("\t\ttext, native floating point, or HDF SDS format.  ");
    (void) printf("If an\n");
    (void) printf("\t\tHDF file is used for input, it must contain an ");
    (void) printf("SDS.\n");
    (void) printf("\t\tThe SDS need only contain a dimension record and ");
    (void) printf("the\n");
    (void) printf("\t\tdata, but if it also contains maximum and ");
    (void) printf("minimum values\n");
    (void) printf("\t\tand/or scales for each axis, these will be ");
    (void) printf("used.  If the\n");
    (void) printf("\t\tinput format is ASCII text or native floating ");
    (void) printf("point, see\n");
    (void) printf("\t\t\"Notes\" below on how it must be organized.\n\n");
    (void) printf("\t-o[utfile] <outfile>:\n");
    (void) printf("\t\tData from one or more input files are stored as ");
    (void) printf("one or\n");
    (void) printf("\t\tmore data sets and/or images in one HDF output ");
    (void) printf("file,\n\t\t\"outfile\".\n\n");
    (void) printf("\t-r[aster]:\n");
    (void) printf("\t\tStore output as a raster image set in the ");
    (void) printf("output file\n\n");
    (void) printf("\t-f[loat]:\n");
    (void) printf("\t\tStore output as a scientific data set in the ");
    (void) printf("the output file.\n");
    (void) printf("\t\tThis is the default if the \"-r\" option is not ");
    (void) printf("specified.\n\n");
    (void) printf("\tras_opts ...\n\n");
    (void) printf("\t-e[xpand] <horiz> <vert> [<depth>]:\n");
    (void) printf("\t\tExpand float data via pixel replication to ");
    (void) printf("produce the\n");
    (void) printf("\t\timage(s).  \"horiz\" and \"vert\" give the ");
    (void) printf("horizontal and\n");
    (void) printf("\t\tvertical resolution of the image(s) to be ");
    (void) printf("produced; and\n");
    (void) printf("\t\toptionally, \"depth\" gives the number of ");
    (void) printf("images or depth\n");
    (void) printf("\t\tplanes (for 3D input data).\n\n");
    (void) printf("\t-i[nterp] <horiz> <vert> [<depth>]:\n");
    (void) printf("\t\tApply bilinear, or trilinear, interpolation to ");
    (void) printf("the float\n");
    (void) printf("\t\tdata to produce the image(s).  \"horiz\", ");
    (void) printf("\"vert\", and \"depth\"\n");
    (void) printf("\t\tmust be greater than or equal to the dimensions ");
    (void) printf("of the\n");
    (void) printf("\t\toriginal dataset.\n\n");
    (void) printf("\t-p[alfile] <palfile>:\n");
    (void) printf("\t\tStore the palette with the image.  Get the ");
    (void) printf("palette from\n");
    (void) printf("\t\t\"palfile\"; which may be an HDF file containing ");
    (void) printf("a palette,\n");
    (void) printf("\t\tor a file containing a raw palette.\n\n");
    (void) printf("\t-m[ean] <mean>:\n");
    (void) printf("\t\tIf a floating point mean value is given, the ");
    (void) printf("image will be\n");
    (void) printf("\t\tscaled about the mean.  The new extremes ");
    (void) printf("(newmax and newmin),\n");
    (void) printf("\t\tas given by:\n\n");
    (void) printf("\t\t   newmax = mean + max(abs(max-mean), ");
    (void) printf("abs(mean-min))\n");
    (void) printf("\t\t   newmin = mean - max(abs(max-mean), ");
    (void) printf("abs(mean-min))\n\n");
    (void) printf("\t\twill be equidistant from the mean value.  If ");
    (void) printf("no mean value\n");
    (void) printf("\t\tis given, then the mean will be:  0.5 * (max ");
    (void) printf("+ min)\n\n");
    (void) printf("Notes:\n");
    (void) printf("\tIf the input file format is ASCII text or native ");
    (void) printf("floating point, it\n");
    (void) printf("\tmust have the following input fields:\n\n");
    (void) printf("\t\tformat\n");
    (void) printf("\t\tnplanes\n");
    (void) printf("\t\tnrows\n");
    (void) printf("\t\tncols\n");
    (void) printf("\t\tmax_value\n");
    (void) printf("\t\tmin_value\n");
    (void) printf("\t\t[plane1 plane2 plane3 ...]\n");
    (void) printf("\t\trow1 row2 row3 ...\n");
    (void) printf("\t\tcol1 col2 col3 ...\n");
    (void) printf("\t\tdata1 data2 data3 ...\n");
    (void) printf("\t\t...\n\n");
    (void) printf("\tWhere:\n");
    (void) printf("\t\tformat:\n");
    (void) printf("\t\t\tFormat designator (\"TEXT\", \"FP32\" or ");
    (void) printf("\"FP64\").\n");
    (void) printf("\t\tnplanes:\n");
    (void) printf("\t\t\tDimension of the depth axis (\"1\" for 2D ");
    (void) printf("input).\n");
    (void) printf("\t\tnrows:\n");
    (void) printf("\t\t\tDimension of the vertical axis.\n");
    (void) printf("\t\tncols:\n");
    (void) printf("\t\t\tDimension of the horizontal axis.\n");
    (void) printf("\t\tmax_value:\n");
    (void) printf("\t\t\tMaximum data value.\n");
    (void) printf("\t\tmin_value:\n");
    (void) printf("\t\t\tMinimum data value.\n");
    (void) printf("\t\tplane1, plane2, plane3, ...:\n");
    (void) printf("\t\t\tScales for depth axis.\n");
    (void) printf("\t\trow1, row2, row3, ...:\n");
    (void) printf("\t\t\tScales for the vertical axis.\n");
    (void) printf("\t\tcol1, col2, col3, ...:\n");
    (void) printf("\t\t\tScales for the horizontal axis.\n");
    (void) printf("\t\tdata1, data2, data3, ...:\n");
    (void) printf("\t\t\tThe data ordered by rows, left to right and ");
    (void) printf("top\n");
    (void) printf("\t\t\tto bottom; then optionally, ordered by planes,\n");
    (void) printf("\t\t\tfront to back.\n\n");
    (void) printf("\tFor FP32 and FP64 input format, \"format\", ");
    (void) printf("\"nplanes\", \"nrows\", \"ncols\",\n");
    (void) printf("\tand \"nplanes\" are native integers; where ");
    (void) printf("\"format\" is the integer\n");
    (void) printf("\trepresentation of the appropriate 4-character ");
    (void) printf("string (0x46503332 for\n");
    (void) printf("\t\"FP32\" and 0x46503634 for \"FP64\").  The ");
    (void) printf("remaining input fields are\n");
    (void) printf("\tcomposed of native 32-bit floating point values for ");
    (void) printf("FP32 input format,\n");
    (void) printf("\tor native 64-bit floating point values for FP64 ");
    (void) printf("input format.\n\n");
    (void) printf("Examples:\n");
    (void) printf("\tConvert floating point data in \"f1.txt\" to SDS ");
    (void) printf("format, and store it\n");
    (void) printf("\tas an SDS in HDF file \"o1\":\n\n");
    (void) printf("\t\t%s f1.txt -o o1\n\n", name);
    (void) printf("\tConvert floating point data in \"f2.hdf\" to ");
    (void) printf("8-bit raster format, and\n");
    (void) printf("\tstore it as an RIS8 in HDF file \"o2\":\n\n");
    (void) printf("\t\t%s f2.hdf -o o2 -r\n\n", name);
    (void) printf("\tConvert floating point data in \"f3.bin\" to ");
    (void) printf("8-bit raster format and\n");
    (void) printf("\tSDS format, and store both the RIS8 and the SDS ");
    (void) printf("in HDF file \"o3\":\n\n");
    (void) printf("\t\t%s f3.bin -o o3 -r -f\n\n", name);
    (void) printf("\tConvert floating point data in \"f4\" to a ");
    (void) printf("500x600 raster image, and\n");
    (void) printf("\tstore the RIS8 in HDF file \"o4\".  Also store a ");
    (void) printf("palette from \"palfile\"\n");
    (void) printf("\twith the image:\n\n");
    (void) printf("\t\t%s f4 -o o4 -r -e 500 600 -p palfile\n\n", name);
    (void) printf("\tConvert floating point data in \"f5\" to 200 ");
    (void) printf("planes of 500x600 raster\n");
    (void) printf("\timages, and store the RIS8 in HDF file \"o5\".  ");
    (void) printf("Also scale the image\n");
    (void) printf("\tdata so that it is centered about a mean value ");
    (void) printf("of 10.0:\n\n");
    (void) printf("\t\t%s f5 -o o5 -r -i 500 600 200 -m 10.0\n", name);

    return;
}

/*
 * Name:
 *      indexes
 *
 * Purpose:
 *      For each pixel location along an axis, determine the nearest
 *      scale value neighbor.  Return a list of indexes into the scale
 *      array.
 */
static int
indexes(float32 *scale, int dim, int *idx, int res)
{
    int         i, j;
    float32    *midpt;
    float32     loc;
    float32     delta;

    const char *err1 = "Unable to allocate dynamic memory.\n";

    /*
     * determine the midpoints between scale values
     */
    if ((midpt = (float32 *) HDmalloc((size_t) dim * sizeof(float32))) == NULL)
      {
          (void) fprintf(stderr, err1);
          goto err;
      }
    for (i = 0; i < dim - 1; i++)
        midpt[i] = (scale[i] + scale[i + 1]) * (float32)0.5;
    midpt[dim - 1] = scale[dim - 1] + (scale[dim - 1] - midpt[dim - 2]);

    /*
     * determine the distance between pixel locations
     */
    delta = (scale[dim - 1] - scale[0]) / (float32)(res - 1);

    /*
     * compute indexes, keeping the index the same until the location
     * extends beyond the midpoint
     */
    for (i = 1, j = 0, idx[0] = 0, loc = scale[0]; i < res; i++)
      {
          loc += delta;
          idx[i] = idx[i - 1];
          while (loc >= midpt[j])
            {
                idx[i] += 1;
                j += 1;
            }
      }

    /*
     * free dynamically allocated memory
     */
    HDfree((char *) midpt);

    return (0);

  err:
    return (1);
}

/*
 * Name:
 *      interp
 *
 * Purpose:
 *      Use a bilinear, or trilinear, interpolation scheme to construct
 *      the raster image(s).
 *
 *  Bug revision:  the line that previously read:
 *
 *      hratio[i] = ((hrange > 0) ? 1.0 : -1.0) * (in->hscale[j+1] -
 *                    loc) / (in->hscale[j+1] - in->hscale[j]);
 *    has been changed to read:
 *      hratio[i] = (in->hscale[j+1] - loc) / (in->hscale[j+1] - in->hscale[j]);
 *
 *    Similar changes were made to the corresponding lines for
 *    computing vratio and dratio.
 *
 *  Bug revision: If values occur that are outside the ranges of the
 *    max and min values provided, these values are now "clipped" to
 *    be the same as the max and min, respectively.
 */

static int
interp(struct Input *in, struct Raster *im)
{
    int         i, j, k, m;
    int        *hinc, *voff, *doff = NULL;
    float32     pix;
    float32     loc;
    float32     range;
    float32     ratio;
    float32     hrange, vrange, drange = (float32)0.0;
    float32     hdelta, vdelta, ddelta = (float32)0.0;
    float32     t1, t2, t3, t4, t5, t6;
    float32    *hratio, *vratio, *dratio = NULL;
    float32    *pt[8];
    unsigned char *ip = im->image;

    const char *err1 = "Unable to allocate dynamic memory.\n";

    /*
     * determine the range of pixel locations
     */
    range = in->max - in->min;
    ratio = (float32)237.9 / range;
    hrange = in->hscale[in->dims[0] - 1] - in->hscale[0];
    vrange = in->vscale[in->dims[1] - 1] - in->vscale[0];
    if (in->rank == 3)
        drange = in->dscale[in->dims[2] - 1] - in->dscale[0];

    /*
     * determine the distance between pixel locations
     */
    hdelta = hrange / (float32)(im->hres - 1);
    vdelta = vrange / (float32)(im->vres - 1);
    if (in->rank == 3)
        ddelta = drange / (float32)(im->dres - 1);

    /*
     * allocate dynamic memory for the interpolation ratio buffers
     */
    if ((hratio = (float32 *) HDmalloc((size_t) im->hres * sizeof(float32))) == NULL)
      {
          (void) fprintf(stderr, err1);
          goto err;
      }
    if ((vratio = (float32 *) HDmalloc((unsigned int) im->vres *
                                         sizeof(float32))) == NULL)
      {
          (void) fprintf(stderr, err1);
          goto err;
      }
    if (in->rank == 3)
      {
          if ((dratio = (float32 *) HDmalloc((unsigned int) im->dres *
                                               sizeof(float32))) == NULL)
            {
                (void) fprintf(stderr, err1);
                goto err;
            }
      }

    /*
     * allocate dynamic memory for the pixel location offset/increment
     * buffers
     */
    if ((hinc = (int *) HDmalloc((unsigned int) im->hres *
                                   sizeof(int))) == NULL)
      {
          (void) fprintf(stderr, err1);
          goto err;
      }
    if ((voff = (int *) HDmalloc((unsigned int) (im->vres + 1) *
                                   sizeof(int))) == NULL)
      {
          (void) fprintf(stderr, err1);
          goto err;
      }
    if (in->rank == 3)
      {
          if ((doff = (int *) HDmalloc((unsigned int) (im->dres + 1) *
                                         sizeof(int))) == NULL)
            {
                (void) fprintf(stderr, err1);
                goto err;
            }
      }

    /*
     * compute the interpolation ratios and pixel location
     * offsets/increments for each axis
     */
    for (i = 0, j = 0; i < im->hres; i++)
      {
          loc = hdelta * (float) i + in->hscale[0];
          hinc[i] = 0;
          while ((j < (in->dims[0] - 2)) && ((hrange > (float32)0.0) ?
                     (in->hscale[j + 1] < loc) : (in->hscale[j + 1] > loc)))
            {
                hinc[i] += 1;
                j += 1;
            }
          hratio[i] = (in->hscale[j + 1] - loc) / (in->hscale[j + 1] - in->hscale[j]);
      }
    for (i = 0, j = 0, voff[0] = 0; i < im->vres; i++)
      {
          loc = vdelta * (float) i + in->vscale[0];
          while ((j < (in->dims[1] - 2)) && ((vrange > (float32)0.0) ?
                     (in->vscale[j + 1] < loc) : (in->vscale[j + 1] > loc)))
            {
                voff[i] += 1;
                j += 1;
            }
          vratio[i] = (in->vscale[j + 1] - loc) / (in->vscale[j + 1] - in->vscale[j]);
          voff[i + 1] = voff[i];
      }
    if (in->rank == 3)
      {
          for (i = 0, j = 0, doff[0] = 0; i < im->dres; i++)
            {
                loc = ddelta * (float) i + in->dscale[0];
                while ((j < (in->dims[2] - 2)) && ((drange > (float32)0.0) ?
                     (in->dscale[j + 1] < loc) : (in->dscale[j + 1] > loc)))
                  {
                      doff[i] += 1;
                      j += 1;
                  }
                dratio[i] = (in->dscale[j + 1] - loc) /
                    (in->dscale[j + 1] - in->dscale[j]);
                doff[i + 1] = doff[i];
            }
      }

    /*
     * do the interpolation for each point in the target image, taking
     * advantage of the fact that the target is evenly spaced along each
     * axis
     */
    if (in->rank == 2)
      {
          for (i = 0; i < im->vres; i++)
            {
                pt[0] = (float32 *) in->data + (in->dims[0] * voff[i]);
                pt[1] = pt[0] + 1;
                pt[2] = pt[0] + in->dims[0];
                pt[3] = pt[2] + 1;
                for (j = 0; j < im->hres; j++)
                  {
                      for (m = 0; m < 4; m++)
                          pt[m] += hinc[j];
                      t1 = *pt[2] - ((*pt[2] - *pt[0]) * vratio[i]);
                      t2 = *pt[3] - ((*pt[3] - *pt[1]) * vratio[i]);
                      pix = t2 - ((t2 - t1) * hratio[j]);
                      if (pix > in->max)
                          pix = in->max;    /* clip (bug fix) */
                      if (pix < in->min)
                          pix = in->min;    /* ditto */
                      *ip++ = (unsigned char)((ratio * (pix - in->min)) + (float32)1.5);
                  }
            }
      }
    else
      {     /* rank == 3 */
          for (i = 0; i < im->dres; i++)
            {
                for (j = 0; j < im->vres; j++)
                  {
                      pt[0] = (float32 *) in->data + (in->dims[0] * voff[j]) +
                          (in->dims[0] * in->dims[1] * doff[i]);
                      pt[1] = pt[0] + 1;
                      pt[2] = pt[0] + in->dims[0];
                      pt[3] = pt[2] + 1;
                      pt[4] = pt[0] + (in->dims[0] * in->dims[1]);
                      pt[5] = pt[4] + 1;
                      pt[6] = pt[4] + in->dims[0];
                      pt[7] = pt[6] + 1;
                      for (k = 0; k < im->hres; k++)
                        {
                            for (m = 0; m < 8; m++)
                                pt[m] += hinc[k];
                            t1 = *pt[4] - ((*pt[4] - *pt[0]) *
                                           dratio[i]);
                            t2 = *pt[6] - ((*pt[6] - *pt[2]) *
                                           dratio[i]);
                            t3 = *pt[5] - ((*pt[5] - *pt[1]) *
                                           dratio[i]);
                            t4 = *pt[7] - ((*pt[7] - *pt[3]) *
                                           dratio[i]);
                            t5 = t2 - ((t2 - t1) * vratio[j]);
                            t6 = t4 - ((t4 - t3) * vratio[j]);
                            pix = t6 - ((t6 - t5) * hratio[k]);
                            if (pix > in->max)
                                pix = in->max;  /* clip (bug fix) */
                            if (pix < in->min)
                                pix = in->min;  /* ditto */
                            *ip++ = (unsigned char)((ratio * (pix - in->min)) + (float32)1.5);
                        }
                  }
            }
      }

    /*
     * free dynamically allocated memory
     */
    HDfree((char *) hratio);
    HDfree((char *) vratio);
    if (in->rank == 3)
        HDfree((char *) dratio);
    HDfree((char *) hinc);
    HDfree((char *) voff);
    if (in->rank == 3)
        HDfree((char *) doff);

    return (0);

  err:
    return (1);
}

/*
 * Name:
 *      isnum
 *
 * Purpose:
 *      Determine whether or not the string is representative of an
 *      integer or floating point number.  If it is, a non-zero value
 *      is returned.  A leading (-) to denote sign is acceptable.
 */
static int
isnum(char *s)
{
    char       *cp;
    int         rval = FALSE;

    /*
     * check to see if its a floating point number
     */
    cp = s;
    (void) strtod(s, &cp);
    if ((*cp == '\0') && (cp != s))
        rval = TRUE;

    /*
     * check to see if its an integer number (radix 8, 10, or 16)
     */
    else
      {
          cp = s;
          (void) strtol(s, &cp, 0);
          if ((*cp == '\0') && (cp != s))
              rval = TRUE;
      }

    return (rval);
}

/*
 * Name:
 *      mean
 *
 * Purpose:
 *      Reset the maximum and minimum data values to be symmetric about
 *      the user-specified mean value.
 */
void
mean(struct Input *in,struct Options * opt)
{
    float32     delta, delta_max, delta_min;

    delta_max = (float32)fabs((double)(in->max - opt->meanval));
    delta_min = (float32)fabs((double)(opt->meanval - in->min));
    delta = (delta_max > delta_min) ? delta_max : delta_min;

    in->max = opt->meanval + delta;
    in->min = opt->meanval - delta;

    return;
}

/*
 * Name:
 *      palette
 *
 * Purpose:
 *      Process the (user specified) palette input file.
 */
static int
palette(char *palfile)
{
    unsigned char *color;
    unsigned char pal[1024], red[256], green[256], blue[256];
    FILE       *strm;
    int         i;

    const char *err1 = "Unable to get palette from file: %s.\n";
    const char *err2 = "Unable to open palette file: %s.\n";
    const char *err3 = "Unable to set default palette.\n";

    /*
     * extract a palette from an HDF file
     */
    if (Hishdf(palfile))
      {
          if (DFPgetpal(palfile, pal))
            {
                (void) fprintf(stderr, err1, palfile);
                goto err;
            }

          /*
           * read in a raw palette file
           */
      }
    else
      {
          if ((strm = fopen(palfile, "r")) == NULL)
            {
                (void) fprintf(stderr, err2, palfile);
                goto err;
            }
          if (fread((char *) red, 1, 256, strm) != 256)
            {
                (void) fprintf(stderr, err1, palfile);
                goto err;
            }
          else if (fread((char *) green, 1, 256, strm) != 256)
            {
                (void) fprintf(stderr, err1, palfile);
                goto err;
            }
          else if (fread((char *) blue, 1, 256, strm) != 256)
            {
                (void) fprintf(stderr, err1, palfile);
                goto err;
            }
          (void) fclose(strm);

          /*
           * interleave the R,G,B values
           */
          color = pal;
          for (i = 0; i < 256; i++)
            {
                *color++ = red[i];
                *color++ = green[i];
                *color++ = blue[i];
            }
      }

    /*
     * set up the palette as the default for subsequent images
     */
    if (DFR8setpalette(pal))
      {
          (void) fprintf(stderr, err3);
          goto err;
      }

    return (0);

  err:
    return (1);
}

/*
 * Name:
 *      pixrep
 *
 * Purpose:
 *      Expand the image(s) to the desired resolution using pixel
 *      replication.
 */
static int
pixrep(struct Input *in, struct Raster *im)
{
    int        *hidx, *vidx, *didx;
    int         ovidx, odidx;
    int         dummy;
    int32       i, j, k;
    float32    *dp;
    float32     range;
    float32     ratio;
    unsigned char *ip, *plane, *row, *pix;

    const char *err1 = "Unable to dynamically allocate memory.\n";

    dp = (float32 *) in->data;
    ip = im->image;
    range = in->max - in->min;
    ratio = (float32)237.9 / range;

    /*
     * determine the scale indexes of the horizontal pixel locations
     */
    if ((hidx = (int *) HDmalloc((unsigned int) (im->hres + 1) * sizeof(int))) == NULL)
      {
          (void) fprintf(stderr, err1);
          goto err;
      }

    if (indexes(in->hscale, in->dims[0], hidx, im->hres))
        goto err;

    /*
     * determine the scale indexes of the vertical pixel locations
     */
    if ((vidx = (int *) HDmalloc((unsigned int) (im->vres + 1) *
                                   sizeof(int))) == NULL)
      {
          (void) fprintf(stderr, err1);
          goto err;
      }

    if (indexes(in->vscale, in->dims[1], vidx, im->vres))
        goto err;

    /*
     * determine the scale indexes of the depth plane locations
     */
    dummy = 0;
    didx = &dummy;
    if (in->rank == 3)
      {
          if ((didx = (int *) HDmalloc((unsigned int) (im->dres + 1) *
                                         sizeof(int))) == NULL)
            {
                (void) fprintf(stderr, err1);
                goto err;
            }

          if (indexes(in->dscale, in->dims[2], didx, im->dres))
              goto err;
      }

    /*
     * compute the expanded image
     */
    if ((pix = (unsigned char *) HDmalloc((unsigned int) (in->dims[0] + 1))) ==
        NULL)
      {
          (void) fprintf(stderr, err1);
          goto err;
      }
    for (k = 0, odidx = didx[0] - 1; k < im->dres; k++)
      {
          /*
           * construct a new depth plane
           */
          if (didx[k] > odidx)
            {
                for (j = 0, ovidx = vidx[0] - 1; j < im->vres; j++)
                  {
                      /*
                       * construct a new row
                       */
                      if (vidx[j] > ovidx)
                        {
                            for (i = 0; i < in->dims[0]; i++)
                                pix[i] = (unsigned char )((ratio * (*dp++ - in->min)) + (float32)1.5);
                            for (i = 0; i < im->hres; i++)
                                *ip++ = pix[hidx[i]];
                            /*
                             * repeat the previous row
                             */
                        }
                      else
                        {
                            row = ip - im->hres;
                            for (i = 0; i < im->hres; i++)
                                *ip++ = *row++;
                        }
                      ovidx = vidx[j];
                  }
                /*
                 * repeat the previous depth plane
                 */
            }
          else
            {
                plane = ip - (im->hres * im->vres);
                for (j = 0; j < im->vres; j++)
                    for (i = 0; i < im->hres; i++)
                        *ip++ = plane[(j * im->hres) + i];
            }
          odidx = didx[k];
      }

    /*
     * free dynamically allocated space
     */
    HDfree((char *) hidx);
    HDfree((char *) vidx);
    if (in->rank == 3)
        HDfree((char *) didx);
    HDfree((char *) pix);

    return (0);

  err:
    return (1);
}

/*
 * Name:
 *      process
 *
 * Purpose:
 *      Process each input file.
 */
static int
process(struct Options *opt)
{
    struct Input in;
    struct Raster im;
    unsigned char *ip;
    int         i, j;
    int         is_maxmin;
    int         is_scale;
    int32       hdfdims[3];     /* order: ZYX or YX */
    int32       len;
    FILE       *strm;
    int32       hdf;
#ifdef  DEBUG
    int         h, v, d;
#endif /* DEBUG */

    const char *err1 = "Error creating HDF output file: %s.\n";
    const char *err2 = "Unable to dynamically allocate memory.\n";
    const char *err3a = "Warning: cannot make image smaller using -e ";
    const char *err3b = "option.\n\t %s resolution will be made the ";
    const char *err3c = "same as %s dimension of the\n\t dataset, ";
    const char *err3d = "which is: %d.\n\n";
    const char *err4 = "Unable to write an RIS8 to the HDF output file\n";
    const char *err5 = "Unable to write an SDS to the HDF output file\n";

    /*
     * process the palette file (if one was specified)
     */
    if (opt->pal == TRUE)
        if (palette(opt->palfile))
            goto err;

    /*
     * create the HDF output file
     */
    if ((hdf = Hopen(opt->outfile, DFACC_CREATE, 0)) == FAIL)
      {
          (void) fprintf(stderr, err1, opt->outfile);
          goto err;
      }
    (void) Hclose(hdf);

    /*
     * main loop: process input files, one per pass
     */
    for (i = 0; i < opt->fcount; i++)
      {
          /*
           * initialize key parameters
           */
          in.is_hdf = FALSE;
          in.is_text = FALSE;
          in.is_fp32 = FALSE;
          in.is_fp64 = FALSE;
          is_maxmin = FALSE;
          is_scale = FALSE;

          /*
           * get the file type, input data dimensions, and input data
           * max/min values
           */
          if (gtype(opt->infiles[i], &in, &strm))
              goto err;
          if (gdimen(opt->infiles[i], &in, strm))
              goto err;
          if (gmaxmin(opt->infiles[i], &in, strm, &is_maxmin))
              goto err;

          /*
           * get the scale for each axis
           */
          if ((in.hscale = (float32 *) HDmalloc((size_t)
                             (in.dims[0] + 1) * sizeof(float32))) == NULL)
            {
                (void) fprintf(stderr, err2);
                goto err;
            }
          if ((in.vscale = (float32 *) HDmalloc((size_t)
                             (in.dims[1] + 1) * sizeof(float32))) == NULL)
            {
                (void) fprintf(stderr, err2);
                goto err;
            }
          if (in.rank == 3)
            {
                if ((in.dscale = (float32 *) HDmalloc((size_t)
                             (in.dims[2] + 1) * sizeof(float32))) == NULL)
                  {
                      (void) fprintf(stderr, err2);
                      goto err;
                  }
            }
          if (gscale(opt->infiles[i], &in, strm, &is_scale))
              goto err;

          /*
           * get the input data
           */
          len = in.dims[0] * in.dims[1] * in.dims[2];
          if ((in.data = (VOIDP) HDmalloc((size_t) len *
                                                 sizeof(float32))) == NULL)
            {
                (void) fprintf(stderr, err2);
                goto err;
            }
          if (gdata(opt->infiles[i], &in, strm, &is_maxmin))
              goto err;

          /*
           * put the input data in the HDF output file, in SDS format
           */
          if (opt->to_float == TRUE)
            {
                /*
                 * hdfdims is ordered: ZYX or YX
                 * in.dims is ordered: XYZ
                 */
                if (in.rank == 2)
                  {
                      hdfdims[0] = in.dims[1];
                      hdfdims[1] = in.dims[0];
                  }
                else
                  {
                      hdfdims[0] = in.dims[2];
                      hdfdims[1] = in.dims[1];
                      hdfdims[2] = in.dims[0];
                  }

                if (DFSDsetNT(DFNT_FLOAT32))
                  {
                      (void) fprintf(stderr, err5);
                      goto err;
                  }
                if (is_scale == TRUE)
                  {
                      if (DFSDsetdims(in.rank, hdfdims))
                        {
                            (void) fprintf(stderr, err5);
                            goto err;
                        }
                      if (DFSDsetrange(&in.max, &in.min))
                        {
                            (void) fprintf(stderr, err5);
                            goto err;
                        }
                      if (in.rank == 2)
                        {
                            if (DFSDsetdimscale(1, hdfdims[0],
                                                in.vscale))
                              {
                                  (void) fprintf(stderr, err5);
                                  goto err;
                              }
                            if (DFSDsetdimscale(2, hdfdims[1],
                                                in.hscale))
                              {
                                  (void) fprintf(stderr, err5);
                                  goto err;
                              }
                        }
                      else
                        {
                            if (DFSDsetdimscale(1, hdfdims[0],
                                                in.dscale))
                              {
                                  (void) fprintf(stderr, err5);
                                  goto err;
                              }
                            if (DFSDsetdimscale(2, hdfdims[1],
                                                in.vscale))
                              {
                                  (void) fprintf(stderr, err5);
                                  goto err;
                              }
                            if (DFSDsetdimscale(3, hdfdims[2],
                                                in.hscale))
                              {
                                  (void) fprintf(stderr, err5);
                                  goto err;
                              }
                        }
                  }
                if (DFSDadddata(opt->outfile, in.rank, hdfdims, in.data))
                  {
                      (void) fprintf(stderr, err5);
                      goto err;
                  }
            }

          /*
           * put the input data in the HDF output file, in RIS8 format
           */
          if (opt->to_image == TRUE)
            {
                /*
                 * allocate a buffer for the output image
                 */
                im.hres = (opt->hres == 0) ? in.dims[0] : opt->hres;
                if ((im.hres < in.dims[0]) && (opt->ctm == EXPAND))
                  {
                      (void) fprintf(stderr, err3a);
                      (void) fprintf(stderr, err3b, "Horiz.");
                      (void) fprintf(stderr, err3c, "horiz.");
                      (void) fprintf(stderr, err3d, in.dims[0]);
                      im.hres = in.dims[0];
                      opt->hres = in.dims[0];
                  }
                im.vres = (opt->vres == 0) ? in.dims[1] : opt->vres;
                if ((im.vres < in.dims[1]) && (opt->ctm == EXPAND))
                  {
                      (void) fprintf(stderr, err3a);
                      (void) fprintf(stderr, err3b, "Vert.");
                      (void) fprintf(stderr, err3c, "vert.");
                      (void) fprintf(stderr, err3d, in.dims[1]);
                      im.vres = in.dims[1];
                      opt->vres = in.dims[1];
                  }
                im.dres = 1;
                if (in.rank == 3)
                  {
                      im.dres = (opt->dres == 0) ? in.dims[2] :
                          opt->dres;
                      if ((im.dres < in.dims[2]) &&
                          (opt->ctm == EXPAND))
                        {
                            (void) fprintf(stderr, err3a);
                            (void) fprintf(stderr, err3b, "Depth");
                            (void) fprintf(stderr, err3c, "depth");
                            (void) fprintf(stderr, err3d,
                                           in.dims[2]);
                            im.dres = in.dims[2];
                            opt->dres = in.dims[2];
                        }
                  }
                len = im.hres * im.vres * im.dres;
                if ((im.image = (unsigned char *) HDmalloc((unsigned
                                                         int) len)) == NULL)
                  {
                      (void) fprintf(stderr, err2);
                      goto err;
                  }

                /*
                 * reset max/min symmetrically about the mean value
                 */
                if (opt->mean == TRUE)
                    mean(&in, opt);

                /*
                 * perform pixel replication or interpolation
                 */
                if (opt->ctm == EXPAND)
                  {
                      if (pixrep(&in, &im))
                          goto err;
                  }
                else
                  {     /* INTERP */
                      if (interp(&in, &im))
                          goto err;
                  }

                len = im.hres * im.vres;
                for (j = 0, ip = im.image; j < im.dres; j++, ip += len)
                  {
                      if (DFR8addimage(opt->outfile, ip, im.hres,
                                       im.vres, DFTAG_RLE))
                        {
                            (void) fprintf(stderr, err4);
                            goto err;
                        }
                  }

#ifdef  DEBUG
                (void) printf("Output Raster Information ...\n\n");
                (void) printf("\tresolution (horiz,vert,[depth]):\n\n");
                if (in.rank == 2)
                    (void) printf("\t%d %d\n\n", im.hres, im.vres);
                else
                    (void) printf("\t%d %d %d\n\n", im.hres,
                                  im.vres, im.dres);
                if (opt->mean == TRUE)
                  {
                      (void) printf("\tadjusted max/min values:\n\n");
                      (void) printf("\t%f %f\n\n", in.max, in.min);
                  }
                (void) printf("\tcolor index values:");
                for (d = 0, ip = im.image; d < im.dres; d++)
                  {
                      (void) printf("\n");
                      for (v = 0; v < im.vres; v++)
                        {
                            (void) printf("\n");
                            for (h = 0; h < im.hres; h++, ip++)
                                (void) printf("\t%d", *ip);
                        }
                  }
                (void) printf("\n");
#endif /* DEBUG */
            }

          /*
           * free dynamically allocated space
           */
          HDfree((char *) in.hscale);
          HDfree((char *) in.vscale);
          if (in.rank == 3)
              HDfree((char *) in.dscale);
          HDfree((char *) in.data);
          if (opt->to_image == TRUE)
              HDfree((char *) im.image);
      }

    return (0);

  err:
    return (1);
}

/*
 * Name:
 *      usage
 *
 * Purpose:
 *      Print a summary of command usage.
 */
void
usage(char *name)
{
    (void) fprintf(stderr, "\nUsage:\t%s -h[elp], OR\n", name);
    (void) fprintf(stderr, "\t%s <infile> [<infile>...] ", name);
    (void) fprintf(stderr, "-o[utfile] <outfile> [options...]\n\n");
    (void) fprintf(stderr, "\toptions...\n");
    (void) fprintf(stderr, "\t    -r[aster]:\n");
    (void) fprintf(stderr, "\t        produce an image.  Could be ");
    (void) fprintf(stderr, "followed by:\n");
    (void) fprintf(stderr, "\t        -e[xpand] <horiz> <vert> ");
    (void) fprintf(stderr, "[<depth>]:\n");
    (void) fprintf(stderr, "\t            resolution with pixel ");
    (void) fprintf(stderr, "replication\n");
    (void) fprintf(stderr, "\t        -i[nterp] <horiz> <vert> ");
    (void) fprintf(stderr, "[<depth>]:\n");
    (void) fprintf(stderr, "\t            resolution with interpolation\n");
    (void) fprintf(stderr, "\t        -p[alfile] <palfile>:\n");
    (void) fprintf(stderr, "\t            include palette from palfile\n");
    (void) fprintf(stderr, "\t        -m[ean] <meanval>:\n");
    (void) fprintf(stderr, "\t            mean value to scale image ");
    (void) fprintf(stderr, "around\n");
    (void) fprintf(stderr, "\t    -f[loat]:\n");
    (void) fprintf(stderr, "\t        produce floating point data\n\n");

    return;
}
