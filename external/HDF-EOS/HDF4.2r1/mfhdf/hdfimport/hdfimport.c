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
/* 
 * Name:
 *      hdfimport (previously fp2hdf) 
 *
 * Purpose:
 *      To convert floating point and/or integer data to HDF Scientific Data Set (SDS)
 *      and/or 8-bit Raster Image Set (RIS8) format, storing the results
 *      in an HDF file.  The image data can be scaled about the mean value.
 *
 *                                  -------------
 *      floating point data        |             | ----------> RIS8
 *      (SDS, ASCII text, or  ---> | hdfimport   |   and/or
 *      native floating point)     |             | ----------> SDS
 *                                  -------------
 *					   AND / OR
 *	                           ---------------
 *      integer data               |              | 
 *      (ASCII text, or       ---> |  hdfimport   |  ----------> SDS 
 *      binary integer)            |              | 
 *                                  --------------
 * Synopsis:
 *      hdfimport -h[elp], OR
 *      hdfimport <infile> [ [-t[ype] <output-type> | -n] [<infile> [-t[ype] <output-type> | -n ]]...] 
 *                            -o[utfile] <outfile> [-r[aster] [ras_opts ...]] [-f[loat]]
 *      
 *      -h[elp]:
 *              Print this summary of usage, and exit.
 *
 *      <infile(s)>:
 *              Name of the input file(s), containing a single 
 *		two-dimensional or three-dimensional floating point array 
 *		in either ASCII text, native floating point, native integer 
 *		or HDF SDS format.  If an HDF file is used for input, it 
 *		must contain an SDS. The SDS need only contain a dimension 
 *		record and the data, but if it also contains maximum and 
 *		minimum values and/or scales for each axis, these will 
 *		be used.  If the input format is ASCII text or native 
 *		floating point or native integer, see "Notes" below on 
 *		how it must be organized.
 *      
 *      -t[ype] <output_type>: 
 *              Optionally used for every input ASCII file to specify the 
 *	        data type of the data-set to be written. If not specified               
 *              default data type is 32-bit floating point. <output-type>
 *              can be any of the following: FP32 (default), FP64, INT32
 * 	        INT16, INT8. It can be used only with ASCII files.
 *     
 *      -n:  
 *		This option is to be used only if the binary input file 
 *		contains 64-bit floating point data and the default
 *		behaviour (default behaviour is to write it to a 32-bit
 *		floating point data-set) should be overridden to write 
 *		it to a 64-bit floating point data-set.
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
 *      If the input file format is ASCII text or native floating point or native integer(32-bit,
 *      16-bit, 8-bit), it
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
 *                      Format designator ("TEXT", "FP32", "FP64", "IN32", "IN16", "IN08").
 *                      nplanes, nrows, ncols:
 *                      Dimensions are specified in the order slowest changing dimension first.
 *			ncols is dimension of the fastest changing dimension. (horizontal axis
 *			or X-axis in a 3D scale)
 *			nrows corresponds to dimension of the vertical axis or Y-axis in a 3D 
 *			scale.
 *			nplanes corresponds to the slowest changing dimension i.e. dimension of 
 *			the depth axis or the Z-axis in a 3D scale ("1" for 2D input).
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
 *      For IN32, IN16 and IN08 input format, "format", "nplanes", "nrows", "ncols",
 *      and "nplanes" are native integers; where "format" is the integer
 *      representation of the appropriate 4-character string. The remaining input 
 *      fields are composed of native 32-bit integer values for IN32 input format,
 *      or native 16-bit integer values for IN16 input format or native 8-bit 
 *      integer values for IN08 input format.
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
 *      Revision to fix bug in interp() :                       17-Oct-90
 *              (by Fred Walsteijn nwalstyn@fys.ruu.n)
 *      Revision to fix bug in interp() :                       23-Nov-90
 *              Now it clips values outside of max and min.
 *              (by Fred Walsteijn nwalstyn@fys.ruu.n)
 *      Revision to start to use HDF 3.2 (and 3.3) library:     22-Jun-93
 *              Still lots to do to support other number types.
 *              (by Chris Houck chouck@ncsa.uiuc.edu)
 *      Revision to incorporate 32-bit integer, 16-bit integer, 08-Jan-02
 *              8-bit integer data types and converting 64-bit 
 *              input data to 64-bit output data.
 *              (by Pankaj Kamat pkamat@uiuc.edu)	 
 */

#include "hdf.h"
#include <stdio.h>
#ifndef MIPSEL
#include <math.h>
#endif /* MIPSEL */
#include <string.h>
#include <ctype.h>
#include <mfhdf.h>

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
 * structure definition to associate input files with the output data types
 */
struct infilesformat
{
	char filename[255];
	int outtype;   /* if the value is "" output type will be FP32. Applicable only to TEXT Files*/
};
/*
 * structure definition for command line options
 */
struct Options
  {
      struct infilesformat  infiles[30];  /* structure to hold the list of input file names. Limited to 30*/
      char        outfile[32];  /* output file name */
      char        palfile[32];  /* palette file name, if any */
      int         fcount;       /* number of input files */
      int         to_float;     /* float output is desired */
      int         to_image;     /* image output is desired */
      int         to_int;  
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

/* Additional Structures to handle different data types */
struct int16set /* variables for an INT 16 data set */
{
	int16 max;
	int16 min;
	int16 *hscale;
	int16 *vscale;
	int16 *dscale;
};

struct int32set /* variables for an INT 32 data set */
{
  int32 max;
  int32 min;
  int32 *hscale;
  int32 *vscale;
  int32 *dscale;
};

struct fp64set /* variables for a FLOAT 64 data set */
{
	float64 max;
	float64 min;
	float64 *hscale;
	float64 *vscale;
	float64 *dscale;
};

struct int8set /* variables for an INT 8 data set */
{
	int8 max;
	int8 min;
	int8 *hscale;
	int8 *vscale;
	int8 *dscale;
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
      int 	      is_int32;	    /* 32-bit int */
      int 	      is_int16;     /* 16-bit int */
      int         rank;         /* number of input data dimensions */
      int         dims[3];      /* input dimensions - ncols, nrows, nplanes */
      int         is_vscale;    /* vertical axis scales in the input */
      int         is_hscale;    /* horizontal axis scales in the input */
      int         is_dscale;    /* depth axis scales in the input */
      float32     max;          /* maximum value of the data */
      float32     min;          /* minimum value of the data */
      float32    *hscale;       /* horizontal scales for fp32*/
      float32    *vscale;       /* vertical scales for fp32*/
      float32    *dscale;       /* depth scales for fp32*/
      struct int32set	in32s;
      struct int16set	in16s;
      struct int8set	in8s;
      struct fp64set	fp64s;
      VOIDP       data;         /* input data */
	  int 	      outtype;
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
 *  constants to represent data types
 */

#define FP_32 0
#define FP_64 1
#define INT_32 2
#define INT_16 3
#define INT_8 4
#define NO_NE 5


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
#define OPT_t   10  /* datatype of the SDS to be written */ 
#define OPT_n   11  /* for  a FLOAT 64 binary input file to be accepted as FLOAT 64 SDS (default behaviour is writing it as FLOAT 32 SDS */
#define ERR 20  /* invalid token */

/*
 * state table for parsing the command line.
 */
static int  state_table[19][12] =
{

    /* token ordering:
       FILNAME      OPT_o   OPT_r   OPT_e   OPT_i   NUMBR   OPT_p   OPT_f
       OPT_h        OPT_m   OPT_z */

    /* state 0: start */
    {1, ERR, ERR, ERR, ERR, ERR, ERR, ERR,
    14, ERR, ERR, ERR},

    /* state 1: input files */
    {1, 2, ERR, ERR, ERR, ERR, ERR, ERR,
    ERR, ERR, 17, 18},

    /* state 2: -o[utfile] */
    {3, ERR, ERR, ERR, ERR, ERR, ERR, ERR,
    ERR, ERR, ERR, ERR},

    /* state 3: outfile */
    {ERR, ERR, 4, ERR, ERR, ERR, ERR, 13,
    ERR, ERR, ERR, ERR},

    /* state 4: -r[aster] */
    {ERR, ERR, ERR, 5, 9, ERR, 10, 12,
    ERR, 15, ERR, ERR},

    /* state 5: -e[xpand] */
    {ERR, ERR, ERR, ERR, ERR, 6, ERR, ERR,
    ERR, ERR, ERR, ERR},

    /* state 6: -e[xpand] or -i[nterp] option argument */
    {ERR, ERR, ERR, ERR, ERR, 7, ERR, ERR,
    ERR, ERR, ERR, ERR},

    /* state 7: -e[xpand] or -i[nterp] option argument */
    {ERR, ERR, ERR, ERR, ERR, 8, 10, 12,
    ERR, 15, ERR, ERR},

    /* state 8: -e[xpand] or -i[nterp] option argument */
    {ERR, ERR, ERR, ERR, ERR, ERR, 10, 12,
    ERR, 15, ERR, ERR},

    /* state 9: -i[nterp] */
    {ERR, ERR, ERR, ERR, ERR, 6, ERR, ERR,
    ERR, ERR, ERR, ERR},

    /* state 10: -p[alfile] */
    {11, ERR, ERR, ERR, ERR, ERR, ERR, ERR,
    ERR, ERR, ERR, ERR},

    /* state 11: palfile */
    {ERR, ERR, ERR, 5, 9, ERR, ERR, 12,
    ERR, 15, ERR, ERR},

    /* state 12: -f[loat] (after -r[aster]) */
    {ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR,
    ERR, ERR, ERR, ERR},

    /* state 13: -f[loat] */
    {ERR, ERR, 4, ERR, ERR, ERR, ERR, ERR,
    ERR, ERR, ERR, ERR},

    /* state 14: -h[elp] */
    {ERR, ERR, ERR, ERR, ERR, ERR, ERR, ERR,
    ERR, ERR, ERR, ERR},

    /* state 15: -m[ean] */
    {ERR, ERR, ERR, ERR, ERR, 16, ERR, ERR,
    ERR, ERR, ERR, ERR},

    /* state 16: mean */
    {ERR, ERR, ERR, 5, 9, ERR, 10, 12,
     ERR, ERR, ERR, ERR},

    /* state 17: output type for data set */
    {1, 2, ERR, ERR, ERR, ERR, ERR, ERR,                     
    ERR, ERR, ERR, ERR},
    
     /* state 18: override default behaviour for FP 64 */
    {1, 2, ERR, ERR, ERR, ERR, ERR, ERR,                     
    ERR, ERR, ERR, ERR}
    
    
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
 * Additional functions defined to incorporate the revisions (pkamat)
 */
static int  gfloat64(char *infile, FILE * strm, float64 *fp64, struct Input *in);
static int  gint32(char *infile, FILE * strm, int32 *ival, struct Input *in);
static int  gint16(char *infile, FILE * strm, int16 *ival, struct Input *in);
static int  gint8(char *infile, FILE * strm, int8 *ival, struct Input *in);
static int  init_scales(struct Input * in);
void        fpdeallocate(struct Input *in, struct Raster *im, struct Options *opt);

/*
 * Name:
 *      main
 *
 * Purpose:
 *      The driver for "hdfimport".
 *
 *  Revision (pkamat):
 *		Changes to the state table to handle -t option and the -n option.
 *      Also, a different structure used for holding input files.
 */
int
main(int argc, char *argv[])
{
    struct Options opt;
    int         i,k;
    int         outfile_named = FALSE;
    int         token;
    int         state = 0;
    int flag = 0;
    char types [5][6] = { "FP32", "FP64", "INT32", "INT16", "INT8" };	

    const char *err1 = "Invalid number of arguments:  %d.\n";
    const char *err2 = "Error in state table.\n";
    const char *err3 = "No output file given.\n";
    const char *err4 = "Program aborted.\n";
    /* const char *err5 = "Cannot allooacte memory.\n"; */
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
	      (void) HDstrcpy(opt.infiles[opt.fcount].filename, argv[i]);
	      opt.infiles[opt.fcount].outtype = NO_NE;
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
	    case 17:  /* -t found */
	      i++;
	      flag = 0;
	      for (k=0; ((k<=4) && (!flag)); k++)
		if (!strcmp(argv[i], types[k])) flag = 1;
	      if (flag)
		opt.infiles[opt.fcount-1].outtype = k-1;		
	      else
		{
		  usage(argv[0]);
		  goto err;
		}
	      break;
	    case 18: /* -n found */
	      opt.infiles[opt.fcount-1].outtype = FP_64;
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
 *
 * Revision(pkamat):
 *      Modified to read in data of type INT 32, INT 16, INT 8 
 *      in addition to FP 32 and FP 64.
 */
static int
gdata(char *infile, struct Input *in, FILE *strm, int *is_maxmin)
{
    int32       i, j, k;
    float32    *fp32;
    int32        *in32;
    int16 *in16;
    float64 *fp64;
    int8 *in8;
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
	    if (in->outtype == FP_32)
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
		if (*is_maxmin == FALSE)
		  {
		    in->min = in->max = *(float32*) in->data;
		    for (i = 1; i< len; i++)
		      {
			if (((float32 *) in->data)[i] > in->max)
			  in->max = ((float32 *) in->data)[i];
			if (((float32*) in->data)[i] < in->min)
			  in->min = ((float32*) in->data)[i];
		      }
		    *is_maxmin = TRUE;
		  }
	      }
	    if (in->outtype == INT_32)
	      {
		for (k = 0, in32 = (int32 *) in->data; k < in->dims[2] ; k++)
		  {
		    for (j= 0; j < in->dims[1]; j++)
		      {
			for (i =0; i < in->dims[0]; i++, in32++)
			  {
			    if (gint32(infile, strm, in32, in))
			      {
				(void) fprintf(stderr, err1, infile);
				goto err;
			      }
			  }
		      }
		  }
		if (*is_maxmin == FALSE)
		  {
		    in->in32s.min = in->in32s.max = *(int32 *) in->data;
		    for (i = 1; i<len; i++)
		      {
			if (((int32 *) in->data)[i] > in->in32s.max)
			  in->in32s.max = ((int32 *) in->data)[i];
			if (((int32 *) in->data)[i] < in->in32s.min)
			  in->in32s.min = ((int32 *) in->data)[i];
		      }
		    *is_maxmin = TRUE;
		  }
	      }
	    if (in->outtype == INT_16)
	      {
		for (k = 0, in16 = (int16 *) in->data; k < in->dims[2] ; k++)
		  {
		    for (j= 0; j < in->dims[1]; j++)
		      {
			for (i =0; i < in->dims[0]; i++, in16++)
			  {
			    if (gint16(infile, strm, in16, in))
			      {
				(void) fprintf(stderr, err1, infile);
				goto err;
			      }
			  }
		      }
		  }
		if (*is_maxmin == FALSE)
		  {
		    in->in16s.min = in->in16s.max = *(int16 *) in->data;
		    for (i = 1; i<len; i++)
		      {
			if (((int16 *) in->data)[i] > in->in16s.max)
			  in->in16s.max = ((int16 *) in->data)[i];
			if (((int16 *) in->data)[i] < in->in16s.min)
			  in->in16s.min = ((int16 *) in->data)[i];
		      }
		    *is_maxmin = TRUE;
		  }
	      }
	    
	    if (in->outtype == INT_8)
	      {
		for (k = 0, in8 = (int8 *) in->data; k < in->dims[2] ; k++)
		  {
		    for (j= 0; j < in->dims[1]; j++)
		      {
			for (i =0; i < in->dims[0]; i++, in8++)
			  {
			    if (gint8(infile, strm, in8, in))
			      {
				(void) fprintf(stderr, err1, infile);
				goto err;
			      }
			  }
		      }
		  }
		if (*is_maxmin == FALSE)
		  {
		    in->in8s.min = in->in8s.max = *(int8 *) in->data;
		    for (i = 1; i<len; i++)
		      {
			if (((int8 *) in->data)[i] > in->in8s.max)
			  in->in8s.max = ((int8 *) in->data)[i];
			if (((int8 *) in->data)[i] < in->in8s.min)
			  in->in8s.min = ((int8 *) in->data)[i];
		      }
		    *is_maxmin = TRUE;
		  }
	      }

	    if (in->outtype == FP_64)
	      {
		for (k = 0, fp64 = (float64 *) in->data; k < in->dims[2]; k++)
		  {
		    for (j = 0; j < in->dims[1]; j++)
		      {
			for (i = 0; i < in->dims[0]; i++, fp64++)
			  {
                            if (gfloat64(infile, strm, fp64, in))
                              {
                                  (void) fprintf(stderr, err1, infile);
                                  goto err;
                              }
			  }
		      }
		  }
		if (*is_maxmin == FALSE)
		  {
		    in->fp64s.min = in->fp64s.max = *(float64*) in->data;
		    for (i = 1; i< len; i++)
		      {
			if (((float64 *) in->data)[i] > in->fp64s.max)
			  in->fp64s.max = ((float64*) in->data)[i];
			if (((float64*) in->data)[i] < in->fp64s.min)
			  in->fp64s.min = ((float64*) in->data)[i];
		      }
		    *is_maxmin = TRUE;
		  }
	      }
	    
	    /* } */
          (void) fclose(strm);
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
           * get the rank and dimensions from files of other input formats
           * 
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
 * Name: (pkamat - New function)
 *      gfloat64
 *
 * Purpose:
 *      Read in a double floating point value from the input stream.  The
 *      input format may either be ASCII text ,
 *      or 64-bit native floating point.
 */

static int
gfloat64(char *infile, FILE * strm, float64 *fp64, struct Input *in)
{
    const char *err1 = "Unable to get 'float' value from file: %s.\n";
    
    if (in->is_text == TRUE)
      {
	if (fscanf(strm, "%le", fp64) != 1)
	  {
	    (void) fprintf(stderr, err1, infile);
	    goto err;
	  }
      }
    else
      {
	if (fread((char *) fp64, sizeof(float64), 1, strm) != 1)
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
 * Name: (pkamat - New function)
 *      gint32
 *
 * Purpose:
 *      Read in a single 32-bit integer value from the input stream.  The input
 *      format may either be ASCII text or a native BCD of type integer.
 */

static int
gint32(char *infile, FILE * strm, int32 *ival, struct Input *in)
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
          if (fread((char *) ival, sizeof(int32), 1, strm) != 1)
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
 * Name: (pkamat - New function)
 *      gint16
 *
 * Purpose:
 *      Read in a single 16-bit integer value from the input stream.  The input
 *      format may either be ASCII text or a native BCD of type 16-bit integer.
 */

static int
gint16(char *infile, FILE * strm, int16 *ival, struct Input *in)
{
    const char *err1 = "Unable to get 'int' value from file: %s.\n";
     
    if (in->is_text == TRUE)
      {
	if (fscanf(strm, "%hd", ival) != 1)
	  {
	    (void) fprintf(stderr, err1, infile);
	    goto err;
	  }
      }             
    
 else
 { 
     if (fread((char *) ival, sizeof(int16), 1, strm) != 1)
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
 * Name: (pkamat - New function)
 *      gint8
 *
 * Purpose:
 *      Read in a single 8-bit integer value from the input stream.  The input
 *      format may either be ASCII text or a native BCD of type 8-bit integer.
 */

static int
gint8(char *infile, FILE * strm, int8 *ival, struct Input *in)
{
    const char *err1 = "Unable to get 'int' 8  value from file: %s.\n";
    int16 temp;
    
    if (in->is_text == TRUE)
      {
	if (fscanf(strm, "%hd", &temp) != 1)
	  {
	    (void) fprintf(stderr, err1, infile);
	    goto err;
	  }
	*ival = (int8) temp;
      }
    else
      { 
	if (fread((char *) ival, sizeof(int8), 1, strm) != 1)
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
 *      Supports 32-bit integer, 16-bit integer, 8-bit integer, 32-bit float, 64-bit float
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
	if (in->outtype == FP_32)
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
	if (in->outtype == FP_64)
	  {
	      if (gfloat64(infile, strm, &in->fp64s.max, in))
		{
		  (void) fprintf(stderr, err1, infile);
		  goto err;
		}
	      if (gfloat64(infile, strm, &in->fp64s.min, in))
		{
		  (void) fprintf(stderr, err1, infile);
		  goto err;
		}
	      if (in->fp64s.max > in->fp64s.min)
		*is_maxmin = TRUE;
	  }
	if (in->outtype == INT_32)
	  {
	    if (gint32(infile, strm, &in->in32s.max, in))
		{
		  (void) fprintf(stderr, err1, infile);
		  goto 	err;
		}
	      if (gint32(infile, strm, &in->in32s.min, in))
		{
		  (void) fprintf(stderr, err1, infile);
		  goto err;
		}
	      if (in->in32s.max > in->in32s.min)
		*is_maxmin = TRUE;
	  }
	
	if (in->outtype == INT_16)
	  {
	    if (gint16(infile, strm, &in->in16s.max, in))
	      {
		(void) fprintf(stderr, err1, infile);
		goto 	err;
	      }
	    if (gint16(infile, strm, &in->in16s.min, in))
	      {
		(void) fprintf(stderr, err1, infile);
		goto err;
	      }
	    if (in->in16s.max > in->in16s.min)
	      *is_maxmin = TRUE;
	  }
	  
	if (in->outtype == INT_8)
	  {
	    if (gint8(infile, strm, &in->in8s.max, in))
	      {
		(void) fprintf(stderr, err1, infile);
		goto 	err;
	      }
	    if (gint8(infile, strm, &in->in8s.min, in))
		{
		  (void) fprintf(stderr, err1, infile);
		  goto err;
		}
	    if (in->in8s.max > in->in8s.min)
	      *is_maxmin = TRUE;
	  }
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
 *
 * Revision: (pkamat)
 *      Modified to support 32-bit integer, 16-bit integer, 8-bit integer in
 *		addition to 32-bit float and 64-bit float
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
	switch(in->outtype)
	  {
	  case 0: /* 32-bit float */
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
	    break;
	    
	  case 1: /* 64-bit float */
	    if (in->rank == 2)
		  {
		    for (i = 0; i < hdfdims[0]; i++)
		      {
			if (gfloat64(infile, strm, &in->fp64s.vscale[i], in))
			  {
			    (void) fprintf(stderr, err1, infile);
			    goto err;
			  }
		      }
		    in->fp64s.vscale[i] = in->fp64s.vscale[i - 1];     
		    for (i = 0; i < hdfdims[1]; i++)
		      {
			if (gfloat64(infile, strm, &in->fp64s.hscale[i], in))
			  {
			    (void) fprintf(stderr, err1, infile);
			    goto err;
			  }
		      }
		    in->fp64s.hscale[i] = in->fp64s.hscale[i - 1];
		  }
	    else
	      {
		for (i = 0; i < hdfdims[0]; i++)
		  {
		    if (gfloat64(infile, strm, &in->fp64s.dscale[i], in))
		      {
			(void) fprintf(stderr, err1, infile);
			goto err;
		      }
		  }
		in->fp64s.dscale[i] = in->fp64s.dscale[i - 1];

		for (i = 0; i < hdfdims[1]; i++)
		  {
			if (gfloat64(infile, strm, &in->fp64s.vscale[i], in))
			  {
			    (void) fprintf(stderr, err1, infile);
			    goto err;
			  }
		  }
		in->fp64s.vscale[i] = in->fp64s.vscale[i - 1];

		for (i = 0; i < hdfdims[2]; i++)
		  {
		    if (gfloat64(infile, strm, &in->fp64s.hscale[i], in))
		      {
			(void) fprintf(stderr, err1, infile);
			goto err;                 	
		      }
		  }
		in->fp64s.hscale[i] = in->fp64s.hscale[i - 1];
	      }
	    break;
		
	  case 2: /* 32-bit integer */
	    if (in->rank == 2)
	      {
		for (i = 0; i < hdfdims[0]; i++)
		  {
		    if (gint32(infile, strm, &in->in32s.vscale[i], in))
		      {
			(void) fprintf(stderr, err1, infile);
			    goto err;
		      }
		    
		  }
		in->in32s.vscale[i] = in->in32s.vscale[i - 1];
		for (i = 0; i < hdfdims[1]; i++)
		      {
			if (gint32(infile, strm, &in->in32s.hscale[i], in))
			  {
			    (void) fprintf(stderr, err1, infile);
			    goto err;
			  }
		      }
		in->in32s.hscale[i] = in->in32s.hscale[i - 1];
	      }
	    
	    else
	      {
		    for (i = 0; i < hdfdims[0]; i++)
		      {
			if (gint32(infile, strm, &in->in32s.dscale[i], in))
			  {
			    (void) fprintf(stderr, err1, infile);
			    goto err;
			  }
		      }
		    in->in32s.dscale[i] = in->in32s.dscale[i - 1];
		    for (i = 0; i < hdfdims[1]; i++)
		      {
			if (gint32(infile, strm, &in->in32s.vscale[i], in))
			  {
			    (void) fprintf(stderr, err1, infile);
			    goto err;
			  }
		      }
		    in->in32s.vscale[i] = in->in32s.vscale[i - 1];
		    for (i = 0; i < hdfdims[2]; i++)
		      {
			if (gint32(infile, strm, &in->in32s.hscale[i], in))
			  {
			    (void) fprintf(stderr, err1, infile);
			    goto err;                 	
			  }
		      }
		    in->in32s.hscale[i] = in->in32s.hscale[i - 1];
	      }
		break;

	  case 3: /* 16-bit integer */
		if (in->rank == 2)
		  {
		    for (i = 0; i < hdfdims[0]; i++)
		      {
			if (gint16(infile, strm, &in->in16s.vscale[i], in))
			  {
			    (void) fprintf(stderr, err1, infile);
			    goto err;
			  }
		      }
		    in->in16s.vscale[i] = in->in16s.vscale[i - 1];
		    for (i = 0; i < hdfdims[1]; i++)
		      {
			if (gint16(infile, strm, &in->in16s.hscale[i], in))
			  {
			    (void) fprintf(stderr, err1, infile);
			    goto err;
			  }
		      }
		    in->in16s.hscale[i] = in->in16s.hscale[i - 1];
		  }
		
		else
		  {
		    for (i = 0; i < hdfdims[0]; i++)
		      {
			if (gint16(infile, strm, &in->in16s.dscale[i], in))
			  {
			    (void) fprintf(stderr, err1, infile);
			    goto err;
			  }
		      }
		    in->in16s.dscale[i] = in->in16s.dscale[i - 1];
		    for (i = 0; i < hdfdims[1]; i++)
		      {
			if (gint16(infile, strm, &in->in16s.vscale[i], in))
			  {
			    (void) fprintf(stderr, err1, infile);
			    goto err;
			  }
		      }
		    in->in16s.vscale[i] = in->in16s.vscale[i - 1];
		    for (i = 0; i < hdfdims[2]; i++)
		      {
			if (gint16(infile, strm, &in->in16s.hscale[i], in))
			  {
			    (void) fprintf(stderr, err1, infile);
			    goto err;                 	
			  }
		      }
		    in->in16s.hscale[i] = in->in16s.hscale[i - 1];
		  }
		break;

	      case 4: /* 8-bit integer */
		if (in->rank == 2)
		  {
		    for (i = 0; i < hdfdims[0]; i++)
		      {
			if (gint8(infile, strm, &in->in8s.vscale[i], in))
			  {
			    (void) fprintf(stderr, err1, infile);
			    goto err;
			  }
		      }
		    in->in8s.vscale[i] = in->in8s.vscale[i - 1];
		    for (i = 0; i < hdfdims[1]; i++)
		      {
			if (gint8(infile, strm, &in->in8s.hscale[i], in))
			  {
			    (void) fprintf(stderr, err1, infile);
			    goto err;
			  }
		      }
		    in->in8s.hscale[i] = in->in8s.hscale[i - 1];
		  }
		
		else
		  {
		    for (i = 0; i < hdfdims[0]; i++)
		      {
			if (gint8(infile, strm, &in->in8s.dscale[i], in))
			  {
			    (void) fprintf(stderr, err1, infile);
			    goto err;
			  }
		      }
		    in->in8s.dscale[i] = in->in8s.dscale[i - 1];
		    for (i = 0; i < hdfdims[1]; i++)
		      {
			if (gint8(infile, strm, &in->in8s.vscale[i], in))
			  {
			    (void) fprintf(stderr, err1, infile);
			    goto err;
			  }
		      }
		    in->in8s.vscale[i] = in->in8s.vscale[i - 1];
		    for (i = 0; i < hdfdims[2]; i++)
		      {
			if (gint8(infile, strm, &in->in8s.hscale[i], in))
			  {
			    (void) fprintf(stderr, err1, infile);
			    goto err;                 	
			  }
		      }
		    in->in8s.hscale[i] = in->in8s.hscale[i - 1];
		  }
		break;
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
			    case 'n':                 
                        token = OPT_n;
	                break;
				case 't':
 					    token = OPT_t;	
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
 *      Determine the type of the input file (HDF, TEXT, FP32, FP64)
 *
 * Revision: (pkamat)
 *		Modified to support INT32, INT16, INT8 formats.
 *      Also determines and validates the outtype type of the data-set
 */
static int
gtype(char *infile, struct Input *in, FILE **strm)
{
    char        buf[8];

    const char *err1 = "Unable to open file: %s.\n";
    const char *err2 = "Unable to get format tag from file: %s.\n";
    const char *err3 = "Invalid file format in file: %s.\n";
    const char *err4 = "Invalid use of -t or -n options. Can be used only for TEXT files or for FP64 binary files\n";

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
	if (!HDmemcmp("TEXT", buf, 4) || !HDmemcmp("text", buf, 4))
	  {
	    in->is_text = TRUE;
	    if (in->outtype == NO_NE)
	      in->outtype = FP_32;
	    
	  }
	else
	  {
	    if (!HDmemcmp("FP64", buf, 4) ||
		     !HDmemcmp("fp64", buf, 4))
	      {
			in->is_fp64 = TRUE;
			if (in->outtype != FP_64)
				if (in->outtype != NO_NE)
				{
					(void) fprintf(stderr, err4, infile);
					goto err;		
				}
				else in->outtype = FP_32;
	      }
	    else
	      {
		if (in->outtype != NO_NE)
		  {   
		    (void) fprintf(stderr, err4, infile);
		    goto err;		
		  }
		if (!HDmemcmp("FP32", buf, 4) || !HDmemcmp("fp32", buf, 4))
		  {
		    in->is_fp32 = TRUE;
		    in->outtype = FP_32;
		
		  }
		
		else if (!HDmemcmp("IN32", buf, 4) ||
                         !HDmemcmp("in32", buf, 4))
		  in->outtype = INT_32;
		else if (!HDmemcmp("IN16", buf, 4) ||
			 !HDmemcmp("in16", buf, 4))
		  in->outtype = INT_16;
		else if (!HDmemcmp("IN08", buf, 4) ||
		       !HDmemcmp("in08", buf, 4))
		  in->outtype = INT_8;
		else
		  {
		    (void) fprintf(stderr, err3, infile);
		    goto err;
		  }
		if (in->outtype == NO_NE)
		  {   
		    (void) fprintf(stderr, err4, infile);
		    goto err;
		  }
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
    (void) printf("\t%s (previously fp2hdf)\n\n", name);
    (void) printf("Purpose:\n");
    (void) printf("\tTo convert floating point data to HDF Scientific ");
    (void) printf("Data Set (SDS)\n");
    (void) printf("\tand/or 8-bit Raster Image Set (RIS8) format, ");
    (void) printf("storing the results\n");
    (void) printf("\tin an HDF file.  The image data can be scaled ");
    (void) printf("about a mean value.\n\n");
    (void) printf("Version:\n");
    (void) printf("\tv1.2 (Jan 10, 2002)\n\n");

    (void) fprintf (stderr, "Synopsis:");
    (void) fprintf (stderr, "\n\t%s -h[elp], OR", name);
    (void) fprintf (stderr, "\n\t%s <infile> [ [-t[ype] <output-type> | -n] [<infile> [-t[ype] <output-type> | -n]...]", name);
    (void) fprintf (stderr, "\n\t\t\t\t\t-o[utfile] <outfile> [-r[aster] [ras_opts ...]] [-f[loat]]");

    (void) fprintf (stderr, "\n\n\t-h[elp]:");
    (void) fprintf (stderr, "\n\t\tPrint this summary of usage, and exit.");
    (void) fprintf (stderr, "\n\t\t");
    (void) fprintf (stderr, "\n\n\t<infile(s)>:");
    (void) fprintf (stderr, "\n\t\tName of the input file(s), containing a single ");
    (void) fprintf (stderr, "\n\t\ttwo-dimensional or three-dimensional floating point array ");
    (void) fprintf (stderr, "\n\t\tin either ASCII text, native floating point, native integer ");
    (void) fprintf (stderr, "\n\t\tor HDF SDS format.  If an HDF file is used for input, it ");
    (void) fprintf (stderr, "\n\t\tmust contain an SDS. The SDS need only contain a dimension ");
    (void) fprintf (stderr, "\n\t\trecord and the data, but if it also contains maximum and ");
    (void) fprintf (stderr, "\n\t\tminimum values and/or scales for each axis, these will ");
    (void) fprintf (stderr, "\n\t\tbe used.  If the input format is ASCII text or native ");
    (void) fprintf (stderr, "\n\t\tfloating point or native integer, see \"Notes\" below on ");
    (void) fprintf (stderr, "\n\t\thow it must be organized.");

    (void) fprintf (stderr, "\n\n\t-t[ype] <output_type>: ");
    (void) fprintf (stderr, "\n\t\tOptionally used for every input ASCII file to specify the ");
    (void) fprintf (stderr, "\n\t\tdata type of the data-set to be written. If not specified               ");
    (void) fprintf (stderr, "\n\t\tdefault data type is 32-bit floating point. <output-type>");
    (void) fprintf (stderr, "\n\t\tcan be any of the following: FP32 (default), FP64, INT32");
    (void) fprintf (stderr, "\n\t\tINT16, INT8. It can be used only with ASCII files.");

    (void) fprintf (stderr, "\n\n\t-n:  ");
    (void) fprintf (stderr, "\n\t\tThis option is to be used only if the binary input file ");
    (void) fprintf (stderr, "\n\t\tcontains 64-bit floating point data and the default");
    (void) fprintf (stderr, "\n\t\tbehaviour (default behaviour is to write it to a 32-bit");
    (void) fprintf (stderr, "\n\t\tfloating point data-set) should be overridden to write ");
    (void) fprintf (stderr, "\n\t\tit to a 64-bit floating point data-set.");

    (void) fprintf (stderr, "\n\n\t-o[utfile] <outfile>:");
    (void) fprintf (stderr, "\n\t\tData from one or more input files are stored as one or");
    (void) fprintf (stderr, "\n\t\tmore data sets and/or images in one HDF output file,");
    (void) fprintf (stderr, "\n\t\t\"outfile\".");

    (void) fprintf (stderr, "\n\n\t-r[aster]:");
    (void) fprintf (stderr, "\n\t\tStore output as a raster image set in the output file.");

    (void) fprintf (stderr, "\n\n\t-f[loat]:");
    (void) fprintf (stderr, "\n\tStore output as a scientific data set in the output file.");
    (void) fprintf (stderr, "\n\tThis is the default if the \"-r\" option is not specified.");

    (void) fprintf (stderr, "\n\n\tras_opts ...");
    (void) fprintf (stderr, "\n\n\t-e[xpand] <horiz> <vert> [<depth>]:");
    (void) fprintf (stderr, "\n\tExpand float data via pixel replication to produce the");
    (void) fprintf (stderr, "\n\timage(s).  \"horiz\" and \"vert\" give the horizontal and");
    (void) fprintf (stderr, "\n\tvertical resolution of the image(s) to be produced; and");
    (void) fprintf (stderr, "\n\toptionally, \"depth\" gives the number of images or depth");
    (void) fprintf (stderr, "\n\tplanes (for 3D input data).");

    (void) fprintf (stderr, "\n\n\t-i[nterp] <horiz> <vert> [<depth>]:");
    (void) fprintf (stderr, "\n\t\tApply bilinear, or trilinear, interpolation to the float");
    (void) fprintf (stderr, "\n\t\tdata to produce the image(s).  \"horiz\", \"vert\", and \"depth\"");
    (void) fprintf (stderr, "\n\t\tmust be greater than or equal to the dimensions of the");
    (void) fprintf (stderr, "\n\t\toriginal dataset.");
    (void) fprintf (stderr, "\n\t\tIf max and min are supplied in input file, this option clips");
    (void) fprintf (stderr, "\n\t\tvalues that are greater than max or less then min, setting");
    (void) fprintf (stderr, "\n\t\tthem to the max and min, respectively.");

    (void) fprintf (stderr, "\n\n\t-p[alfile] <palfile>:");
    (void) fprintf (stderr, "\n\t\tStore the palette with the image.  Get the palette from");
    (void) fprintf (stderr, "\n\t\t\"palfile\"; which may be an HDF file containing a palette,");
    (void) fprintf (stderr, "\n\t\tor a file containing a raw palette.");

    (void) fprintf (stderr, "\n\n\t-m[ean] <mean>:");
    (void) fprintf (stderr, "\n\t\tIf a floating point mean value is given, the image will be");
    (void) fprintf (stderr, "\n\t\tscaled about the mean.  The new extremes (newmax and newmin),");
    (void) fprintf (stderr, "\n\t\tas given by:");

    (void) fprintf (stderr, "\n\n\t\t\tnewmax = mean + max(abs(max-mean), abs(mean-min))");
    (void) fprintf (stderr, "\n\t\t\tnewmin = mean - max(abs(max-mean), abs(mean-min))");

    (void) fprintf (stderr, "\n\n\t\twill be equidistant from the mean value.  If no mean value");
    (void) fprintf (stderr, "\n\t\tis given, then the mean will be:  0.5   (max + min)");

    (void) fprintf (stderr, "\n\n\tNotes:");
    (void) fprintf (stderr, "\n\t\tIf the input file format is ASCII text or native floating point or native integer(32-bit,");
    (void) fprintf (stderr, "\n\t\t16-bit, 8-bit), it");
    (void) fprintf (stderr, "\n\t\tmust have the following input fields:");

    (void) fprintf (stderr, "\n\t\tformat");
    (void) fprintf (stderr, "\n\t\tnplanes");
    (void) fprintf (stderr, "\n\t\tnrows");
    (void) fprintf (stderr, "\n\t\tcols");
    (void) fprintf (stderr, "\n\t\tmax_value");
    (void) fprintf (stderr, "\n\t\tmin_value");
    (void) fprintf (stderr, "\n\t\t[plane1 plane2 plane3 ...]");
    (void) fprintf (stderr, "\n\t\trow1 row2 row3 ...");
    (void) fprintf (stderr, "\n\t\tcol1 col2 col3 ...");
    (void) fprintf (stderr, "\n\t\tdata1 data2 data3 ...");

    (void) fprintf (stderr, "\n\n\t\tWhere:");
    (void) fprintf (stderr, "\n\n\t\tformat:");
    (void) fprintf (stderr, "\n\t\t\tFormat designator (\"TEXT\", \"FP32\", \"FP64\", \"IN32\", \"IN16\", \"IN08\").");
    (void) fprintf (stderr, "\n\t\t\tnplanes, nrows, ncols:");
    (void) fprintf (stderr, "\n\t\t\tDimensions are specified in the order slowest changing dimension first.");
    (void) fprintf (stderr, "\n\t\t\tncols is dimension of the fastest changing dimension. (horizontal axis");
    (void) fprintf (stderr, "\n\t\t\tor X-axis in a 3D scale)");
    (void) fprintf (stderr, "\n\t\t\tnrows corresponds to dimension of the vertical axis or Y-axis in a 3D ");
    (void) fprintf (stderr, "\n\t\t\tscale.");
    (void) fprintf (stderr, "\n\t\t\tnplanes corresponds to the slowest changing dimension i.e. dimension of ");
    (void) fprintf (stderr, "\n\t\t\tthe depth axis or the Z-axis in a 3D scale (\"1\" for 2D input).");
    (void) fprintf (stderr, "\n\t\tmax_value:");
    (void) fprintf (stderr, "\n\t\t\tMaximum data value.");
    (void) fprintf (stderr, "\n\t\tmin_value:");
    (void) fprintf (stderr, "\n\t\t\tMinimum data value.");
    (void) fprintf (stderr, "\n\t\tplane1, plane2, plane3, ...:");
    (void) fprintf (stderr, "\n\t\t\tScales for depth axis.");
    (void) fprintf (stderr, "\n\t\trow1, row2, row3, ...:");
    (void) fprintf (stderr, "\n\t\t\tScales for the vertical axis.");
    (void) fprintf (stderr, "\n\t\tcol1, col2, col3, ...:");
    (void) fprintf (stderr, "\n\t\t\tScales for the horizontal axis.");
    (void) fprintf (stderr, "\n\t\tdata1, data2, data3, ...:");
    (void) fprintf (stderr, "\n\t\t\tThe data ordered by rows, left to right and top");
    (void) fprintf (stderr, "\n\t\t\tto bottom; then optionally, ordered by planes,");
    (void) fprintf (stderr, "\n\t\t\tfront to back.");

    (void) fprintf (stderr, "\n\n\t\tFor FP32 and FP64 input format, \"format\", \"nplanes\", \"nrows\", \"ncols\",");
    (void) fprintf (stderr, "\n\t\tand \"nplanes\" are native integers; where \"format\" is the integer");
    (void) fprintf (stderr, "\n\t\trepresentation of the appropriate 4-character string (0x46503332 for");
    (void) fprintf (stderr, "\n\t\t\"FP32\" and 0x46503634 for \"FP64\").  The remaining input fields are");
    (void) fprintf (stderr, "\n\t\tcomposed of native 32-bit floating point values for FP32 input format,");
    (void) fprintf (stderr, "\n\t\tor native 64-bit floating point values for FP64 input format.");

    (void) fprintf (stderr, "\n\n\tFor IN32, IN16 and IN08 input format, \"format\", \"nplanes\", \"nrows\", \"ncols\",");
    (void) fprintf (stderr, "\n\t\tand \"nplanes\" are native integers; where \"format\" is the integer");
    (void) fprintf (stderr, "\n\t\trepresentation of the appropriate 4-character string. The remaining input ");
    (void) fprintf (stderr, "\n\t\tfields are composed of native 32-bit integer values for IN32 input format,");
    (void) fprintf (stderr, "\n\t\tor native 16-bit integer values for IN16 input format or native 8-bit ");
    (void) fprintf (stderr, "\n\t\tinteger values for IN08 input format.");

    (void) printf("\nExamples:\n");
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
 *
 * Revision: (pkamat)
 *		Modified to support the writing of the data set in any of the
 *		following types: INT32, INT16, INT8 and FP64
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
    int32       len;
    FILE       *strm;
    int32       hdf;
    int32 	    sd_id, sds_id;
    int32 	    start3[3], edges3[3], start2[2], edges2[2];
    int32       dim_index = 0, dim_id;
    
#ifdef  DEBUG
    int         h, v, d;
#endif /* DEBUG */

    const char *err1 = "Error creating HDF output file: %s.\n";
    const char *err1a = "Error opening the created HDF output file for writing.\n";
    const char *err2 = "Unable to dynamically allocate memory.\n";
    const char *err3a = "Warning: cannot make image smaller using -e ";
    const char *err3b = "option.\n\t %s resolution will be made the ";
    const char *err3c = "same as %s dimension of the\n\t dataset, ";
    const char *err3d = "which is: %d.\n\n";
    const char *err4 = "Unable to write an RIS8 to the HDF output file\n";
    const char *err5 = "Unable to write an SDS to the HDF output file\n";
    const char *err6 = "Unable to close the HDF output file\n";
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

	/* new interface */
    if ((sd_id = SDstart(opt->outfile, DFACC_WRITE)) == FAIL)
      {
	 (void) fprintf(stderr, err1a, opt->outfile);
          goto err;
      }

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
	      
	      in.outtype = opt->infiles[i].outtype;
          is_maxmin = FALSE;
          is_scale = FALSE;

          /*
           * get the file type, input data dimensions, and input data
           * max/min values
           */
	     
	  if (gtype(opt->infiles[i].filename, &in, &strm))
	    goto err;

	  if (gdimen(opt->infiles[i].filename, &in, strm))
	    goto err;
	  
	  if (gmaxmin(opt->infiles[i].filename, &in, strm, &is_maxmin))
	    goto err;

          /*
           * initialise the scale variables according to the output type of data set
           */

	  if (init_scales(&in))
	    goto err;

	      /*
           * get the scale for each axis
           */
      if (gscale(opt->infiles[i].filename, &in, strm, &is_scale))
              goto err;

          /*
           * get the input data
           */
      len = in.dims[0] * in.dims[1] * in.dims[2];
	
	  switch(in.outtype)
	    {
	    case 0: /* 32-bit float */
	      if ((in.data = (VOIDP) HDmalloc((size_t) len * sizeof(float32))) == NULL)
		{
		  (void) fprintf(stderr, err2);
		  goto err;
		}
	      break;
	    case 1: /* 64-bit float */
	      if ((in.data = (VOIDP) HDmalloc((size_t) len * sizeof(float64))) == NULL)
		{
		  (void) fprintf(stderr, err2);
		  goto err;
		}
	      break;
	    case 2: /* 32-bit integer */
	      if ((in.data = (VOIDP) HDmalloc((size_t) len * sizeof(int32))) == NULL)
		{
		  (void) fprintf(stderr, err2);
		  goto err;
		}
	      break;
	    case 3: /* 16-bit integer */
	      if ((in.data = (VOIDP) HDmalloc((size_t) len * sizeof(int16))) == NULL)
		{
		  (void) fprintf(stderr, err2);
		  goto err;
		}
	      break;
	    case 4: /* 8-bit integer */
	      if ((in.data = (VOIDP) HDmalloc((size_t) len * sizeof(int8))) == NULL)
		{
		  (void) fprintf(stderr, err2);
		  goto err;
		}
	      break;
	    }
	  
          if (gdata(opt->infiles->filename, &in, strm, &is_maxmin))
	    goto err;
		
          /*
           * put the input data in the HDF output file, in SDS format
           */

	  if (opt->to_float == TRUE)
	    {

	  switch(in.outtype)
	    {

	    case 0: /* 32-bit float */
            
			/* create data-set */ 
			if (in.rank == 2)
                  {
                     	edges2[0] = in.dims[1];
		  	edges2[1] = in.dims[0];
		  	
			
			sds_id = SDcreate (sd_id, NULL, DFNT_FLOAT32, in.rank, edges2);
		  	start2[0] = 0;
		  	start2[1] = 0;	
                  }
                else
                  {
		        edges3[0] = in.dims[2];
		  	edges3[1] = in.dims[1];
			edges3[2] = in.dims[0];
			sds_id = SDcreate (sd_id, NULL, DFNT_FLOAT32, in.rank, edges3);
			start3[0] = 0;
			start3[1] = 0;	
			start3[2] = 0;
                  }

                
                if (is_scale == TRUE)
                  {
			/* set range */
		    if (SDsetrange(sds_id, &in.max, &in.min)!=0)
		      {
			(void) fprintf(stderr, err5);
			    goto err;
		      }	
		    
			/* set dimension scale */
			if (in.rank == 2)
		      {
			dim_index = 0;
			dim_id = SDgetdimid (sds_id, dim_index);
			    
			
			if (SDsetdimscale(dim_id, edges2[0],DFNT_FLOAT32,(VOIDP)in.vscale)!=0)
			  {
			    (void) fprintf(stderr, err5);
			    goto err;
			  }
			    
			dim_index = 1;  
			dim_id = SDgetdimid (sds_id, dim_index); 
			
			if (SDsetdimscale(dim_id, edges2[1], DFNT_FLOAT32, (VOIDP)in.hscale)!=0)
			  {
			    (void) fprintf(stderr, err5);
			    goto err;
			  }
		      }
		    else
		      {
			dim_index = 0; 
			dim_id = SDgetdimid (sds_id, dim_index);
			if (SDsetdimscale(dim_id, edges3[0], DFNT_FLOAT32, (VOIDP)in.dscale)!=0)
			  {
			    (void) fprintf(stderr, err5);
			    goto err;
			  }
			dim_index = 1; 
			dim_id = SDgetdimid (sds_id, dim_index); 
			
			if (SDsetdimscale(dim_id, edges3[1], DFNT_FLOAT32, (VOIDP)in.vscale)!=0)
			  {
			      (void) fprintf(stderr, err5);
			  			goto err;
			    }
			  dim_index = 2; 
			  dim_id = SDgetdimid (sds_id, dim_index);	
			  
			  if (SDsetdimscale(dim_id, edges3[2], DFNT_FLOAT32, (VOIDP)in.hscale)!=0)
			    {
			      (void) fprintf(stderr, err5);
			      goto err;
			    }
		      }
                  }
				/* write data to the data set */
		if (in.rank == 2)
		  {
		    if (SDwritedata(sds_id, start2, NULL, edges2, (VOIDP)in.data)!=0)
		      {
			(void) fprintf(stderr, err5);
			goto err;
		      }
		  }
		else
		  {
		    if (SDwritedata(sds_id, start3, NULL, edges3, (VOIDP)in.data)!=0)
		      {
			(void) fprintf(stderr, err5);
			goto err;
		      }
		    
		  }
		break; 
		
	    case 1:/* 64-bit float */
            
			/* create data-set */ 
	       
	      if (in.rank == 2)
		{
		  edges2[0] = in.dims[1];
		  edges2[1] = in.dims[0];
		  sds_id = SDcreate (sd_id, NULL, DFNT_FLOAT64, in.rank, edges2);
		  start2[0] = 0;
		  start2[1] = 0;	
		}
	      else
		{
		  edges3[0] = in.dims[2];
		  edges3[1] = in.dims[1];
		  edges3[2] = in.dims[0];
		  sds_id = SDcreate (sd_id, NULL, DFNT_FLOAT64, in.rank, edges3);
		  start3[0] = 0;
		  start3[1] = 0;	
		  start3[2] = 0;
		}
	      
	      
	      if (is_scale == TRUE)
		{
		  /* set range */
		  if (SDsetrange(sds_id, &in.fp64s.max, &in.fp64s.min)!=0)
		    {
			(void) fprintf(stderr, err5);
			goto err;
		    }
		  
		  /* set dimension scale */
		  if (in.rank == 2)
		    {
		      dim_index = 0;
		      dim_id = SDgetdimid (sds_id, dim_index); 
		 
		      if (SDsetdimscale(dim_id, edges2[0],DFNT_FLOAT64,(VOIDP)in.fp64s.vscale)!=0)
			{
			  (void) fprintf(stderr, err5);
			  goto err;
			}
			
		      dim_index = 1; 
		      dim_id = SDgetdimid (sds_id, dim_index); 
		      if (SDsetdimscale(dim_id, edges2[1], DFNT_FLOAT64, (VOIDP)in.fp64s.hscale)!=0)
			{
			  (void) fprintf(stderr, err5);
			  goto err;
			}
		    }
		  else
		    {
		      dim_index = 0;
		      dim_id = SDgetdimid (sds_id, dim_index);  	
		      if (SDsetdimscale(dim_id, edges3[0], DFNT_FLOAT64, (VOIDP)in.fp64s.dscale)!=0)
			  {
			    (void) fprintf(stderr, err5);
			    goto err;
			  }
		      dim_index = 1; 
		      dim_id = SDgetdimid (sds_id, dim_index); 
		      if (SDsetdimscale(dim_id, edges3[1], DFNT_FLOAT64, (VOIDP)in.fp64s.vscale)!=0)
			{
			  (void) fprintf(stderr, err5);
			  goto err;
			}
		      dim_index = 2;
		      dim_id = SDgetdimid (sds_id, dim_index);  	
		      if (SDsetdimscale(dim_id, edges3[2], DFNT_FLOAT64, (VOIDP)in.fp64s.hscale)!=0)
			{
			  (void) fprintf(stderr, err5);
			  goto err;
			}
		    }
		}
		  
		/* write data to the data set */
	      if (in.rank == 2)
		{
		  if (SDwritedata(sds_id, start2, NULL, edges2, (VOIDP)in.data)!=0)
		    {
		      (void) fprintf(stderr, err5);
		      goto err;
		    }
		}
	      else
		  {
		    if (SDwritedata(sds_id, start3, NULL, edges3, (VOIDP)in.data)!=0)
		      {
			(void) fprintf(stderr, err5);
			goto err;
		      }
		    
		  }
	      break; 
	    case 2: /* 32-bit integer */
            
			/* create data-set */ 
	      
	      if (in.rank == 2)
		{
		  edges2[0] = in.dims[1];
		  edges2[1] = in.dims[0];
		  sds_id = SDcreate (sd_id, NULL, DFNT_INT32, in.rank, edges2);
		  start2[0] = 0;
		  start2[1] = 0;	
		}
	      else
		{
		  edges3[0] = in.dims[2];
		  edges3[1] = in.dims[1];
		  edges3[2] = in.dims[0];
		  sds_id = SDcreate (sd_id, NULL, DFNT_INT32, in.rank, edges3);
		  start3[0] = 0;
		  start3[1] = 0;	
		  start3[2] = 0;
		}
	      
	      if (is_scale == TRUE)
		{
		  /* set range */
		  if (SDsetrange(sds_id, &in.in32s.max, &in.in32s.min)!=0)
		    {
		      (void) fprintf(stderr, err5);
		      goto err;
		    }

		  /* set dimension scale */
		  if (in.rank == 2)
		    {	
		       dim_index = 0;
		      dim_id = SDgetdimid (sds_id, dim_index);
			if (SDsetdimscale(dim_id, edges2[0],DFNT_INT32,(VOIDP)in.in32s.vscale)!=0)
			  {
			    (void) fprintf(stderr, err5);
			    goto err;
			  }
			
			dim_index = 1; 
			dim_id = SDgetdimid (sds_id, dim_index);  
			if (SDsetdimscale(dim_id, edges2[1], DFNT_INT32, (VOIDP)in.in32s.hscale)!=0)
			  {
			    (void) fprintf(stderr, err5);
		 		goto err;
			  }
		    }
		  else 
		    {
		      
		      dim_index = 0;
		      dim_id = SDgetdimid (sds_id, dim_index);	
		      if (SDsetdimscale(dim_id, edges3[0], DFNT_INT32, (VOIDP)in.in32s.dscale)!=0)
			{
			  (void) fprintf(stderr, err5);
			  goto err;
			}
		      dim_index = 1; 
		      dim_id = SDgetdimid (sds_id, dim_index); 
		      if (SDsetdimscale(dim_id, edges3[1], DFNT_INT32, (VOIDP)in.in32s.vscale)!=0)
			{
			  (void) fprintf(stderr, err5);
			  goto err;
			}
		      dim_index = 2;
		      dim_id = SDgetdimid (sds_id, dim_index);	
		      if (SDsetdimscale(dim_id, edges3[2], DFNT_INT32, (VOIDP)in.in32s.hscale)!=0)
			{
			  (void) fprintf(stderr, err5);
			  goto err;
			}
		    }
		}				
	    
		  /* write data to the data set */
		  if (in.rank == 2)
		{
		  if (SDwritedata(sds_id, start2, NULL, edges2, (VOIDP)in.data)!=0)
		    {
		      (void) fprintf(stderr, err5);
		      goto err;
		    }
		}
	      else
		{
		  if (SDwritedata(sds_id, start3, NULL, edges3, (VOIDP)in.data)!=0)
		    {
		      (void) fprintf(stderr, err5);
		      goto err;
		    }			
		}
	      
	      break;
	    case 3: /* 16-bit integer */
            
			/* create data-set */ 
	      
	      if (in.rank == 2)
		{
		  edges2[0] = in.dims[1];
		  edges2[1] = in.dims[0];
		  sds_id = SDcreate (sd_id, NULL, DFNT_INT16, in.rank, edges2);
		  start2[0] = 0;
		  start2[1] = 0;	
		}
	      else
		{
		  edges3[0] = in.dims[2];
		  edges3[1] = in.dims[1];
		  edges3[2] = in.dims[0];
		  sds_id = SDcreate (sd_id, NULL, DFNT_INT16, in.rank, edges3);
		  start3[0] = 0;
		  start3[1] = 0;	
		  start3[2] = 0;
		}
		
	      if (is_scale == TRUE)
		{
		  /* set range */
		  if (SDsetrange(sds_id, &in.in16s.max, &in.in16s.min)!=0)
		    {
		      (void) fprintf(stderr, err5);
		      goto err;
		    }

		  /* set dimension scale */
		  if (in.rank == 2)
		    {	
		      dim_index = 0;
		      dim_id = SDgetdimid (sds_id, dim_index);  
		      if (SDsetdimscale(dim_id, edges2[0],DFNT_INT16,(VOIDP)in.in16s.vscale)!=0)
			{
			  (void) fprintf(stderr, err5);
			  goto err;
			}
		      
		      dim_index = 1; 
		      dim_id = SDgetdimid (sds_id, dim_index);  
		      if (SDsetdimscale(dim_id, edges2[1], DFNT_INT16, (VOIDP)in.in16s.hscale)!=0)
			{
			  (void) fprintf(stderr, err5);
			  goto err;
			}
		    }
		  else 
		    {
		      dim_index = 0;
		      dim_id = SDgetdimid (sds_id, dim_index);	
		      if (SDsetdimscale(dim_id, edges3[0], DFNT_INT16, (VOIDP)in.in16s.dscale)!=0)
			{
			  (void) fprintf(stderr, err5);
			  goto err;
			}
		      dim_index = 1; 
		      dim_id = SDgetdimid (sds_id, dim_index); 
		      if (SDsetdimscale(dim_id, edges3[1], DFNT_INT16, (VOIDP)in.in16s.vscale)!=0)
			{
			  (void) fprintf(stderr, err5);
			  goto err;
			}
		      dim_index = 2;
		      dim_id = SDgetdimid (sds_id, dim_index);	
		      if (SDsetdimscale(dim_id, edges3[2], DFNT_INT16, (VOIDP)in.in16s.hscale)!=0)
			{
			  (void) fprintf(stderr, err5);
			  goto err;
			}
		    }
		}				
	      if (in.rank == 2)
		{
		  if (SDwritedata(sds_id, start2, NULL, edges2, (VOIDP)in.data)!=0)
		    {
		      (void) fprintf(stderr, err5);
		      goto err;
		    }
		}
	      else
		{
			  /* write data to the data set */
		  if (SDwritedata(sds_id, start3, NULL, edges3, (VOIDP)in.data)!=0)
		    {
		      (void) fprintf(stderr, err5);
		      goto err;
		    }			
		}
	      
	break;
	    case 4: /* 8-bit integer */
            
			/* create data-set */ 
	      
	      if (in.rank == 2)
		{
		  edges2[0] = in.dims[1];
		  edges2[1] = in.dims[0];
		  sds_id = SDcreate (sd_id, NULL, DFNT_INT8, in.rank, edges2);
		  start2[0] = 0;
		  start2[1] = 0;	
		}
	      else
		{
		  edges3[0] = in.dims[2];
		  edges3[1] = in.dims[1];
		  edges3[2] = in.dims[0];
		  sds_id = SDcreate (sd_id, NULL, DFNT_INT8, in.rank, edges3);
		  start3[0] = 0;
		  start3[1] = 0;	
		  start3[2] = 0;
		}
		
	      if (is_scale == TRUE)
		{
		  /* set range */
		  if (SDsetrange(sds_id, &in.in8s.max, &in.in8s.min)!=0)
		    {
		      (void) fprintf(stderr, err5);
		      goto err;
		    }

		  /* set dimension scale */
		  if (in.rank == 2)
		    {	
		      dim_index = 0;
		      dim_id = SDgetdimid (sds_id, dim_index);
		      if (SDsetdimscale(dim_id, edges2[0],DFNT_INT8,(VOIDP)in.in8s.vscale)!=0)
			{
			  (void) fprintf(stderr, err5);
			  goto err;
			}
		      
		      dim_index = 1; 
		      dim_id = SDgetdimid (sds_id, dim_index); 
		      if (SDsetdimscale(dim_id, edges2[1], DFNT_INT8, (VOIDP)in.in8s.hscale)!=0)
			{
			  (void) fprintf(stderr, err5);
			  goto err;
			}
		    }
		  else 
		    {
		      dim_index = 0;
		      dim_id = SDgetdimid (sds_id, dim_index);	
		      if (SDsetdimscale(dim_id, edges3[0], DFNT_INT8, (VOIDP)in.in8s.dscale)!=0)
			{
			  (void) fprintf(stderr, err5);
			  goto err;
			}
		      dim_index = 1; 
		      dim_id = SDgetdimid (sds_id, dim_index); 
		      if (SDsetdimscale(dim_id, edges3[1], DFNT_INT8, (VOIDP)in.in8s.vscale)!=0)
			{
			  (void) fprintf(stderr, err5);
			  goto err;
			}
		      dim_index = 2;
		      dim_id = SDgetdimid (sds_id, dim_index);	
		      if (SDsetdimscale(dim_id, edges3[2], DFNT_INT8, (VOIDP)in.in8s.hscale)!=0)
			{
			    (void) fprintf(stderr, err5);
			    goto err;
			}
		    }
		}	
		  
		  /* write data to the data set */
	      if (in.rank == 2)
		{
		  if (SDwritedata(sds_id, start2, NULL, edges2, (VOIDP)in.data)!=0)
		    {
		      (void) fprintf(stderr, err5);
		      goto err;
		    }
		}
	      else
		{
		  if (SDwritedata(sds_id, start3, NULL, edges3, (VOIDP)in.data)!=0)
		    {
		      (void) fprintf(stderr, err5);
		      goto err;
		    }			
		}
	      break;
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
                if ((im.image = (unsigned char *) HDmalloc((unsigned int) len)) == NULL)
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
	  fpdeallocate(&in,&im,opt);
	 
	  /* close data-set */
	  if (opt->to_float)
	    {
	      if (SDendaccess (sds_id) !=0 )
		{
		  (void) fprintf(stderr, err5);
		  goto err;
		}
	    }
	    
      }

	  /* close file */
	if (SDend (sd_id) != 0)
	  {
	    (void) fprintf(stderr, err6);
	    goto err;
	  }
	
    return (0);

  err:
    return (1);
}

/*
 * Name: (pkamat - New function)
 *      fpdeallocate
 *
 * Purpose:
 *      Deallocate memory of all data structures
 */

void fpdeallocate(struct Input *in, struct Raster *im, struct Options *opt)
{
 switch(in->outtype)
    {
    case 0 :
      HDfree((char *) in->hscale);
      HDfree((char *) in->vscale);
      if (in->rank == 3)
	HDfree((char *) in->dscale);
      
      if (opt->to_image == TRUE)
	HDfree((char *) im->image);
      break;
      
    case 1 :
      HDfree((char *) in->fp64s.hscale);
      HDfree((char *) in->fp64s.vscale);
      if (in->rank == 3)
	HDfree((char *) in->fp64s.dscale);
      
      if (opt->to_image == TRUE)
	HDfree((char *) im->image);
      break;
      
    case 2 :
      HDfree((char *) in->in32s.hscale);
      HDfree((char *) in->in32s.vscale);
      if (in->rank == 3)
	HDfree((char *) in->in32s.dscale);
      break;
		
    case 3 :
      HDfree((char *) in->in16s.hscale);
      HDfree((char *) in->in16s.vscale);
      if (in->rank == 3)
	HDfree((char *) in->in16s.dscale);
      break;
      
    case 4 :
      HDfree((char *) in->in8s.hscale);
      HDfree((char *) in->in8s.vscale);
      if (in->rank == 3)
	HDfree((char *) in->in8s.dscale);
      break;
      
    }
   HDfree((char *) in->data);

}


/*
 * Name: (pkamat - New function)
 *      init_scales
 *
 * Purpose:
 *      Initialise the data-structures to hold scale information
 */
static int init_scales(struct Input * in)
{
  const char *err1 = "Unable to dynamically allocate memory.\n";
  switch(in->outtype)
    {
    case 0: /* 32-bit float */
      if ((in->hscale = (float32 *) HDmalloc((size_t)
					     (in->dims[0] + 1) * sizeof(float32))) == NULL)
	{
	  (void) fprintf(stderr, err1);
	  goto err;
	}
      if ((in->vscale = (float32 *) HDmalloc((size_t)
					     (in->dims[1] + 1) * sizeof(float32))) == NULL)
	{
	  (void) fprintf(stderr, err1);
	  goto err;
	}
      if (in->rank == 3)
	{
	  if ((in->dscale = (float32 *) HDmalloc((size_t)
						 (in->dims[2] + 1) * sizeof(float32))) == NULL)
	    {
	      (void) fprintf(stderr, err1);
	      goto err;
	    }
	}
      break;
      
    case 1: /* 64-bit float */
      
      if ((in->fp64s.hscale = (float64 *) HDmalloc((size_t)
						   (in->dims[0] + 1) * sizeof(float64))) == NULL)
	{
	    (void) fprintf(stderr, err1);
	    goto err;
	}
      if ((in->fp64s.vscale = (float64 *) HDmalloc((size_t)
						   (in->dims[1] + 1) * sizeof(float64))) == NULL)
	{
	  (void) fprintf(stderr, err1);
	  goto err;
	}
      if (in->rank == 3)
	{
	  if ((in->fp64s.dscale = (float64 *) HDmalloc((size_t)
						  (in->dims[2] + 1) * sizeof(float64))) == NULL)
	    {
	      (void) fprintf(stderr, err1);
	      goto err;
	    }
	}
      break;
    case 2: /* 32-bit integer */
      if ((in->in32s.hscale = (int32 *) HDmalloc((size_t)
						 (in->dims[0] + 1) * sizeof(int32))) == NULL)
	{
	  (void) fprintf(stderr, err1);
	  goto err;
	}
      if ((in->in32s.vscale = (int32 *) HDmalloc((size_t)
						 (in->dims[1] + 1) * sizeof(int32))) == NULL)
	{
	  (void) fprintf(stderr, err1);
	  goto err;
	}
      if (in->rank == 3)
	{
	  if ((in->in32s.dscale = (int32 *) HDmalloc((size_t)
                             (in->dims[2] + 1) * sizeof(int32))) == NULL)
	    {
	      (void) fprintf(stderr, err1);
	      goto err;
	    }
	}
      break;
      
    case 3: /* 16-bit integer */
      if ((in->in16s.hscale = (int16 *) HDmalloc((size_t)
						 (in->dims[0] + 1) * sizeof(int16))) == NULL)
	{
	  (void) fprintf(stderr, err1);
	  goto err;
	}
      if ((in->in16s.vscale = (int16 *) HDmalloc((size_t)
						 (in->dims[1] + 1) * sizeof(int16))) == NULL)
	{
	  (void) fprintf(stderr, err1);
	  goto err;
	}
      if (in->rank == 3)
	{
	  if ((in->in16s.dscale = (int16 *) HDmalloc((size_t)
						     (in->dims[2] + 1) * sizeof(int16))) == NULL)
	    {
	      (void) fprintf(stderr, err1);
	      goto err;
	    }
	}
      break;
	
    case 4: /* 8-bit integer */
      if ((in->in8s.hscale = (int8 *) HDmalloc((size_t)
					       (in->dims[0] + 1) * sizeof(int8))) == NULL)
	{
	  (void) fprintf(stderr, err1);
	  goto err;
	}
      if ((in->in8s.vscale = (int8 *) HDmalloc((size_t)
                             (in->dims[1] + 1) * sizeof(int8))) == NULL)
	{
	  (void) fprintf(stderr, err1);
	  goto err;
	}
      if (in->rank == 3)
	{
	  if ((in->in8s.dscale = (int8 *) HDmalloc((size_t)
						   (in->dims[2] + 1) * sizeof(int8))) == NULL)
	    {
	      (void) fprintf(stderr, err1);
	      goto err;
	    }
	}
      break;	
      
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
    (void) fprintf(stderr, "\t%s <infile> [ [-t[ype] <output-type> | -n] ", name);
    (void) fprintf(stderr, "[<infile> [-t[ype] <output-type> | -n ]]...]\n");
    (void) fprintf(stderr, "\t\t\t\t\t-o[utfile] <outfile> [options..]\n");
    (void) fprintf(stderr, "\n\t-t[ype] <output_type>");
     
    (void) fprintf(stderr, "\n\t\tOptionally used for every input ASCII file to specify the");
    (void) fprintf(stderr, "\n\t\tdata type of the data-set to be written. If not specified");
    (void) fprintf(stderr, "\n\t\tdefault data type is 32-bit floating point. <output-type>");
    (void) fprintf(stderr, "\n\t\tcan be any of the following: FP32 (default), FP64, INT32");
    (void) fprintf(stderr, "\n\t\tINT16, INT8. It can be used only with ASCII files.");

    (void) fprintf(stderr, "\n\t-n");
    (void) fprintf(stderr, "\n\t\tThis option is to be used only if the binary input file ");
    (void) fprintf(stderr, "\n\t\tcontains 64-bit floating point data and the default");
    (void) fprintf(stderr, "\n\t\tbehaviour (default behaviour is to write it to a 32-bit");
    (void) fprintf(stderr, "\n\t\tfloating point data-set) should be overridden to write ");
    (void) fprintf(stderr, "\n\t\tit to a 64-bit floating point data-set.");
    (void) fprintf(stderr, "\n\n\toptions...\n");
    (void) fprintf(stderr, "\n\t-r[aster]:\n");
    (void) fprintf(stderr, "\t\tproduce an image.  Could be ");
    (void) fprintf(stderr, "followed by:\n");
    (void) fprintf(stderr, "\t\t-e[xpand] <horiz> <vert> ");
    (void) fprintf(stderr, "[<depth>]:\n");
    (void) fprintf(stderr, "\t\t\t resolution with pixel ");
    (void) fprintf(stderr, "replication\n");
    (void) fprintf(stderr, "\t\t-i[nterp] <horiz> <vert> ");
    (void) fprintf(stderr, "[<depth>]:\n");
    (void) fprintf(stderr, "\t\t\tresolution with interpolation\n");
    (void) fprintf(stderr, "\t\t-p[alfile] <palfile>:\n");
    (void) fprintf(stderr, "\t\t\tinclude palette from palfile\n");
    (void) fprintf(stderr, "\t\t-m[ean] <meanval>:\n");
    (void) fprintf(stderr, "\t\t\tmean value to scale image ");
    (void) fprintf(stderr, "around\n");
    (void) fprintf(stderr, "\t-f[loat]:\n");
    (void) fprintf(stderr, "\t\tproduce floating point data\n\n");

    return;
}





















