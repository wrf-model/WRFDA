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
static char *RcsId[] = "@(#)$Revision: 1.13 $";
#endif

/* $Id: jpeg2hdf.c,v 1.13 1996/11/11 20:40:25 koziol Exp $ */
#if defined __MWERKS__
#include <console.h>
#endif

#include "hdf.h"
#ifndef I860
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#endif /* I860 */

/* Size of the file buffer to copy through */
#define MAX_FILE_BUF    16384

typedef enum
  {                             /* JPEG marker codes */
      M_SOF0 = 0xc0,
      M_SOF1 = 0xc1,
      M_SOF2 = 0xc2,
      M_SOF3 = 0xc3,

      M_SOF5 = 0xc5,
      M_SOF6 = 0xc6,
      M_SOF7 = 0xc7,

      M_JPG = 0xc8,
      M_SOF9 = 0xc9,
      M_SOF10 = 0xca,
      M_SOF11 = 0xcb,

      M_SOF13 = 0xcd,
      M_SOF14 = 0xce,
      M_SOF15 = 0xcf,

      M_DHT = 0xc4,

      M_DAC = 0xcc,

      M_RST0 = 0xd0,
      M_RST1 = 0xd1,
      M_RST2 = 0xd2,
      M_RST3 = 0xd3,
      M_RST4 = 0xd4,
      M_RST5 = 0xd5,
      M_RST6 = 0xd6,
      M_RST7 = 0xd7,

      M_SOI = 0xd8,
      M_EOI = 0xd9,
      M_SOS = 0xda,
      M_DQT = 0xdb,
      M_DNL = 0xdc,
      M_DRI = 0xdd,
      M_DHP = 0xde,
      M_EXP = 0xdf,

      M_APP0 = 0xe0,
      M_APP15 = 0xef,

      M_JPG0 = 0xf0,
      M_JPG13 = 0xfd,
      M_COM = 0xfe,

      M_TEM = 0x01,

      M_ERROR = 0x100
  }
JPEG_MARKER;

PRIVATE int32 num_bytes;        /* number of bytes until the SOS code. */
PRIVATE int32 image_width = 0;  /* width of the JPEG image in pixels */
PRIVATE int32 image_height = 0; /* height of the JPEG image in pixels */
PRIVATE intn num_components = 0;    /* number of components in the JPEG image */
PRIVATE uint8 file_buf[MAX_FILE_BUF];   /* size of the buffer to copy through */

/*
 * Routines to parse JPEG markers & save away the useful info.
 */

static intn
jgetc(FILE * f)
/* Get a 2-byte unsigned integer (e.g., a marker parameter length field) */
{
    intn        a;

    a = fgetc(f);
    if (a != EOF)
        num_bytes++;
    return (a);
}

static int32
get_2bytes(FILE * f)
/* Get a 2-byte unsigned integer (e.g., a marker parameter length field) */
{
    int32       a;

    a = jgetc(f);
    return (a << 8) + jgetc(f);
}

static      VOID
get_sof(FILE * f)
/* Process a SOFn marker */
{
    short       ci;
    int         data_precision;

    (VOID) get_2bytes(f);

    data_precision = jgetc(f);
    image_height = get_2bytes(f);
    image_width = get_2bytes(f);
    num_components = jgetc(f);

    for (ci = 0; ci < num_components; ci++)
      {
          jgetc(f);
          jgetc(f);
          jgetc(f);
      }
}

static      VOID
skip_variable(FILE * f)
/* Skip over an unknown or uninteresting variable-length marker */
{
    int32       length;

    length = get_2bytes(f);

    for (length -= 2; length > 0; length--)
        (VOID) jgetc(f);
}

static intn
next_marker(FILE * f)
/* Find the next JPEG marker */
/* Note that the output might not be a valid marker code, */
/* but it will never be 0 or FF */
{
    intn        c, nbytes;

    nbytes = 0;
    do
      {
          do
            {   /* skip any non-FF bytes */
                nbytes++;
                c = jgetc(f);
            }
          while (c != 0xFF);
          do
            {   /* skip any duplicate FFs */
                nbytes++;
                c = jgetc(f);
            }
          while (c == 0xFF);
      }
    while (c == 0);     /* repeat if it was a stuffed FF/00 */

    return c;
}

static      JPEG_MARKER
process_tables(FILE * f)
/* Scan and process JPEG markers that can appear in any order */
/* Return when an SOI, EOI, SOFn, or SOS is found */
{
    int         c;

    while (TRUE)
      {
          c = next_marker(f);

          switch (c)
            {
#ifdef QAK
                case M_SOF0:
                case M_SOF1:
                case M_SOF2:
                case M_SOF3:
                case M_SOF5:
                case M_SOF6:
                case M_SOF7:
                case M_JPG:
                case M_SOF9:
                case M_SOF10:
                case M_SOF11:
                case M_SOF13:
                case M_SOF14:
                case M_SOF15:
                case M_SOI:
#endif
                case M_EOI:
#ifdef OLD_WAY
                case M_SOS:
#endif
                    return ((JPEG_MARKER) c);

                case M_SOF0:
                case M_SOF1:
                case M_SOF9:
                    get_sof(f);
                    return ((JPEG_MARKER) c);

#ifdef QAK
                case M_DHT:
                    get_dht(cinfo);
                    break;

                case M_DAC:
                    get_dac(cinfo);
                    break;

                case M_DQT:
                    get_dqt(cinfo);
                    break;

                case M_DRI:
                    get_dri(cinfo);
                    break;

                case M_APP0:
                    get_app0(cinfo);
                    break;
#endif

                case M_RST0:    /* these are all parameterless */
                case M_RST1:
                case M_RST2:
                case M_RST3:
                case M_RST4:
                case M_RST5:
                case M_RST6:
                case M_RST7:
                case M_TEM:
                    break;

                default:    /* must be DNL, DHP, EXP, APPn, JPGn, COM, or RESn */
                    skip_variable(f);
                    break;
            }
      }
}

/*
 * Initialize and read the file header (everything through the SOF marker).
 */

static int32
read_file_header(FILE * f)
{
    int         c;

    num_bytes = 0;  /* reset the number of bytes into the file we are */

    /* Demand an SOI marker at the start of the file --- otherwise it's
     * probably not a JPEG file at all.  If the user interface wants to support
     * nonstandard headers in front of the SOI, it must skip over them itself
     * before calling jpeg_decompress().
     */
    if (jgetc(f) != 0xFF || jgetc(f) != M_SOI)
        return (0);

    /* Process markers until SOF */
    c = (int)process_tables(f);

    switch (c)
      {
          case M_SOF0:      /* ok, now we know the correct number of bytes to grab */
          case M_SOF1:
          case M_SOF9:
#ifdef OLD_WAY
              return (num_bytes - 2);
#else
              return (num_bytes);
#endif

          default:
              return (0);
      }
}

/*-----------------------------------------------------------------------------
 * Name:    DFJPEGaddrig
 * Purpose: Write RIG struct for the new JPEG image out to HDF file
 * Inputs:  file_id: HDF file pointer
 *          ref: ref to write RIG with
 * Returns: 0 on success, -1 on failure with DFerror set
 * Users:   hopefully only routines in this utility
 * Invokes: DFdistart, DFdiadd, DFdiend, DFputelement
 * Remarks: another really, really, nasty hack brought to you by QAK in the
 *          interest of not decompressing the JPEG image before stuffing it
 *          into the HDF file...
 *---------------------------------------------------------------------------*/

static intn
DFJPEGaddrig(int32 file_id, uint16 ref, uint16 ctag)
{
#ifdef OLD_WAY
    char       *FUNC = "DFJPEGaddrig";
#endif
    uint8       ntstring[4];
#ifdef OLD_WAY
    int32       lutsize;
#endif
    int32       GroupID;
    uint8      *p;

    ntstring[0] = DFNT_VERSION;     /* version */
    ntstring[1] = DFNT_UCHAR;   /* type */
    ntstring[2] = 8;    /* width: RIG data is 8-bit chars */
    ntstring[3] = DFNTC_BYTE;   /* class: data are numeric values */
    if (Hputelement(file_id, DFTAG_NT, ref,
                    (uint8 *) ntstring, (int32) 4) == FAIL)
        return FAIL;
#ifdef OLD_WAY
    rig->datadesc[IMAGE].nt.tag = DFTAG_NT;
    rig->datadesc[IMAGE].nt.ref = ref;

    if (Ref.dims[IMAGE] == 0)
      {
#endif
          p = file_buf;
          INT32ENCODE(p, image_width);  /* width */
          INT32ENCODE(p, image_height);     /* height */
          UINT16ENCODE(p, DFTAG_NT);    /* number type */
          UINT16ENCODE(p, ref);
          INT16ENCODE(p, num_components);   /* number of components */
          INT16ENCODE(p, 0);    /* interlace scheme */
          UINT16ENCODE(p, ctag);    /* compression type */
          UINT16ENCODE(p, ref);
          if (Hputelement(file_id, DFTAG_ID, ref,
                          file_buf, (int32) (p - file_buf)) == FAIL)
              return FAIL;

#ifdef OLD_WAY
          Ref.dims[IMAGE] = ref;
      }
    if (!Ref.lut)
      {     /* associated lut not written to this file */
          if (Grlutdata == NULL)
            {   /* no lut associated */
                HERROR(DFE_ARGS);
                return FAIL;
            }
          lutsize = Grwrite.datadesc[LUT].xdim * Grwrite.datadesc[LUT].ydim *
              Grwrite.datadesc[LUT].ncomponents;
          if (Hputelement(file_id, DFTAG_LUT, ref,
                          Grlutdata, (int32) lutsize) == FAIL)
              return FAIL;
          rig->data[LUT].tag = DFTAG_LUT;
          rig->data[LUT].ref = ref;
          Ref.lut = ref;
      }

    if (Ref.dims[LUT] == 0)
      {
          uint8      *p;
          p = GRtbuf;
          INT32ENCODE(p, rig->datadesc[LUT].xdim);
          INT32ENCODE(p, rig->datadesc[LUT].ydim);
          UINT16ENCODE(p, rig->datadesc[LUT].nt.tag);
          UINT16ENCODE(p, rig->datadesc[LUT].nt.ref);
          INT16ENCODE(p, rig->datadesc[LUT].ncomponents);
          INT16ENCODE(p, rig->datadesc[LUT].interlace);
          UINT16ENCODE(p, rig->datadesc[LUT].compr.tag);
          UINT16ENCODE(p, rig->datadesc[LUT].compr.ref);
          if (Hputelement(file_id, DFTAG_LD, ref,
                          GRtbuf, (int32) (p - GRtbuf)) == FAIL)
              return FAIL;
          Ref.dims[LUT] = ref;
      }
#endif

    /* prepare to start writing rig */
    /* ### NOTE: the parameter to this call may go away */
    if ((GroupID = DFdisetup(10)) == FAIL)
        return FAIL;    /* max 10 tag/refs in set */
    /* add tag/ref to RIG - image description, image and lookup table */
    if (DFdiput(GroupID, DFTAG_ID, ref) == FAIL)
        return FAIL;

    if (DFdiput(GroupID, DFTAG_CI, ref) == FAIL)
        return FAIL;

#ifdef OLD_WAY
    if ((Ref.dims[LUT] > 0)
        && (DFdiput(GroupID, DFTAG_LD, (uint16) Ref.dims[LUT]) == FAIL))
        return FAIL;

    if ((Ref.lut > 0)
      && (DFdiput(GroupID, rig->data[LUT].tag, rig->data[LUT].ref) == FAIL))
        return FAIL;
#endif

    /* write out RIG */
    return (DFdiwrite(file_id, GroupID, DFTAG_RIG, ref));
}

static      VOID
usage(void)
{
    printf("USAGE: jpeg2hdf <input JPEG file> <output HDF file>\n");
    printf("    <input JPEG file> : JPEG file containing input image \n");
    printf("    <output HDF file> : HDF file to store the image\n");
    exit(1);
}   /* end usage() */

int
main(int argc, char *argv[])
{
    int32       off_image;      /* offset of the JPEG image in the JFIF file */
    int32       file_len;       /* total length of the JPEG file */
#ifdef OLD_WAY
    int32       image_len;      /* length of the image in the JPEG file (in bytes) */
#endif /* OLD_WAY */
    FILE       *jfif_file;      /* file handle of the JFIF image */
    int32       file_id;        /* HDF file ID of the file to write */
    uint16      wtag;           /* tag number to use for the image */
    uint16      wref;           /* reference number to use for the image */
    uint16      ctag;           /* tag for the compression to do */
    int32       aid;            /* access ID for the JPEG image to stuff */

#if defined __MWERKS__
    argc = ccommand(&argv);
#endif

    if (argc != 3)
        usage();

    if (argv[1][0] == '-' || argv[1][0] == '/')     /* check command line */
        usage();

    jfif_file = fopen(argv[1], "rb");
    if (jfif_file == NULL)
      {
          printf("Error opening JPEG file: %s\n", argv[1]);
          exit(1);
      }     /* end if */

    off_image = read_file_header(jfif_file);
    if (off_image == 0 || image_width <= 0 || image_height <= 0 || num_components <= 0)
      {
          printf("Error reading JPEG file: %s, could not find a JFIF header\n",
                 argv[1]);
          exit(1);
      }     /* end if */

    if (!fseek(jfif_file, 0, SEEK_END))
      {
          file_len = (int32)ftell(jfif_file);
#ifdef OLD_WAY
          image_len = file_len - off_image;
#endif /* OLD_WAY */
          fseek(jfif_file, 0, SEEK_SET);    /* go back to beginning of JFIF file */
      }     /* end if */
    else
      {
          printf("Error, cannot fseek in %s(?!)\n", argv[1]);
          exit(1);
      }     /* end else */

    if ((file_id = Hopen(argv[2], DFACC_RDWR, 0)) != FAIL)
      {
          wref = Hnewref(file_id);
          if (!wref)
            {
                printf("Error getting a reference number for HDF file: %s\n", argv[2]);
                Hclose(file_id);
                exit(1);
            }   /* end if */
          wtag = DFTAG_CI;  /* yes, this is a compressed image */
#ifdef OLD_WAY
          if (num_components == 1)
              ctag = DFTAG_GREYJPEG;
          else if (num_components == 3)
              ctag = DFTAG_JPEG;
#else /* OLD_WAY */
          if (num_components == 1)
              ctag = DFTAG_GREYJPEG5;
          else if (num_components == 3)
              ctag = DFTAG_JPEG5;
#endif /* OLD_WAY */
          else
            {
                printf("Error, cannot support JPEG file containing %d components\n",
                       num_components);
                Hclose(file_id);
                exit(1);
            }   /* end else */
#ifdef OLD_WAY
          if (fread(file_buf, sizeof(uint8), (size_t) off_image, jfif_file)
              !=          (size_t) off_image)
            {
                printf("Error reading JFIF header from %s\n", argv[1]);
                exit(1);
            }   /* end if */
          if (Hputelement(file_id, ctag, wref, file_buf, off_image) == FAIL)
            {
                printf("Error writing JPEG header to HDF file: %s\n", argv[2]);
                exit(1);
            }   /* end if */
          if ((aid = Hstartwrite(file_id, wtag, wref, image_len)) == FAIL)
            {
                printf("Error from Hstartwrite() for JPEG image data\n");
                exit(1);
            }   /* end if */
          while (image_len > MAX_FILE_BUF)
            {
                if (fread(file_buf, sizeof(uint8), MAX_FILE_BUF, jfif_file) !=
                    MAX_FILE_BUF)
                  {
                      printf("Error reading JFIF image data from %s\n", argv[1]);
                      exit(1);
                  }     /* end if */
                if (Hwrite(aid, MAX_FILE_BUF, file_buf) != (int32) (MAX_FILE_BUF))
                  {
                      printf("Error writing JPEG image data to HDF file\n");
                      exit(1);
                  }     /* end if */
                image_len -= MAX_FILE_BUF;
            }   /* end while */
          if (image_len > 0)
            {
                if (fread(file_buf, sizeof(uint8), (size_t) image_len, jfif_file)
                    !=          (size_t) image_len)
                  {
                      printf("Error reading JFIF image data from %s\n", argv[1]);
                      exit(1);
                  }     /* end if */
                if (Hwrite(aid, image_len, file_buf) != (int32) (image_len))
                  {
                      printf("Error writing last of JPEG image data to HDF file\n");
                      exit(1);
                  }     /* end if */
            }   /* end if */
          Hendaccess(aid);  /* done with JPEG data, create RIG */
          if (DFJPEGaddrig(file_id, wref, ctag) == FAIL)
            {
                printf("Error writing JPEG RIG information\n");
                exit(1);
            }   /* end if */
#else /* OLD_WAY */
          if ((aid=Hstartwrite(file_id, ctag, wref, 0)) == FAIL)
            {
                printf("Error writing JPEG header to HDF file: %s\n", argv[2]);
                exit(1);
            }   /* end if */
          Hendaccess(aid);
          if ((aid = Hstartwrite(file_id, wtag, wref, file_len)) == FAIL)
            {
                printf("Error from Hstartwrite() for JPEG image data\n");
                exit(1);
            }   /* end if */
          while (file_len > MAX_FILE_BUF)
            {
                if (fread(file_buf, sizeof(uint8), MAX_FILE_BUF, jfif_file) !=
                    MAX_FILE_BUF)
                  {
                      printf("Error reading JFIF image data from %s\n", argv[1]);
                      exit(1);
                  }     /* end if */
                if (Hwrite(aid, MAX_FILE_BUF, file_buf) != (int32) (MAX_FILE_BUF))
                  {
                      printf("Error writing JPEG image data to HDF file\n");
                      exit(1);
                  }     /* end if */
                file_len -= MAX_FILE_BUF;
            }   /* end while */
          if (file_len > 0)
            {
                if (fread(file_buf, sizeof(uint8), (size_t) file_len, jfif_file)
                    !=          (size_t) file_len)
                  {
                      printf("Error reading JFIF image data from %s\n", argv[1]);
                      exit(1);
                  }     /* end if */
                if (Hwrite(aid, file_len, file_buf) != (int32) (file_len))
                  {
                      printf("Error writing last of JPEG image data to HDF file\n");
                      exit(1);
                  }     /* end if */
            }   /* end if */
          Hendaccess(aid);  /* done with JPEG data, create RIG */
          if (DFJPEGaddrig(file_id, wref, ctag) == FAIL)
            {
                printf("Error writing JPEG RIG information\n");
                exit(1);
            }   /* end if */
#endif /* OLD_WAY */
          Hclose(file_id);
      }     /* end if */
    else
      {
          printf("Error opening HDF file: %s\b", argv[2]);
          exit(1);
      }     /* end else */

    return (0);
}   /* end jpeg2hdf */
