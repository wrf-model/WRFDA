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

/* $Id: litend.c,v 1.18 2003/12/10 21:13:33 epourmal Exp $ */

#include "tproto.h"

/* Internal Variables */
#define CDIM_X  7
#define CDIM_Y  9

#define FILENAME    "litend.dat"
#define TMPFILE     "temp.hdf"

/* for those machines with imprecise IEEE<-> conversions, this should be */
/* close enough */
#define FLOAT64_FUDGE  ((float64)0.00000001)

static int8  cdata_i8[CDIM_Y][CDIM_X];
static uint8  cdata_u8[CDIM_Y][CDIM_X];
static int16  cdata_i16[CDIM_Y][CDIM_X];
static uint16  cdata_u16[CDIM_Y][CDIM_X];
static int32  cdata_i32[CDIM_Y][CDIM_X];
static uint32  cdata_u32[CDIM_Y][CDIM_X];
static float32  cdata_f32[CDIM_Y][CDIM_X];
static float64  cdata_f64[CDIM_Y][CDIM_X];

static VOID init_cdata(void);
static VOID wrapup_cdata(void);
static VOID test_little_read(void);
static VOID test_little_write(void);

static VOID
init_cdata(void)
{
    int         i, j;

    for (i = 0; i < CDIM_Y; i++)
        for (j = 0; j < CDIM_X; j++)
          {
              cdata_i8[i][j] = (int8) (i * 10 + j);
              cdata_u8[i][j] = (uint8) (i * 10 + j);
              cdata_i16[i][j] = (int16) (i * 10 + j);
              cdata_u16[i][j] = (uint16) (i * 10 + j);
              cdata_i32[i][j] = (int32) (i * 10 + j);
              cdata_u32[i][j] = (uint32) (i * 10 + j);
              cdata_f32[i][j] = (float32) (i * 10 + j);
              cdata_f64[i][j] = (float64) (i * 10 + j);
          }     /* end for */
}   /* end init_cdata() */

static VOID
wrapup_cdata(void)
{
}   /* end wrapup_cdata() */

static VOID
test_little_read(void)
{
    intn rank;
    int32 dimsizes[2];
    int32 numbertype;
    int8 *data_i8;
    uint8 *data_u8;
    int16 *data_i16;
    uint16 *data_u16;
    int32 *data_i32;
    uint32 *data_u32;
    float32 *data_f32;
    float64 *data_f64;
    int ret;

    char        filename[512] = "";
    char       *srcdir = getenv("srcdir");

    MESSAGE(5,printf("Testing Little-Endian Read Routines\n"););

    MESSAGE(10,printf("Testing Little-Endian INT8 Reading Routines\n"););

    /* Generate the correct name for the test file, by prepending the source path */
    if (srcdir && ((strlen(srcdir) + strlen(FILENAME) + 1) < sizeof(filename))) {
        strcpy(filename, srcdir);
        strcat(filename, "/");
    }
    strcat(filename, FILENAME);

    ret=DFSDgetdims(filename,&rank,dimsizes,2);
    RESULT("DFSDgetdims");
    if(dimsizes[0]!=CDIM_Y || dimsizes[1]!=CDIM_X) {
        fprintf(stderr, "Dimensions for INT8 data were incorrect\n");
        num_errs++;
      } /* end if */
    else {
        ret=DFSDgetNT(&numbertype);
        RESULT("DFSDgetNT");
        if(numbertype!=DFNT_LINT8) {
            fprintf(stderr, "Numbertype for INT8 data were incorrect\n");
            num_errs++;
          } /* end if */
        else {
            data_i8=(int8 *)HDmalloc((size_t)(dimsizes[0]*dimsizes[1])*sizeof(int8));
            ret=DFSDgetdata(filename,rank,dimsizes,(VOIDP)data_i8);
            RESULT("DFSDgetdata");

            if(HDmemcmp(cdata_i8,data_i8,CDIM_X*CDIM_Y*sizeof(int8))) {
                fprintf(stderr,"INT8 data was incorrect\n");
                num_errs++;
              } /* end if */
            HDfree((VOIDP)data_i8);
          } /* end else */
      } /* end else */

    MESSAGE(10,printf("Testing Little-Endian UINT8 Reading Routines\n"););

    ret=DFSDgetdims(filename,&rank,dimsizes,2);
    RESULT("DFSDgetdims");
    if(dimsizes[0]!=CDIM_Y || dimsizes[1]!=CDIM_X) {
        fprintf(stderr, "Dimensions for UINT8 data were incorrect\n");
        num_errs++;
      } /* end if */
    else {
        ret=DFSDgetNT(&numbertype);
        RESULT("DFSDgetNT");
        if(numbertype!=DFNT_LUINT8) {
            fprintf(stderr, "Numbertype for UINT8 data were incorrect\n");
            num_errs++;
          } /* end if */
        else {
            data_u8=(uint8 *)HDmalloc((size_t)(dimsizes[0]*dimsizes[1])*sizeof(uint8));
            ret=DFSDgetdata(filename,rank,dimsizes,(VOIDP)data_u8);
            RESULT("DFSDgetdata");

            if(HDmemcmp(cdata_u8,data_u8,CDIM_X*CDIM_Y*sizeof(uint8))) {
                fprintf(stderr,"UINT8 data was incorrect\n");
                num_errs++;
              } /* end if */
            HDfree((VOIDP)data_u8);
          } /* end else */
      } /* end else */

    MESSAGE(10,printf("Testing Little-Endian INT16 Reading Routines\n"););

    ret=DFSDgetdims(filename,&rank,dimsizes,2);
    RESULT("DFSDgetdims");
    if(dimsizes[0]!=CDIM_Y || dimsizes[1]!=CDIM_X) {
        fprintf(stderr, "Dimensions for INT16 data were incorrect\n");
        num_errs++;
      } /* end if */
    else {
        ret=DFSDgetNT(&numbertype);
        RESULT("DFSDgetNT");
        if(numbertype!=DFNT_LINT16) {
            fprintf(stderr, "Numbertype for INT16 data were incorrect\n");
            num_errs++;
          } /* end if */
        else {
            data_i16=(int16 *)HDmalloc((size_t)(dimsizes[0]*dimsizes[1])*sizeof(int16));
            ret=DFSDgetdata(filename,rank,dimsizes,(VOIDP)data_i16);
            RESULT("DFSDgetdata");

            if(HDmemcmp(cdata_i16,data_i16,CDIM_X*CDIM_Y*sizeof(int16))) {
                fprintf(stderr,"INT16 data was incorrect\n");
                num_errs++;
              } /* end if */
            HDfree((VOIDP)data_i16);
          } /* end else */
      } /* end else */

    MESSAGE(10,printf("Testing Little-Endian UINT16 Reading Routines\n"););

    ret=DFSDgetdims(filename,&rank,dimsizes,2);
    RESULT("DFSDgetdims");
    if(dimsizes[0]!=CDIM_Y || dimsizes[1]!=CDIM_X) {
        fprintf(stderr, "Dimensions for UINT16 data were incorrect\n");
        num_errs++;
      } /* end if */
    else {
        ret=DFSDgetNT(&numbertype);
        RESULT("DFSDgetNT");
        if(numbertype!=DFNT_LUINT16) {
            fprintf(stderr, "Numbertype for UINT16 data were incorrect\n");
            num_errs++;
          } /* end if */
        else {
            data_u16=(uint16 *)HDmalloc((size_t)(dimsizes[0]*dimsizes[1])*sizeof(uint16));
            ret=DFSDgetdata(filename,rank,dimsizes,(VOIDP)data_u16);
            RESULT("DFSDgetdata");

            if(HDmemcmp(cdata_u16,data_u16,CDIM_X*CDIM_Y*sizeof(uint16))) {
                fprintf(stderr,"UINT16 data was incorrect\n");
                num_errs++;
              } /* end if */
            HDfree((VOIDP)data_u16);
          } /* end else */
      } /* end else */

    MESSAGE(10,printf("Testing Little-Endian INT32 Reading Routines\n"););

    ret=DFSDgetdims(filename,&rank,dimsizes,2);
    RESULT("DFSDgetdims");
    if(dimsizes[0]!=CDIM_Y || dimsizes[1]!=CDIM_X) {
        fprintf(stderr, "Dimensions for INT32 data were incorrect\n");
        num_errs++;
      } /* end if */
    else {
        ret=DFSDgetNT(&numbertype);
        RESULT("DFSDgetNT");
        if(numbertype!=DFNT_LINT32) {
            fprintf(stderr, "Numbertype for INT32 data were incorrect\n");
            num_errs++;
          } /* end if */
        else {
            data_i32=(int32 *)HDmalloc((size_t)(dimsizes[0]*dimsizes[1])*sizeof(int32));
            ret=DFSDgetdata(filename,rank,dimsizes,(VOIDP)data_i32);
            RESULT("DFSDgetdata");

            if(HDmemcmp(cdata_i32,data_i32,CDIM_X*CDIM_Y*sizeof(int32))) {
                fprintf(stderr,"INT32 data was incorrect\n");
                num_errs++;
              } /* end if */
            HDfree((VOIDP)data_i32);
          } /* end else */
      } /* end else */

    MESSAGE(10,printf("Testing Little-Endian UINT32 Reading Routines\n"););

    ret=DFSDgetdims(filename,&rank,dimsizes,2);
    RESULT("DFSDgetdims");
    if(dimsizes[0]!=CDIM_Y || dimsizes[1]!=CDIM_X) {
        fprintf(stderr, "Dimensions for UINT32 data were incorrect\n");
        num_errs++;
      } /* end if */
    else {
        ret=DFSDgetNT(&numbertype);
        RESULT("DFSDgetNT");
        if(numbertype!=DFNT_LUINT32) {
            fprintf(stderr, "Numbertype for UINT32 data were incorrect\n");
            num_errs++;
          } /* end if */
        else {
            data_u32=(uint32 *)HDmalloc((size_t)(dimsizes[0]*dimsizes[1])*sizeof(uint32));
            ret=DFSDgetdata(filename,rank,dimsizes,(VOIDP)data_u32);
            RESULT("DFSDgetdata");

            if(HDmemcmp(cdata_u32,data_u32,CDIM_X*CDIM_Y*sizeof(uint32))) {
                fprintf(stderr,"UINT32 data was incorrect\n");
                num_errs++;
              } /* end if */
            HDfree((VOIDP)data_u32);
          } /* end else */
      } /* end else */

    MESSAGE(10,printf("Testing Little-Endian FLOAT32 Reading Routines\n"););

    ret=DFSDgetdims(filename,&rank,dimsizes,2);
    RESULT("DFSDgetdims");
    if(dimsizes[0]!=CDIM_Y || dimsizes[1]!=CDIM_X) {
        fprintf(stderr, "Dimensions for FLOAT32 data were incorrect\n");
        num_errs++;
      } /* end if */
    else {
        ret=DFSDgetNT(&numbertype);
        RESULT("DFSDgetNT");
        if(numbertype!=DFNT_LFLOAT32) {
            fprintf(stderr, "Numbertype for FLOAT32 data were incorrect\n");
            num_errs++;
          } /* end if */
        else {
            data_f32=(float32 *)HDmalloc((size_t)(dimsizes[0]*dimsizes[1])*sizeof(float32));
            ret=DFSDgetdata(filename,rank,dimsizes,(VOIDP)data_f32);
            RESULT("DFSDgetdata");

            if(HDmemcmp(cdata_f32,data_f32,CDIM_X*CDIM_Y*sizeof(float32))) {
                fprintf(stderr,"FLOAT32 data was incorrect\n");
                num_errs++;
              } /* end if */
            HDfree((VOIDP)data_f32);
          } /* end else */
      } /* end else */

    MESSAGE(10,printf("Testing Little-Endian FLOAT64 Reading Routines\n"););

    ret=DFSDgetdims(filename,&rank,dimsizes,2);
    RESULT("DFSDgetdims");
    if(dimsizes[0]!=CDIM_Y || dimsizes[1]!=CDIM_X) {
        fprintf(stderr, "Dimensions for FLOAT64 data were incorrect\n");
        num_errs++;
      } /* end if */
    else {
        ret=DFSDgetNT(&numbertype);
        RESULT("DFSDgetNT");
        if(numbertype!=DFNT_LFLOAT64) {
            fprintf(stderr, "Numbertype for FLOAT64 data were incorrect\n");
            num_errs++;
          } /* end if */
        else {
            data_f64=(float64 *)HDmalloc((size_t)(dimsizes[0]*dimsizes[1])*sizeof(float64));
            ret=DFSDgetdata(filename,rank,dimsizes,(VOIDP)data_f64);
            RESULT("DFSDgetdata");

#if defined CONVEXNATIVE
#ifdef OLD_WAY
        if(Verbocity>9) {
            intn i;
            uint8 *u8_s=(uint8 *)cdata_f64,
	        *u8_d2=(uint8 *)data_f64;

            printf("cdata_f64:  ");
            for(i=0; i<80; i++)
              printf("%.2x ",u8_s[i]);
            printf("\ndata_f64: ");
            for(i=0; i<80; i++)
              printf("%.2x ",u8_d2[i]);
            printf("\n");
        }
#else /* OLD_WAY */
{
	intn i;
	float64 *cd_f64=(float64 *)cdata_f64,
		*d_f64=(float64 *)data_f64;

	for(i=0; i<CDIM_X*CDIM_Y; i++) {
	    if(d_f64[i]<(cd_f64[i]-FLOAT64_FUDGE)
		|| d_f64[i]>(cd_f64[i]+FLOAT64_FUDGE)) {
            fprintf(stderr,"FLOAT64 data was incorrect\n");
printf("cd_f64[%d]=%lf, d_f64[%d]=%lf\n",i,cd_f64[i],i,d_f64[i]);
{
            intn j;
            uint8 *u8_s=(uint8 *)&cd_f64[i],
	        *u8_d2=(uint8 *)&d_f64[i];

            printf("cdata_f64:  ");
            for(j=0; j<sizeof(float64); j++)
              printf("%.2x ",u8_s[j]);
            printf("\ndata_f64: ");
            for(j=0; j<sizeof(float64); j++)
              printf("%.2x ",u8_d2[j]);
            printf("\n");
}
              HEprint(stdout,0);
              num_errs++;
            } /* end if */
	  } /* end for */
}
#endif /* OLD_WAY */
#else
            if(HDmemcmp(cdata_f64,data_f64,CDIM_X*CDIM_Y*sizeof(float64))) {
                fprintf(stderr,"FLOAT64 data was incorrect\n");
                num_errs++;
              } /* end if */
#endif /* Wierd machines */
            HDfree((VOIDP)data_f64);
          } /* end else */
      } /* end else */
}   /* end test_little_read */

static VOID
test_little_write(void)
{
    intn        rank;
    int32       dimsizes[2];
    int32       numbertype;
    int8       *data_i8;
    uint8      *data_u8;
    int16      *data_i16;
    uint16     *data_u16;
    int32      *data_i32;
    uint32     *data_u32;
    float32    *data_f32;
    float64    *data_f64;
    int         ret;

    MESSAGE(5, printf("Testing Little-Endian Write Routines\n");
        );

    rank = 2;
    dimsizes[0] = CDIM_Y;
    dimsizes[1] = CDIM_X;

    MESSAGE(10, printf("Testing Little-Endian INT8 Writing Routines\n");
        );

    ret = DFSDsetdims(2, dimsizes);
    RESULT("DFSDsetdims");
    ret = DFSDsetNT(DFNT_LINT8);
    RESULT("DFSDsetNT");
    ret = DFSDadddata(TMPFILE, rank, dimsizes, (VOIDP) cdata_i8);
    RESULT("DFSDadddata");

    MESSAGE(10, printf("Testing Little-Endian UINT8 Writing Routines\n");
        );

    ret = DFSDsetdims(2, dimsizes);
    RESULT("DFSDsetdims");
    ret = DFSDsetNT(DFNT_LUINT8);
    RESULT("DFSDsetNT");
    ret = DFSDadddata(TMPFILE, rank, dimsizes, (VOIDP) cdata_u8);
    RESULT("DFSDadddata");

    MESSAGE(10, printf("Testing Little-Endian INT16 Writing Routines\n");
        );

    ret = DFSDsetdims(2, dimsizes);
    RESULT("DFSDsetdims");
    ret = DFSDsetNT(DFNT_LINT16);
    RESULT("DFSDsetNT");
    ret = DFSDadddata(TMPFILE, rank, dimsizes, (VOIDP) cdata_i16);
    RESULT("DFSDadddata");

    MESSAGE(10, printf("Testing Little-Endian UINT16 Writing Routines\n");
        );

    ret = DFSDsetdims(2, dimsizes);
    RESULT("DFSDsetdims");
    ret = DFSDsetNT(DFNT_LUINT16);
    RESULT("DFSDsetNT");
    ret = DFSDadddata(TMPFILE, rank, dimsizes, (VOIDP) cdata_u16);
    RESULT("DFSDadddata");

    MESSAGE(10, printf("Testing Little-Endian INT32 Writing Routines\n");
        );

    ret = DFSDsetdims(2, dimsizes);
    RESULT("DFSDsetdims");
    ret = DFSDsetNT(DFNT_LINT32);
    RESULT("DFSDsetNT");
    ret = DFSDadddata(TMPFILE, rank, dimsizes, (VOIDP) cdata_i32);
    RESULT("DFSDadddata");

    MESSAGE(10, printf("Testing Little-Endian UINT32 Writing Routines\n");
        );

    ret = DFSDsetdims(2, dimsizes);
    RESULT("DFSDsetdims");
    ret = DFSDsetNT(DFNT_LUINT32);
    RESULT("DFSDsetNT");
    ret = DFSDadddata(TMPFILE, rank, dimsizes, (VOIDP) cdata_u32);
    RESULT("DFSDadddata");

    MESSAGE(10, printf("Testing Little-Endian FLOAT32 Writing Routines\n");
        );

    ret = DFSDsetdims(2, dimsizes);
    RESULT("DFSDsetdims");
    ret = DFSDsetNT(DFNT_LFLOAT32);
    RESULT("DFSDsetNT");
    ret = DFSDadddata(TMPFILE, rank, dimsizes, (VOIDP) cdata_f32);
    RESULT("DFSDadddata");

    MESSAGE(10, printf("Testing Little-Endian FLOAT64 Writing Routines\n");
        );

    ret = DFSDsetdims(2, dimsizes);
    RESULT("DFSDsetdims");
    ret = DFSDsetNT(DFNT_LFLOAT64);
    RESULT("DFSDsetNT");
    ret = DFSDadddata(TMPFILE, rank, dimsizes, (VOIDP) cdata_f64);
    RESULT("DFSDadddata");

    ret = DFSDrestart();
    RESULT("DFSDrestart");

    ret = DFSDgetdims(TMPFILE, &rank, dimsizes, 2);
    RESULT("DFSDgetdims");
    if (dimsizes[0] != CDIM_Y || dimsizes[1] != CDIM_X)
      {
          fprintf(stderr, "Dimensions for INT8 data were incorrect\n");
          num_errs++;
      }     /* end if */
    else
      {
          ret = DFSDgetNT(&numbertype);
          RESULT("DFSDgetNT");
          if (numbertype != DFNT_LINT8)
            {
                fprintf(stderr, "Numbertype for INT8 data were incorrect\n");
                num_errs++;
            }   /* end if */
          else
            {
                data_i8 = (int8 *) HDmalloc((size_t)(dimsizes[0] * dimsizes[1]) * sizeof(int8));
                ret = DFSDgetdata(TMPFILE, rank, dimsizes, (VOIDP) data_i8);
                RESULT("DFSDgetdata");

                if (HDmemcmp(cdata_i8, data_i8, CDIM_X * CDIM_Y * sizeof(int8)))
                  {
                      fprintf(stderr, "INT8 data was incorrect\n");
                      num_errs++;
                  }     /* end if */
                HDfree((VOIDP) data_i8);
            }   /* end else */
      }     /* end else */

    ret = DFSDgetdims(TMPFILE, &rank, dimsizes, 2);
    RESULT("DFSDgetdims");
    if (dimsizes[0] != CDIM_Y || dimsizes[1] != CDIM_X)
      {
          fprintf(stderr, "Dimensions for UINT8 data were incorrect\n");
          num_errs++;
      }     /* end if */
    else
      {
          ret = DFSDgetNT(&numbertype);
          RESULT("DFSDgetNT");
          if (numbertype != DFNT_LUINT8)
            {
                fprintf(stderr, "Numbertype for UINT8 data were incorrect\n");
                num_errs++;
            }   /* end if */
          else
            {
                data_u8 = (uint8 *) HDmalloc((size_t)(dimsizes[0] * dimsizes[1]) * sizeof(uint8));
                ret = DFSDgetdata(TMPFILE, rank, dimsizes, (VOIDP) data_u8);
                RESULT("DFSDgetdata");

                if (HDmemcmp(cdata_u8, data_u8, CDIM_X * CDIM_Y * sizeof(uint8)))
                  {
                      fprintf(stderr, "UINT8 data was incorrect\n");
                      num_errs++;
                  }     /* end if */
                HDfree((VOIDP) data_u8);
            }   /* end else */
      }     /* end else */

    ret = DFSDgetdims(TMPFILE, &rank, dimsizes, 2);
    RESULT("DFSDgetdims");
    if (dimsizes[0] != CDIM_Y || dimsizes[1] != CDIM_X)
      {
          fprintf(stderr, "Dimensions for INT16 data were incorrect\n");
          num_errs++;
      }     /* end if */
    else
      {
          ret = DFSDgetNT(&numbertype);
          RESULT("DFSDgetNT");
          if (numbertype != DFNT_LINT16)
            {
                fprintf(stderr, "Numbertype for INT16 data were incorrect\n");
                num_errs++;
            }   /* end if */
          else
            {
                data_i16 = (int16 *) HDmalloc((size_t)(dimsizes[0] * dimsizes[1]) * sizeof(int16));
                ret = DFSDgetdata(TMPFILE, rank, dimsizes, (VOIDP) data_i16);
                RESULT("DFSDgetdata");

                if (HDmemcmp(cdata_i16, data_i16, CDIM_X * CDIM_Y * sizeof(int16)))
                  {
                      fprintf(stderr, "INT16 data was incorrect\n");
                      num_errs++;
                  }     /* end if */
                HDfree((VOIDP) data_i16);
            }   /* end else */
      }     /* end else */

    ret = DFSDgetdims(TMPFILE, &rank, dimsizes, 2);
    RESULT("DFSDgetdims");
    if (dimsizes[0] != CDIM_Y || dimsizes[1] != CDIM_X)
      {
          fprintf(stderr, "Dimensions for UINT16 data were incorrect\n");
          num_errs++;
      }     /* end if */
    else
      {
          ret = DFSDgetNT(&numbertype);
          RESULT("DFSDgetNT");
          if (numbertype != DFNT_LUINT16)
            {
                fprintf(stderr, "Numbertype for UINT16 data were incorrect\n");
                num_errs++;
            }   /* end if */
          else
            {
                data_u16 = (uint16 *) HDmalloc((size_t)(dimsizes[0] * dimsizes[1]) * sizeof(uint16));
                ret = DFSDgetdata(TMPFILE, rank, dimsizes, (VOIDP) data_u16);
                RESULT("DFSDgetdata");

                if (HDmemcmp(cdata_u16, data_u16, CDIM_X * CDIM_Y * sizeof(uint16)))
                  {
                      fprintf(stderr, "UINT16 data was incorrect\n");
                      num_errs++;
                  }     /* end if */
                HDfree((VOIDP) data_u16);
            }   /* end else */
      }     /* end else */

    ret = DFSDgetdims(TMPFILE, &rank, dimsizes, 2);
    RESULT("DFSDgetdims");
    if (dimsizes[0] != CDIM_Y || dimsizes[1] != CDIM_X)
      {
          fprintf(stderr, "Dimensions for INT32 data were incorrect\n");
          num_errs++;
      }     /* end if */
    else
      {
          ret = DFSDgetNT(&numbertype);
          RESULT("DFSDgetNT");
          if (numbertype != DFNT_LINT32)
            {
                fprintf(stderr, "Numbertype for INT32 data were incorrect\n");
                num_errs++;
            }   /* end if */
          else
            {
                data_i32 = (int32 *) HDmalloc((size_t)(dimsizes[0] * dimsizes[1]) * sizeof(int32));
                ret = DFSDgetdata(TMPFILE, rank, dimsizes, (VOIDP) data_i32);
                RESULT("DFSDgetdata");

                if (HDmemcmp(cdata_i32, data_i32, CDIM_X * CDIM_Y * sizeof(int32)))
                  {
                      fprintf(stderr, "INT32 data was incorrect\n");
                      num_errs++;
                  }     /* end if */
                HDfree((VOIDP) data_i32);
            }   /* end else */
      }     /* end else */

    ret = DFSDgetdims(TMPFILE, &rank, dimsizes, 2);
    RESULT("DFSDgetdims");
    if (dimsizes[0] != CDIM_Y || dimsizes[1] != CDIM_X)
      {
          fprintf(stderr, "Dimensions for UINT32 data were incorrect\n");
          num_errs++;
      }     /* end if */
    else
      {
          ret = DFSDgetNT(&numbertype);
          RESULT("DFSDgetNT");
          if (numbertype != DFNT_LUINT32)
            {
                fprintf(stderr, "Numbertype for UINT32 data were incorrect\n");
                num_errs++;
            }   /* end if */
          else
            {
                data_u32 = (uint32 *) HDmalloc((size_t)(dimsizes[0] * dimsizes[1]) * sizeof(uint32));
                ret = DFSDgetdata(TMPFILE, rank, dimsizes, (VOIDP) data_u32);
                RESULT("DFSDgetdata");

                if (HDmemcmp(cdata_u32, data_u32, CDIM_X * CDIM_Y * sizeof(uint32)))
                  {
                      fprintf(stderr, "UINT32 data was incorrect\n");
                      num_errs++;
                  }     /* end if */
                HDfree((VOIDP) data_u32);
            }   /* end else */
      }     /* end else */

    ret = DFSDgetdims(TMPFILE, &rank, dimsizes, 2);
    RESULT("DFSDgetdims");
    if (dimsizes[0] != CDIM_Y || dimsizes[1] != CDIM_X)
      {
          fprintf(stderr, "Dimensions for FLOAT32 data were incorrect\n");
          num_errs++;
      }     /* end if */
    else
      {
          ret = DFSDgetNT(&numbertype);
          RESULT("DFSDgetNT");
          if (numbertype != DFNT_LFLOAT32)
            {
                fprintf(stderr, "Numbertype for FLOAT32 data were incorrect\n");
                num_errs++;
            }   /* end if */
          else
            {
                data_f32 = (float32 *) HDmalloc((size_t)(dimsizes[0] * dimsizes[1]) * sizeof(float32));
                ret = DFSDgetdata(TMPFILE, rank, dimsizes, (VOIDP) data_f32);
                RESULT("DFSDgetdata");

                if (HDmemcmp(cdata_f32, data_f32, CDIM_X * CDIM_Y * sizeof(float32)))
                  {
                      fprintf(stderr, "FLOAT32 data was incorrect\n");
                      num_errs++;
                  }     /* end if */
                HDfree((VOIDP) data_f32);
            }   /* end else */
      }     /* end else */

    ret = DFSDgetdims(TMPFILE, &rank, dimsizes, 2);
    RESULT("DFSDgetdims");
    if (dimsizes[0] != CDIM_Y || dimsizes[1] != CDIM_X)
      {
          fprintf(stderr, "Dimensions for FLOAT64 data were incorrect\n");
          num_errs++;
      }     /* end if */
    else
      {
          ret = DFSDgetNT(&numbertype);
          RESULT("DFSDgetNT");
          if (numbertype != DFNT_LFLOAT64)
            {
                fprintf(stderr, "Numbertype for FLOAT64 data were incorrect\n");
                num_errs++;
            }   /* end if */
          else
            {
                data_f64 = (float64 *) HDmalloc((size_t)(dimsizes[0] * dimsizes[1]) * sizeof(float64));
                ret = DFSDgetdata(TMPFILE, rank, dimsizes, (VOIDP) data_f64);
                RESULT("DFSDgetdata");

                if (HDmemcmp(cdata_f64, data_f64, CDIM_X * CDIM_Y * sizeof(float64)))
                  {
                      fprintf(stderr, "FLOAT64 data was incorrect\n");
                      num_errs++;
                  }     /* end if */
                HDfree((VOIDP) data_f64);
            }   /* end else */
      }     /* end else */
}   /* end test_little_write */

void
test_litend(void)
{
    init_cdata();

    test_little_read();
    test_little_write();

    wrapup_cdata();
}   /* end test_litend() */
