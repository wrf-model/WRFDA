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
static char RcsId[] = "@(#)$Revision: 1.13 $";
#endif

/* $Id: sdmms.c,v 1.13 1996/03/28 21:58:12 koziol Exp $ */

#include "tproto.h"

static float32  f32[10][10],  tf32[10][10];
static float32  f32scale[10],  tf32scale[10];
static float32 f32max = (float32) 40.0, f32min = (float32) 0.0;
static float32 tf32max, tf32min;

static int8  i8[10][10],  ti8[10][10];
static int8  i8scale[10],  ti8scale[10];
static int8 i8max = 127, i8min = -128;
static int8 ti8max, ti8min;

static uint8  ui8[10][10],  tui8[10][10];
static uint8  ui8scale[10],  tui8scale[10];
static uint8 ui8max = 255, ui8min = 0;
static uint8 tui8max, tui8min;

static int16  i16[10][10],  ti16[10][10];
static int16  i16scale[10],  ti16scale[10];
static int16 i16max = 1200, i16min = -1200;
static int16 ti16max, ti16min;

static uint16  ui16[10][10],  tui16[10][10];
static uint16  ui16scale[10],  tui16scale[10];
static uint16 ui16max = 20000, ui16min = 0;
static uint16 tui16max, tui16min;

static int32  i32[10][10],  ti32[10][10];
static int32  i32scale[10],  ti32scale[10];
static int32 i32max = 99999999, i32min = -999999999;
static int32 ti32max, ti32min;

static uint32  ui32[10][10],  tui32[10][10];
static uint32  ui32scale[10],  tui32scale[10];
static uint32 ui32max = 999999999, ui32min = 0;
static uint32 tui32max, tui32min;

static float64 cal1, cal2, cal3, cal4;
static int32 cal5;

static float64 ical1, ical2, ical3, ical4;
static int32 ical5;

void
test_sdmms(void)
{
    int         i, j, err, err1, err2, ret;
    intn        rank;
    int32       dims[2];

    rank = 2;
    dims[0] = 10;
    dims[1] = 10;

    MESSAGE(5, printf("Creating arrays...\n");
        );

    for (i = 0; i < 10; i++)
      {
          for (j = 0; j < 10; j++)
            {
                f32[i][j] = (float32)((i * 40) + j);   /* range: 0 ~ 4-billion */

                i8[i][j] = (int8) ((i * 20) - 100 + j);     /* range: 0 ~ 100 */
                ui8[i][j] = (uint8) ((i * 20) + j);     /* range: 0 ~ 200 */

                i16[i][j] = (int16) ((i * 6000) - 30000 + j);   /* range: 0 ~ 30000 */
                ui16[i][j] = (uint16) ((i * 6000) + j);     /* range: 0 ~ 60000 */

                i32[i][j] = (int32) ((i * 60000) - 300000 + j);     /* range: 0 ~ 2-billion */
                ui32[i][j] = (uint32) ((i * 400000000) + j);    /* range: 0 ~ 4-billion */
            }

          f32scale[i] = (float32) ((i * 40) + j);   /* range: 0 ~ 4-billion */

          i8scale[i] = (int8) ((i * 20) - 100 + j);     /* range: 0 ~ 100 */
          ui8scale[i] = (uint8) ((i * 20) + j);     /* range: 0 ~ 200 */

          i16scale[i] = (int16) ((i * 6000) - 30000 + j);   /* range: 0 ~ 30000 */
          ui16scale[i] = (uint16) ((i * 6000) + j);     /* range: 0 ~ 60000 */

          i32scale[i] = (int32) ((i * 60000) - 300000 + j);     /* range: 0 ~ 2-billion */
          ui32scale[i] = (uint32) ((i * 400000000) + j);    /* range: 0 ~ 4-billion */
      }

    cal1 = (float64) 10.0;
    cal2 = (float64) 0.0;
    cal3 = (float64) 27.0;
    cal4 = (float64) 1.5;
    cal5 = (int32) DFNT_INT16;

    ret = DFSDsetdims(rank, dims);
    RESULT("DFSDsetdims");
    MESSAGE(5, printf("Writing arrays to single file...\n");
        );
    ret = DFSDsetNT(DFNT_FLOAT32);
    RESULT("DFSDsetNT");

    ret = DFSDsetdimscale(1, (int32) 10, (void *) f32scale);
    RESULT("DFSDsetdimscale");
    ret = DFSDsetrange((VOIDP) &f32max, (VOIDP) &f32min);
    RESULT("DFSDsetrange");
    ret = DFSDsetcal(cal1, cal2, cal3, cal4, cal5);
    RESULT("DFSDsetcal");
    ret = DFSDputdata("ntcheck.hdf", rank, dims, (VOIDP) f32);
    RESULT("DFSDputdata");

    ret = DFSDsetNT(DFNT_INT8);
    RESULT("DFSDsetNT");
    ret = DFSDsetdimscale(1, (int32) 10, (void *) i8scale);
    RESULT("DFSDsetdimscale");
    ret = DFSDsetrange((VOIDP) &i8max, (VOIDP) &i8min);
    RESULT("DFSDsetrange");
    ret = DFSDadddata("ntcheck.hdf", rank, dims, (VOIDP) i8);
    RESULT("DFSDadddata");

    ret = DFSDsetNT(DFNT_UINT8);
    RESULT("DFSDsetNT");
    ret = DFSDsetdimscale(1, (int32) 10, (void *) ui8scale);
    RESULT("DFSDsetdimscale");
    ret = DFSDsetrange((VOIDP) &ui8max, (VOIDP) &ui8min);
    RESULT("DFSDsetrange");
    ret = DFSDadddata("ntcheck.hdf", rank, dims, (VOIDP) ui8);
    RESULT("DFSDadddata");

    ret = DFSDsetNT(DFNT_INT16);
    RESULT("DFSDsetNT");
    ret = DFSDsetdimscale(1, (int32) 10, (void *) i16scale);
    RESULT("DFSDsetdimscale");
    ret = DFSDsetrange((VOIDP) &i16max, (VOIDP) &i16min);
    RESULT("DFSDsetrange");
    ret = DFSDadddata("ntcheck.hdf", rank, dims, (VOIDP) i16);
    RESULT("DFSDadddata");

    ret = DFSDsetNT(DFNT_UINT16);
    RESULT("DFSDsetNT");
    ret = DFSDsetdimscale(1, (int32) 10, (void *) ui16scale);
    RESULT("DFSDsetdimscale");
    ret = DFSDsetrange((VOIDP) &ui16max, (VOIDP) &ui16min);
    RESULT("DFSDsetrange");
    ret = DFSDadddata("ntcheck.hdf", rank, dims, (VOIDP) ui16);
    RESULT("DFSDadddata");

    ret = DFSDsetNT(DFNT_INT32);
    RESULT("DFSDsetNT");
    ret = DFSDsetdimscale(1, (int32) 10, (void *) i32scale);
    RESULT("DFSDsetdimscale");
    ret = DFSDsetrange((VOIDP) &i32max, (VOIDP) &i32min);
    RESULT("DFSDsetrange");
    ret = DFSDadddata("ntcheck.hdf", rank, dims, (VOIDP) i32);
    RESULT("DFSDadddata");

    ret = DFSDsetNT(DFNT_UINT32);
    RESULT("DFSDsetNT");
    ret = DFSDsetdimscale(1, (int32) 10, (void *) ui32scale);
    RESULT("DFSDsetdimscale");
    ret = DFSDsetrange((VOIDP) &ui32max, (VOIDP) &ui32min);
    RESULT("DFSDsetrange");
    ret = DFSDadddata("ntcheck.hdf", rank, dims, (VOIDP) ui32);
    RESULT("DFSDadddata");

    MESSAGE(5, printf("Reading arrays from single file...\n");
        );

    ret = DFSDgetdata("ntcheck.hdf", rank, dims, (VOIDP) tf32);
    RESULT("DFSDgetdata");
    ret = DFSDgetdimscale(1, (int32) 10, (void *) tf32scale);
    RESULT("DFSDgetdimscale");
    ret = DFSDgetrange((VOIDP) &tf32max, (VOIDP) &tf32min);
    RESULT("DFSDgetrange");
    ret = DFSDgetcal(&ical1, &ical2, &ical3, &ical4, &ical5);
    RESULT("DFSDgetcal");

    ret = DFSDgetdata("ntcheck.hdf", rank, dims, (VOIDP) ti8);
    RESULT("DFSDgetdata");
    ret = DFSDgetdimscale(1, (int32) 10, (void *) ti8scale);
    RESULT("DFSDgetdimscale");
    ret = DFSDgetrange((VOIDP) &ti8max, (VOIDP) &ti8min);
    RESULT("DFSDgetrange");

    /* this test should return FAIL so that we can verify that
       when we don't store calibration info we don't get any
       info returned */
    ret = DFSDgetcal(&ical1, &ical2, &ical3, &ical4, &ical5);
    CHECK(ret, SUCCEED, "DFSDgetcal");

    ret = DFSDgetdata("ntcheck.hdf", rank, dims, (VOIDP) tui8);
    RESULT("DFSDgetdata");
    ret = DFSDgetdimscale(1, (int32) 10, (void *) tui8scale);
    RESULT("DFSDgetdimscale");
    ret = DFSDgetrange((VOIDP) &tui8max, (VOIDP) &tui8min);
    RESULT("DFSDgetrange");

    ret = DFSDgetdata("ntcheck.hdf", rank, dims, (VOIDP) ti16);
    RESULT("DFSDgetdata");
    ret = DFSDgetdimscale(1, (int32) 10, (void *) ti16scale);
    RESULT("DFSDgetdimscale");
    ret = DFSDgetrange((VOIDP) &ti16max, (VOIDP) &ti16min);
    RESULT("DFSDgetrange");

    ret = DFSDgetdata("ntcheck.hdf", rank, dims, (VOIDP) tui16);
    RESULT("DFSDgetdata");
    ret = DFSDgetdimscale(1, (int32) 10, (void *) tui16scale);
    RESULT("DFSDgetdimscale");
    ret = DFSDgetrange((VOIDP) &tui16max, (VOIDP) &tui16min);
    RESULT("DFSDgetrange");

    ret = DFSDgetdata("ntcheck.hdf", rank, dims, (VOIDP) ti32);
    RESULT("DFSDgetdata");
    ret = DFSDgetdimscale(1, (int32) 10, (void *) ti32scale);
    RESULT("DFSDgetdimscale");
    ret = DFSDgetrange((VOIDP) &ti32max, (VOIDP) &ti32min);
    RESULT("DFSDgetrange");

    ret = DFSDgetdata("ntcheck.hdf", rank, dims, (VOIDP) tui32);
    RESULT("DFSDgetdata");
    ret = DFSDgetdimscale(1, (int32) 10, (void *) tui32scale);
    RESULT("DFSDgetdimscale");
    ret = DFSDgetrange((VOIDP) &tui32max, (VOIDP) &tui32min);
    RESULT("DFSDgetrange");

    MESSAGE(5, printf("Checking arrays from single file...\n\n");
        );
    err = 0;
    err1 = 0;
    err2 = 0;
    for (i = 0; i < 10; i++)
      {
          for (j = 0; j < 10; j++)
              if (f32[i][j] != tf32[i][j])
                  err = 1;
          if (f32scale[i] != tf32scale[i])
              err2 = 1;
      }
    if ((f32max != tf32max) || (f32min != tf32min))
        err1 = 1;

    num_errs += err + err1 + err2;
    MESSAGE(5,
            {
            if (err == 1)
            printf(">>> Test failed for float32 array.\n");
            else
            printf("Test passed for float32 array.\n");
            }
    ) ;
    MESSAGE(5, if (err2 == 1)
            printf(">>> Test failed for float32 scales.\n");
            else
            printf("Test passed for float32 scales.\n");) ;
    MESSAGE(5, if (err1 == 1)
            printf(">>> Test failed for float32 max/min.\n");
            else
            printf("Test passed for float32 max/min.\n");) ;

    if ((cal1 != ical1) ||
        (cal2 != ical2) ||
        (cal3 != ical3) ||
        (cal4 != ical4) ||
        (cal5 != ical5))
      {
          MESSAGE(5, printf(">>> Test failed for float32 calibration.\n");
              );
          MESSAGE(5, printf(" Is %f %f %f %f %d\n", ical1, ical2, ical3, ical4, (int) ical5);
              );
          MESSAGE(5, printf(" sld be %f %f %f %f %d\n", cal1, cal2, cal3, cal4, (int) cal5);
              );
      }
    else
        MESSAGE(5, printf("Test passed for float32 calibration.\n");
        );

    err = 0;
    err1 = 0;
    err2 = 0;
    for (i = 0; i < 10; i++)
      {
          for (j = 0; j < 10; j++)
              if (i8[i][j] != ti8[i][j])
                  err = 1;
          if (i8scale[i] != ti8scale[i])
              err2 = 1;
      }
    if ((i8max != ti8max) || (i8min != ti8min))
        err1 = 1;

    num_errs += err + err1 + err2;
    MESSAGE(5, if (err == 1)
            printf(">>> Test failed for int8 array.\n");
            else
            printf("Test passed for int8 array.\n");) ;
    MESSAGE(5, if (err2 == 1)
            printf(">>> Test failed for int8 scales.\n");
            else
            printf("Test passed for int8 scales.\n");) ;
    MESSAGE(5, if (err1 == 1)
            printf(">>> Test failed for int8 max/min.\n");
            else
            printf("Test passed for int8 max/min.\n");) ;

    err = 0;
    err1 = 0;
    err2 = 0;
    for (i = 0; i < 10; i++)
      {
          for (j = 0; j < 10; j++)
              if (ui8[i][j] != tui8[i][j])
                  err = 1;
          if (ui8scale[i] != tui8scale[i])
              err2 = 1;
      }
    if ((ui8max != tui8max) || (ui8min != tui8min))
        err1 = 1;

    num_errs += err + err1 + err2;
    MESSAGE(5, if (err == 1)
            printf(">>> Test failed for uint8 array.\n");
            else
            printf("Test passed for uint8 array.\n");) ;
    MESSAGE(5, if (err2 == 1)
            printf(">>> Test failed for uint8 scales.\n");
            else
            printf("Test passed for uint8 scales.\n");) ;
    MESSAGE(5, if (err1 == 1)
            printf(">>> Test failed for uint8 max/min.\n");
            else
            printf("Test passed for uint8 max/min.\n");) ;

    err = 0;
    err1 = 0;
    err2 = 0;
    for (i = 0; i < 10; i++)
      {
          for (j = 0; j < 10; j++)
              if (i16[i][j] != ti16[i][j])
                  err = 1;
          if (i16scale[i] != ti16scale[i])
              err2 = 1;
      }
    if ((i16max != ti16max) || (i16min != ti16min))
        err1 = 1;

    num_errs += err + err1 + err2;
    MESSAGE(5, if (err == 1)
            printf(">>> Test failed for int16 array.\n");
            else
            printf("Test passed for int16 array.\n");) ;
    MESSAGE(5, if (err2 == 1)
            printf(">>> Test failed for int16 scales.\n");
            else
            printf("Test passed for int16 scales.\n");) ;
    MESSAGE(5, if (err1 == 1)
            printf(">>> Test failed for int16 max/min.\n");
            else
            printf("Test passed for int16 max/min.\n");) ;

    err = 0;
    err1 = 0;
    err2 = 0;
    for (i = 0; i < 10; i++)
      {
          for (j = 0; j < 10; j++)
              if (ui16[i][j] != tui16[i][j])
                  err = 1;
          if (ui16scale[i] != tui16scale[i])
              err2 = 1;
      }
    if ((ui16max != tui16max) || (ui16min != tui16min))
        err1 = 1;

    num_errs += err + err1 + err2;
    MESSAGE(5, if (err == 1)
            printf(">>> Test failed for uint16 array.\n");
            else
            printf("Test passed for uint16 array.\n");) ;
    MESSAGE(5, if (err2 == 1)
            printf(">>> Test failed for uint16 scales.\n");
            else
            printf("Test passed for uint16 scales.\n");) ;
    MESSAGE(5, if (err1 == 1)
            printf(">>> Test failed for uint16 max/min.\n");
            else
            printf("Test passed for uint16 max/min.\n");) ;

    err = 0;
    err1 = 0;
    err2 = 0;
    for (i = 0; i < 10; i++)
      {
          for (j = 0; j < 10; j++)
              if (i32[i][j] != ti32[i][j])
                  err = 1;
          if (i32scale[i] != ti32scale[i])
              err2 = 1;
      }
    if ((i32max != ti32max) || (i32min != ti32min))
        err1 = 1;

    num_errs += err + err1 + err2;
    MESSAGE(5, if (err == 1)
            printf(">>> Test failed for int32 array.\n");
            else
            printf("Test passed for int32 array.\n");) ;
    MESSAGE(5, if (err2 == 1)
            printf(">>> Test failed for int32 scales.\n");
            else
            printf("Test passed for int32 scales.\n");) ;
    MESSAGE(5, if (err1 == 1)
            printf(">>> Test failed for int32 max/min.\n");
            else
            printf("Test passed for int32 max/min.\n");) ;

    err = 0;
    err1 = 0;
    err2 = 0;
    for (i = 0; i < 10; i++)
      {
          for (j = 0; j < 10; j++)
              if (ui32[i][j] != tui32[i][j])
                  err = 1;
          if (ui32scale[i] != tui32scale[i])
              err2 = 1;
      }
    if ((ui32max != tui32max) || (ui32min != tui32min))
        err1 = 1;

    num_errs += err + err1 + err2;
    MESSAGE(5, if (err == 1)
            printf(">>> Test failed for uint32 array.\n");
            else
            printf("Test passed for uint32 array.\n");) ;
    MESSAGE(5, if (err2 == 1)
            printf(">>> Test failed for uint32 scales.\n");
            else
            printf("Test passed for uint32 scales.\n");) ;
    MESSAGE(5, if (err1 == 1)
            printf(">>> Test failed for uint32 max/min.\n");
            else
            printf("Test passed for uint32 max/min.\n");) ;

}
