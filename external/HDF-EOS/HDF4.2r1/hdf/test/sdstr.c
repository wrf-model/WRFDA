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
static char RcsId[] = "@(#)$Revision: 1.12 $";
#endif

/* $Id: sdstr.c,v 1.12 1996/05/16 18:42:58 koziol Exp $ */

/***************************************************************
**
** This program tests correctness of writing and read datastrings
** and dimension strings.
** To avoid the '\0' inserted by HDstrncpy, compare the first 14
** characters of output and input strings in subroutine compare()
**
****************************************************************/

#include "tproto.h"

static int  number_failed = 0;

static VOID compare
            (const char *outstring, const char *instring);

void
test_tsdstr(void)
{
    int         i, j, ret;
    intn        rank;
    int32       dims[2];
    float32     f32[10][10], tf32[10][10];
    const char *datalabel = "Datalabel", *dataunit = "Dataunit", *datafmt = "Datafmt",
               *coordsys = "coordsys";
    char        in_datalabel[256], in_dataunit[256], in_datafmt[256], in_coordsys[256];

    const char  *dimlabels[2], *dimunits[2], *dimfmts[2];
    char        in_dimlabels[2][256], in_dimunits[2][256], in_dimfmts[2][256];

    rank = 2;
    dims[0] = 10;
    dims[1] = 10;

    dimlabels[0] = "c_dim1_label_a";
    dimunits[0] = "c_dim1_unit_a";
    dimfmts[0] = "c_dim1_fmt_a";

    dimlabels[1] = "c_dim2_label_b";
    dimunits[1] = "c_dim2_unit_b";
    dimfmts[1] = "c_dim2_fmt_b";

    MESSAGE(5, printf("Creating arrays...\n");
        );

    for (i = 0; i < 10; i++)
      {
          for (j = 0; j < 10; j++)
            {
                f32[i][j] = (float32)((i * 10) + j);   /* range: 0 ~ 4-billion */
            }
      }

    ret = DFSDsetdims(rank, dims);
    RESULT("DFSDsetdims");
    /* individual files */
    MESSAGE(5, printf("Testing arrays in individual files...\n");
        );

    ret = DFSDsetNT(DFNT_NFLOAT32);
    RESULT("DFSDsetNT");
    ret = DFSDsetdims(rank, dims);
    RESULT("DFSDsetdims");

    ret = DFSDsetdatastrs(datalabel, dataunit, datafmt, coordsys);
    RESULT("DFSDsetdatastrs");
    ret = DFSDsetdimstrs(1, dimlabels[0], dimunits[0], dimfmts[0]);
    RESULT("DFSDsetdimstrs");
    ret = DFSDsetdimstrs(2, dimlabels[1], dimunits[1], dimfmts[1]);
    RESULT("DFSDsetdimstrs");

    ret = DFSDputdata("sdstrings.hdf", rank, dims, (VOIDP) f32);
    RESULT("DFSDputdata");

    ret = DFSDgetdata("sdstrings.hdf", rank, dims, (VOIDP) tf32);
    RESULT("DFSDgetdata");

    ret = DFSDgetdatastrs(in_datalabel, in_dataunit, in_datafmt, in_coordsys);
    RESULT("DFSDgetdatastrs");
    ret = DFSDgetdimstrs(1, in_dimlabels[0], in_dimunits[0], in_dimfmts[0]);
    RESULT("DFSDgetdimstrs");
    ret = DFSDgetdimstrs(2, in_dimlabels[1], in_dimunits[1], in_dimfmts[1]);
    RESULT("DFSDgetdimstrs");

    compare(datalabel, in_datalabel);
    compare(dataunit, in_dataunit);
    compare(datafmt, in_datafmt);
    compare(coordsys, in_coordsys);

    compare(dimlabels[0], in_dimlabels[0]);
    compare(dimunits[0], in_dimunits[0]);
    compare(dimfmts[0], in_dimfmts[0]);

    compare(dimlabels[1], in_dimlabels[1]);
    compare(dimunits[1], in_dimunits[1]);
    compare(dimfmts[1], in_dimfmts[1]);

    if (number_failed > 0)
      {
          MESSAGE(7, printf("\n\t>>> %d TESTS FAILED <<<\n\n", number_failed);
              )
      }
    else
        MESSAGE(7, printf("\n\t>>> ALL TESTS PASSED <<<\n\n");
        )

        num_errs = num_errs + number_failed;

}

static      VOID
compare(const char *outstring, const char *instring)
{
    if (0 == HDstrcmp(outstring, instring))
        MESSAGE(5, printf("Test passed for %s\n", outstring);
        )
        else
      {
          MESSAGE(5, printf(">>> Test failed for %s\n", outstring);
              );
          MESSAGE(5, printf("    Input string =  %s\n", instring);
              );
          number_failed++;
      }
}
