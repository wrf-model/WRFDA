
#include <stdio.h>
#include "hdf.h"

#ifdef WIN32
#include <sys/stat.h>
#include <fcntl.h>
#endif

/*
 * Name:
 *      fptest
 *
 * Description:
 *      This program creates six floating point files that can be
 *      used to test the fp2hdf program.  [Note: the 32-bit floating
 *      point files are omitted for Cray (UNICOS) systems]
 *
 *      June 1, 1990
 *      Bob Weaver, baw@inel.gov
 *
 *      The file names that are created are named:
 *              ctxtr2, type 'TEXT', size 3x4
 *              ctxtr3, type 'TEXT', size 3x4x5
 *              cb32r2, type 'FP32', size 3x4
 *              cb32r3, type 'FP32', size 3x4x5
 *              cb64r2, type 'FP64', size 3x4
 *              cb64r3, type 'FP64', size 3x4x5
 *
 *      row     values start at 11 and increment by 1 => 11, 12, 13
 *      column  values start at 21 and increment by 2 => 21, 23, 25, 27
 *      plane   values start at 51 and increment by 5 => 51, 56, 61, 66, 71
 *
 *      data element value = row value + column value [+ plane value, if rank=3]
 */
int
main(int argc, char * argv[] )
{
    int         nrow = 3, ncol = 4, npln = 5, ione = 1;
    int         i, j, k;
    FILE       *sp;

    float32     b32r2[3][4], b32r3[5][3][4];
    float32     row4[3], col4[4], pln4[5];
    float32     rowo4 = (float32)11.0e0, colo4 = (float32)21.0e0, plno4 = (float32)51.0e0;
    float32     rowi4 = (float32)1.0e0, coli4 = (float32)2.0e0, plni4 = (float32)5.0e0;
    float32     ezero = (float32)0.0e0;

    float64     b64r2[3][4], b64r3[5][3][4];
    float64     row8[3], col8[4], pln8[5];
    float64     rowo8 = 11.0e0, colo8 = 21.0e0, plno8 = 51.0e0;
    float64     rowi8 = 1.0e0, coli8 = 2.0e0, plni8 = 5.0e0;
    float64     dzero = 0.0e0;

    const char *text = "TEXT";
    const char *fp32 = "FP32";
    const char *fp64 = "FP64";

    /* shut compiler up */
    argv=argv; argc=argc;

    /*
     * initialize the row, column, and plane vectors
     *
     * row values start at 11 and increment by 1 => 11, 12, 13
     * column values start at 21 and increment by 2 => 21, 23, 25, 27
     * plane values start at 51 and increment by 5 => 51, 56, 61, 66, 71
     */

    row4[0] = rowo4;
    col4[0] = colo4;
    pln4[0] = plno4;
    row8[0] = rowo8;
    col8[0] = colo8;
    pln8[0] = plno8;

    for (i = 1; i < nrow; i++)
      {
          row4[i] = row4[i - 1] + rowi4;
          row8[i] = row8[i - 1] + rowi8;
      }
    for (j = 1; j < ncol; j++)
      {
          col4[j] = col4[j - 1] + coli4;
          col8[j] = col8[j - 1] + coli8;
      }
    for (k = 1; k < npln; k++)
      {
          pln4[k] = pln4[k - 1] + plni4;
          pln8[k] = pln8[k - 1] + plni8;
      }

    /*
     * build array elements - rank 2
     *
     * element value = sum of row value and col values
     */

    for (i = 0; i < nrow; i++)
      {
          for (j = 0; j < ncol; j++)
            {
                b32r2[i][j] = row4[i] + col4[j];
                b64r2[i][j] = row8[i] + col8[j];
            }
      }

    /*
     * build array elements - rank 3
     *
     * element value = sum of row value, col, and plane values
     */

    for (i = 0; i < nrow; i++)
      {
          for (j = 0; j < ncol; j++)
            {
                for (k = 0; k < npln; k++)
                  {
                      b32r3[k][i][j] = row4[i] + col4[j] + pln4[k];
                      b64r3[k][i][j] = row8[i] + col8[j] + pln8[k];
                  }
            }
      }

    /*
     * text file - rank 2 & 3
     */

/* For WINDOWS platform, file mode should be set explicitly. 
   For text mode, set it to Text; for binary mode, set it to BINARY. */

#ifdef WIN32
     _fmode = _O_TEXT;
#endif
    sp = fopen("ctxtr2", "w");
    (void) fprintf(sp, "%s\n", text);
    (void) fprintf(sp, "%10d%10d%10d\n", ione, nrow, ncol);
    (void) fprintf(sp, "%14.6E%14.6E\n", ezero, ezero);
    for (i = 0; i < nrow; i++)
        (void) fprintf(sp, "%14.6E", row4[i]);
    (void) fprintf(sp, "\n");
    for (j = 0; j < ncol; j++)
        (void) fprintf(sp, "%14.6E", col4[j]);
    (void) fprintf(sp, "\n");
    for (i = 0; i < nrow; i++)
      {
          for (j = 0; j < ncol; j++)
              (void) fprintf(sp, "%14.6E", b32r2[i][j]);
          (void) fprintf(sp, "\n");
      }
    (void) fclose(sp);

    sp = fopen("ctxtr3", "w");
    (void) fprintf(sp, "%s\n", text);
    (void) fprintf(sp, "%10d%10d%10d\n", npln, nrow, ncol);
    (void) fprintf(sp, "%14.6E%14.6E\n", ezero, ezero);
    for (k = 0; k < npln; k++)
        (void) fprintf(sp, "%14.6E", pln4[k]);
    (void) fprintf(sp, "\n");
    for (i = 0; i < nrow; i++)
        (void) fprintf(sp, "%14.6E", row4[i]);
    (void) fprintf(sp, "\n");
    for (j = 0; j < ncol; j++)
        (void) fprintf(sp, "%14.6E", col4[j]);
    (void) fprintf(sp, "\n");
    for (k = 0; k < npln; k++)
        for (i = 0; i < nrow; i++)
          {
              for (j = 0; j < ncol; j++)
                  (void) fprintf(sp, "%14.6E", b32r3[k][i][j]);
              (void) fprintf(sp, "\n");
          }
    (void) fclose(sp);

    /*
     * binary 32-bit file - rank 2 & 3
     */
#ifdef WIN32                                                                                       
   _fmode = _O_BINARY;
#endif 
#ifndef UNICOS
    sp = fopen("cb32r2", "w");
    (void) fwrite(fp32, sizeof(int), 1, sp);
    (void) fwrite((char *) &ione, sizeof(int), 1, sp);
    (void) fwrite((char *) &nrow, sizeof(int), 1, sp);
    (void) fwrite((char *) &ncol, sizeof(int), 1, sp);
    (void) fwrite((char *) &ezero, sizeof(float32), 1, sp);
    (void) fwrite((char *) &ezero, sizeof(float32), 1, sp);
    for (i = 0; i < nrow; i++)
        (void) fwrite((char *) &row4[i], sizeof(float32), 1, sp);
    for (j = 0; j < ncol; j++)
        (void) fwrite((char *) &col4[j], sizeof(float32), 1, sp);
    for (i = 0; i < nrow; i++)
        for (j = 0; j < ncol; j++)
            (void) fwrite((char *) &b32r2[i][j], sizeof(float32), 1,
                          sp);
    (void) fclose(sp);

    sp = fopen("cb32r3", "w");
    (void) fwrite(fp32, sizeof(int), 1, sp);
    (void) fwrite((char *) &npln, sizeof(int), 1, sp);
    (void) fwrite((char *) &nrow, sizeof(int), 1, sp);
    (void) fwrite((char *) &ncol, sizeof(int), 1, sp);
    (void) fwrite((char *) &ezero, sizeof(float32), 1, sp);
    (void) fwrite((char *) &ezero, sizeof(float32), 1, sp);
    for (k = 0; k < npln; k++)
        (void) fwrite((char *) &pln4[k], sizeof(float32), 1, sp);
    for (i = 0; i < nrow; i++)
        (void) fwrite((char *) &row4[i], sizeof(float32), 1, sp);
    for (j = 0; j < ncol; j++)
        (void) fwrite((char *) &col4[j], sizeof(float32), 1, sp);
    for (k = 0; k < npln; k++)
        for (i = 0; i < nrow; i++)
            for (j = 0; j < ncol; j++)
                (void) fwrite((char *) &b32r3[k][i][j],
                              sizeof(float32), 1, sp);
    (void) fclose(sp);
#endif

    /*
     * binary 64-bit file - rank 2 & 3
     */

    sp = fopen("cb64r2", "w");
    (void) fwrite(fp64, sizeof(int), 1, sp);
    (void) fwrite((char *) &ione, sizeof(int), 1, sp);
    (void) fwrite((char *) &nrow, sizeof(int), 1, sp);
    (void) fwrite((char *) &ncol, sizeof(int), 1, sp);
    (void) fwrite((char *) &dzero, sizeof(float64), 1, sp);
    (void) fwrite((char *) &dzero, sizeof(float64), 1, sp);
    for (i = 0; i < nrow; i++)
        (void) fwrite((char *) &row8[i], sizeof(float64), 1, sp);
    for (j = 0; j < ncol; j++)
        (void) fwrite((char *) &col8[j], sizeof(float64), 1, sp);
    for (i = 0; i < nrow; i++)
        for (j = 0; j < ncol; j++)
            (void) fwrite((char *) &b64r2[i][j], sizeof(float64), 1,
                          sp);
    (void) fclose(sp);

    sp = fopen("cb64r3", "w");
    (void) fwrite(fp64, sizeof(int), 1, sp);
    (void) fwrite((char *) &npln, sizeof(int), 1, sp);
    (void) fwrite((char *) &nrow, sizeof(int), 1, sp);
    (void) fwrite((char *) &ncol, sizeof(int), 1, sp);
    (void) fwrite((char *) &dzero, sizeof(float64), 1, sp);
    (void) fwrite((char *) &dzero, sizeof(float64), 1, sp);
    for (k = 0; k < npln; k++)
        (void) fwrite((char *) &pln8[k], sizeof(float64), 1, sp);
    for (i = 0; i < nrow; i++)
        (void) fwrite((char *) &row8[i], sizeof(float64), 1, sp);
    for (j = 0; j < ncol; j++)
        (void) fwrite((char *) &col8[j], sizeof(float64), 1, sp);
    for (k = 0; k < npln; k++)
        for (i = 0; i < nrow; i++)
            for (j = 0; j < ncol; j++)
                (void) fwrite((char *) &b64r3[k][i][j],
                              sizeof(float64), 1, sp);
    (void) fclose(sp);
    return (0);
}
