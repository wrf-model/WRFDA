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
#include <stdio.h>
#include "hdf.h"

/*
 * Name:
 *      hdfimporttest
 *
 * Description:
 *      This program creates binary and text input files that can be
 *      used to test the hdfimport program.  
 *
 *      June 1, 1990
 *      Bob Weaver, baw@inel.gov
 *
 *      Last Revision: December 10, 2001
 *      Pankaj Kamat, pkamat@uiuc.edu
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
    
    int32     b32i2[3][4], b32i3[5][3][4];          
    int32     row4i[3], col4i[4], pln4i[5];
    int32     rowo4i = (int32)11 , colo4i = (int32)21 , plno4i = (int32)51 ;
    int32     rowi4i = (int32)1 , coli4i = (int32)2 , plni4i = (int32)5 ;
    int32 	  ezeroi = (int32)0;
    	
    int16     b16i2[3][4], b16i3[5][3][4];          
    int16     row4i16[3], col4i16[4], pln4i16[5];
    int16     rowo4i16 = (int16)11 , colo4i16 = (int16)21 , plno4i16 = (int16)51 ;
    int16     rowi4i16 = (int16)1 , coli4i16 = (int16)2 , plni4i16 = (int16)5 ;
    int16 	  ezeroi16 = (int16)0;
    
    int8     b8i2[3][4], b8i3[5][3][4];          
    int8     row4i8[3], col4i8[4], pln4i8[5];
    int8     rowo4i8 = (int8)11 , colo4i8 = (int8)21 , plno4i8 = (int8)51 ;
    int8     rowi4i8 = (int8)1 , coli4i8 = (int8)2 , plni4i8 = (int8)5 ;
    int8 	  ezeroi8 = (int8)0;
    
    float64     b64r2[3][4], b64r3[5][3][4];
    float64     row8[3], col8[4], pln8[5];
    float64     rowo8 = 11.0e0, colo8 = 21.0e0, plno8 = 51.0e0;
    float64     rowi8 = 1.0e0, coli8 = 2.0e0, plni8 = 5.0e0;
    float64     dzero = 0.0e0;

    const char *text = "TEXT";
    const char *fp32 = "FP32";
    const char *fp64 = "FP64";
    const char *in32 = "IN32";
    const char *in16 = "IN16";
    const char *in8  = "IN08";	

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
    
    row4i[0] = rowo4i;
    col4i[0] = colo4i;
    pln4i[0] = plno4i;
    
    row4i16[0] = rowo4i16;
    col4i16[0] = colo4i16;
    pln4i16[0] = plno4i16;
    
    row4i8[0] = rowo4i8;
    col4i8[0] = colo4i8;
    pln4i8[0] = plno4i8;
    
    
    

    for (i = 1; i < nrow; i++)
      {
          row4[i] = row4[i - 1] + rowi4;
          row8[i] = row8[i - 1] + rowi8;
	    row4i[i] = row4i[i - 1] + rowi4i;
	    row4i16[i] = row4i16[i - 1] + rowi4i16;
	    row4i8[i] = row4i8[i - 1] + rowi4i8;      
	}
	    
    for (j = 1; j < ncol; j++)
      {
          col4[j] = col4[j - 1] + coli4;
          col8[j] = col8[j - 1] + coli8;
	    col4i[j] = col4i[j - 1] + coli4i;
	    col4i16[j] = col4i16[j - 1] + coli4i16;	    
	    col4i8[j] = col4i8[j - 1] + coli4i8;
      }
    for (k = 1; k < npln; k++)
      {
          pln4[k] = pln4[k - 1] + plni4;
          pln8[k] = pln8[k - 1] + plni8;
	    pln4i[k] = pln4i[k - 1] + plni4i;
	    pln4i16[k] = pln4i16[k - 1] + plni4i16;	    
	    pln4i8[k] = pln4i8[k - 1] + plni4i8;
	    	    
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
		    b32i2[i][j] = row4i[i] + col4i[j];  
		    b16i2[i][j] = row4i16[i] + col4i16[j]; 
		    b8i2[i][j] = row4i8[i] + col4i8[j]; 
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
			    b32i3[k][i][j] = row4i[i] + col4i[j] + pln4i[k];
			    b16i3[k][i][j] = row4i16[i] + col4i16[j] + pln4i16[k];
			    b8i3[k][i][j] = row4i8[i] + col4i8[j] + pln4i8[k];
                  }
            }
      }

    /*
     * text file - rank 2 & 3
     */

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
    
     sp = fopen("ctxti2", "w");
    (void) fprintf(sp, "%s\n", text);
    (void) fprintf(sp, "%10d%10d%10d\n", ione, nrow, ncol);
    (void) fprintf(sp, "%10d%10d\n", ezeroi, ezeroi);
    for (i = 0; i < nrow; i++)
        (void) fprintf(sp, "%10d", row4i[i]);
    (void) fprintf(sp, "\n");
    for (j = 0; j < ncol; j++)
        (void) fprintf(sp, "%10d", col4i[j]);
    (void) fprintf(sp, "\n");
    for (i = 0; i < nrow; i++)
      {
          for (j = 0; j < ncol; j++)
              (void) fprintf(sp, "%10d", b32i2[i][j]);
          (void) fprintf(sp, "\n");
      }
    (void) fclose(sp);

 	sp = fopen("ctxti162", "w");
    (void) fprintf(sp, "%s\n", text);
    (void) fprintf(sp, "%10d%10d%10d\n", ione, nrow, ncol);
    (void) fprintf(sp, "%10u%10u\n", ezeroi16, ezeroi16);
    for (i = 0; i < nrow; i++)
        (void) fprintf(sp, "%10u", row4i16[i]);
    (void) fprintf(sp, "\n");
    for (j = 0; j < ncol; j++)
        (void) fprintf(sp, "%10u", col4i16[j]);
    (void) fprintf(sp, "\n");
    for (i = 0; i < nrow; i++)
      {
          for (j = 0; j < ncol; j++)
              (void) fprintf(sp, "%10u", b16i2[i][j]);
          (void) fprintf(sp, "\n");
      }
    (void) fclose(sp);
    
    sp = fopen("ctxti82", "w");
    (void) fprintf(sp, "%s\n", text);
    (void) fprintf(sp, "%10d%10d%10d\n", ione, nrow, ncol);
    (void) fprintf(sp, "%10c%10c\n", ezeroi8, ezeroi8);
    for (i = 0; i < nrow; i++)
        (void) fprintf(sp, "%10c", row4i8[i]);
    (void) fprintf(sp, "\n");
    for (j = 0; j < ncol; j++)
        (void) fprintf(sp, "%10c", col4i8[j]);
    (void) fprintf(sp, "\n");
    for (i = 0; i < nrow; i++)
      {
          for (j = 0; j < ncol; j++)
              (void) fprintf(sp, "%10c", b8i2[i][j]);
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

    sp = fopen("cb32r2", "w");
    (void) fwrite(fp32, strlen(fp32), 1, sp);
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

    sp = fopen("cb32i2", "w");
    (void) fwrite(in32, strlen(in32), 1, sp);
    (void) fwrite((char *) &ione, sizeof(int), 1, sp);
    (void) fwrite((char *) &nrow, sizeof(int), 1, sp);
    (void) fwrite((char *) &ncol, sizeof(int), 1, sp);
    (void) fwrite((char *) &ezeroi, sizeof(int32), 1, sp);
    (void) fwrite((char *) &ezeroi, sizeof(int32), 1, sp);
    for (i = 0; i < nrow; i++)
        (void) fwrite((char *) &row4i[i], sizeof(int32), 1, sp);
    for (j = 0; j < ncol; j++)
        (void) fwrite((char *) &col4i[j], sizeof(int32), 1, sp);
    for (i = 0; i < nrow; i++)
        for (j = 0; j < ncol; j++)
            (void) fwrite((char *) &b32i2[i][j], sizeof(int32), 1,
                          sp);
    (void) fclose(sp);

    sp = fopen("cb16i2", "w");
    (void) fwrite(in16, strlen(in16), 1, sp);
    (void) fwrite((char *) &ione, sizeof(int), 1, sp);
    (void) fwrite((char *) &nrow, sizeof(int), 1, sp);
    (void) fwrite((char *) &ncol, sizeof(int), 1, sp);
    (void) fwrite((char *) &ezeroi16, sizeof(int16), 1, sp);
    (void) fwrite((char *) &ezeroi, sizeof(int16), 1, sp);
    for (i = 0; i < nrow; i++)
        (void) fwrite((char *) &row4i16[i], sizeof(int16), 1, sp);
    for (j = 0; j < ncol; j++)
        (void) fwrite((char *) &col4i16[j], sizeof(int16), 1, sp);
    for (i = 0; i < nrow; i++)
        for (j = 0; j < ncol; j++)
            (void) fwrite((char *) &b16i2[i][j], sizeof(int16), 1,
                          sp);
    (void) fclose(sp);
    
     sp = fopen("cb8i2", "w");
    (void) fwrite(in8, strlen(in8), 1, sp);
    (void) fwrite((char *) &ione, sizeof(int), 1, sp);
    (void) fwrite((char *) &nrow, sizeof(int), 1, sp);
    (void) fwrite((char *) &ncol, sizeof(int), 1, sp);
    (void) fwrite((char *) &ezeroi8, sizeof(int8), 1, sp);
    (void) fwrite((char *) &ezeroi8, sizeof(int8), 1, sp);
    for (i = 0; i < nrow; i++)
        (void) fwrite((char *) &row4i8[i], sizeof(int8), 1, sp);
    for (j = 0; j < ncol; j++)
        (void) fwrite((char *) &col4i8[j], sizeof(int8), 1, sp);
    for (i = 0; i < nrow; i++)
        for (j = 0; j < ncol; j++)
            (void) fwrite((char *) &b8i2[i][j], sizeof(int8), 1,
                          sp);
    (void) fclose(sp);

    sp = fopen("cb32r3", "w");
    (void) fwrite(fp32, strlen(fp32), 1, sp);
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
    
     sp = fopen("cb32i3", "w");
    (void) fwrite(in32, strlen(in32), 1, sp);
    (void) fwrite((char *) &npln, sizeof(int), 1, sp);
    (void) fwrite((char *) &nrow, sizeof(int), 1, sp);
    (void) fwrite((char *) &ncol, sizeof(int), 1, sp);
    (void) fwrite((char *) &ezeroi, sizeof(int32), 1, sp);
    (void) fwrite((char *) &ezeroi, sizeof(int32), 1, sp);
    for (k = 0; k < npln; k++)
        (void) fwrite((char *) &pln4i[k], sizeof(int32), 1, sp);
    for (i = 0; i < nrow; i++)
        (void) fwrite((char *) &row4i[i], sizeof(int32), 1, sp);
    for (j = 0; j < ncol; j++)
        (void) fwrite((char *) &col4i[j], sizeof(int32), 1, sp);
    for (k = 0; k < npln; k++)
    	for (i = 0; i < nrow; i++)
        for (j = 0; j < ncol; j++)
            (void) fwrite((char *) &b32i3[k][i][j], sizeof(int32), 1,
                          sp);
    (void) fclose(sp);
    
     sp = fopen("cb16i3", "w");
    (void) fwrite(in16, strlen(in16), 1, sp);
    (void) fwrite((char *) &npln, sizeof(int), 1, sp);
    (void) fwrite((char *) &nrow, sizeof(int), 1, sp);
    (void) fwrite((char *) &ncol, sizeof(int), 1, sp);
    (void) fwrite((char *) &ezeroi16, sizeof(int16), 1, sp);
    (void) fwrite((char *) &ezeroi16, sizeof(int16), 1, sp);
    for (k = 0; k < npln; k++)
        (void) fwrite((char *) &pln4i16[k], sizeof(int16), 1, sp);
    for (i = 0; i < nrow; i++)
        (void) fwrite((char *) &row4i16[i], sizeof(int16), 1, sp);
    for (j = 0; j < ncol; j++)
        (void) fwrite((char *) &col4i16[j], sizeof(int16), 1, sp);
    for (k = 0; k < npln; k++)
    	for (i = 0; i < nrow; i++)
        for (j = 0; j < ncol; j++)
            (void) fwrite((char *) &b16i3[k][i][j], sizeof(int16), 1,
                          sp);
    (void) fclose(sp);
    
     sp = fopen("cb8i3", "w");
    (void) fwrite(in8, strlen(in8), 1, sp);
    (void) fwrite((char *) &npln, sizeof(int), 1, sp);
    (void) fwrite((char *) &nrow, sizeof(int), 1, sp);
    (void) fwrite((char *) &ncol, sizeof(int), 1, sp);
    (void) fwrite((char *) &ezeroi8, sizeof(int8), 1, sp);
    (void) fwrite((char *) &ezeroi8, sizeof(int8), 1, sp);
    for (k = 0; k < npln; k++)
        (void) fwrite((char *) &pln4i8[k], sizeof(int8), 1, sp);
    for (i = 0; i < nrow; i++)
        (void) fwrite((char *) &row4i8[i], sizeof(int8), 1, sp);
    for (j = 0; j < ncol; j++)
        (void) fwrite((char *) &col4i8[j], sizeof(int8), 1, sp);
    for (k = 0; k < npln; k++)
    	for (i = 0; i < nrow; i++)
        for (j = 0; j < ncol; j++)
            (void) fwrite((char *) &b8i3[k][i][j], sizeof(int8), 1,
                          sp);
    (void) fclose(sp);
  
    /*
     * binary 64-bit file - rank 2 & 3
     */

    sp = fopen("cb64r2", "w");
    (void) fwrite(fp64, strlen(fp64), 1, sp);
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
    (void) fwrite(fp64, strlen(fp64), 1, sp);
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
