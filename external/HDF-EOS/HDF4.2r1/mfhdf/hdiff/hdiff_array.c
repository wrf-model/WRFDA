
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>
#include <math.h>
#include <limits.h>
#include <float.h>
#include <assert.h>
#include "hdf.h"
#include "mfhdf.h"
#include "hdiff.h"
#include "vgint.h"

#define MYMAX(A,B) (((A) > (B)) ? (A) : (B))
#define MYMIN(A,B) (((A) < (B)) ? (A) : (B))
#define PRINT_FSTATS(T) {\
 printf("Type: %s  Npts: %d  Ndiff: %d (%f%%)\n", \
 T, tot_cnt, n_diff, 100.*(float64)n_diff/(float64)tot_cnt); \
 printf("Avg Diff: %.3e  Max Diff: %.3e\n",  \
 d_avg_diff/n_stats, d_max_diff); \
 printf("Range File1: %f/%f  File2: %f/%f\n", \
d_min_val1, d_max_val1, d_min_val2, d_max_val2); }
#define PRINT_ISTATS(T) {\
 printf("Type: %s  Npts: %d  Ndiff: %d (%f%%)\n", \
 T, tot_cnt,n_diff, 100.*(float64)n_diff/(float64)tot_cnt); \
 printf("Avg Diff: %e   Max. Diff: %d\n",  \
 (d_avg_diff / n_stats), i4_max_diff); \
 printf("Range File1: %d/%d  File2: %d/%d\n", \
i4_min_val1, i4_max_val1, i4_min_val2, i4_max_val2); }


/*-------------------------------------------------------------------------
 * printf formatting
 *-------------------------------------------------------------------------
 */
#define SPACES  "          "
#define FFORMAT "%-15f %-15f %-15f\n"
#define IFORMAT "%-15d %-15d %-15d\n"
#define CFORMAT "%-16c %-17c\n"
#define SFORMAT "%-16s %-17s\n"
#define UIFORMAT "%-15u %-15u %-15u\n"
#define LIFORMAT "%-15ld %-15ld %-15ld\n"
#define ULIFORMAT "%-15lu %-15lu %-15lu\n"


/*-------------------------------------------------------------------------
 * Function: print_pos
 *
 * Purpose: convert an array index position to matrix notation
 *
 * Return: pos matrix array
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 *-------------------------------------------------------------------------
 */
void print_pos( int        *ph, 
                uint32     curr_pos, 
                int32      *acc, 
                int32      *pos, 
                int        rank, 
                const char *obj1, 
                const char *obj2 )
{
 int i;

 /* print header */
 if ( *ph==1 )
 {
  *ph=0;
  printf("%-15s %-15s %-15s %-20s\n", 
   "position", 
   (obj1!=NULL) ? obj1 : " ", 
   (obj2!=NULL) ? obj2 : " ",
   "difference");
  printf("------------------------------------------------------------\n");
 }

 for ( i = 0; i < rank; i++)
 {
  pos[i] = curr_pos/acc[i];
  curr_pos -= acc[i]*pos[i];
 }
 assert( curr_pos == 0 );

 printf("[ " );  
 for ( i = 0; i < rank; i++)
 {
  fprintf(stdout,"%d ", pos[i]  );
 }
 printf("]" );
}

/*-------------------------------------------------------------------------
 * Function: array_diff
 *
 * Purpose: memory compare
 *
 *-------------------------------------------------------------------------
 */

int array_diff(void *buf1, 
               void *buf2, 
               uint32 tot_cnt, 
               const char *name1,
               const char *name2,
               int rank,
               int32 *dims,
               int32 type, 
               float32 err_limit, 
               uint32 max_err_cnt, 
               int32 statistics,
               void *fill1, 
               void *fill2)


{
 uint32   i;
 int8    *i1ptr1, *i1ptr2;
 int16   *i2ptr1, *i2ptr2;
 int32   *i4ptr1, *i4ptr2;
 float32 *fptr1, *fptr2;
 float64 *dptr1, *dptr2;
 float64 d_diff, d_avg_diff = 0., d_max_diff = 0.;
 float64 d_max_val1=0, d_min_val1=0, d_max_val2=0, d_min_val2=0;
 float64 d_val1, d_val2;
 float64 d_sumx = 0., d_sumy = 0., d_sumx2 = 0., d_sumy2 = 0., d_sumxy=0.;
 float64 slope, intercept, correlation;
 float32 f_diff;
 int32   i4_diff, i4_max_diff = 0;
 int32   i4_max_val1=0, i4_min_val1=0, i4_max_val2=0, i4_min_val2=0;
 int16   i2_diff;
 int8    c_diff;
 uint32  n_diff = 0;
 int     is_fill1, is_fill2;
 int     n_stats = 0;
 char    *debug;
 FILE    *fp=NULL;
 int32   acc[MAX_VAR_DIMS];   /* accumulator position */
 int32   pos[MAX_VAR_DIMS];   /* matrix position */
 int     ph=1;                /* print header  */
 int     j;

 acc[rank-1]=1;
 for(j=(rank-2); j>=0; j--)
 {
  acc[j]=acc[j+1]*(int)dims[j+1];
 }
 for ( j = 0; j < rank; j++)
  pos[j]=0;


 debug = getenv("DEBUG");
 if (debug) {
  fp = fopen("hdiff.debug", "w");
 }
 
 switch(type) {
 case DFNT_INT8:
 case DFNT_CHAR8:
  i4_max_val1 = SCHAR_MIN;
  i4_min_val1 = SCHAR_MAX;
  i4_max_val2 = SCHAR_MIN;
  i4_min_val2 = SCHAR_MAX;
  break;
 case DFNT_UINT8:
 case DFNT_UCHAR8:
  i4_max_val1 = -UCHAR_MAX -1;
  i4_min_val1 = UCHAR_MAX;
  i4_max_val2 = -UCHAR_MAX -1;
  i4_min_val2 = UCHAR_MAX;
  break;
 case DFNT_INT16:
  i4_max_val1 = SHRT_MIN;
  i4_min_val1 = SHRT_MAX;
  i4_max_val2 = SHRT_MIN;
  i4_min_val2 = SHRT_MAX;
  break;
 case DFNT_UINT16:
  i4_max_val1 = -USHRT_MAX -1;
  i4_min_val1 = USHRT_MAX;
  i4_max_val2 = -USHRT_MAX -1;
  i4_min_val2 = USHRT_MAX;
  break;
 case DFNT_INT32:
  i4_max_val1 = INT_MIN;
  i4_min_val1 = INT_MAX;
  i4_max_val2 = INT_MIN;
  i4_min_val2 = INT_MAX;
  break;
 case DFNT_UINT32:
  i4_max_val1 = INT_MIN;
  i4_min_val1 = INT_MAX;
  i4_max_val2 = INT_MIN;
  i4_min_val2 = INT_MAX;
  break;
 case DFNT_FLOAT:
  d_max_val1 = -FLT_MAX;
  d_min_val1 = FLT_MAX;
  d_max_val2 = -FLT_MAX;
  d_min_val2 = FLT_MAX;
  break;
 case DFNT_DOUBLE:
  d_max_val1 = -DBL_MAX;
  d_min_val1 = DBL_MAX;
  d_max_val2 = -DBL_MAX;
  d_min_val2 = DBL_MAX;
  break;
 default:
  printf(" bad type - %d\n", type);
 }
 switch(type)
 {
 case DFNT_INT8:
 case DFNT_UINT8:
 case DFNT_UCHAR8:
 case DFNT_CHAR8:
  i1ptr1 = (int8 *) buf1;
  i1ptr2 = (int8 *) buf2;
  for (i=0; i<tot_cnt; i++)
  {
   c_diff = (int8)abs(*i1ptr1 - *i1ptr2);
   is_fill1 = fill1 && (*i1ptr1 == *((int8 *)fill1));
   is_fill2 = fill2 && (*i1ptr2 == *((int8 *)fill2));
   if (!is_fill1 && !is_fill2) {
    d_avg_diff += (float64)c_diff;
    i4_max_diff = MYMAX(i4_max_diff, c_diff);
    d_val2 = (float64)(*i1ptr2);
    d_val1 = (float64)(*i1ptr1);
    d_sumx += d_val1;
    d_sumy += d_val2;
    d_sumx2 += d_val1 * d_val1;
    d_sumy2 += d_val2 * d_val2;
    d_sumxy += d_val1 * d_val2;
    n_stats++;
   }
   if (!is_fill1) {
    i4_max_val1 = MYMAX(i4_max_val1, (int32)(*i1ptr1));
    i4_min_val1 = MYMIN(i4_min_val1, (int32)(*i1ptr1));
   }
   if (!is_fill2) {
    i4_max_val2 = MYMAX(i4_max_val2, (int32)(*i1ptr2));
    i4_min_val2 = MYMIN(i4_min_val2, (int32)(*i1ptr2));
   }
   if (c_diff > (int32) err_limit)
   {
    n_diff++;
    if (n_diff <= max_err_cnt) {
     print_pos(&ph,i,acc,pos,rank,name1,name2);
     printf(SPACES);
     printf(IFORMAT,*i1ptr1,*i1ptr2,abs(*i1ptr1-*i1ptr2));
    }
   }                                               
   i1ptr1++;  i1ptr2++;
  }
  if (statistics) {
   PRINT_ISTATS("Byte");
  }
  
  break;
  
 case DFNT_INT16:
 case DFNT_UINT16:
  i2ptr1 = (int16 *) buf1;
  i2ptr2 = (int16 *) buf2;
  for (i=0; i<tot_cnt; i++)
  {
   i2_diff = (int16)abs(*i2ptr1 - *i2ptr2);
   is_fill1 = fill1 && (*i2ptr1 == *((int16 *)fill1));
   is_fill2 = fill2 && (*i2ptr2 == *((int16 *)fill2));
   if (debug) {
    fprintf(fp, "%d %d %d %d\n", is_fill1, is_fill2, (int32)(*i2ptr1), (int32)(*i2ptr2));
   }
   if (!is_fill1 && !is_fill2) {
    d_val1 = (float64)(*i2ptr1);
    d_val2 = (float64)(*i2ptr2);
    d_sumx += d_val1;
    d_sumy += d_val2;
    d_sumx2 += d_val1 * d_val1;
    d_sumy2 += d_val2 * d_val2;
    d_sumxy += d_val1 * d_val2;
    d_avg_diff += (float64)i2_diff;
    i4_max_diff = MYMAX(i4_max_diff, i2_diff);
    n_stats++;
   }
   if (!is_fill1) {
    i4_max_val1 = MYMAX(i4_max_val1, (int32)(*i2ptr1));
    i4_min_val1 = MYMIN(i4_min_val1, (int32)(*i2ptr1));
   }
   if (!is_fill2) {
    i4_max_val2 = MYMAX(i4_max_val2, (int32)(*i2ptr2));
    i4_min_val2 = MYMIN(i4_min_val2, (int32)(*i2ptr2));
   }
   if (i2_diff > (int) err_limit)
   {
    n_diff++;
    if (n_diff <= max_err_cnt) {
     print_pos(&ph,i,acc,pos,rank,name1,name2);
     printf(SPACES);
     printf(IFORMAT,*i2ptr1,*i2ptr2,abs(*i2ptr1-*i2ptr2));
    }
   }                                               
   i2ptr1++;  i2ptr2++;
  }
  if (statistics) {
   PRINT_ISTATS("Integer2");
  }
  break;
  
 case DFNT_INT32:
 case DFNT_UINT32:
  i4ptr1 = (int32 *) buf1;
  i4ptr2 = (int32 *) buf2;
  for (i=0; i<tot_cnt; i++)
  {
   i4_diff = labs(*i4ptr1 - *i4ptr2);
   is_fill1 = fill1 && (*i4ptr1 == *((int32 *)fill1));
   is_fill2 = fill2 && (*i4ptr2 == *((int32 *)fill2));
   if (!is_fill1 && !is_fill2) {
    d_avg_diff += (float64)i4_diff;
    d_val1 = (float64)(*i4ptr1);
    d_val2 = (float64)(*i4ptr2);
    d_sumx += d_val1;
    d_sumy += d_val2;
    d_sumx2 += d_val1 * d_val1;
    d_sumy2 += d_val2 * d_val2;
    d_sumxy += d_val1 * d_val2;
    i4_max_diff = (int32)MYMAX(i4_max_diff, (float64)(i4_diff));
    n_stats++;
   }
   if (! is_fill1) {
    i4_max_val1 = MYMAX(i4_max_val1,*i4ptr1);
    i4_min_val1 = MYMIN(i4_min_val1,*i4ptr1);
   }
    if (! is_fill2) {
    i4_max_val2 = MYMAX(i4_max_val2,*i4ptr2);
    i4_min_val2 = MYMIN(i4_min_val2,*i4ptr2);
   }
   if (i4_diff > (int32) err_limit)
   {
    n_diff++;
    if (n_diff <= max_err_cnt) {
     print_pos(&ph,i,acc,pos,rank,name1,name2);
     printf(SPACES);
     printf(IFORMAT,*i4ptr1,*i4ptr2,abs(*i4ptr1-*i4ptr2));
    }
   }                                               
   i4ptr1++;  i4ptr2++;
  }
  if (statistics) {
   PRINT_ISTATS("Integer4");
  }
  
  break;
  
 case DFNT_FLOAT:
  fptr1 = (float32 *) buf1;
  fptr2 = (float32 *) buf2;
  for (i=0; i<tot_cnt; i++)
  {
   f_diff = (float32)fabs(*fptr1 - *fptr2);
   is_fill1 = fill1 && (*fptr1 == *((float32 *)fill1));
   is_fill2 = fill2 && (*fptr2 == *((float32 *)fill2));
   if (debug) {
    fprintf(fp, "%d %d %f %f\n", is_fill1, is_fill2, *fptr1, *fptr2);
   }
   if (!is_fill1 && !is_fill2) {
    d_avg_diff += (float64)f_diff;
    d_val1 = (float64)(*fptr1);
    d_val2 = (float64)(*fptr2);
    d_sumx += d_val1;
    d_sumy += d_val2;
    d_sumx2 += d_val1 * d_val1;
    d_sumy2 += d_val2 * d_val2;
    d_sumxy += d_val1 * d_val2;
    d_max_diff = MYMAX(d_max_diff, (float64)(f_diff));
    n_stats++;
   }
   if (!is_fill1) {
    d_max_val1 = MYMAX(d_max_val1, (float64)(*fptr1));
    d_min_val1 = MYMIN(d_min_val1, (float64)(*fptr1));
   }
   if (!is_fill2) {
    d_max_val2 = MYMAX(d_max_val2, (float64)(*fptr2));
    d_min_val2 = MYMIN(d_min_val2, (float64)(*fptr2));
   }
   if (f_diff > err_limit)
   {
    n_diff++;
    if (n_diff <= max_err_cnt) {
     print_pos(&ph,i,acc,pos,rank,name1,name2);
     printf(SPACES);
     printf(FFORMAT,*fptr1,*fptr2,fabs(*fptr1-*fptr2));
    }
   }                                               
   fptr1++;  fptr2++;
  }
  if (statistics) {
   PRINT_FSTATS("Float");
  }
  break;
  
 case DFNT_DOUBLE:
  dptr1 = (float64 *) buf1;
  dptr2 = (float64 *) buf2;
  for (i=0; i<tot_cnt; i++)
  {
   d_diff = fabs(*dptr1 - *dptr2);
   is_fill1 = fill1 && (*dptr1 == *((float64 *)fill1));
   is_fill2 = fill2 && (*dptr2 == *((float64 *)fill2));
   if (!is_fill1 && !is_fill2) {
    d_avg_diff += d_diff;
    d_val1 = (float64)(*dptr1);
    d_val2 = (float64)(*dptr2);
    d_sumx += d_val1;
    d_sumy += d_val2;
    d_sumx2 += d_val1 * d_val1;
    d_sumy2 += d_val2 * d_val2;
    d_sumxy += d_val1 * d_val2;
    d_max_diff = MYMAX(d_max_diff, (d_diff));
    n_stats++;
   }
   if (! is_fill1) {
    d_max_val1 = MYMAX(d_max_val1, (*dptr1));
    d_min_val1 = MYMIN(d_min_val1, (*dptr1));
   }
   if (! is_fill2) {
    d_max_val2 = MYMAX(d_max_val2, (*dptr2));
    d_min_val2 = MYMIN(d_min_val2, (*dptr2));
   }
   if (d_diff > (float64) err_limit)
   {
    n_diff++;
    if (n_diff <= max_err_cnt) {
     print_pos(&ph,i,acc,pos,rank,name1,name2);
     printf(SPACES);
     printf(FFORMAT,*dptr1,*dptr2,fabs(*dptr1-*dptr2));
    }
   }
   dptr1++;  dptr2++;
  }
  if (statistics) {
   PRINT_FSTATS("Double");
  }
  break;
  
 default:
  printf(" bad type - %d\n", type);
  }
  if (statistics) {
   float64 sqrt_arg;
   if ((float64)n_stats * d_sumx2 - d_sumx * d_sumx != 0.0) {
    slope = ((float64)n_stats * d_sumxy - d_sumx * d_sumy) / 
     ((float64)n_stats * d_sumx2 - d_sumx * d_sumx);
    intercept = (d_sumy - slope * d_sumx) / (float64)n_stats;
    sqrt_arg = ((float64)n_stats*d_sumx2 - d_sumx*d_sumx) /
     ((float64)n_stats * d_sumy2 - d_sumy * d_sumy);
    correlation = slope * sqrt(sqrt_arg);
    printf("Regression  N: %d  Slope: %e  Intercept: %e  R: %e\n",
     n_stats, slope, intercept, correlation);
   }
   else {
    printf("Regression  Slope: NaN  Intercept: NaN  R: NaN\n");
   }
  }
  if (debug) {
   fclose(fp);
  }
  return (n_diff>0 ? 1 : 0 );
}


/*-------------------------------------------------------------------------
 * Function: vdata_cmp
 *
 * Purpose: memory compare
 *
 *-------------------------------------------------------------------------
 */


void
fmt_print(uint8 *x, int32 type)
{
 int16    s = 0;
 int32    l = 0;
 float32  f = 0;
 float64  d = 0;
 
 switch(type) 
 {
 case DFNT_CHAR:
  putchar(*x);
  break;
  
 case DFNT_UINT8:
 case DFNT_INT8:
  printf("%02x ", *x);
  break;
  
 case DFNT_UINT16:
 case DFNT_INT16:
  HDmemcpy(&s, x, sizeof(int16));
  printf("%d", s);
  break;
  
 case DFNT_UINT32:
 case DFNT_INT32:
  HDmemcpy(&l, x, sizeof(int32));
  printf("%d", l);
  break;
  
 case DFNT_FLOAT32:
  HDmemcpy(&f, x, sizeof(float32));
  printf("%f", f);
  break;
  
 case DFNT_FLOAT64:
  HDmemcpy(&d, x, sizeof(float64));
  printf("%f", f);
  break;
  
 default: 
  fprintf(stderr,"sorry, type [%d] not supported\n", type); 
  break;
  
 }
}


int
vdata_cmp(int32  vs1, int32  vs2, 
          char   *gname, 
          char   *cname, 
          uint32  max_err_cnt)
{
 int32   i, j, k, iflag;
 uint32  err_cnt;
 int32   nv1, interlace1, vsize1;
 int32   vsotag1;
 char    fields1[VSFIELDMAX*FIELDNAMELENMAX];
 char    vsclass1[VSNAMELENMAX], vsname1[VSNAMELENMAX];
 int32   nv2, interlace2, vsize2;
 int32   vsotag2;
 char    fields2[VSFIELDMAX*FIELDNAMELENMAX];
 char    vsclass2[VSNAMELENMAX], vsname2[VSNAMELENMAX];
 uint8   *buf1, *buf2, *b1, *b2;
 int32   off1[60], off2[60];
 int     ret=0;
 DYN_VWRITELIST *w1, *w2;
 
 VSinquire(vs1, &nv1, &interlace1, fields1, &vsize1, vsname1);
 VSinquire(vs2, &nv2, &interlace2, fields2, &vsize2, vsname2);
 
 vsotag1 = VSQuerytag(vs1);
 VSgetclass(vs1,vsclass1);
 
 vsotag2 = VSQuerytag(vs2);
 VSgetclass(vs2,vsclass2);
 
 if (vsotag1 != vsotag2 || nv1 != nv2 || interlace1 != interlace2 ||
  strcmp(fields1, fields2) != 0 || strcmp(vsclass1, vsclass2) != 0 ||
  (strcmp(vsclass1, "Attr0.0") != 0 && vsize1 != vsize2))
 {
  printf("\n---------------------------\n");
  printf("Vdata Name: %s <%s/%s> (Different attributes)\n",
   vsname1, gname, cname);
  printf("> <%d> nrec=%d interlace=%d fld=[%s] vsize=%d class={%s})\n",
   vsotag1, nv1, interlace1, fields1, vsize1, vsclass1);
  printf("< <%d> nrec=%d interlace=%d fld=[%s] vsize=%d class={%s})\n",
   vsotag2, nv2, interlace2, fields2, vsize2, vsclass2);
  return 1;
 }
 
 
 /* compare the data */
 
 buf1 = (uint8 *) malloc((unsigned) (nv1 * vsize1));
 buf2 = (uint8 *) malloc((unsigned) (nv2 * vsize2));
 if (!buf1 || !buf2) 
 {
  printf("Out of memory!");
  exit(0);
 }
 
 VSsetfields(vs1, fields1);
 VSread(vs1, buf1, nv1, interlace1);
 w1 = (DYN_VWRITELIST*) vswritelist(vs1);
 
 VSsetfields(vs2, fields2);
 VSread(vs2, buf2, nv2, interlace2);
 w2 = (DYN_VWRITELIST*) vswritelist(vs2);
 
 b1 = buf1;
 b2 = buf2;
 
 for (j=0; j < w1->n; j++)
  off1[j] = DFKNTsize(w1->type[j] | DFNT_NATIVE);
 
 for (j=0; j < w2->n; j++)
  off2[j] = DFKNTsize(w2->type[j] | DFNT_NATIVE);
 
 iflag = 0;
 
 err_cnt = 0;
 
 if (vsize1 == vsize2)
 {
  for (i=0; i<nv1; i++)
  {
   if (memcmp(b1, b2, (size_t)vsize1) == 0)
   {
    b1 += vsize1;   
    b2 += vsize2;
    continue;
   }
   if (iflag == 0)
   {
    iflag = 1;         /* there is a difference */
    printf("\n---------------------------\n");
    printf("Vdata Name: %s (Data record comparison)\n", 
     vsname1);
    ret=1;
   }
   
   printf("> %d: ", i);
   for (j=0; j<w1->n; j++)
   {
    for (k=0; k<w1->order[j]; k++)
    {
     fmt_print(b1, w1->type[j]);
     b1 += off1[j];
     if (w1->type[j] != DFNT_CHAR)
      putchar(' ');
    }
   }        
   putchar('\n');
   printf("< %d: ", i);
   for (j=0; j<w2->n; j++)
   {
    for (k=0; k<w2->order[j]; k++)
    {
     fmt_print(b2, w2->type[j]);
     b2 += off2[j];
     if (w2->type[j] != DFNT_CHAR)
      putchar(' ');
    }
   }        
   putchar('\n');
   
   if (max_err_cnt > 0)
   {
    err_cnt++;
    if (err_cnt >= max_err_cnt)
     break;
   }
  }
 }
 else
 {
  printf("****....\n");
  for (i=0; i<nv1; i++)
  {
   if (iflag == 0)
   {
    iflag = 1;         /* there is a difference */
    printf("\n---------------------------\n");
    printf("Vdata Name: %s (Data record comparison)\n", 
     vsname1);
    ret=1;
   }
   printf("> %d: ", i);
   for (j=0; j<w1->n; j++)
   {
    for (k=0; k<w1->order[j]; k++)
    {
     fmt_print(b1, w1->type[j]);
     b1 += off1[j];
     if (w1->type[j] != DFNT_CHAR)
      putchar(' ');
    }
   }  
   putchar('\n');
   printf("< %d: ", i);
   for (j=0; j<w2->n; j++)
   {
    for (k=0; k<w2->order[j]; k++)
    {
     fmt_print(b2, w2->type[j]);
     b1 += off2[j];
     if (w2->type[j] != DFNT_CHAR)
      putchar(' ');
    }
   }  
   putchar('\n');
   
   if (max_err_cnt > 0)
   {
    err_cnt++;
    if (err_cnt >= max_err_cnt)
     break;
   }
  }
  
 }
 
 if (buf1)free((char *) buf1);
 if (buf2)free((char *) buf2);

 return ret;
}

