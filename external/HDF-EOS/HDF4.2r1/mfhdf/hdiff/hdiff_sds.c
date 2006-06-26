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


#include "hdiff.h"
#include "hdiff_list.h"
#include "hdiff_mattbl.h"


static
int diff_sds_attrs(int32 sds1_id,int32 nattrs1,int32 sds2_id,int32 nattrs2,char* sds1_name);


/*-------------------------------------------------------------------------
 * Function: diff_sds
 *
 * Purpose: diff for SDS
 *
 * Return: Number of differences found
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: August 25, 2003
 *
 *-------------------------------------------------------------------------
 */

int diff_sds(const char  *fname1, 
             const char  *fname2,
             int32 sd1_id,              
             int32 sd2_id,
             int32 ref1,
             int32 ref2,
             diff_opt_t * opt)
{
 int32 sds1_id,                /* data set identifier */
       sds1_index,             /* index number of the data set */
       dtype1,                 /* SDS data type */
       dimsizes1[MAX_VAR_DIMS],/* dimensional size of SDS */
       nattrs1,                /* number of SDS attributes */
       rank1,                  /* rank of SDS */
       sds2_id,                /* data set identifier */
       sds2_index,             /* index number of the data set */
       dtype2,                 /* SDS data type */
       dimsizes2[MAX_VAR_DIMS],/* dimensional size of SDS */
       nattrs2,                /* number of SDS attributes */
       rank2,                  /* rank of SDS */
       start[MAX_VAR_DIMS],    /* read start */
       edges[MAX_VAR_DIMS],    /* read edges */
       numtype,                /* number type */
       eltsz,                  /* element size */
       nelms;                  /* number of elements */
 char  sds1_name[MAX_NC_NAME]; 
 char  sds2_name[MAX_NC_NAME]; 
 int   nfound=0;
 int   dim_diff=0;             /* dimensions are different */
 intn  empty1_sds;
 intn  empty2_sds;
 VOIDP buf1=NULL;
 VOIDP buf2=NULL;
 uint32 max_err_cnt;
 int   i;
 VOIDP fill1=NULL;
 VOIDP fill2=NULL;

/*-------------------------------------------------------------------------
 * object 1
 *-------------------------------------------------------------------------
 */
  
 sds1_index = SDreftoindex(sd1_id,ref1);
 sds1_id    = SDselect(sd1_id,sds1_index);
 
 /*obtain name,rank,dimsizes,datatype and num of attributes of sds */
 if (SDgetinfo(sds1_id,sds1_name,&rank1,dimsizes1,&dtype1,&nattrs1)==FAIL) {
   printf( "Failed to get info for SDS ref <%d>\n",ref1);
   SDendaccess(sds1_id);
   return FAIL;
  }


/*-------------------------------------------------------------------------
 * object 2
 *-------------------------------------------------------------------------
 */

 sds2_index = SDreftoindex(sd2_id,ref2);
 sds2_id    = SDselect(sd2_id,sds2_index);
 
 /*obtain name,rank,dimsizes,datatype and num of attributes of sds */
 if (SDgetinfo(sds2_id,sds2_name,&rank2,dimsizes2,&dtype2,&nattrs2)==FAIL) {
   printf( "Failed to get info for SDS ref <%d>\n",ref2);
   SDendaccess(sds2_id);
   return FAIL;
  }


 /* flag to compare SDSs */
 if (opt->sd == 1)
 {

/*-------------------------------------------------------------------------
 * check for input SDs
 *-------------------------------------------------------------------------
 */
 
 if (opt->nlvars > 0)   /* if specified vdata is selected */
 {
  int imatch = 0, j;
  for (j = 0; j < opt->nlvars; j++)
  {
   if (strcmp(sds1_name, opt->lvars[j]) == 0)
   {
    imatch = 1;
    break;
   }
  }
  if (imatch == 0)
  {
   goto out;
  }
 }  

/*-------------------------------------------------------------------------
 * check for different type
 *-------------------------------------------------------------------------
 */
 
 if (dtype1 != dtype2) 
 {
  printf("Comparison not supported\n");
  printf("<%s> has datatype %d, <%s> has datatype %d ",sds1_name,dtype1,sds2_name,dtype2);
  goto out;
 }

/*-------------------------------------------------------------------------
 * check for the same rank
 *-------------------------------------------------------------------------
 */
 
 if ( rank1 != rank2 )
 {
  printf("Comparison not supported\n");
  printf("<%s> has rank %d, dimensions ", sds1_name, rank1);
  print_dims(rank1,dimsizes1);
  printf("\n" );
  printf("<%s> has rank %d, dimensions ", sds2_name, rank2);
  print_dims(rank2,dimsizes2);
  goto out;
 }

/*-------------------------------------------------------------------------
 * check for different dimensions
 *-------------------------------------------------------------------------
 */
 
 for ( i=0; i<rank1; i++) 
 {
  if ( dimsizes1[i] != dimsizes2[i] )
   dim_diff=1;
 }

/*-------------------------------------------------------------------------
 * dimensions
 *-------------------------------------------------------------------------
 */

 if (dim_diff==1)
 {
  printf("Comparison not supported\n");
  printf("<%s> has rank %d, dimensions ", sds1_name, rank1);
  print_dims(rank1,dimsizes1);
  printf("\n" );
  printf("<%s> has rank %d, dimensions ", sds2_name, rank2);
  print_dims(rank2,dimsizes2);
  goto out;
 }

/*-------------------------------------------------------------------------
 * get size 
 *-------------------------------------------------------------------------
 */

 /* compute the number of the bytes for each value. */
 numtype = dtype1 & DFNT_MASK;
 eltsz = DFKNTsize(numtype | DFNT_NATIVE);

 /* set edges of SDS */
 nelms=1;
 for (i = 0; i < rank1; i++) {
  nelms   *= dimsizes1[i];
  edges[i] = dimsizes1[i];
  start[i] = 0;
 }

/*-------------------------------------------------------------------------
 * check if the input SDSs are empty. if so , do not read its data 
 *-------------------------------------------------------------------------
 */ 
 if (SDcheckempty( sds1_id, &empty1_sds ) == FAIL) {
  printf( "Failed to check empty SDS <%s>\n", sds1_name);
  nfound=FAIL;
  goto out;
 }
 if (empty1_sds==1 ) {
  printf( "Empty SDS <%s>\n", sds1_name);
  goto out;
 }
 if (SDcheckempty( sds2_id, &empty2_sds ) == FAIL) {
  printf( "Failed to check empty SDS <%s>\n", sds2_name);
  nfound=FAIL;
  goto out;
 }
 if (empty2_sds==1 ) {
  printf( "Empty SDS <%s>\n", sds2_name);
  goto out;
 }

/*-------------------------------------------------------------------------
 * Read
 *-------------------------------------------------------------------------
 */
 
 /* alloc */
 if ((buf1 = (VOIDP) HDmalloc(nelms * eltsz)) == NULL) {
  printf( "Failed to allocate %d elements of size %d\n", nelms, eltsz);
  nfound=FAIL;
  goto out;
 }
 /* read data */
 if (SDreaddata (sds1_id, start, NULL, edges, buf1) == FAIL) {
  printf( "Could not read SDS <%s>\n", sds1_name);
  nfound=FAIL;
  goto out;
 }
 /* alloc */
 if ((buf2 = (VOIDP) HDmalloc(nelms * eltsz)) == NULL) {
  printf( "Failed to allocate %d elements of size %d\n", nelms, eltsz);
  nfound=FAIL;
  goto out;
 }
 /* read data */
 if (SDreaddata (sds2_id, start, NULL, edges, buf2) == FAIL) {
  printf( "Could not read SDS <%s>\n", sds2_name);
  nfound=FAIL;
  goto out;
 }
 
/*-------------------------------------------------------------------------
 * get fill values
 *-------------------------------------------------------------------------
 */

 fill1 = (VOIDP) HDmalloc(eltsz);
 fill2 = (VOIDP) HDmalloc(eltsz);
 if (fill1!=NULL && SDgetfillvalue(sds1_id,fill1)<0)
 {
  HDfree(fill1);
  fill1=NULL;
 }
 if (fill2!=NULL && SDgetfillvalue(sds2_id,fill2)<0)
 {
  HDfree(fill2);
  fill2=NULL;
 }

/*-------------------------------------------------------------------------
 * Comparing
 *-------------------------------------------------------------------------
 */

 if (opt->verbose)
 printf("Comparing <%s>\n",sds1_name); 

 /* 
  If max_err_cnt is set (i.e. not its default -1), use it otherwise set it
  to tot_err_cnt so it doesn't trip  
 */
 max_err_cnt = (opt->max_err_cnt >= 0) ? opt->max_err_cnt : nelms;
 nfound=array_diff(buf1, 
                   buf2, 
                   nelms, 
                   sds1_name,
                   sds2_name,
                   rank1,
                   dimsizes1,
                   dtype1, 
                   opt->err_limit, 
                   max_err_cnt, 
                   opt->statistics, 
                   fill1, 
                   fill2);
  
 } /* flag to compare SDSs */
 
 /* flag to compare SDSs local attributes */
 if (opt->sa == 1)
 {
  diff_sds_attrs(sds1_id,nattrs1,sds2_id,nattrs2,sds1_name);
 }
 
/*-------------------------------------------------------------------------
 * close
 *-------------------------------------------------------------------------
 */

out:
 if (SDendaccess(sds1_id)<0) {
  fprintf(stderr,"SDendaccess FAIL\n");
  nfound=FAIL;
 }
 if (SDendaccess(sds2_id)<0) {
  fprintf(stderr,"SDendaccess FAIL\n");
  nfound=FAIL;
 }
 if (buf1) free(buf1);
 if (buf2) free(buf2);
 if (fill1!=NULL) HDfree(fill1);
 if (fill2!=NULL) HDfree(fill2);

 return nfound;
}




/*-------------------------------------------------------------------------
 * Function: diff_sds_attrs
 *
 * Purpose: compare SDS attributes
 *
 * Return: 
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: September 2, 2003
 *
 *-------------------------------------------------------------------------
 */

static
int diff_sds_attrs(int32 sds1_id,int32 nattrs1,int32 sds2_id,int32 nattrs2,char* sds1_name)
{
 int32 dtype1,                 /* SDS data type */
       nelms1,                 /* number of elements */
       dtype2,                 /* SDS data type */
       nelms2;                 /* number of elements */
 char  attr1_name[MAX_NC_NAME];
 char  attr2_name[MAX_NC_NAME];
 VOIDP attr1_buf=NULL;
 VOIDP attr2_buf=NULL;
 int   i, cmp;

 if ( nattrs1!=nattrs2) {
  printf( "Different number of atrributes\n");
  return -1;
 }

 /* loop through attributes */
 for (i = 0; i < nattrs1; i++) 
 {
  if (SDattrinfo (sds1_id, i, attr1_name, &dtype1, &nelms1) == FAIL) {
   printf( "Cannot get info for attribute number %d\n", i);
   continue;
  }
  
  if (SDattrinfo (sds2_id, i, attr2_name, &dtype2, &nelms2) == FAIL) {
   printf( "Cannot get info for attribute number %d\n", i);
   continue;
  }

  if (dtype1 != dtype2 || nelms1 != nelms2 || (strcmp(attr1_name,attr2_name)!=0)) {
   printf( "Different information for attribute <%d>\n", i);
   continue;
  }
  
  attr1_buf = (void *) malloc((unsigned)nelms1*DFKNTsize(dtype1 | DFNT_NATIVE));
  if (!attr1_buf) {
   printf("Out of memory!");
   return -1;
  }
  attr2_buf = (void *) malloc((unsigned)nelms2*DFKNTsize(dtype2 | DFNT_NATIVE));
  if (!attr2_buf) {
   printf("Out of memory!");
   return -1;
  }
 
  if (SDreadattr(sds1_id, i, attr1_buf)==FAIL ) {
   printf( "Could not read attribute number %d\n", i);
   if (attr1_buf) free(attr1_buf);
   if (attr2_buf) free(attr2_buf);
   continue;
  }
  if (SDreadattr(sds2_id, i, attr2_buf)==FAIL ) {
   printf( "Could not read attribute number %d\n", i);
   if (attr1_buf) free(attr1_buf);
   if (attr2_buf) free(attr2_buf);
   continue;
  }


  cmp = HDmemcmp(attr1_buf,attr2_buf,nelms1*DFKNTsize(dtype1 | DFNT_NATIVE));
  if (cmp!=0)
  {
   printf("\n---------------------------\n");
   printf ("%s:%s = \n",sds1_name,attr1_name);
   printf("<<<<\n");
   pr_att_vals((nc_type)dtype1, nelms1, attr1_buf);
   printf (" ;\n");
   printf(">>>>\n");
   pr_att_vals((nc_type)dtype2, nelms2, attr2_buf);
   printf (" ;\n");

  }

  if (attr1_buf) free(attr1_buf);
  if (attr2_buf) free(attr2_buf);
 
 }

 return 1;
}



/*-------------------------------------------------------------------------
 * Function: print_dims
 *
 * Purpose: print dimensions
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: May 9, 2003
 *
 * Comments: 
 *
 *-------------------------------------------------------------------------
 */
void print_dims( int r, int32 *d )
{
 int i;
 printf("[ " );  
 for ( i=0; i<r; i++ ) 
  printf("%d ",(int)d[i]  );
 printf("] " );
}
