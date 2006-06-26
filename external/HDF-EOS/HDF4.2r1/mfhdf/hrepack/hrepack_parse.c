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


#include <string.h>
#include <stdio.h>
#include <ctype.h> /*isdigit*/

#include "hrepack_parse.h"

#ifdef H4_HAVE_LIBSZ
#include "szlib.h"
#endif

/*-------------------------------------------------------------------------
 * Function: parse_comp
 *
 * Purpose: read compression info
 *
 * Return: a list of names, the number of names and its compression type
 *
 * Examples: 
 * "AA,B,CDE:RLE" 
 * "*:GZIP 6"
 * "A,B:NONE"
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 15, 2003
 *
 *-------------------------------------------------------------------------
 */


obj_list_t* parse_comp(const char *str, 
                       int *n_objs, 
                       comp_info_t *comp)
{
 unsigned    i, u;
 char        c;
 size_t      len=strlen(str);
 int         j, m, n, k, end_obj=-1, no_param=0, l;
 char        obj[MAX_NC_NAME]; 
 char        scomp[10];
 char        stype[5];
 char        smask[3]; 
 obj_list_t* obj_list=NULL;

 /* check for the end of object list and number of objects */
 for ( i=0, n=0; i<len; i++)
 {
  c = str[i];
  if ( c==':' )
  {
   end_obj=i;
  }
  if ( c==',' )
  {
   n++;
  }
 }

 if (end_obj==-1) { /* missing : */
  printf("Input Error: Invalid compression input in <%s>\n",str);
  exit(1);
 }
  
 n++;
 obj_list=HDmalloc(n*sizeof(obj_list_t));
 *n_objs=n;

 /* get object list */
 for ( j=0, k=0, n=0; j<end_obj; j++,k++)
 {
  c = str[j];
  obj[k]=c;
  if ( c==',' || j==end_obj-1) 
  {
   if ( c==',') obj[k]='\0'; else obj[k+1]='\0';
   strcpy(obj_list[n].obj,obj);
   memset(obj,0,sizeof(obj));
   n++;
   k=-1;
  }
 }
 /* nothing after : */
 if (end_obj+1==(int)len)
 {
  if (obj_list) free(obj_list);
  printf("Input Error: Invalid compression type in <%s>\n",str);
  exit(1);
 }


 /* get compression type */
 m=0;
 for ( i=end_obj+1, k=0; i<len; i++,k++)
 {
  c = str[i];
  scomp[k]=c;
  if ( c==' ' || i==len-1) 
  {
   if ( c==' ')  /*one more parameter */
   {     
    scomp[k]='\0';     /*cut space */
   
   /*
     SZIP is a special case , it can be
     SZIP=8,EC
     SZIP=8,NN
    */
    
    if (strcmp(scomp,"SZIP")==0)
    {
     l=-1; /* mask index check */
     for ( m=0,u=i+1; u<len; u++,m++) 
     {
      if (str[u]==',')
      {
       stype[m]='\0'; /* end digit of szip */
       l=0;  /* start EC or NN search */
       u++;  /* skip ',' */
      }
      c = str[u];
      if (!isdigit(c) && l==-1){
       if (obj_list) free(obj_list);
       printf("Input Error: Compression parameter not digit in <%s>\n",str);
       exit(1);
      }
      if (l==-1)
       stype[m]=c;
      else 
      {
       smask[l]=c;
       l++;
       if (l==2)
       {
        smask[l]='\0';
        i=len-1; /* end */
        (*n_objs)--; /* we counted an extra ',' */
        if (strcmp(smask,"NN")==0) 
         comp->szip_mode=NN_MODE;
        else if (strcmp(smask,"EC")==0)
         comp->szip_mode=EC_MODE;
        else
        {
         printf("Input Error: szip mask must be 'NN' or 'EC' \n");
         exit(1);
        }
       }
      }
     }  /* u */
    } /* SZIP */
    
    else
     
    {
     /* here we could have 1, 2 or 3 digits (2 and 3 in the JPEG case) */
     for ( m=0,u=i+1; u<len; u++,m++) 
     {
      c = str[u];
      if (!isdigit(c)){
       printf("Input Error: Compression parameter not digit in <%s>\n",str);
       exit(1);
      }
      stype[m]=c;
     } /* m */
     
    } /* else , no SZIP */
    
    
    /* set return value of the compression parameter */
    stype[m]='\0';
    comp->info=atoi(stype);
    i+=m; /* jump */
    
    
   } /* if c==' ' */


   else if (i==len-1) { /*no more parameters */
    scomp[k+1]='\0';
    no_param=1;
   }

   if (HDstrcmp(scomp,"NONE")==0)
    comp->type=COMP_CODE_NONE;
   else if (HDstrcmp(scomp,"RLE")==0)
   {
    comp->type=COMP_CODE_RLE;
    if (m>0){ /*RLE does not have parameter */
     if (obj_list) free(obj_list);
     printf("Input Error: Extra compression parameter in RLE <%s>\n",str);
     exit(1);
    }
   }
   else if (HDstrcmp(scomp,"HUFF")==0)
   {
    comp->type=COMP_CODE_SKPHUFF;
    if (no_param) { /*no more parameters, HUFF must have parameter */
     if (obj_list) free(obj_list);
     printf("Input Error: Missing compression parameter in <%s>\n",str);
     exit(1);
    }
   }
   else if (HDstrcmp(scomp,"GZIP")==0)
   {
    comp->type=COMP_CODE_DEFLATE;
    if (no_param) { /*no more parameters, GZIP must have parameter */
     if (obj_list) free(obj_list);
     printf("Input Error: Missing compression parameter in <%s>\n",str);
     exit(1);
    }
   }
   else if (HDstrcmp(scomp,"JPEG")==0)
   {
    comp->type=COMP_CODE_JPEG;
    if (no_param) { /*no more parameters, JPEG must have parameter */
     if (obj_list) free(obj_list);
     printf("Input Error: Missing compression parameter in <%s>\n",str);
     exit(1);
    }
   }
   else if (HDstrcmp(scomp,"SZIP")==0)
   {
#ifdef H4_HAVE_LIBSZ
    if (SZ_encoder_enabled()) {
       comp->type=COMP_CODE_SZIP;
       if (no_param) { /*no more parameters, SZIP must have parameter */
        if (obj_list) free(obj_list);
        printf("Input Error: Missing compression parameter in <%s>\n",str);
        exit(1);
       }
	if (comp->szip_mode==FAIL) {
		printf("Input Error: SZIP compression mode must be NN_MODE or EC_MODE");
	     exit(1);
	}
    } else {
     printf("Input Error: SZIP encoder is not available\n");
     exit(1);
    }
#else
     printf("Input Error: SZIP compression is not available\n");
     exit(1);
#endif
   }
   else {
    if (obj_list) free(obj_list);
    printf("Input Error: Invalid compression type in <%s>\n",str);
    exit(1);
   }
  }
 } /*i*/


 /* check valid parameters */
 switch (comp->type)
  {
  default:
   break;
  case COMP_CODE_RLE:
   break;
  case COMP_CODE_SKPHUFF:
   if (comp->info<=0 ){
    if (obj_list) free(obj_list);
    printf("Input Error: Invalid compression parameter in <%s>\n",str);
    exit(1);
   }
   break;
  case COMP_CODE_DEFLATE:
   if (comp->info<0 || comp->info>9 ){
    if (obj_list) free(obj_list);
    printf("Input Error: Invalid compression parameter in <%s>\n",str);
    exit(1);
   }
   break;
  case COMP_CODE_JPEG:
   if (comp->info<0 || comp->info>100 ){
    if (obj_list) free(obj_list);
    printf("Input Error: Invalid compression parameter in <%s>\n",str);
    exit(1);
   }
   break;
  case COMP_CODE_SZIP:
#ifdef H4_HAVE_LIBSZ
    if ( (comp->info<=1 || comp->info > SZ_MAX_PIXELS_PER_BLOCK) || 
         (comp->info%2!=0)  ){
    if (obj_list) free(obj_list);
    printf("Input Error: Invalid compression parameter in <%s>. \
     Pixels per block must be an even number < %d\n",str,SZ_MAX_PIXELS_PER_BLOCK);
    exit(1);
   }
#else 
    if (obj_list) free(obj_list);
    printf("Input Error: Invalid compression method in <%s>. \
     SZIP is not available < %d\n",str);
    exit(1);
#endif
   break;
  };

 return obj_list;
}


/*-------------------------------------------------------------------------
 * Function: get_scomp
 *
 * Purpose: return the compression type as a string
 *
 * Return: void
 *
 *-------------------------------------------------------------------------
 */

const char* get_scomp(comp_coder_t code)
{
 if (code==COMP_CODE_RLE)
  return "RLE";
 else if (code==COMP_CODE_NBIT)
  return "COMP_CODE_NBIT";
 else if (code==COMP_CODE_SKPHUFF)
  return "HUFF";
 else if (code==COMP_CODE_DEFLATE)
  return "GZIP";
 else if (code==COMP_CODE_JPEG)
  return "JPEG";
 else if (code==COMP_CODE_SZIP)
  return "SZIP";
 else if (code==COMP_CODE_NONE)
  return "NONE";
 else if (code==COMP_CODE_INVALID)
  return "COMP_CODE_INVALID";
 else {
  printf("Input Error in compression type\n");
  exit(1);
 }
 return NULL;
} 




/*-------------------------------------------------------------------------
 * Function: parse_chunk
 *
 * Purpose: read chunkink info
 *
 * Return: a list of names, the number of names and its chunking info
 *
 * Examples: 
 * "AA,B,CDE:10X10 
 * "*:10X10"
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 17, 2003
 *
 *-------------------------------------------------------------------------
 */


obj_list_t* parse_chunk(const char *str, 
                        int *n_objs, 
                        int32 *chunk_lengths, 
                        int *chunk_rank)
{
 obj_list_t* obj_list=NULL;
 unsigned    i;
 char        c;
 size_t      len=strlen(str);
 int         j, n, k, end_obj=-1, c_index;
 char        obj[MAX_NC_NAME]; 
 char        sdim[10];
 
 /* check for the end of object list and number of objects */
 for ( i=0, n=0; i<len; i++)
 {
  c = str[i];
  if ( c==':' )
  {
   end_obj=i;
  }
  if ( c==',' )
  {
   n++;
  }
 }

 if (end_obj==-1) { /* missing : */
  printf("Input Error: Invalid chunking input in <%s>\n",str);
  exit(1);
 }
  
 n++;
 obj_list=HDmalloc(n*sizeof(obj_list_t));
 *n_objs=n;

 /* get object list */
 for ( j=0, k=0, n=0; j<end_obj; j++,k++)
 {
  c = str[j];
  obj[k]=c;
  if ( c==',' || j==end_obj-1) 
  {
   if ( c==',') obj[k]='\0'; else obj[k+1]='\0';
   strcpy(obj_list[n].obj,obj);
   memset(obj,0,sizeof(obj));
   n++;
   k=-1;
  }
 }

 /* nothing after : */
 if (end_obj+1==(int)len)
 {
  if (obj_list) free(obj_list);
  printf("Input Error: Invalid chunking in <%s>\n",str);
  exit(1);
 }

 /* get chunk info */
 k=0; 
 for ( i=end_obj+1, c_index=0; i<len; i++)
 {
  c = str[i];
  sdim[k]=c;
  k++; /*increment sdim index */

  if (!isdigit(c) && c!='x' && c!='N' && c!='O' && c!='N' && c!='E'){
   if (obj_list) free(obj_list);
   printf("Input Error: Invalid chunking in <%s>\n",str);
   exit(1);
  }

  if ( c=='x' || i==len-1) 
  {
   if ( c=='x') {  
    sdim[k-1]='\0';  
    k=0;
    chunk_lengths[c_index]=atoi(sdim);
    if (chunk_lengths[c_index]==0) {
      if (obj_list) free(obj_list);
      printf("Input Error: Invalid chunking in <%s>\n",str);
      exit(1);
     }
    c_index++;
   }
   else if (i==len-1) { /*no more parameters */
    sdim[k]='\0';  
    k=0;
    if (strcmp(sdim,"NONE")==0)
    {
     *chunk_rank=-2;
    }
    else
    {
     chunk_lengths[c_index]=atoi(sdim);
     if (chunk_lengths[c_index]==0){
      if (obj_list) free(obj_list);
      printf("Input Error: Invalid chunking in <%s>\n",str);
      exit(1);
     }
     *chunk_rank=c_index+1;
    }
   } /*if */
  } /*if c=='x' || i==len-1 */
 } /*i*/

 return obj_list;
}



/*-------------------------------------------------------------------------
 * Function: parse_number
 *
 * Purpose: read a number from command line argument
 *
 * Return: number, -1 for FAIL
 *
 * Programmer: Pedro Vicente, pvn@ncsa.uiuc.edu
 *
 * Date: July 31, 2003
 *
 *-------------------------------------------------------------------------
 */


int parse_number(char *str)
{
 unsigned    i;
 int         n;
 char        c;
 size_t      len=strlen(str);
  
 for ( i=0; i<len; i++)
 {
  c = str[i];
  if (!isdigit(c)){
   return -1;
  }
 }
 str[i]='\0';     
 n=atoi(str);
 return n;
}




