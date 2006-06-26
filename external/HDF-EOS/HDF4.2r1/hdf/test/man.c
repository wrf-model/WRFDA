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
static char RcsId[] = "@(#)$Revision: 1.25 $";
#endif

/* $Id: man.c,v 1.25 1998/01/27 17:50:58 bmribler Exp $ */

/***********************************************************
*
* Test program:  man
*
* This program stores annotations in a file using Multi-file interface ANxxx
* It tests writing/reading file labels/descriptions and reading/writing 
* anntotations to SDS's and RIS. The old interfaces for these elements
* are used to write several SDSs and corresponding RISs to a file.
* It doesn't test writing annotations for Vgroups or Vdata.
* 
*  NOTE: No verification of the SDS or RI data in the file
*        is done i.e. data is not read back in.
*
* 1. Writes file labels and descriptions.
*
* 2. It writes labels and descriptions for all 2 out of 3 SDSs.
*
* 3. It writes labels and descriptions for all RISs.
*
* 4. Tests re-writing of annotation(only one type is tested but
*    it should suffice to test the internals) while preserving
*    original tag/ref of element.
*
*************************************************************/

/* includes */
#include "tproto.h"

#define TESTFILE "tman.hdf" /* testfile to write annotations to */
#define ROWS          10    /* row size of dataset/image */
#define COLS          10    /* column size of dataset/image */
#define REPS           3    /* number of images/data sets to write to file */

/* File labels/desriptions to write */
static const char *file_lab[3] =
{"File label #1: aaa", 
 "File label #2: bbbbbb",
 "File label #3: cccc"};

static const char *file_desc[2] =
{"File Descr #1: 1  2  3  4  5  6  7  8  9 10 11 12 13"
 "\n        14 15 16 17 18 19 20 **END FILE DESCR**\n",
 "File Descr #2: A B C D E F G H I J K L \n"
 "               M N O  **END FILE DESCR**\n"};

/* Data labels /descriptions to write */
static const char *labsds[2] = 
{"Object label #1:  sds", 
 "Object label #1.1:sds"};

static const char *labris[2] = 
{"Object label #2:  image", 
 "Object label #2.1:image"};

static const char *descsds[2] =
{"Object Descr #1:   1  2  3  4  5  6  7  8  9 10 11 12 "
 "\n       13 14 15 16 17 18 19 20 **END SDS DESCR**\n",
 "Object Descr #1.1: 1  2  3  4  5  6  7  8  9 10 11 12 "
 "\n       13 14 15 16 17 18 19 20 **END SDS DESCR**\n"};

static const char *descris[2] = 
{"Object Descr #2:   A B C D E F G H I J K L \n"
 "                 M N O **END IMAGE DESCR **\n",
 "Object Descr #2.1: A B C D E F G H I J K L \n"
 "                 M N O **END IMAGE DESCR **\n"};

/* fcn Prototypes */
static VOID 
genimage (int height, int width, float32 *data, uint8 *image);

static VOID 
gen2Dfloat (int height, int width, float32 *data);

static int32
check_fann(const char *fname);

static int32
check_lab_desc (const char *fname, uint16 tag, uint16 ref,const char* label[],
                const char *desc[]);

/****************************************************************
**
**  gen2Dfloat:  generate 2-D data array
**
****************************************************************/
static      VOID
gen2Dfloat(int height, int width, float32 *data)
{
    int    i, j;
    float32  *pdata = NULL;

    /* store one value per row, increasing by one for each row */
    pdata = data;
    for (i = 0; i < height; i++)
        for (j = 0; j < width; j++)
            *pdata++ = (float32) (i + 1);

} /* gen2Dfloat() */

/****************************************************************
**
**  genimage:  generate image from 2-D float array
**
****************************************************************/
static      VOID
genimage(int height, int width, float32 *data, uint8 *image)
{
    int    i, limit;
    float32  *pdata = NULL;
    float32  max, min, multiplier;

    limit = height * width;
    pdata = data;
    max   = min = *pdata;
    for (i = 0; i < limit; i++, pdata++)
      {
          max = (max > *pdata) ? max : *pdata;
          min = (min < *pdata) ? min : *pdata;
      }

    /* store one value per row, increasing by one for each row */
    pdata  = data;
    multiplier = (float32) 255.0 / (max - min);
    for (i = 0; i < limit; i++)
        *image++ = (uint8) (((*pdata++) - min) * multiplier);
} /* geniamge() */

/****************************************************************
**
**  check_fann_rewrite:  Check rewriting a file label while
**                       preserving original tag/ref of element 
**
****************************************************************/
static int32
check_fann_rewrite(const char *fname)
{
    int32 ret = SUCCEED;         /* return value */
    int32 file_handle; /* file handle */
    int32 an_handle;   /* annotation interface handle */
    int32 ann_handle;  /* annotation handle */
    int32 nflabs,      /* number of file labels */
        nfdescs,     /* number of file descs */
        nolabs,      /* total number of data labels */
        nodescs;     /* total number of data descs */
    int32 ann_len;     /* length of annotation */
    uint16 atag;       /* annotation tag */
    uint16 aref;       /* annotation ref */
    char *ann_label = NULL; /* annotation label */
    int32 ann_id;
    uint16 ann_tag;
    uint16 ann_ref;
    uint16 b_ann_tag;
    uint16 b_ann_ref;

    /* open file again for writing */
    ret = file_handle = Hopen(fname, DFACC_RDWR,0);
    RESULT("Hopen");

    /* Start annotation handling */
    ret = an_handle = ANstart(file_handle);
    RESULT("ANstart");

    /* Get Info On Annotations In File */
    ret = ANfileinfo(an_handle, &nflabs, &nfdescs, &nolabs, &nodescs);
    RESULT("Anfileinfo");

#ifdef AN_DEBUG
    printf("There Are Nflabs=%d, Nfdescs=%d, Nolabs=%d, Nodescs=%d \n",nflabs,
           nfdescs, nolabs, nodescs);
#endif

    /* get first label */
    ann_handle = ret = ANselect(an_handle, 0, AN_FILE_LABEL);
    RESULT("ANselect");

    /* save tag ref here */
    ret = ANget_tagref(an_handle,0,AN_FILE_LABEL,&b_ann_tag,&b_ann_ref);
    RESULT("ANget_tagref");

    /* rewrite it with 3rd label entry */
    ret = ANwriteann(ann_handle, file_lab[2], (int32)HDstrlen(file_lab[2]));
    RESULT("ANwriteann");

    ret = ANendaccess(ann_handle);
    RESULT("ANendaccess");

    ret = ANend(an_handle);
    RESULT("ANend");

    /* Now get ready to read the first file label back in */

    /* Start annotation handling */
    ret = an_handle = ANstart(file_handle);
    RESULT("ANstart");

    /* read the first file label */
    ann_handle = ret = ANselect(an_handle, 0, AN_FILE_LABEL);
    RESULT("ANselect");

    /* get file label length */
    ann_len = ret = ANannlen(ann_handle);
    RESULT("ANannlen");

    /* see if this routine works */
    ret = ANget_tagref(an_handle,0,AN_FILE_LABEL,&atag,&aref);
    RESULT("ANget_tagref");

    /* verify against previous tag/ref, should be the same as before
       re-write */
    if (b_ann_tag != atag || b_ann_ref != aref)
      {
          printf(">>> Failed to preseve tag/ref for rewriting of file label\n");
          num_errs++;
      }

    /* see if this routine works. Use tag/ref from ANget_tagref() */
    ann_id = ret = ANtagref2id(an_handle,atag,aref);
    RESULT("ANtagref2id");

    if (ann_id != ann_handle)
      {
          printf(">>> ANtagref2id failed to return valid annotation handle \n");
          num_errs++;
      }

    /* see if this routine works. Use annotation id from ANtagref2id() */
    ret = ANid2tagref(ann_id,&ann_tag,&ann_ref);
    RESULT("ANid2tagref");

    if (ann_tag != atag || ann_ref != aref)
      {
          printf(">>> ANid2tagref failed to return valid tag and ref \n");
          num_errs++;
      }

    /* see if this routine works. Just a type, not specific. -BMR */
    ann_tag = ANatype2tag (AN_DATA_DESC);
    ret = (ann_tag != DFTAG_DIA) ? FAIL : 0;  /*these two statements may not */
    RESULT("ANatype2tag");	/* be necessary but that seems like a pattern */
				/* in the test so I put them here too. -BMR */

    if (ann_tag != DFTAG_DIA)
      {
          printf(">>> ANatype2tag failed to return valid tag from a type \n");
          num_errs++;
      }

    /* check ann length against 3rd label */
    if (ann_len != (int32) HDstrlen(file_lab[2]))
      {
          printf("\t>>>BAD FILE LABEL LENGTH.\n\t    IS: %d\n\tSHOULD BE: %d<<<\n",
                 (int) ann_len, (int) HDstrlen(file_lab[2]));
          num_errs++;
      }

    /* allocate space for label */
    if (ann_label == NULL)
      {
          if ((ann_label = (char *)HDmalloc((ann_len+1)*sizeof(char))) 
              == NULL)
            {
                printf("Error: failed to allocate space to hold file label \n");
                return FAIL;
            }
          HDmemset(ann_label,'\0', ann_len+1);
      }
      
    /* read label */
    ret = ANreadann(ann_handle, ann_label, ann_len+1);
    RESULT("ANreadann");

    /* end acces to label */
    ret = ANendaccess(ann_handle);
    RESULT("ANendaccess");      
      
    /* check read label against 3rd label*/
    if (HDstrncmp((const char *)ann_label, (const char *)file_lab[2],(size_t)(ann_len+1)) != 0)
      {
          printf("\t>>>BAD FILE LABEL. \n\t       IS: %s; \n\tSHOULD BE: %s<<<\n",
                 ann_label, file_lab[2]);
          num_errs++;
      }

#ifdef AN_DEBUG
    printf("found ann_len=%d, file label=%s\n", strlen(ann_label),ann_label);
#endif

    /* Clean up */
    HDfree(ann_label); /* free up space */
    ann_label = NULL;

    /* end access to annotations */
    ret = ANend(an_handle);
    RESULT("ANend");

    ret = Hclose(file_handle);   /* close file */
    RESULT("Hclose");

    return SUCCEED;
} /* check_fann_rewrite() */

/****************************************************************
**
**  check_fann:  Check file labels and descriptions in file
**
****************************************************************/
static int32
check_fann(const char *fname)
{
    int32 ret = SUCCEED;         /* return value */
    int32 file_handle; /* file handle */
    int32 an_handle;   /* annotation interface handle */
    int32 ann_handle;  /* annotation handle */
    int32 nflabs,      /* number of file labels */
        nfdescs,     /* number of file descs */
        nolabs,      /* total number of data labels */
        nodescs;     /* total number of data descs */
    int32 ann_len;     /* length of annotation */
    uint16 atag;       /* annotation tag */
    uint16 aref;       /* annotation ref */
    char *ann_label = NULL; /* annotation label */
    char *ann_desc = NULL;  /* annotation desc */
    intn indx;
    int32 ann_id;
    uint16 ann_tag;
    uint16 ann_ref;
    intn  i;

    /* open file again */
    ret = file_handle = Hopen(fname, DFACC_READ,0);
    RESULT("Hopen");

    /* Start annotation handling */
    ret = an_handle = ANstart(file_handle);
    RESULT("ANstart");

    /* Get Info On Annotations In File */
    ret = ANfileinfo(an_handle, &nflabs, &nfdescs, &nolabs, &nodescs);
    RESULT("Anfileinfo");

#ifdef AN_DEBUG
    printf("There Are Nflabs=%d, Nfdescs=%d, Nolabs=%d, Nodescs=%d \n",nflabs,
           nfdescs, nolabs, nodescs);
#endif

    /* read file labels */
    for (i = 0; i < nflabs; i++)
      { /* select file label */
          indx = i;
          ann_handle = ret = ANselect(an_handle, indx, AN_FILE_LABEL);
          RESULT("ANselect");

          /* get file label length */
          ann_len = ret = ANannlen(ann_handle);
          RESULT("ANannlen");

          /* see if this routine works */
          ret = ANget_tagref(an_handle,indx,AN_FILE_LABEL,&atag,&aref);
          RESULT("ANget_tagref");

          /* see if this routine works. Use tag/ref from ANget_tagref() */
          ann_id = ret = ANtagref2id(an_handle,atag,aref);
          RESULT("ANtagref2id");

          if (ann_id != ann_handle)
            {
                printf(">>> ANtagref2id failed to return valid annotation handle \n");
                num_errs++;
            }

          /* see if this routine works. Use annotation id from ANtagref2id() */
          ret = ANid2tagref(ann_id,&ann_tag,&ann_ref);
          RESULT("ANid2tagref");

          if (ann_tag != atag || ann_ref != aref)
            {
                printf(">>> ANid2tagref failed to return valid tag and ref \n");
                num_errs++;
            }

          /* check ann length */
          if (ann_len != (int32) HDstrlen(file_lab[i]))
            {
                printf("\t>>>BAD FILE LABEL LENGTH.\n\t    IS: %d\n\tSHOULD BE: %d<<<\n",
                       (int) ann_len, (int) HDstrlen(file_lab[i]));
                num_errs++;
            }

          /* allocate space for label */
          if (ann_label == NULL)
            {
                if ((ann_label = (char *)HDmalloc((ann_len+1)*sizeof(char))) 
                    == NULL)
                  {
                      printf("Error: failed to allocate space to hold file label \n");
                      return FAIL;
                  }
                HDmemset(ann_label,'\0', ann_len+1);
            }
      
          /* read label */
          ret = ANreadann(ann_handle, ann_label, ann_len+1);
          RESULT("ANreadann");

          /* end acces to label */
          ret = ANendaccess(ann_handle);
          RESULT("ANendaccess");      
      
          /* check label */
          if (HDstrncmp((const char *)ann_label, (const char *)file_lab[i],(size_t)(ann_len+1)) != 0)
            {
                printf("\t>>>BAD FILE LABEL. \n\t       IS: %s; \n\tSHOULD BE: %s<<<\n",
                       ann_label, file_lab[i]);
                num_errs++;
            }

#ifdef AN_DEBUG
          printf("found ann_len=%d, file label=%s\n", strlen(ann_label),ann_label);
#endif

          HDfree(ann_label); /* free up space */
          ann_label = NULL;
      } /* end for nflabs */

    /* read file descriptions */
    for (i = 0; i < nfdescs; i++)
      { /* select file label */
          indx = i;
          ann_handle = ret = ANselect(an_handle, indx, AN_FILE_DESC);
          RESULT("ANselect");

          /* get file label length */
          ann_len = ret = ANannlen(ann_handle);
          RESULT("ANannlen");
        
          /* check ann length */
          if (ann_len != (int32) HDstrlen(file_desc[i]))
            {
                printf("\t>>>BAD FILE DESC LENGTH.\n\t    IS: %d\n\tSHOULD BE: %d<<<\n",
                       (int) ann_len, (int) HDstrlen(file_desc[i]));
                num_errs++;
            }

          /* allocate space for desc */
          if (ann_desc == NULL)
            {
                if ((ann_desc = (char *)HDmalloc((ann_len+1)*sizeof(char))) 
                    == NULL)
                  {
                      printf("Error: failed to allocate space to hold file desc  \n");
                      return FAIL;
                  }
                HDmemset(ann_desc,'\0', ann_len+1);
            }
      
          /* read desc */
          ret = ANreadann(ann_handle, ann_desc, ann_len+1);
          RESULT("ANreadann");

          /* end access to desc */
          ret = ANendaccess(ann_handle);
          RESULT("ANendaccess");      
            
          /* check desc */
          if (HDstrncmp((const char *)ann_desc,(const char *)file_desc[i],(size_t)(ann_len+1)) != 0)
            {
                printf("\t>>>BAD FILE DESC. \n\t       IS: %s; \n\tSHOULD BE: %s<<<\n",
                       ann_desc, file_desc[i]);
                num_errs++;
            }

#ifdef AN_DEBUG
          printf("found ann_len=%d, file desc=%s\n", strlen(ann_desc),ann_desc);
#endif

          HDfree(ann_desc); /* free up space */
          ann_desc = NULL;
      } /* end for nfdescs */

    /* Clean up */
    if (ann_label != NULL)
        HDfree(ann_label);
    if (ann_desc != NULL)
        HDfree(ann_desc);

    /* end access to annotations */
    ANend(an_handle);
    Hclose(file_handle);   /* close file */

    return SUCCEED;
} /* check_fann() */


/****************************************************************
**
**  check_lab_desc:  read and compare label and description
**                   with expected ones
**
****************************************************************/
static  int32
check_lab_desc(const char *fname, uint16 tag, uint16 ref, const char *label[], 
               const char *desc[])
{
    int32 ret = SUCCEED;         /* return value */
    int32 file_handle; /* file handle */
    int32 an_handle;   /* annotation interface handle */
    int32 nflabs,      /* number of file labels */
        nfdescs,     /* number of file descs */
        nolabs,      /* total number of data labels */
        nodescs;     /* total number of data descs */
    int32 ann_len;     /* length of annotation */
    char *ann_label = NULL; /* annotation label */
    char *ann_desc = NULL;  /* annotation desc */
    intn  num_dlabels,  /* number of data labels for an element */
        num_ddescs;   /* number of data descs for an element */
    int32 *dlabels = NULL; /* array of data lables for an element */
    int32 *ddescs = NULL;  /* array of data descs for an element */
    int i;

    /* open file again */
    ret = file_handle = Hopen(fname, DFACC_READ, 0);
    RESULT("Hopen");

    /* Start annotation interface */
    ret = an_handle = ANstart(file_handle);
    RESULT("ANstart");

    /* Get Info On Annotations In File */
    ret = ANfileinfo(an_handle, &nflabs, &nfdescs, &nolabs, &nodescs);
    RESULT("Anfileinfo");
#ifdef AN_DEBUG
    printf("There Are Nflabs=%d, Nfdescs=%d, Nolabs=%d, Nodescs=%d \n",nflabs,
           nfdescs, nolabs, nodescs);
#endif

    /* Get number of label annotations with this tag/ref */
    num_dlabels = ret = ANnumann(an_handle, AN_DATA_LABEL, tag, ref);
    RESULT("ANnumann");
#ifdef AN_DEBUG
    printf("there are %d data labels for tag=%d, ref=%d \n", num_dlabels, 
           tag, ref);
#endif

    /* Get number of label descs with this tag/ref */
    num_ddescs = ret = ANnumann(an_handle, AN_DATA_DESC, tag, ref);
    RESULT("ANnumann"); 
#ifdef AN_DEBUG
    printf("There Are %d Data Descriptions For Tag=%d, Ref=%d \n", 
           num_ddescs, tag, ref);
#endif

    /* allocate space for list of label annotation id's with this tag/ref */
    if (num_dlabels == 0)
      {
          printf("Error: no data labels found\n");
          num_errs++;
      }
    else
      {
          if ((dlabels = (int32 *)HDmalloc(num_dlabels * sizeof(int32))) == NULL)
            {
                printf("Error: failed to allocate space to hold data label ids\n");
                return FAIL;
            }
      }

    /* allocate space for list of description annotation id's with this tag/ref */
    if (num_ddescs == 0)
      {
          printf("Error: no data descriptions found\n");
          num_errs++;
      }
    else
      {
          if ((ddescs = (int32 *)HDmalloc(num_ddescs * sizeof(int32))) == NULL)
            {
                printf("Error: failed to allocate space to hold data descs ids\n");
                return FAIL;
            }
      }

    /* get list of label annotations id's with this tag/ref */
    ret = ANannlist(an_handle, AN_DATA_LABEL, tag, ref, dlabels);
    RESULT("ANannlist");
    if (ret != num_dlabels)
        printf("Error:ret!=nlabels there are %d data labels for tag=%d,ref=%d \n", 
               num_dlabels, tag, ref);

    /* get list of description annotations id's with this tag/ref */
    ret = ANannlist(an_handle, AN_DATA_DESC, tag, ref, ddescs);
    RESULT("ANannlist");
    if (ret != num_ddescs)
        printf("Error:ret!=ndescs there are %d data descss for tag=%d,ref=%d \n", 
               num_ddescs, tag, ref);

    /* loop through label list */
    for (i = 0; i < num_dlabels; i++)
      {
          /* get annotation label length */
          ann_len = ret = ANannlen(dlabels[i]);
          RESULT("ANannlen");
        
          /* check ann length */
          if (ann_len != (int32) HDstrlen(label[i]))
            {
                printf("\t>>>BAD DATA LABEL LENGTH.\n\t    IS: %d\n\tSHOULD BE: %d<<<\n",
                       (int) ann_len, (int) HDstrlen(label[i]));
                num_errs++;
            }

          /* allocate space for label */
          if (ann_label == NULL)
            {
                if ((ann_label = (char *)HDmalloc((ann_len+1)*sizeof(char))) 
                    == NULL)
                  {
                      printf("Error: failed to allocate space to hold data label \n");
                      return FAIL;
                  }
                HDmemset(ann_label,'\0', ann_len+1);
            }
      
          /* read label */
          ret = ANreadann(dlabels[i], ann_label, ann_len+1);
          RESULT("ANreadann");

          /* end acces to label */
          ret = ANendaccess(dlabels[i]);
          RESULT("ANendaccess");      

          /* check label */
          if (HDstrncmp((const char *)ann_label, (const char *)label[i], (size_t)(ann_len+1)) != 0)
            {
                printf("\t>>>BAD DATA LABEL. \n\t       IS: %s; \n\tSHOULD BE: %s<<<\n",
                       ann_label, label[i]);
                num_errs++;
            }

#ifdef AN_DEBUG
          printf("found data_len=%d, data label=%s\n", strlen(ann_label),ann_label);
#endif

          HDfree(ann_label); /* free up space */
          ann_label = NULL;
      } /* end for labels */

    /* loop through desc list */
    for (i = 0; i < num_ddescs; i++)
      {
          /* get desc length */
          ann_len = ret = ANannlen(ddescs[i]);
          RESULT("ANannlen");

          /* check desc length */
          if (ann_len != (int32) HDstrlen(desc[i]))
            {
                printf("\t>>>BAD DATA DESC LENGTH.\n\t    IS: %d\n\tSHOULD BE: %d<<<\n",
                       (int) ann_len, (int) HDstrlen(desc[i]));
                num_errs++;
            }

          /* allocate space for descritpion */
          if (ann_desc == NULL)
            {
                if ((ann_desc = (char *)HDmalloc((ann_len+1)*sizeof(char))) 
                    == NULL)
                  {
                      printf("Error: failed to allocate space to hold data desc \n");
                      return FAIL;
                  }
                HDmemset(ann_desc,'\0', ann_len+1);
            }

          /* read description */
          ret = ANreadann(ddescs[i], ann_desc, ann_len+1);
          RESULT("ANreadann");

          /* end access to desc */
          ret = ANendaccess(ddescs[i]);
          RESULT("ANendaccess");      

          /* check desc */
          if (HDstrncmp((const char *)ann_desc, (const char *)desc[i], (size_t)ann_len) != 0)
            {
                printf("\t>>>BAD DATA DESC. \n\t       IS: %s; \n\tSHOULD BE: %s<<<\n",
                       ann_desc, desc[i]);
                num_errs++;
            }

#ifdef AN_DEBUG
          printf("found data desclen=%d, desc=%s\n", strlen(ann_desc),ann_desc);
#endif

          HDfree(ann_desc); /* free up space */
          ann_desc = NULL;
      } /* end for descs */
    
    /* free space */
    if (dlabels != NULL)
        HDfree(dlabels);
    if (ddescs != NULL)
        HDfree(ddescs);
    if (ann_label != NULL)
        HDfree(ann_label);
    if (ann_desc != NULL)
        HDfree(ann_desc);

    /* End annotation interface */
    ANend(an_handle);
    Hclose(file_handle);   /* close file */
  
    return SUCCEED;
} /* check_lab_desc() */

/****************************************************************
**
**  test_man(): Main annotation test routine
** 
**  NOTES: Keep in mind the order in which labels/descriptiosn
**         are written to the file to make sense of the indices
**         returned by ANfileinfo().
**  
**  Steps in testing:
**  1. Create arrays for SDS and RI
**  2. Open/Create test file.
**  3. Start Annotation inteface.
**  4. Write file labels and descriptions.
**  5. For 'REPs' count do
**     A. create/write SDS(DFSDxxx) to file.
**     B. Write 2 labels and 2 descriptons for SDS.
**     C. Write image(DFR8xxx) to file
**     D. Write 2 labels and 2 descriptions for RI.
**  6. Close annotation inteface 
**  7. Close file
**  8. For 'REPs' count do
**     A. Get dimesion and ref of SDS
**     B. Verify label and descriptions for SDS ->check_lab_desc()
**     C. Get image ref and image
**     D. Verify label and descriptions for Image->check_lab_desc()
**  9. Check file labels and descriptions->check_fann()
** 10. Clean up.
****************************************************************/
void
test_man(void)
{
    uint8  pal[768];
    uint8  *image = NULL;
    uint8  *newimage = NULL;
    uint16 refnum;
    int32  ret;
    intn   rank;
    int    i,j;
    int32  dimsizes[2];
    float32  *data = NULL;
    int32   file_handle; /* file handle */
    int32   an_handle;  /* annotation interface handle */
    int32   ann_handle;  /* annotation handle */

    /***** generate float array and image *****/
    if ((data     = (float32 *) HDmalloc(ROWS * COLS * sizeof(float32))) == NULL)
      {
          fprintf(stderr,"Error: unable to allocate space\n");
          return;
      }
    if ((image    = (uint8 *) HDmalloc(ROWS * COLS * sizeof(char))) == NULL)
      {
          fprintf(stderr,"Error: unable to allocate space\n");
          return;
      }
    if ((newimage = (uint8 *) HDmalloc(ROWS * COLS * sizeof(char))) == NULL)
      {
          fprintf(stderr,"Error: unable to allocate space\n");
          return;
      }

    /* dimensions of SDS/image */
    dimsizes[0] = ROWS;
    dimsizes[1] = COLS;

    /* generate images */
    gen2Dfloat(ROWS, COLS, data);
    genimage(ROWS, COLS, data, image);

    /* Start annotation Interface on file */
    ret = file_handle = Hopen(TESTFILE, DFACC_CREATE,0);
    RESULT("Hopen");
    ret = an_handle = ANstart(file_handle);
    RESULT("ANstart");

    /********  Write file labels and descriptions *********/
    MESSAGE(5, printf("***  Writing file labels and descriptions ***\n"););

    /* create and write file labels */
    for (i = 1; i >= 0; i--)
      {
          ret = ann_handle = ANcreatef(an_handle, AN_FILE_LABEL);
          RESULT("ANcreatef");
          ret = ANwriteann(ann_handle, file_lab[i], (int32)HDstrlen(file_lab[i]));
          RESULT("ANwriteann");
          ret = ANendaccess(ann_handle);
          RESULT("ANendaccess");
      }

    /* create and write file descriptions */
    for (i = 1; i >= 0; i--)
      {
          ret = ann_handle = ANcreatef(an_handle, AN_FILE_DESC);
          RESULT("ANcreatef");
          ret = ANwriteann(ann_handle, file_desc[i], (int32)HDstrlen(file_desc[i]));
          RESULT("ANwriteann");
          ret = ANendaccess(ann_handle);
          RESULT("ANendaccess");
      }

    /* set dimensions for SDS */
    ret = DFSDsetdims(2, dimsizes);
    RESULT("DFSDsetdims");

    /********  Write labels and descriptions *********/
    MESSAGE(5, printf("***  Writing labels and descriptions along with SDS and RIS ***\n"););

    for (j = 0; j < REPS; j++)
      {
          /* Set number type */
          ret = DFSDsetNT(DFNT_FLOAT32);
          RESULT("DFSDsetNT");

          /* write out scientific data set first */
          ret = DFSDadddata(TESTFILE, 2, dimsizes, (VOIDP) data);
          RESULT("DFSDadddata");

          /* write out annotations for 2 out of every 3 SDS */
          if ((j % 2) != 0)
            {   
                refnum = DFSDlastref(); /* get ref of SDS */

                /* create and write data labels */
                for (i = 1; i >=0; i--)
                  {
                      ret = ann_handle = ANcreate(an_handle, DFTAG_NDG, refnum, 
                                                  AN_DATA_LABEL);
                      RESULT("ANcreate");
                      ret = ANwriteann(ann_handle, labsds[i], (int32)HDstrlen(labsds[i]));
                      RESULT("ANwriteann");
                      ret = ANendaccess(ann_handle);
                      RESULT("ANendaccess");
                  }

                /* create and write data descriptions */
                for (i = 1; i >=0; i--)
                  {
                      ret = ann_handle = ANcreate(an_handle, DFTAG_NDG, refnum, 
                                                  AN_DATA_DESC);
                      RESULT("ANcreate");
                      ret = ANwriteann(ann_handle, descsds[i], (int32)HDstrlen(descsds[i]));
                      RESULT("ANwriteann");
                      ret = ANendaccess(ann_handle);
                      RESULT("ANendaccess");
                  }
            }

          /* Write image out */
          ret = DFR8addimage(TESTFILE, (VOIDP) image, COLS, ROWS, 0);
          RESULT("DFR8addimage");

          refnum = DFR8lastref(); /* get ref of image */

          /* create and write image labels */
          for (i = 1; i >=0; i--)
            {
                ret = ann_handle = ANcreate(an_handle, DFTAG_RIG, refnum, 
                                            AN_DATA_LABEL);
                RESULT("ANcreate");
                ret = ANwriteann(ann_handle, labris[i], (int32)HDstrlen(labris[i]));
                RESULT("ANwriteann");
                ret = ANendaccess(ann_handle);
                RESULT("ANendaccess");
            }

          /* create and write image descriptions */
          for (i = 1; i >=0; i--)
            {
                ret = ann_handle = ANcreate(an_handle, DFTAG_RIG, refnum, 
                                            AN_DATA_DESC);
                RESULT("ANcreate");
                ret = ANwriteann(ann_handle, descris[i], (int32)HDstrlen(descris[i]));
                RESULT("ANwriteann");
                ret = ANendaccess(ann_handle);
                RESULT("ANendaccess");
            }
      } /* end for j */

    /* End writing annotations */
    ANend(an_handle);
    Hclose(file_handle); /* close file */
  
    /********  Read labels and descriptions *********/
    MESSAGE(5, printf("*** Reading labels and descriptions for SDS and RIS ***\n"););

    for (j = 0; j < REPS; j++)
      { /* get dims for SDS */
          ret = DFSDgetdims(TESTFILE, &rank, dimsizes, 3);
          RESULT("DFSDgetdims");

          refnum = DFSDlastref(); /* now get ref of SDS */

          /* Check data set labels/descriptions of SDS */
          if ((j % 2) != 0)     /* read in annotations for 2 out of every 3 */
            {
                if (check_lab_desc(TESTFILE, DFTAG_NDG, refnum, labsds, descsds) == FAIL)
                    return; /* end of test */
            }

          /* get image */
          ret = DFR8getimage(TESTFILE, newimage, (int32) COLS, (int32) ROWS, pal);
          RESULT("DFR8getimage");

          refnum = DFR8lastref(); /* now get ref of image */

          /* Check image labels/descriptions of image */
          if (check_lab_desc(TESTFILE, DFTAG_RIG, refnum, labris, descris) == FAIL)
              return; /* end of test */
      }

    /***************** Read file labels and descriptions */
    MESSAGE(5, printf("*** Reading file label and descriptions ***\n"););

    /* Verify file lables/descs */
    if (check_fann(TESTFILE) == FAIL)
        return; /* end of test */

    /* check the re-writing of annotations works. 
       Only file labels are tested but it should suffice to
       test the internals */
    if (check_fann_rewrite(TESTFILE) == FAIL)
        return; /* end of test */

    /* free up space */
    HDfree((VOIDP) data);
    HDfree((VOIDP) image);
    HDfree((VOIDP) newimage);
} /* test_man() */
