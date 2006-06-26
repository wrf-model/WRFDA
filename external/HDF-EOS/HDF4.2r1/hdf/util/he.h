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

/* $Id: he.h,v 1.19 1996/11/11 20:40:20 koziol Exp $ */

/* he.h -- header file for HDFedit */
#include "hdf.h"
#include <ctype.h>
#include <stdio.h>
/* #include <sys/wait.h> */

#include <string.h>

/*
 * the following may not exist on all systems it should define stuff
 * like fork(), and other system calls
 */
#if defined (IBM6000) || defined (SUN)
#include <unistd.h>
#endif /* Unix std libs */
#if defined HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef VMS
#include <unixlib.h>
#include <descrip.h>
#endif

#ifdef DEBUG
#define ASSERT(e) {if(!(e)){fprintf(stderr,"Assertion failed: file %s, line %d\n",__FILE__,__LINE__);exit(1);}}
#define NOT_REACHED() {fprintf(stderr,"Should not reach: file %s, line %d\n",__FILE__,__LINE__);exit(1);}
#else
#define ASSERT(e)   /* empty assertion */
#define NOT_REACHED()   /* empty */
#endif /* DEBUG */

#define HE_ARG_SZ 30
#define HE_DESC_SZ 8192
#define HE_COLOR_SZ 256
#define HE_PALETTE_SZ 768
#define HE_BUF_SZ 1024

/* return status */
#define HE_OK 0
#define HE_FAIL -1
#define YES 1
#define NO 0

/* options */
#define HE_AMBIG -2
#define HE_NOTFOUND -1
#define HE_ALL 1
#define HE_BACKUP 2
#define HE_HELP 3
#define HE_LONGOUT 4
#define HE_NOBACKUP 5
#define HE_REMOTE 6
#define HE_VERBOSE 7
#define HE_FILE 8
#define HE_KEEP 9
#define HE_POSITION 10
#define HE_EXPANSION 11
#define HE_LARGE 12
#define HE_RDONLY 13
#define HE_BATCH 14
#define HE_OFFSET 15
#define HE_ASCII 16
#define HE_OCTAL 17
#define HE_HEX 18
#define HE_DECIMAL 19
#define HE_FLOAT 20
#define HE_DIMS 21
#define HE_IMAGE 22
#define HE_PALETTE 23
#define HE_RASTER 24
#define HE_RLE 25
#define HE_IMCOMP 26
#define HE_DOGROUP 27
#define HE_LENGTH 28
#define HE_ATTACHTO 29
#define HE_LABEL 30
#define HE_DESCRIPTOR 31
#define HE_EDITOR 32

#define HE_BYTE    33
#define HE_SHORT   34
#define HE_DOUBLE  35
#define HE_RAW     36
#define HE_USHORT  37
#define HE_UDECIMAL 38

/* label bits for predicate keys */
#define HE_PREDICATE 0x8000
#define HE_COMPARATOR 0x4000

/* predicate keys */
#define HEK_NULL 0
#define HEK_TAG 1
#define HEK_REF 2
#define HEK_EQUAL 3
#define HEK_NEQUAL 4
#define HEK_GRT 5
#define HEK_GEQUAL 6
#define HEK_LESST 7
#define HEK_LEQUAL 8
#define HEK_ALL 9
#define HEK_SUCCEED 10
#define HEK_FAIL 11
#define HEK_GROUP 12

/* argument type */
#define HE_NUMBER 1
#define HE_STRING 2

typedef int (*HE_FUNC) (void *);

typedef struct he_cmd
  {
      int         argc;
      char       *argv[HE_ARG_SZ];
      HE_FUNC     func;
      struct he_cmd *sub, *next;
  }
HE_CMD;

typedef struct he_pred
  {
      int         key, Comp, argType;
      union
        {
            int         i;
            char       *str;
        }
      arg;
  }
HE_PRED;

typedef struct DFdesc_str
  {
      uint16      tag, ref;
      int32       length, offset;
  }
DFdesc     , *DFdesc_ptr;

typedef struct tag_ref_struct
  {
      uint16      tag;
      uint16      ref;
  }
tag_ref    , *tag_ref_ptr;

typedef struct he_group
  {
      int         desc;
      int         size;
      tag_ref_ptr ddList;
  }
HE_GROUP;

extern int  he_status;
extern int  he_numDesc;
extern int  he_currDesc;
extern int  he_numGrp;
extern int  he_remote;
extern char *he_file;
extern DFdesc he_desc[];
extern HE_GROUP he_grp[];

/*
 *  ----------------- Convenient Macros ---------------------
 */

#define unkOpt(st)     fprintf(stderr,"Unknown option: %s.\n",st)
#define unkArg(st)     fprintf(stderr,"Unknown argument: %s.\n",st)
#define ambigOpt(st)   fprintf(stderr,"Ambigupus option: %s.\n",st)
#define irrOpt(st)     fprintf(stderr,"Irrelevant option: %s.\n",st)
#define noFile()      fprintf(stderr,"No file is opened.\n")

#define isAnnot(t)    (t == DFTAG_DIL || t == DFTAG_DIA)
#define isRig(t)      (t == DFTAG_RIG)
#define currTag       (he_desc[he_currDesc].tag)
#define currGrpNo     (desc2Grp(he_currDesc))
#define currDesc      ((DFdesc*) he_desc + he_currDesc)

#include "he_proto.h"

/* end of he.h */
