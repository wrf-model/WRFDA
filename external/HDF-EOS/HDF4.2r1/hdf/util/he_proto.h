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

/* $Id: he_proto.h,v 1.14 1997/11/05 19:39:39 koziol Exp $ */

/* Prototypes for hdfed functions */
int         HEalias
            (HE_CMD * cmd);

int         HEannotate
            (HE_CMD * cmd);

int         HEclose
            (HE_CMD * cmd);

int         HEdelete
            (HE_CMD * cmd);

int         HEdisplay
            (HE_CMD * cmd);

int         HEdump
            (HE_CMD * cmd);

int         HEgetR8
            (HE_CMD * cmd);

int         HEhelp
            (HE_CMD * cmd);

int         HEif
            (HE_CMD * cmd);

int         HEinfo
            (HE_CMD * cmd);

int         HEnext
            (HE_CMD * cmd);

int         HEopen
            (HE_CMD * cmd);

int         HEprev
            (HE_CMD * cmd);

int         HEput
            (HE_CMD * cmd);

int         HEputR8
            (HE_CMD * cmd);

int         HEquit
            (HE_CMD * cmd);

int         HErevert
            (HE_CMD * cmd);

int         HEselect
            (HE_CMD * cmd);

int         HEunalias
            (HE_CMD * cmd);

int         HEwait
            (HE_CMD * cmd);

int         HEwrite
            (HE_CMD * cmd);

int         annotate
            (const char *editor, int ann);

int         backupFile
            (char *file);

char       *backupName
            (const char *file);

int         bigImg
            (unsigned char *targ, unsigned char *src);

char       *catStr
            (const char *s1, const char *s2);

int         closeFile
            (int keep);

void        cmdLoop
            (void);

void        convertTemplate
            (char *template, int n1, int n2, int n3, char **pname);

int         copyFile
            (char *from, char *to);

char       *copyStr
            (char *str);

int         delete
            (int curr);

void        deleteCmd
            (HE_CMD * cmd);

int         deleteDesc
            (int desc);

int         desc2Grp
            (int desc);

int         display
            (int c, int x, int y, int f, int l);

int         dump
            (int32 length, int offset, char *foramt, int raw);

int         fileOpen
            (void);

void        fillTemplate
            (char **template, char **pout, char *s, char templateChar);

HE_CMD     *findAlias
            (char *str);

int         findDesc
            (tag_ref_ptr dd);

HE_FUNC     findFunc
            (char *word);

int         findKey
            (char *word);

int         findOpt
            (char *word);

int32       getAnn
            (int ann, uint16 tag, uint16 ref, char **pBuf);

HE_CMD     *getCmd
            (void);

int         getCurrRig
            (int32 *pXdim, int32 *pYdim, char **pPalette, char **pRaster);

int32       getElement
            (int desc, char **pdata);

int         getLine
            (char *);

int         getNewRef
            (char *file, uint16 *pRef);

int         getPix
            (void);

int         getR8
            (int xdim, int ydim, char *image, char *pal, int compress);

int         getSpace
            (void);

int         getTmpName
            (char **pname);

void        goTo
            (int desc);

int         hasReference
            (int desc);

void        help
            (void);

int         info
            (int all, int longout, int group, int label);

void        infoDesc
            (int desc, int longout, int label);

int         initFile
            (char *file);

int         isGrp
            (uint16 tag);

int         isNumber
            (char *s);

int         largeSet
            (void);

int         main
            (int argc, char *argv[]);

HE_CMD     *mkDupCmd
            (HE_CMD * cmd);

char       *nextToken
            (char **p);

char       *nextWord
            (char **p);

int         numCompare
            (int n1, int Comp, int n2);

int         od
            (char *format, char *file);

int         openFile
            (char *file, int backup);

HE_CMD     *parse
            (void);

HE_CMD     *parseCmd
            (char **p);

HE_PRED    *parsePred
            (int argc, char *argv[]);

int         pixImage
            (int usepal);

void        printAlias
            (char *word, HE_CMD * cmd);

void        prompt
            (void);

int         put
            (char *template, int verbose);

int         putAnn
            (int ann, uint16 tag, uint16 ref, char *buf, int32 len);

int         putElement
            (char *file, uint16 tag, uint16 ref, char *data, int32 len);

int         putR8
            (char *image, char *pal, int verbose);

int         putWithTempl
            (char *template, int n1, int n2, int n3, char *data, int length, int verbose);

int         quit
            (int status);

int         rImage
            (int usepal);

int32       readFromFile
            (char *file, char **pBuf);

int         recurseDel
            (int curr);

int         removeFile
            (char *file);

int         resetPred
            (void);

int         revert
            (void);

int         rleIt
            (char *buf, char *bufto, int len);

int         satPred
            (DFdesc * desc, HE_PRED pred[]);

int         setAlias
            (char *str, HE_CMD * cmd);

int         setPal
            (char *pal);

int         updateDesc
            (void);

int         writ
            (char *file, uint16 tag, uint16 ref);

int         writeAnnot
            (char *file, uint16 tag, uint16 ref);

int         writeElt
            (char *file, uint16 ref, int elt);

int         writeGrp
            (char *file);

int         writeToFile
            (char *file, char *data, int32 length);
