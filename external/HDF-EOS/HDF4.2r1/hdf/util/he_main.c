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
static char RcsId[] = "@(#)$Revision: 1.39 $";
#endif

/* $Id: he_main.c,v 1.39 1997/12/04 01:50:05 sxu Exp $ */

/******************************************************************************
 * he - HDF editor
 *
 * This program has a long history, starting with its creation as a tool to
 * help HDF developers work at a low-level on HDF files.  It has evolved into
 * user-level program, though it is still designed for small editing tasks on
 * HDF files.
 *
 * he allows sophisticated HDF users to manipulate the elements in an HDF file.
 * These manipulations include selecting groups and showing information about
 * them, dumping them to the output, writing them to new files, deleting them,
 * inserting them, replacing, say, the palette of an r8 group, and editing the
 * text labels and descriptions of any element.
 *
 * he will NOT allow the user to *arbitrarily* modify binary data in the file
 * or any element, though it allows modification of tag and reference numbers
 * within strict constraints.  The user should not attempt to alter individual
 * bytes.  It is acceptable, however, to replace an element with another of
 * the same type.
 *
 * he can be used both interactively or in "batch" mode.  Here is a sample
 * batch program:
 *
 *      #!/bin/csh -f
 *      set file=$1
 *      shift
 *      he -batch $file -nobackup << EOF
 *      info -all -group $*
 *      close
 *      quit
 *      EOF
 *
 * This makes use of C-shell variable substitution to pass a filename to he,
 * invokes he, and then lists out on separate lines the commands to give once
 * he is running.  The $* trailing the info command is also C-shell variable
 * substitution.
 *
 * List of commands:       annotate   dump    if      open    putr8    unalias
 *                         close      getr8   info    prev    revert   wait
 *                         delete     help    next    put     select   write
 *                         display
 *
 * Predicates are of the form TAG = 3 IMAGE_SIZE < 1000  LABEL = "abc"
 * Type <command> -help for usage of <command>.  DO NOT type the command
 * without arguments and expect help.  Some commands delete objects and do not
 * need any arguments.  If you are learning to use he, try it on an expendable
 * file.
 *****************************************************************************/
/* ------ he.c ------- main() main HDF interfacing routines */

#if defined __MWERKS__
#include <console.h>
#endif

#include "he.h"

#include <stdio.h>
#ifndef WIN32
#include <unistd.h>
#endif

/* the return status of last command executed */
int         he_status = HE_OK;

/* is this on the console or remote terminals?
   this should eventually be detected automatically */
int         he_remote = YES;

/* is this batch mode or interactive? */
extern int  he_batch;

int
main(int argc, char *argv[])
{
    int         backup = YES;   /* Backup files when opening? */
    int i;
    char       *fileName = NULL;

#if defined __MWERKS__
    argc = ccommand(&argv);
#endif

    for (i = 1; i < argc; i++)
      {
          if (argv[i][0] == '-')
            {
                switch (findOpt(argv[i] + 1))
                  {
                      case HE_HELP:
                          printf("he [<file>] [-nobackup] [-batch]\n");
                          help();
                          quit(0);
                          break;
                      case HE_BATCH:
                          he_batch = YES;
                          break;
                      case HE_REMOTE:
                          he_remote = YES;
                          break;
                      case HE_NOBACKUP:
                          backup = NO;
                          break;
                      case HE_BACKUP:
                          backup = YES;
                          break;
                      case HE_NOTFOUND:
                          unkOpt(argv[i]);
                          quit(1);  /* does not return */
                          break;
                      case HE_AMBIG:
                          ambigOpt(argv[i]);
                          quit(1);
                          break;
                      default:
                          irrOpt(argv[i]);
                          quit(1);  /* does not return */
                          break;
                  }
            }
          else
            {
                /* Must be a filename */
                if (!fileName)
                    fileName = argv[i];
                else
                    fprintf(stderr, "Single file only. %s not open.\n", argv[i]);
            }
      }

    /* if there is a file name given in the command line, open it */

    if (fileName)
        he_status = openFile(fileName, backup);

    /* read, execute loop */
    cmdLoop();

    if (fileOpen())
        closeFile(YES);     /* close with keep */
    quit(0);
    return 0;
}

/* cmdLoop -- read commands and execute them */
void
cmdLoop(void)
{
    HE_CMD     *cmd;

    for (cmd = getCmd(); cmd; cmd = getCmd())
      {
          if (cmd->func)
              he_status = (*cmd->func) (cmd);
          else
            {
                fprintf(stderr, "Unknown command: %s.\n", cmd->argv[0]);
                he_status = HE_FAIL;
            }
          deleteCmd(cmd);
      }
}

int         he_currDesc;
int         he_numDesc;
int         he_numGrp;
int         he_backup;
char       *he_file;
DFdesc      he_desc[HE_DESC_SZ];
HE_GROUP    he_grp[HE_DESC_SZ];

int32
getElement(int desc, char **pdata)
{
    int32       length;
    int32       fid;

    length = he_desc[desc].length;

    /* alloc memory to read the element in */
    *pdata = (char *) HDmalloc(length);
    if (*pdata == NULL)
        return FAIL;

    /* read in the element and check for error */
    if ((fid = Hopen(he_file, DFACC_READ, 0)) == (int32) NULL)
      {
          HEprint(stderr, 0);
          return FAIL;
      }
    if (Hgetelement(fid, he_desc[desc].tag, he_desc[desc].ref, (unsigned char *) (*pdata)) < 0)
      {
          HDfree(*pdata);
          fprintf(stderr, "Cannot read element.\n");
          return FAIL;
      }
    Hclose(fid);
    return length;
}

/* The function is not called.

   int od(char *format, char *file)
   {
   fork a child and let the child run od. Use vfork for VMS.

   if (fork() == 0)

   {
   this is the child

   if (execl("/bin/od", "od", format, file, 0) == -1)
   fprintf(stderr, "Error while executing od.\n");

   return control to the parent
   exit(0);
   }

   the parent waits for the child to die

   wait(0);

   this is a bug because it always returns OK, will expand this as
   soon as the status return mechanism from wait is understood
   return HE_OK;

   }
 */

/* the tmp directory, currently set for unix */
#define TDIR "/tmp/"

int
getTmpName(char **pname)
{
    int         length;
    static int  count = 0;
    char        s[32];

    (void) sprintf(s, "%she%d.%d", TDIR, (int)getpid(), count);
    count++;

    length = (int)HDstrlen(s);
    if (length <= 0)
        return FAIL;

    *pname = (char *) HDmalloc(length + 1);
    HDstrcpy(*pname, s);

    return length;
}

int
writeToFile(char *file, char *data, int32 length)
{
    FILE       *fd;
    int         written;

    fd = fopen(file, "w");
    if (fd == NULL)
        return FAIL;

    written = (int)fwrite(data, sizeof(char), (size_t) length, fd);
    if (written != length)
      {
          fprintf(stderr, "Error in write.\n");
          return FAIL;
      }
    fclose(fd);

    return HE_OK;
}

int
removeFile(char *file)
{
#ifndef VMS
    return unlink(file);
#else
    return remove((const char *) file);
#endif
}

/* is a file currently opened */
int
fileOpen(void)
{
    return (he_file != NULL);
}

char       *
backupName(const char *file)
{
    return catStr(file, "$hdfed$");
}

int
backupFile(char *file)
{
    char       *back;           /* backup file name */

    back = backupName(file);
    return copyFile(file, back);
}

int
copyFile(char *from, char *to)
{
    int         num_read;
    char        buf[HE_BUF_SZ]; /* copying buffer */
    FILE       *fp;
    FILE       *bfp;

    /* open the hdf file for backing up */
    if ((fp = fopen(from, "r")) == NULL)
      {
          fprintf(stderr, "Unable to open file: <%s>\n", from);
          return FAIL;
      }
    if ((bfp = fopen(to, "w")) == NULL)
      {
          fclose(fp);
          fprintf(stderr, "Unable to open backup file.\n");
          return FAIL;
      }
    /* copy the contents from hdf file to backup file */
    while ((num_read = (int)fread(buf, 1, HE_BUF_SZ, fp)) > 0)
        fwrite(buf, 1, (size_t)num_read, bfp);

    fclose(fp);
    fclose(bfp);

    return HE_OK;
}

int
updateDesc(void)
{
    int32      fid;
    int32       groupID;
    int32       aid, status;
    int i, j;

    if ((fid = Hopen(he_file, DFACC_READ, 0)) == 0)
      {
          printf("failed opening\n");
          HEprint(stdout, 0);
          return FAIL;
      }

    aid = Hstartread(fid, DFTAG_WILDCARD, DFREF_WILDCARD);
    if (aid == FAIL)
      {
          HEprint(stderr, 0);
          return FAIL;
      }

    status = SUCCEED;
    for (i = 0; (i < HE_DESC_SZ) && (status != FAIL); i++)
      {
          Hinquire(aid, NULL, &he_desc[i].tag, &he_desc[i].ref, &he_desc[i].length,
                   &he_desc[i].offset, NULL, (int16 *) NULL, (int16 *) NULL);
          status = Hnextread(aid, DFTAG_WILDCARD, DFREF_WILDCARD, DF_CURRENT);
      }
    he_numDesc = i;

    /* get informations about the groups */
    he_numGrp = 0;
    for (i = 0; i < he_numDesc; i++)
      {
          if (isGrp(he_desc[i].tag))
            {
                he_grp[he_numGrp].desc = i;
/*                he_grp[he_numGrp].size = (int) (he_desc[i].length / sizeof(tag_ref));
                he_grp[he_numGrp].ddList = (tag_ref_ptr) HDmalloc(he_desc[i].length);
*/
                he_grp[he_numGrp].size = (int) (he_desc[i].length / 4);
                he_grp[he_numGrp].ddList = (tag_ref_ptr) HDmalloc(he_grp[he_numGrp].size*sizeof(tag_ref));

                if (!he_grp[he_numGrp].ddList)
                  {
                      fprintf(stderr, "Out of memory. Closing file.\n");
                      closeFile(1);     /* keep the backup */
                      return FAIL;
                  }
                groupID = DFdiread(fid, he_desc[i].tag, he_desc[i].ref);
                if (groupID < 0)
                  {
                      HEprint(stderr, 0);
                      return FAIL;
                  }
                for (j = 0; j < he_grp[he_numGrp].size; j++)
                    DFdiget(groupID, &he_grp[he_numGrp].ddList[j].tag,
                            &he_grp[he_numGrp].ddList[j].ref);

                he_numGrp++;
            }
      }
    Hendaccess(aid);
    Hclose(fid);
    return SUCCEED;
}

int
initFile(char *file)
{
    if (he_file)
        HDfree(he_file);
    he_file = copyStr(file);

    if (updateDesc() < 0)
        return FAIL;

    /* if there are groups in this file, go to the first group tag */
    /* otherwise, just go to the first element */
    if (he_numGrp > 0)
        he_currDesc = he_grp[0].desc;
    else
        he_currDesc = 0;

    return resetPred();
}

int
closeFile(int keep)
{
    int i;
    char       *back;

    if (!fileOpen())
      {
          fprintf(stderr, "No open file to close.\n");
          return FAIL;
      }
    /* free some dynamic storages */
    if (he_backup && !keep)
      {
          back = backupName(he_file);
          (void) removeFile(back);
          HDfree(back);
      }
    HDfree(he_file);
    he_file = NULL;
    for (i = 0; i < he_numGrp; i++)
        HDfree(he_grp[i].ddList);

    return HE_OK;
}

int
getR8(int xdim, int ydim, char *image, char *pal, int compress)
{
    FILE       *fp;
    int32       length;
    char       *buf;

    if (!fileOpen())
      {
          noFile();
          return FAIL;
      }
    if (pal)
        if (setPal(pal) < 0)
            /* Error already signalled by setPal */
            return FAIL;

    length = xdim * ydim;
    buf = (char *) HDmalloc(length);

    if ((fp = fopen(image, "r")) == NULL)
      {
          fprintf(stderr, "Error opening image file: %s.\n", image);
          return FAIL;
      }
    if (fread(buf, (size_t)xdim, (size_t)ydim, fp) < (size_t)ydim)
      {
          fprintf(stderr, "Error reading image file: %s.\n", image);
          return FAIL;
      }
    fclose(fp);

    if (DFR8addimage(he_file, buf, (int32) xdim, (int32) ydim, (uint16) compress) < 0)
      {
          HEprint(stderr, 0);
          return FAIL;
      }
    HDfree(buf);

    if (updateDesc() < 0)
        return FAIL;

    return HE_OK;
}

int
setPal(char *pal)
{
    FILE       *fp;
    char        reds[HE_COLOR_SZ], greens[HE_COLOR_SZ], blues[HE_COLOR_SZ];
    char        palette[HE_PALETTE_SZ];
    char *p;
    int i;

    if ((fp = fopen(pal, "r")) == NULL)
      {
          fprintf(stderr, "Error opening palette file: %s.\n", pal);
          return FAIL;
      }
    if (fread(reds, 1, HE_COLOR_SZ, fp) < HE_COLOR_SZ ||
        fread(greens, 1, HE_COLOR_SZ, fp) < HE_COLOR_SZ ||
        fread(blues, 1, HE_COLOR_SZ, fp) < HE_COLOR_SZ)
      {
          fprintf(stderr, "Error reading palette file: %s.\n", pal);
          return FAIL;
      }

    /* convert sun palette to hdf palette */
    p = palette;
    for (i = 0; i < HE_COLOR_SZ; i++)
      {
          *p++ = reds[i];
          *p++ = greens[i];
          *p++ = blues[i];
      }

    if (DFR8setpalette((uint8 *) palette) < 0)
      {
          fputs("Error setting palette.\n", stderr);
          return FAIL;
      }

    return HE_OK;
}

int
findDesc(tag_ref_ptr dd)
{
    int i;

    for (i = 0; i < he_numDesc; i++)
        if ((he_desc[i].tag == dd->tag) && (he_desc[i].ref == dd->ref))
            return i;

    return FAIL;
}

int
desc2Grp(int desc)
{
    int i;

    for (i = 0; i < he_numGrp; i++)
        if (he_grp[i].desc == desc)
            return i;

    NOT_REACHED();
    return FAIL;
}

int
hasReference(int desc)
{
    int i, j;

    for (i = 0; i < he_numGrp; i++)
        for (j = 0; j < he_grp[i].size; j++)
            if (he_grp[i].ddList[j].tag == he_desc[desc].tag &&
                he_grp[i].ddList[j].ref == he_desc[desc].ref)
                return YES;
    return NO;
}

int
deleteDesc(int desc)
{
    int32       fid;

    if ((fid = Hopen(he_file, DFACC_WRITE, 0)) == (int32) NULL)
      {
          HEprint(stderr, 0);
          return FAIL;
      }

    if (Hdeldd(fid, he_desc[desc].tag, he_desc[desc].ref) == FAIL)
      {
          HEprint(stderr, 0);
          return FAIL;
      }
    return Hclose(fid);
}

int
getCurrRig(int32 *pXdim, int32 *pYdim, char **pPalette, char **pRaster)
{
    int         ispal;

    goTo(he_currDesc);

    if (DFR8getdims(he_file, pXdim, pYdim, &ispal) < 0)
      {
          fprintf(stderr, "Error getting image group.\n");
          HEprint(stderr, 0);
          return FAIL;
      }
    if (ispal)
        *pPalette = (char *) HDmalloc(HE_PALETTE_SZ);
    else
        *pPalette = (char *) NULL;
    *pRaster = (char *) HDmalloc((size_t)(*pXdim) * (size_t)(*pYdim));

    if (DFR8getimage(he_file, (unsigned char *) *pRaster, *pXdim, *pYdim,
                     (unsigned char *) *pPalette) == FAIL)
      {
          fprintf(stderr, "Error getting image group.\n");
          HEprint(stderr, 0);
          return FAIL;
      }

    return HE_OK;
}

int
putWithTempl(char *template, int n1, int n2, int n3, char *data, int length, int verbose)
{
    char       *file;
    int         ret;

    convertTemplate(template, n1, n2, n3, &file);
    if (verbose)
        printf("Writing to file: %s\n", file);
    ret = writeToFile(file, data, length);
    HDfree(file);

    return ret;
}

int
revert(void)
{
    char       *back;

    back = backupName(he_file);
    if (copyFile(back, he_file) < 0)
        return FAIL;
    return initFile(he_file);
}

int
writeElt(char *file, uint16 ref, int elt)
{
    int         ret;
    char       *data;
    int32       eltLength;
    char       *p;
    uint16      rank;
    int i;
    uint16      ntTag;
    uint16      ntRef;
    tag_ref_ptr ntDesc;
    int         nt;

    eltLength = getElement(elt, &data);
    if (eltLength <= 0)
      {
          fprintf(stderr, "Cannot get element: tag %d ref %d.\n",
                  he_desc[elt].tag, he_desc[elt].ref);
          return FAIL;
      }
    /* special case */

    if (he_desc[elt].tag == DFTAG_SDD)
      {

          /* lots of hack here */
          /* assume that the number types are of the same ref as this elt */
          p = data;

          /* get the rank */
          /* NOTE: UINT16READ and UINT16WRITE advances p */
          UINT16DECODE(p, rank);
          /* move to the NT of the data */
          p += (rank * 4);
          UINT16DECODE(p, ntTag);
          UINT16DECODE(p, ntRef);

          /* set up to write the number type element */
          ntDesc = (tag_ref_ptr) HDmalloc(sizeof(tag_ref));
          ntDesc->tag = ntTag;
          ntDesc->ref = ntRef;
          nt = findDesc(ntDesc);
          HDfree(ntDesc);
          writeElt(file, ref, nt);

          p -= 2;
          UINT16ENCODE(p, ref);
          /* do the NT of scales */
          for (i = 0; (uint16) i < rank; i++)
            {
                p += 2;
                UINT16ENCODE(p, ref);
            }
      }

    ret = putElement(file, he_desc[elt].tag, ref, data, eltLength);
    HDfree(data);
    return ret;
}

int
putElement(char *file, uint16 tag, uint16 ref, char *data, int32 len)
{
    int32       ret;
    int32       fid;

    if ((fid = Hopen(file, DFACC_READ | DFACC_WRITE, 0)) == FAIL)
        /* a little tricky here */
        if (HEvalue(0) != DFE_FNF || (fid = Hopen(file, DFACC_ALL, 0)) == FAIL)
          {
              HEprint(stderr, 0);
              return FAIL;
          }
    if ((ret = Hputelement(fid, tag, ref, (unsigned char *) data, len)) < 0)
      {
          HEprint(stderr, 0);
          return (int) ret;
      }
    return Hclose(fid);
}

int
writeGrp(char *file)
{
    int i;
    uint16      ref;
    int         grp;
    int         elt;
    int         ret;
    int32       fid;
    int32       gid;

    getNewRef(file, &ref);

    grp = currGrpNo;
    gid = DFdisetup(he_grp[grp].size);
    for (i = 0; i < he_grp[grp].size; i++)
      {
          elt = findDesc(he_grp[grp].ddList + i);
          if (elt >= 0)
              writeElt(file, ref, elt);
          /* update the group dd list */
          DFdiput(gid, he_grp[grp].ddList[i].tag, ref);
      }
    /* do the group now */

    if ((fid = Hopen(file, DFACC_READ | DFACC_WRITE, 0)) == FAIL)
      {
          HEprint(stderr, 0);
          return FAIL;
      }
    if ((ret = DFdiwrite(fid, gid, currTag, ref)) < 0)
      {
          HEprint(stderr, 0);
          return ret;
      }
    return Hclose(fid);
}

int
getNewRef(char *file, uint16 *pRef)
{
    int32       fid;

    if ((fid = Hopen(file, DFACC_READ | DFACC_WRITE, 0)) == FAIL)
        /* a little tricky here */
        if (HEvalue(0) != DFE_FNF || (fid = Hopen(file, DFACC_ALL, 0)) == FAIL)
          {
              HEprint(stderr, 0);
              return FAIL;
          }
    *pRef = Hnewref(fid);
    return Hclose(fid);
}

int
writeAnnot(char *file, uint16 tag, uint16 ref)
{
    char       *data;
    int32       eltLength;
    intn        tmp;
    char       *p;
    uint16      newRef;

    while (tag == 0)
      {
          printf("Attach to what tag? (> 0)");
          scanf("%d", &tmp);
          tag = (uint16) tmp;
      }

    while (ref == 0)
      {
          printf("Attach to what ref? (> 0)");
          scanf("%d", &tmp);
          ref = (uint16) tmp;
      }

    eltLength = getElement(he_currDesc, &data);
    if (eltLength <= 0)
      {
          fprintf(stderr, "Cannot get element: tag %d ref %d.\n",
                  he_desc[he_currDesc].tag, he_desc[he_currDesc].ref);
          return FAIL;
      }

    p = data;

    /*
     *  This is really ugly...
     */
    UINT16ENCODE(p, tag);
    UINT16ENCODE(p, ref);

    if (getNewRef(file, &newRef) < 0)
      {
          fprintf(stderr, "Error getting new ref number.\n");
          return FAIL;
      }

    return putElement(file, he_desc[he_currDesc].tag, newRef, data, eltLength);
}

int32
getAnn(int ann, uint16 tag, uint16 ref, char **pBuf)
{
    int32       len;

    if (ann == HE_LABEL)
      {
          len = DFANgetlablen(he_file, tag, ref);
          if (len > 0)
            {
                *pBuf = (char *) HDmalloc((size_t)(len + 1));
                DFANgetlabel(he_file, tag, ref, *pBuf, len + 1);
            }
          else
              *pBuf = NULL;
      }
    else
      {
          len = DFANgetdesclen(he_file, tag, ref);
          if (len > 0)
            {
                *pBuf = (char *) HDmalloc((size_t)len);
                DFANgetdesc(he_file, tag, ref, *pBuf, len);
            }
          else
              *pBuf = NULL;
      }
    return len;
}

int
putAnn(int ann, uint16 tag, uint16 ref, char *buf, int32 len)
{
    int         ret;

    if (ann == HE_LABEL)
        ret = DFANputlabel(he_file, tag, ref, buf);
    else
        ret = DFANputdesc(he_file, tag, ref, buf, len);
    if (ret < 0)
        HEprint(stderr, 0);

    return ret;
}

int32
readFromFile(char *file, char **pBuf)
{
    FILE       *fp;
    int32       soFar;
    int32       bufLen;
    int32       num_read;

    fp = fopen(file, "r");
    if (fp == NULL)
        return FAIL;

    soFar = 0;
    bufLen = 0;
    for (num_read = HE_BUF_SZ; num_read == HE_BUF_SZ; soFar += num_read)
      {
          bufLen += HE_BUF_SZ;
          if (bufLen == HE_BUF_SZ)
              *pBuf = (char *) HDmalloc(bufLen);
          else
              *pBuf = (char *) HDrealloc(*pBuf, bufLen);
          if (*pBuf == NULL)
              return FAIL;

          num_read = (int32)fread((*pBuf) + soFar, 1, HE_BUF_SZ, fp);
      }
    *pBuf = (char *) HDrealloc(*pBuf, soFar + 1);
    (*pBuf)[soFar] = '\0';
    fclose(fp);
    return soFar;
}

/* ---- table for operators -------- */
struct
{
    const char *str;
    int         key;
}
he_optTab[] =
{
    {
        "readonly", HE_RDONLY
    }
    ,
    {
        "all", HE_ALL
    }
    ,
    {
        "backup", HE_BACKUP
    }
    ,
    {
        "batch", HE_BATCH
    }
    ,
    {
        "help", HE_HELP
    }
    ,
    {
        "longout", HE_LONGOUT
    }
    ,
    {
        "nobackup", HE_NOBACKUP
    }
    ,
    {
        "remote", HE_REMOTE
    }
    ,
    {
        "verbose", HE_VERBOSE
    }
    ,
    {
        "position", HE_POSITION
    }
    ,
    {
        "expansion", HE_EXPANSION
    }
    ,
    {
        "large", HE_LARGE
    }
    ,
    {
        "offset", HE_OFFSET
    }
    ,
    {
        "ascii", HE_ASCII
    }
    ,
    {
        "octal", HE_OCTAL
    }
    ,
    {
        "hexadecimal", HE_HEX
    }
    ,
    {
        "decimal", HE_DECIMAL
    }
    ,
    {
        "float", HE_FLOAT
    }
    ,
    {
        "dimensions", HE_DIMS
    }
    ,
    {
        "image", HE_IMAGE
    }
    ,
    {
        "palette", HE_PALETTE
    }
    ,
    {
        "raster", HE_RASTER
    }
    ,
    {
        "rle", HE_RLE
    }
    ,
    {
        "compress", HE_RLE
    }
    ,
    {
        "imcomp", HE_IMCOMP
    }
    ,
    {
        "group", HE_DOGROUP
    }
    ,
    {
        "file", HE_FILE
    }
    ,
    {
        "keep", HE_KEEP
    }
    ,
    {
        "length", HE_LENGTH
    }
    ,
    {
        "attachto", HE_ATTACHTO
    }
    ,
    {
        "label", HE_LABEL
    }
    ,
    {
        "descriptor", HE_DESCRIPTOR
    }
    ,
    {
        "editor", HE_EDITOR
    }
    ,
    {
        "byte", HE_BYTE
    }
    ,
    {
        "short", HE_SHORT
    }
    ,
    {
        "double", HE_DOUBLE
    }
    ,
    {
        "ushort", HE_USHORT
    }
    ,
    {
        "udecimal", HE_UDECIMAL
    }
    ,
    {
        "raw", HE_RAW
    }
    ,
};

int
findOpt(char *word)
{
    unsigned    len;
    int         found = -1;
    uintn i;

    len = HDstrlen(word);

    for (i = 0; i < sizeof(he_optTab) / sizeof(he_optTab[0]); i++)
        if (!HDstrncmp(he_optTab[i].str, word, len))
          {
              /* exact match */
              if (HDstrlen(he_optTab[i].str) == len)
                  return he_optTab[i].key;

              if (found < 0)
                  found = (int)i;
              else
                  return HE_AMBIG;
          }

    if (found < 0)
        return HE_NOTFOUND;

    return he_optTab[found].key;
}

char       *
catStr(const char *s, const char *s1)
{
    char       *t;

    t = (char *) HDmalloc(HDstrlen(s) + HDstrlen(s1) + 1);
    HDstrcpy(t, s);
    HDstrcat(t, s1);
    return t;
}

char       *
copyStr(char *s)
{
    char       *t;

    t = (char *) HDmalloc(HDstrlen(s) + 1);
    HDstrcpy(t, s);
    return t;
}

int
isGrp(uint16 tag)
{
    switch (tag)
      {
          case DFTAG_RIG:
          case DFTAG_SDG:
          case DFTAG_NDG:
              /* and other group tags */
              return 1;
          default:
              return 0;
      }
}

int
HEquit(HE_CMD * cmd)
{
    if (cmd->argc > 1)
      {
          puts("quit");
          puts("\tQuits this application.");
          return HE_OK;
      }
    return quit(0);
}

int
quit(int status)
{
    if (fileOpen())
      {
          if (closeFile(0) < 0)
              return HE_FAIL;
      }
    exit(status);
    return(status);	/* never excuted.  Just to shut up some comilers */
}

int
HEhelp(HE_CMD * dummy)
{
    /* shut compiler up */
    dummy = dummy;

    help();
    return HE_OK;
}

void
help(void)
{
    /* print some help informations */
    printf("hdfed allows sophisticated HDF users to manipulate the elements in");
    printf(" an HDF file.\nThese manipulations include selecting groups and ");
    printf("showing information about\nthem, dumping them to the output, ");
    printf("writing them to new files, deleting them,\ninserting them, ");
    printf("replacing, say, the palette of an r8 group, and editing the\n");
    printf("text labels and descriptions of any element.\n\n");

    printf("hdfed will NOT allow the user to *arbitrarily* modify binary data ");
    printf("in the file or\nany element, though it allows modification of tag ");
    printf("and reference numbers within\nstrict constraints.  The user should");
    printf(" not attempt to alter individual bytes.  It\nis acceptable,");
    printf(" however, to replace an element with another of the same type.\n\n");

    printf("hdfed can be used both interactively or in 'batch' mode.  See the ");
    printf("\"HDF Calling\nInterfaces and Utilities\" manual section on hdfed ");
    printf("for an example.\n\n");

    printf("List of commands:");
    printf("\tannotate   dump    if      open    putr8    unalias\n");
    printf("\t\t\tclose      getr8   info    prev    revert   wait\n");
    printf("\t\t\tdelete     help    next    put     select   write\n");

    printf("\nPredicates for 'if' 'select' 'next' and 'prev' are of the form\n");
    printf("\ttag = 3 ref = 2 image_size < 1000 label = \"abc\"\n");
    printf("Type <command> -help for usage of <command>.  DO NOT type the");
    printf(" command without\narguments and expect help.  Some commands delete");
    printf(" objects and do not need any\narguments.  If you are learning to ");
    printf("use hdfed, try it on an expendable file.\n");

}

int
HEwait(HE_CMD * cmd)
{
    int         c;

    if (cmd->argv[1][0] == '-' && findOpt(cmd->argv[1] + 1) == HE_HELP)
      {
          printf("wait [<message>]\n");
          printf("\tPrints message and then wait for user to hit return\n");
          return HE_OK;
      }

    printf("%s\nPress return to continue.", cmd->argv[1]);
    do
        c = getchar();
    while (c != '\n');

    return HE_OK;
}

void
deleteCmd(HE_CMD * cmd)
{
    intn i;

    if (cmd == NULL)
        return;
    if (cmd->next != NULL)
        deleteCmd(cmd->next);
    if (cmd->sub != NULL)
        deleteCmd(cmd->sub);
    for (i = 0; i < cmd->argc; i++)
        if (cmd->argv[i] != NULL)
            HDfree(cmd->argv[i]);
    HDfree(cmd);
}

/* -------------- routines to manipulate templates --------------------- */

#define TEMPLATE_CHAR1 '#'
#define TEMPLATE_CHAR2 '@'
#define TEMPLATE_CHAR3 '%'

void
convertTemplate(char *template, int n1, int n2, int n3, char **pname)
{
    char        s1[20], s2[20], s3[20];
    char       *t;

    sprintf(s1, "%1d", n1);
    sprintf(s2, "%1d", n2);
    sprintf(s3, "%1d", n3);

    *pname = t = (char *) HDmalloc(HDstrlen(template) + 61);

    while (*template)
        switch (*template)
          {
              case TEMPLATE_CHAR1:
                  fillTemplate(&template, &t, s1, TEMPLATE_CHAR1);
                  break;
              case TEMPLATE_CHAR2:
                  fillTemplate(&template, &t, s2, TEMPLATE_CHAR2);
                  break;
              case TEMPLATE_CHAR3:
                  fillTemplate(&template, &t, s3, TEMPLATE_CHAR3);
                  break;
              default:
                  *t++ = *template++;
          }
    *t = '\0';
}

void
fillTemplate(char **template, char **pout, char *s, char templateChar)
{
    int         templateLen, sLen;

    /* count length of template to replace */
    for (templateLen = 0; **template == templateChar;
         (*template)++, templateLen++) ;
    sLen = (int)HDstrlen(s);

    /* fill with zero's if the space reserved in template is
       longer than the length of s */
    for (; templateLen > sLen; templateLen--)
        *(*pout)++ = '0';

    while (*s)
        *(*pout)++ = *s++;
}

/* end of he-main.c */
