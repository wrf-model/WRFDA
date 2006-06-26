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
static char *RcsId[] = "@(#)$Revision: 1.34 $";
#endif

/* $Id: vmake.c,v 1.34 1998/12/08 21:37:58 koziol Exp $ */

/*****************************************************************************
*
* vmake.c
*
*   HDF Vset utility.
*
*   vmake:  utility creates vsets. May be used in 3 ways:
*           (1) add a new vgroup.
*           (2) add a new vdata from ascii data.
*           (3) link vgroups and vdatas to a vgroup.
*
*    USAGE:
*           (1)     vmake file vgname
*           (2) vmake file vsname {format} {ascii data stream}
*           (3) vmake file -l vgref v1 v2 ... vn
*
*
******************************************************************************/

#if defined __MWERKS__
#include <console.h>
#endif

#include "hdf.h"

/*
 * Prototypes of local functions
 */
int32       vsetlink
            (char *hfile, int32 vgid, int32 ids[], int32 n);

int32       vgadd
            (char *hfile, char *vgname);

int32       inpdata
            (unsigned char **bp);

void        vsadd
            (char *hfile, char *vsname, char *format);

int32       scanit
            (char *string, char ***fields, int32 **type, int32 **order);

int32       savfld
            (char *ss, int p1, int p2);

int32       compact
            (char *ss, char *dd);

int32       savtype
            (char *ss, int p1, int p2);

int32       separate
            (char *ss, char *fmt, int32 *num);

int         show_help_msg
            (void);

void showfmttypes 
            (void);

/*
 *  Main entry point
 */
int
main(int ac, char **av)
{

    char       *hfile, *vgname, *vsname, *fmt;
#if defined __MWERKS__
    ac = ccommand(&av);
#endif

    if (ac < 3)
      {
          show_help_msg();
          exit(0);
      }
    else if (ac == 3)
      {
          if (!HDstrcmp(av[2], "-l"))
            {
                show_help_msg();
                exit(0);
            }
          hfile = av[1];
          vgname = av[2];
          vgadd(hfile, vgname);
      }

    else if (ac == 4)
      {
          if (!HDstrcmp(av[2], "-l"))
            {
                show_help_msg();
                exit(0);
            }
          hfile = av[1];
          vsname = av[2];
          fmt = av[3];
          vsadd(hfile, vsname, fmt);
      }

    else if (!HDstrcmp(av[2], "-l"))
      {
          int         i;
          int32       n;
          int32       vgref, ids[50];

          hfile = av[1];
          sscanf(av[3], "%d", (int *)&vgref);
          for (n = 0, i = 4; i < ac; i++, n++)
            {
                sscanf(av[i], "%d", (int *)&ids[n]);
            }
          vsetlink(hfile, vgref, ids, n);
      }
    else
      {
          show_help_msg();
          exit(0);
      }
    return (0);
}   /* main */

void
showfmttypes(void)
{
    fprintf(stderr, "\tvalid fmt types: \n");
    fprintf(stderr, "\t  c - char    (char in HDF file)    \n");
    fprintf(stderr, "\t  b - byte    (int8 in HDF file)    \n");
    fprintf(stderr, "\t  s - short   (int16 in HDF file)   \n");
    fprintf(stderr, "\t  l - long    (int32 in HDF file)   \n");
    fprintf(stderr, "\t  f - float   (float32 in HDF file) \n");
}

int
show_help_msg(void)
{

    printf("\nvmake: creates vsets.\n");
    printf("\nUSAGE:\n");

    printf(" (1) vmake file vgname             (adds a new vgroup)\n");
    printf(" (2) vmake file vsname format      (adds a new vdata)\n");
    printf(" (3) vmake file -l vgref v1 .. vn  (links v1 v2 .. vn into vgref)\n");

    printf("\nwhere\n");
    printf("  vgref is the ref of a vgroup\n");
    printf("  v1,..,vn are refs of vgroups and vdatas\n");
    printf("  format is <field=fmt,field=fmt,..>\n");
    printf("    field is any text string\n");
    printf("    fmt is one of the following optionally preceded by a decimal.\n");
    showfmttypes();

    printf("\nTo create a vdata, vmake reads ascii data from stdin\n");

    printf("EXAMPLES:\n");
    printf("\t cat dat.txt | vmake hh.hdf \"triangles\" \"PLIST3=3l\"\n");
    printf("\t vmake abc.hdf \"xyvals\" \"X=d,Y=f\" < abc.dat\n");
    printf("\n");

    return (1);

}   /* show_help_msg */

/* ------------------------------------------------------- */

int32
vsetlink(char *hfile, int32 vgid, int32 ids[], int32 n)
{
    HFILEID     f;
    int32       vgmain, vg;
    int32       vs;
    int32       err = 0;
    int32       i;

    f = Hopen(hfile, DFACC_ALL, 0);
    if (f == FAIL)
      {
          fprintf(stderr, "cannot open %s.  \n", hfile);
          exit(0);
      }
    Vinitialize(f);
    vgmain = Vattach(f, vgid, "w");
    if (vgmain == FAIL)
      {
          fprintf(stderr, "0\n");
          Vfinish(f);
          Hclose(f);
          exit(-1);
      }

    for (i = 0; i < n; i++)
      {
          if (-1 != vexistvg(f, (uint16) ids[i]))
            {
                if ((vg = Vattach(f, ids[i], "r")) != FAIL)
                  {
                      if (Vinsert(vgmain, vg) == -1)
                        {   /*  is really VGROUP* */
                            err = 1;
                            fprintf(stderr, "insert a vg (%d)fails!!\n", (int) ids[i]);
                        }
                      Vdetach(vg);
                  }
            }
          else if (-1 != vexistvs(f, (uint16) ids[i]))
            {
                if ((vs = VSattach(f, ids[i], "r")) != FAIL)
                  {
                      if (Vinsert(vgmain, vs) == FAIL)
                        {
                            err = 1;
                            fprintf(stderr, "insert a vs (%d)fails!!\n", (int) ids[i]);
                        }
                      VSdetach(vs);
                  }
            }
          else
            {
                fprintf(stderr, "no such vgroup or vdata [%d]\n", (int) ids[i]);
                err = 1;
            }
      }

    Vdetach(vgmain);
    Vfinish(f);
    Hclose(f);

    if (err)
        exit(-1);
    else
        fprintf(stderr, "1\n");     /* success */
    return (1);
}   /* vsetlink */

/* ------------------------------------------------------- */
/*
   add a (new) vgroup to the file
 */

int32
vgadd(char *hfile, char *vgname)
{
    HFILEID     f;
    int32       ref;
    int32       vg;

    f = Hopen(hfile, DFACC_ALL, 0);
    if (f == FAIL)
      {
          fprintf(stderr, "cannot open %s. \n", hfile);
          exit(0);
      }
    Vinitialize(f);
    vg = Vattach(f, -1, "w");
    if (vg == FAIL)
      {
          fprintf(stderr, "cannot attach vg\n");
          exit(0);
      }
    ref = VQueryref(vg);
    Vsetname(vg, vgname);
    Vdetach(vg);
    Vfinish(f);
    Hclose(f);
    fprintf(stderr, "%d\n", (int) ref);
    return (1);

}   /* vgadd */

/* ------------------------------------------------------- */
/*
   add a (new) vdata to the file.
   Data will be ascii and will come in from stdin
   according to the format (c-style).
 */
void
vsadd(char *hfile, char *vsname, char *format)
{
    int32       ret, i, n, nwritten;
    unsigned char *buf;
    char      **fields;
    int32      *type, *order, nfld;
    char        allfields[100];
    HFILEID     f;
    int32       vs;
    int32       ref, ftype;

    nfld = scanit(format, &fields, &type, &order);
    if (nfld < 1)
      {
          fprintf(stderr, "bad fields\n");
          exit(-1);
      }

    if ((f = Hopen(hfile, DFACC_ALL, 0)) == FAIL)
      {
          fprintf(stderr, "cannot open %s.  \n", hfile);
          exit(-1);
      }
    Vinitialize(f);
    vs = VSattach(f, -1, "w");
    ref = VSQueryref(vs);

    printf("vsadd: ref is %d\n", (int) ref);

    allfields[0] = '\0';
    for (i = 0; i < nfld; i++)
      {
          switch (type[i])
            {
                case 'c':
                    ftype = DFNT_CHAR;
                    break;
                case 's':
                    ftype = DFNT_INT16;
                    break;
                case 'f':
                    ftype = DFNT_FLOAT32;
                    break;
                case 'l':
                    ftype = DFNT_INT32;
                    break;
                case 'b':
                    ftype = DFNT_INT8;
                    break;
                case 'D':
                    ftype = DFNT_DOUBLE;
                    break;

                default:
                    fprintf(stderr, "bad type [%c]\n", (char) type[i]);
                    showfmttypes();
                    exit(-1);
                    break;
            }
          ret = VSfdefine(vs, fields[i], ftype, order[i]);
          HDstrcat(allfields, fields[i]);
          HDstrcat(allfields, ",");
      }

    i = (int32)HDstrlen(allfields);
    allfields[i - 1] = '\0';    /* remove last comma */

    VSsetname(vs, vsname);
    ret = VSsetfields(vs, allfields);

    nwritten = 0;
    while ((n = inpdata(&buf)) > 0)
      {
          /*  printf("inpdata rets n=%d .. ",n); */
          ret = VSwrite(vs, buf, n, FULL_INTERLACE);
          printf("+%d  \n", (int) ret);
          nwritten += n;
          if (ret < 1)
              fprintf(stderr, "Vswrite stat=%d\n", (int) ret);
      }
    VSdetach(vs);
    Vfinish(f);
    Hclose(f);
    fprintf(stderr, "%d, %d\n", (int) ref, (int) nwritten);
    return;

}   /* vsadd */

/* ------------------------------------------------------------------ */
/* This part of the code deals with formatting stdin input data.      */
/* ------------------------------------------------------------------ */

#include <stdio.h>

#define MAXVAR 32
static char *fldptr[MAXVAR];
static char flds[MAXVAR][100];
static char fmts[MAXVAR];
static int32 fords[MAXVAR];
static int32 ftyp[MAXVAR];
static int  ntotal = 0;

/* scanf functions */
static int32
inplong(VOIDP x)
{
    int32       val, ret;

    ret = scanf("%d ", (int *)&val);
    *(int32 *)x = (int32) val;
    return (ret);
}

static int32
inpshort(VOIDP x)
{
    int         ret, val;

    ret = scanf("%d ", &val);
    *(int16 *)x = (int16) val;
    return (ret);
}

static int32
inpbyte(VOIDP x)
{
    int         ret;
    int         val;

    ret = scanf("%d ", &val);
    *(int8 *)x = (int8) val;
    return (ret);
}

static int32
inpfloat(VOIDP x)
{
    int         ret;
    float       val;

    ret = scanf("%f ", &val);
    *(float32 *)x = (float32) val;
    return (ret);
}

static int32
inpchar(VOIDP x)
{
    return (scanf("%c ", (char *)x));
}

#define BUFSIZE 40000
unsigned char inpbuffer[BUFSIZE];

int32
inpdata(unsigned char **bp)
{
    int32       totalsize, nread, t, i, j, k;
    unsigned char *b;
    int32       maxrec;
    int32       (*inpfn[MAXVAR]) (VOIDP);
    int32       inpsiz[MAXVAR];

    for (i = 0; i < ntotal; i++)
      {
          switch (fmts[i])
            {

                case 'c':
                    inpfn[i] = inpchar;
                    inpsiz[i] = sizeof(char);
                    break;

                case 'b':
                    inpfn[i] = inpbyte;
                    inpsiz[i] = sizeof(int8);
                    break;

                case 's':
                    inpfn[i] = inpshort;
                    inpsiz[i] = sizeof(short);
                    break;

                case 'l':
                    inpfn[i] = inplong;
                    inpsiz[i] = sizeof(long);
                    break;

                case 'f':
                    inpfn[i] = inpfloat;
                    inpsiz[i] = sizeof(float);
                    break;

                default:
                    printf("inpdata: fmt routine for [%c] not ready\n", fmts[i]);
            }
      }
    for (totalsize = 0, i = 0; i < ntotal; i++)
        totalsize += (fords[i] * inpsiz[i]);
    maxrec = BUFSIZE / totalsize - 1;

    /* begin reading in the ascii data from stdin */

    *bp = b = inpbuffer;
    for (nread = 0, j = 0; j < maxrec; j++, nread++)
      {
          for (i = 0; i < ntotal; i++)
            {
                for (k = 0; k < fords[i]; k++)
                  {
                      t = (inpfn[i]) (b);
                      if (t == EOF)
                          return (nread);
                      b += inpsiz[i];
                  }
            }
      }

    return (nread);     /* no of recs read */

}   /* inpdata */

int32
scanit(char *string, char ***fields, int32 **type, int32 **order)
{
    int32       ns, i;
    int32       p1, p2;
    char        ss[300];
    int32       c;

    compact(string, ss);
    ns = (int32)HDstrlen(ss);
    ss[ns++] = ',';

    p1 = p2 = 0;
    for (i = 0; i < ns; i++)
      {
          c = (int32)ss[i];
          if (c == '=')
            {
                p2 = i;
                savfld(ss, (int) p1, (int) (p2 - 1));
                p1 = p2 + 1;
            }
          else if (c == ',')
            {
                p2 = i;
                savtype(ss, (int) p1, (int) (p2 - 1));
                p1 = p2 + 1;
            }
      }
    for (i = 0; i < ntotal; i++)
      {
          fldptr[i] = flds[i];
          ftyp[i] = (int32)fmts[i];
      }

    *type = ftyp;
    *order = fords;
    *fields = fldptr;
    return (ntotal);

}   /* scanit */

int32
compact(char *ss, char *dd)
{
    int         i, t, n = (int)HDstrlen(ss);
    for (t = 0, i = 0; i < n; i++)
        if (ss[i] != ' ')
          {
              dd[t++] = ss[i];
          }
    dd[t] = '\0';
    return (1);
}

/* ------------------------------------------------------------------ */
int32
savfld(char *ss, int p1, int p2)
{
    int32       t = p2 - p1 + 1;

    HDstrncpy(flds[ntotal], &ss[p1], (size_t) t);
    flds[ntotal][t] = '\0';
    return (1);
}   /* savfld */

int32
savtype(char *ss, int p1, int p2)
{
    char        temp[20];
    int32       t = p2 - p1 + 1;
    HDstrncpy(temp, &ss[p1], (size_t)(p2 - p1 + 1));
    temp[t] = '\0';
    separate(temp, &fmts[ntotal], &fords[ntotal]);
    ntotal++;
    return (1);

}

int32
separate(char *ss, char *fmt, int32 *num)
{
    int32       i, n;
    i = 0;
    n = (int32)HDstrlen(ss);
    while (i < n)
      {
          if (ss[i] < '0' || ss[i] > '9')
              break;
          i++;
      }
    if (i > 0)
        sscanf(ss, "%d", (int *)num);
    else
        *num = 1;
    *fmt = ss[i];
    return (1);
}

/* ------------------------------------------------------------------ */
