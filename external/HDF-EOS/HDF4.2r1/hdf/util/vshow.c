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
static char *RcsId[] = "@(#)$Revision: 1.61 $";
#endif

/* $Id: vshow.c,v 1.61 2003/12/10 21:13:48 epourmal Exp $ */

/*****************************************************************************
*
* vshow.c
*
*   HDF Vset utility.
*
*   vshow:  dumps out vsets in a hdf file.
*
*   Usage:  vshow file [+|-[n]]
*       the '+' option indicates a full dump
*       the '-' option indicates a full dump with one record per line
*       'n' means only for the nth vdata.
*
*
******************************************************************************/
#if defined __MWERKS__
#include <console.h>
#endif

#define VSET_INTERFACE
#include "hdf.h"

static int  condensed;

static int32 vsdumpfull
            (int32 vs);

static int32 fmtbyte
            (char *x);
static int32 fmtchar
            (char *x);

#ifdef UNUSED
static int32 fmtint
            (char *x);
#endif /* UNUSED */

static int32 fmtfloat
            (char *x);

static int32 fmtulong
            (char *x);

static int32 fmtlong
            (char *x);

static int32 fmtshort
            (char *x);

static int32 fmtdouble
            (char *x);

static intn dumpattr
            (int32 vid, intn full, intn isvs);

int
main(int ac, char **av)
{
    int32       vg, vgt;
    int32       vgotag, vgoref;
    int32       vs;
    int32       vsotag, vsoref;
    HFILEID     f;
    int32       vgid = -1;
    int32       vsid = -1;
    int32       vsno = 0;
    int32       vstag;

    int32       i, t, nvg, n, ne, nv, interlace, vsize;
    int32      *lonevs;         /* array to store refs of all lone vdatas */
    int32       nlone;          /* total number of lone vdatas */

#if defined(MAC) || defined(macintosh) || defined(SYMANTEC_C)
    char        *fields = NULL;
#else
    char        fields[VSFIELDMAX*FIELDNAMELENMAX];
#endif
    char        vgname[VGNAMELENMAX];
    char        vsname[VSNAMELENMAX];
    char        vgclass[VGNAMELENMAX], vsclass[VSNAMELENMAX];
    char *name;
    int32       fulldump = 0, full;

#if defined(MAC) || defined(macintosh) || defined(SYMANTEC_C)
	fields = HDmalloc(VSFIELDMAX*FIELDNAMELENMAX);
	if (fields == NULL)
	{
          printf("Error: Out of memory. Cannot allocate %d bytes space. Quit.\n", VSFIELDMAX*FIELDNAMELENMAX);
          exit(0);
      }
#endif


#if defined __MWERKS__
    ac = ccommand(&av);
#endif

    if (ac == 3)
        if (av[2][0] == '-' || av[2][0] == '+')
          {
              sscanf(&(av[2][1]), "%d", (int *)&vsno);
              if (vsno == 0)
                {
                    printf("FULL DUMP\n");
                }
              else
                {
                    printf("FULL DUMP on vs#%ld\n", (long) vsno);
                }
              fulldump = 1;
              if (av[2][0] == '+')
                  condensed = 1;
              else
                  condensed = 0;
          }

    if (ac < 2)
      {
          printf("%s: dumps HDF vsets info from hdf file\n", av[0]);
          printf("usage: %s file [+|-[n]]\n", av[0]);
          printf("\t +  gives full dump of all vdatas.\n");
          printf("\t -  gives full dump of all vdatas one record per line.\n");
          printf("\t n  gives full dump of vdata with id n.\n");
          exit(0);
      }

    if ((f = Hopen(av[1], DFACC_READ, 0)) == FAIL)
    {
        printf("\nFile (%s) failed to open.\n", av[1]);
        exit(0);
    }

    Vstart(f);
    printf("\nFILE: %s\n", av[1]);

    nvg = 0;
    while ((vgid = Vgetid(f, vgid)) != -1)
      {
          vg = Vattach(f, vgid, "r");
          if (vg == FAIL)
            {
                printf("cannot open vg id=%d\n", (int) vgid);
            }
          Vinquire(vg, &n, vgname);
          vgotag = VQuerytag(vg);
          vgoref = VQueryref(vg);
          Vgetclass(vg, vgclass);
          if (HDstrlen(vgname) == 0)
              HDstrcat(vgname, "NoName");
          printf("\nvg:%d <%d/%d> (%s {%s}) has %d entries:\n",
           (int) nvg, (int) vgotag, (int) vgoref, vgname, vgclass, (int) n);
          dumpattr(vg, fulldump, 0);
          for (t = 0; t < Vntagrefs(vg); t++)
            {
                Vgettagref(vg, t, &vstag, &vsid);

                /* ------ V D A T A ---------- */
                if (vstag == VSDESCTAG)
                  {
                      vs = VSattach(f, vsid, "r");

                      if (vs == FAIL)
                        {
                            printf("cannot open vs id=%d\n", (int) vsid);
                            continue;
                        }

                      VSinquire(vs, &nv, &interlace, fields, &vsize, vsname);
                      vsotag = VSQuerytag(vs);
                      vsoref = VSQueryref(vs);
                      if (HDstrlen(vsname) == 0)
                          HDstrcat(vsname, "NoName");
                      VSgetclass(vs, vsclass);
                      printf("  vs:%d <%d/%d> nv=%d i=%d fld [%s] vsize=%d (%s {%s})\n",
                             (int) t, (int) vsotag, (int) vsoref, (int) nv, (int) interlace, fields, (int) vsize, vsname, vsclass);

                      if (fulldump && vsno == 0)
                          vsdumpfull(vs);
                      else if (fulldump && vsno == vsoref)
                          vsdumpfull(vs);
                      /* dump attributes */
                      full = fulldump && (vsno == 0 || vsno == vsoref);
                      dumpattr(vs, full, 1);
                                         
                      VSdetach(vs);
                  }
                else if (vstag == DFTAG_VG)
                  {
                      /* ------ V G R O U P ----- */
                      vgt = Vattach(f, vsid, "r");

                      if (vgt == FAIL)
                        {
                            printf("cannot open vg id=%d\n", (int) vsid);
                            continue;
                        }

                      Vinquire(vgt, &ne, vgname);
                      if (HDstrlen(vgname) == 0)
                          HDstrcat(vgname, "NoName");
                      vgotag = VQuerytag(vgt);
                      vgoref = VQueryref(vgt);
                      Vgetclass(vgt, vgclass);
                      printf("  vg:%d <%d/%d> ne=%d (%s {%s})\n",
                             (int) t, (int) vgotag, (int) vgoref, (int) ne, vgname, vgclass);
                      dumpattr(vg, fulldump, 0);
                      Vdetach(vgt);
                  }
                else
                  {
                      name = HDgettagsname((uint16) vstag);
                      if (!name)
                          printf("  --:%d <%d/%d> %s\n", (int) t, (int) vstag, (int) vsid, "Unknown Tag");
                      else
                        {
                            printf("  --:%d <%d/%d> %s\n", (int) t, (int) vstag, (int) vsid, name);
                            HDfree(name);
                        } /* end else */
                  }
            }   /* while */

          Vdetach(vg);
          nvg++;

      }     /* while */

    if (nvg == 0)
      {
          printf("No vgroups in this file\n");
      }

    nlone = VSlone(f, NULL, 0);
    if (nlone > 0)
      {

          printf("Lone vdatas:\n");
          if (NULL == (lonevs = (int32 *) HDmalloc(sizeof(int) * (size_t)nlone)))
            {
                printf("%s: File has %d lone vdatas but ", av[0], (int) nlone);
                printf("cannot alloc lonevs space. Quit.\n");
                exit(0);
            }

          VSlone(f, lonevs, nlone);
          for (i = 0; i < nlone; i++)
            {
                vsid = lonevs[i];
                if (FAIL == (vs = VSattach(f, lonevs[i], "r")))
                  {
                      printf("cannot open vs id=%d\n", (int) vsid);
                      continue;
                  }
                VSinquire(vs, &nv, &interlace, fields, &vsize, vsname);
                if (HDstrlen(vsname) == 0)
                    HDstrcat(vsname, "NoName");
                vsotag = VSQuerytag(vs);
                vsoref = VSQueryref(vs);
                VSgetclass(vs, vsclass);
                printf("L vs:%d <%d/%d> nv=%d i=%d fld [%s] vsize=%d (%s {%s})\n",
                       (int) vsid, (int) vsotag, (int) vsoref, (int) nv, (int) interlace, fields, (int) vsize, vsname, vsclass);
                if (fulldump && vsno == 0)
                    vsdumpfull(vs);
                else if (fulldump && vsno == vsoref)
                    vsdumpfull(vs);
                full = fulldump && ( vsno == 0 || vsno == vsoref);
                dumpattr(vs, full, 1);
                VSdetach(vs);
            }
          HDfree(lonevs);
      }

    Vend(f);
    Hclose(f);

#if defined(MAC) || defined(macintosh) || defined(SYMANTEC_C)
	if (fields)
		HDfree(fields);
#endif

    return (0);

}   /* main */

static int32 cn = 0;

/* ------------------------------------------------ */
/* printing functions used by vsdumpfull(). */
static int32
fmtbyte(char *x)
{
    cn += printf("%02x ", *x);
    return (1);
}

static int32
fmtchar(char *x)
{
    cn++;
    putchar(*x);
    return (1);
}

#ifdef UNUSED
static int32
fmtint(char *x)
{
    int         i = 0;
    HDmemcpy(&i, x, sizeof(int32));
    cn += printf("%d", i);
    return (1);
}
#endif /* UNUSED */

static int32
fmtfloat(char *x)
{
    float       f = (float)0.0;
    HDmemcpy(&f, x, sizeof(float32));
    cn += printf("%f", f);
    return (1);
}

static int32
fmtulong(char *x)
{
    unsigned    l = 0;
    HDmemcpy(&l, x, sizeof(int32));
    cn += printf("%u", l);
    return (1);
}

static int32
fmtlong(char *x)
{
    long        l = 0;
    HDmemcpy(&l, x, sizeof(int32));
    cn += printf("%ld", l);
    return (1);
}

static int32
fmtshort(char *x)
{
    short       s = 0;
    HDmemcpy(&s, x, sizeof(int16));
    cn += printf("%d", s);
    return (1);
}

static int32
fmtdouble(char *x)
{
    double      d = 0.0;
    HDmemcpy(&d, x, sizeof(float64));
    cn += printf("%f", d);
    return (1);
}

#define BUFFER 1000000

/* ------------------------------------------------ */

static int32
vsdumpfull(int32 vs)
{
#if defined(MAC) || defined(macintosh) || defined(SYMANTEC_C)
    char        *fields = NULL; 
#else
    char        fields[VSFIELDMAX*FIELDNAMELENMAX]; 
#endif
    char        vsname[100];
    int32       j, i, t, interlace, nv, vsize;
    uint8      *bb, *b;
    DYN_VWRITELIST *w;
    int32       (*fmtfn[VSFIELDMAX]) (char *);
    int32       off[VSFIELDMAX];
    int32       order[VSFIELDMAX];

    int32       bufsize;        /* size of the buffer we are using */
    int32       chunk;          /* number of rows that will fit in the buffer */
    int32       done;           /* number of rows we have done */
    int32       count;          /* number of rows to do this time through the loop */

    int32       nf;             /* number of fields in this Vdata */

#if defined(MAC) || defined(macintosh) || defined(SYMANTEC_C)
	fields = HDmalloc(VSFIELDMAX*FIELDNAMELENMAX);
	if (fields == NULL)
	{
          printf("Error: Out of memory. Cannot allocate %d bytes space. Quit.\n", VSFIELDMAX*FIELDNAMELENMAX);
          return(0);
      }
#endif

    VSinquire(vs, &nv, &interlace, fields, &vsize, vsname);

    if (nv * vsize > BUFFER)
      {
          bufsize = BUFFER;
          chunk = BUFFER / vsize;
      }
    else
      {
          bufsize = nv * vsize;
          chunk = nv;
      }

    done = 0;
    bb = (uint8 *) HDmalloc(bufsize);
    if (bb == NULL)
      {
          printf("vsdumpfull malloc error\n");
#if defined(MAC) || defined(macintosh) || defined(SYMANTEC_C)
	    if (fields)			HDfree(fields);
#endif
          return (0);
      }

    VSsetfields(vs, fields);

    w = vswritelist(vs);

    nf = w->n;
    for (i = 0; i < w->n; i++)
      {
          printf("%d: fld [%s], type=%d, order=%d\n", (int) i, w->name[i], w->type[i], w->order[i]);

          order[i] = (int32)w->order[i];
          off[i] = DFKNTsize(w->type[i] | DFNT_NATIVE);

          switch (w->type[i])
            {

                case DFNT_CHAR:
                case DFNT_UCHAR:
                    fmtfn[i] = fmtchar;
                    break;

                case DFNT_UINT8:
                case DFNT_INT8:
                    fmtfn[i] = fmtbyte;
                    break;

                case DFNT_UINT16:
                case DFNT_INT16:
                    fmtfn[i] = fmtshort;
                    break;

                case DFNT_UINT32:
                      fmtfn[i] = fmtulong;
                      break;

                case DFNT_INT32:
                    fmtfn[i] = fmtlong;
                    break;

                case DFNT_FLOAT32:
                    fmtfn[i] = fmtfloat;
                    break;

                case DFNT_FLOAT64:
                    fmtfn[i] = fmtdouble;
                    break;

                default:
                    fprintf(stderr, "sorry, type [%d] not supported\n", (int) w->type[i]);
                    break;

            }
      }

    cn = 0;

    done = count = 0;
    while (done != nv)
      {

          /* figure out how many to read this time */
          if ((nv - done) > chunk)
              count = chunk;
          else
              count = nv - done;

          /* read and update bookkeeping */
          VSread(vs, bb, count, interlace);
          done += count;
          b = bb;

          /* print out the data */
          for (j = 0; j < count; j++)
            {
                for (i = 0; i < nf; i++)
                  {
                      for (t = 0; t < order[i]; t++)
                        {
                            (fmtfn[i]) ((char *)b);
                            b += off[i];
                            putchar(' ');
                            cn++;
                        }
                      putchar(' ');
                      cn++;
                  }

                /*
                 * if condensed == TRUE put as many as possible on one line else
                 *   put one record per line
                 */
                if (condensed)
                  {
                      if (cn > 65)
                        {
                            putchar('\n');
                            cn = 0;
                        }
                  }
                else
                  {
                      putchar('\n');
                  }
            }

      }

    /* ============================================ */

    HDfree(bb);
    printf("\n");

#if defined(MAC) || defined(macintosh) || defined(SYMANTEC_C)
	if (fields)
		HDfree(fields);
#endif

    return (1);

}   /* vsdumpfull */
/* ------------------------------------------------ */
static intn dumpattr(int32 vid, intn full, intn isvs)
{
   intn i, j, k, cn=0;
   VDATA *vs;
   vsinstance_t *vs_inst;
   VGROUP *vg;
   vginstance_t *v;
   intn ret, nattrs, f_nattrs, alloc_flag=0;
   vs_attr_t *vs_alist;
   vg_attr_t *v_alist;
   int32 i_type, i_count, i_size, off;
   uint8 *buf=NULL, *ptr;
   int32 (*fmtfn)(char *) =NULL;
   char name[FIELDNAMELENMAX+1];
   intn ret_val = SUCCEED;
#if defined(MAC) || defined(macintosh) || defined(SYMANTEC_C)
   long bufsize = BUFFER;
   uint8 *attrbuf = HDmalloc(bufsize);
   if (attrbuf == NULL)
   {
	printf(">>>dumpattr: Out of memory. Cannot allocate %d byte attribute buffer.\n", bufsize);
	return FAIL;
   }
#else
   uint8 attrbuf[BUFFER];
#endif

   if (isvs)  {
      vs_inst = (vsinstance_t *)HAatom_object(vid);
      if (vs_inst == NULL)  {
         printf(">>>dumpattr:failed in getting vdata instance.\n");
         ret_val = FAIL;
         goto done;
      } 
      vs = vs_inst->vs;
      if (vs == NULL)  {
         printf(">>>dumpattr:Failed in getting vs. \n");
         ret_val = FAIL;
         goto done;
      }
      if (0 == (nattrs = VSnattrs(vid))) {
          printf(" 0 attributes.\n");
          ret_val = SUCCEED;
          goto done;
      }
      vs_alist = vs->alist;
      if (!full) {
          printf("     %d attributes:  attr_tag/ref     attr_of_field\n",
                       nattrs);
          for (i=0; i<nattrs; i++)  {
             if (vs_alist->findex != (int)_HDF_VDATA)
                 printf("     %d:               %d/%d               %d\n",
                 i, vs_alist->atag, vs_alist->aref,(int)vs_alist->findex);
             else
                 printf("     %d:               %d/%d         %s\n",
                 i, vs_alist->atag, vs_alist->aref, "VDATA");
             vs_alist++;
         }
         ret_val = SUCCEED;
         goto done;
      }
      printf("%d attributes:\n", nattrs);
      for (j=-1; j<vs->wlist.n; j++)  
      {	  int32 temp;

	  temp = (j == -1) ? (int)_HDF_VDATA : j;
          f_nattrs = VSfnattrs(vid, temp);
          if (f_nattrs == 0) continue;  /* no attr for this field */
          if (j == -1)
              printf("   Attrs of vdata:\n");
          else
  	      printf("   Attrs of field %d:\n", j);
          for (i = 0; i<f_nattrs; i++)  {   /* dump the attrs */
              ret = VSattrinfo(vid, temp, i, name, &i_type, &i_count, &i_size);
              if (ret == FAIL) {
                 printf(">>>dumpattr: failed in getting attr info.\n");
                 continue;
              }
              printf("     %d: name=%s type=%d count=%d size=%d\n",
                       i, name, (int)i_type, (int)i_count, (int)i_size);
              if (i_size > BUFFER) {
                  if (NULL == (buf = HDmalloc(i_size)))  {
                     printf(">>>dumpattr:can't allocate buf.\n");
                     continue;
                  }
                  alloc_flag = 1;
                  if ( FAIL==VSgetattr(vid, temp, i, buf)) {
                     printf(">>>dympattr: failed in VSgetattr.\n");
                     continue;
                  }
              }
              else 
              {
                 if ( FAIL==VSgetattr(vid, temp, i, attrbuf)) {
                     printf(">>>dympattr: failed in VSgetattr.\n");
                     continue;
                  }
              }
              /* format output */
              switch (i_type)  {
                 case DFNT_CHAR:
                 case DFNT_UCHAR:
                      fmtfn = fmtchar;
                      break;
                 case DFNT_UINT8:
                 case DFNT_INT8:
                      fmtfn = fmtbyte;
                      break;
                 case DFNT_UINT16:
                 case DFNT_INT16:
                      fmtfn = fmtshort;
                      break;
                 case DFNT_UINT32:
                      fmtfn = fmtulong;
                      break;
                 case DFNT_INT32:
                      fmtfn = fmtlong;
                      break;
                 case DFNT_FLOAT32:
                      fmtfn = fmtfloat;
                      break;
                 case DFNT_FLOAT64:
                      fmtfn = fmtdouble;
                      break;
                default:
                    printf(">>>dumpattr: sorry, type [%d] not supported\n", (int) i_type);
                    break;
              }
              off = DFKNTsize(i_type | DFNT_NATIVE);
              ptr = (alloc_flag) ? buf : attrbuf;
              putchar('\t');
              cn = 0;
              for (k=0; k<i_count; k++)  {
                  fmtfn((char *)ptr);
                  ptr += off;
                  putchar(' ');
                  cn++;
                  if (cn > 55)  {
                     putchar('\n');
                     putchar('\t');
                     cn = 0;
                  }
              }
              if (cn) putchar('\n');
              if (alloc_flag) {
                 if ( buf != NULL) 
                    HDfree(buf);
                 alloc_flag = 0;
              }
          }  /*  attr */
      }   /* field   */
   }  /* isvs */

   else {  /* vgroup */
      v = (vginstance_t *)HAatom_object(vid);
      if (v== NULL)  {
         printf(">>>dumpattr:failed in getting vgroup instance.\n");
         ret_val = FAIL;
         goto done;
      } 
      vg = v->vg;
      if (vg == NULL)  {
         printf(">>>dumpattr:Failed in getting vg. \n");
         ret_val = FAIL;
         goto done;
      }
      if (0 == (nattrs = Vnattrs(vid)))  {
          printf("  0 attributes.\n");
          ret_val = SUCCEED;
          goto done;
      }
      v_alist = vg->alist;
      if (!full) {
          printf("%d attributes:       attr_tag/ref  \n", nattrs);
          for (i=0; i<nattrs; i++)  {
             printf("     %d:               %d/%d    \n",
                 i, v_alist->atag, v_alist->aref);
             v_alist++;
         }
          ret_val = SUCCEED;
          goto done;
      }
      printf("%d attributes:\n", nattrs);
      for (i = 0; i<nattrs; i++)  {   /* dump the attrs */
          ret = Vattrinfo(vid, i, name, &i_type, &i_count, &i_size);
          if (ret == FAIL) {
              printf(">>>dumpattr: failed in getting attr info.\n");
              continue;
          }
          printf("   %d: name=%s type=%d count=%d size=%d\n",
                     i,  name, (int)i_type, (int)i_count, (int)i_size);
          if (i_size > BUFFER) {
              if (NULL == (buf = HDmalloc(i_size)))  {
                 printf(">>>dumpattr:can't allocate buf.\n");
                 continue;
              }
              alloc_flag = 1;
              if ( FAIL == Vgetattr(vid, i, buf)) {
                 printf(">>>dympattr: failed in Vgetattr.\n");
                 continue;
              }
          }
          else 
          {
             if ( FAIL == Vgetattr(vid, i, attrbuf)) {
                 printf(">>>dympattr: failed in Vgetattr.\n");
                 continue;
              }
          }
          /* format output */
          switch (i_type)  {
             case DFNT_CHAR:
             case DFNT_UCHAR:
                  fmtfn = fmtchar;
                  break;
             case DFNT_UINT8:
             case DFNT_INT8:
                  fmtfn = fmtbyte;
                  break;
             case DFNT_UINT16:
             case DFNT_INT16:
                  fmtfn = fmtshort;
                  break;
             case DFNT_UINT32:
                  fmtfn = fmtulong;
                  break;
             case DFNT_INT32:
                  fmtfn = fmtlong;
                  break;
             case DFNT_FLOAT32:
                  fmtfn = fmtfloat;
                  break;
             case DFNT_FLOAT64:
                  fmtfn = fmtdouble;
                  break;
             default:
                printf(">>>dumpattr: sorry, type [%d] not supported\n", (int) i_type);
                break;
          }
          off = DFKNTsize(i_type | DFNT_NATIVE);
          ptr = (alloc_flag) ? buf : attrbuf;
          putchar('\t');
          cn = 0;
          for (k=0; k<i_count; k++)  {
              fmtfn((char *)ptr);
              ptr += off;
              putchar(' ');
              cn++;
              if (cn > 55)  {
                 putchar('\n');
                 putchar('\t');
                 cn = 0;
              }
          }
          if (cn) putchar('\n');
          if (alloc_flag) {
             if ( buf != NULL) 
                HDfree(buf);
             alloc_flag = 0;
          }
      }  /*  attr */
   }  /* vgroup */
   
   ret_val = SUCCEED;

done:
#if defined(MAC) || defined(macintosh) || defined(SYMANTEC_C)
   if (attrbuf)			HDfree(attrbuf);
#endif

   return ret_val;
}                   
/* ------------------------------------- */
