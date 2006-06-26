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

/* Modified from vshow.c by Eric Tsui, 12/25/1994. */

#ifdef RCSID
static char *RcsId[] = "@(#)$Revision: 1.25 $";
#endif

#define VSET_INTERFACE
#include "hdp.h"

#define BUFFER 1000000

/* ------------------------------------------------ */

int32
dumpvd(int32       vd, 
       file_type_t ft, 
       int         data_only, 
       FILE       *fp, 
       char        separater[2],
       int32       flds_indices[VSFIELDMAX], 
       int         dumpallfields)
{
    char        vdname[VSNAMELENMAX];
    int32       j, i, t, interlace, nv, vsize;
    uint8      *bb = NULL;
    uint8      *b = NULL;
    DYN_VWRITELIST *w = NULL;
    intn       (*vfmtfn[VSFIELDMAX]) (VOIDP ,  file_type_t ft,  FILE *);
    int32       off[VSFIELDMAX];
    int32       order[VSFIELDMAX];
    int32       nattrs[VSFIELDMAX];
    int32       bufsize;		/* size of the buffer we are using */
    int32       chunk;			/* number of rows that will fit in the buffer */
    int32       done;			/* number of rows we have done */
    int32       count;          /* number of rows to do this time through 
                                           the loop */
    int32       nf; 	       	/* number of fields in this Vdata */
    int32       x, display;
    int32       temp;
    int32       addr_width = 0;
    int32       num_digits;
    int32       address = 0;
    int32       nfields;
    int32       cnt1, cnt2;
    int32       cn = 0;
    int32       ret_value = SUCCEED;

#if defined (MAC) || defined (macintosh) || defined (SYMANTEC_C) || defined(__APPLE__)
	/* macintosh cannot handle >32K locals */
    char *fields = (char *)HDmalloc(VSFIELDMAX*FIELDNAMELENMAX* sizeof(char));
    char *flds = (char *)HDmalloc(VSFIELDMAX*FIELDNAMELENMAX* sizeof(char));

    CHECK_ALLOC( fields, "fields", "dumpvd" );
    CHECK_ALLOC( flds, "flds", "dumpvd" );

#else /* !macintosh */
    char        fields[VSFIELDMAX*FIELDNAMELENMAX];
    char        flds[VSFIELDMAX*FIELDNAMELENMAX];
#endif /* !macintosh */    

    /* inquire about vdata */
    if (FAIL == VSinquire(vd, &nv, &interlace, fields, &vsize, vdname))
      {
          ret_value = FAIL;
          goto done;
      }

    if (nv * vsize > BUFFER)	/* If the number of records in the vdata is 
                                   bigger than the buffer size, then divide
                                   the records into chunks. */
      {
          bufsize = BUFFER;
          chunk = BUFFER / vsize;
      }
    else
        /* Otherwise, all the records will be read in at one time. */
      {
          bufsize = nv * vsize;
          chunk = nv;
      }

    done = 0;
    /* Allocate space for the buffer and terminate hdp if allocation fails. */
    bb = (uint8 *) HDmalloc(bufsize);
    CHECK_ALLOC( fields, "fields", "dumpvd" );
    
    if (FAIL == VSsetfields(vd, fields))
      {
          fprintf(stderr,"dumpvd: VSsetfields failed for vd = %d \n",(int)vd);
          ret_value = FAIL;
          goto done;
      }

    w = vswritelist(vd);

    nf = w->n;
    x = 0;	/* Used for accessing the array storing the indices of the 
                   selected fields. */
    for (i = 0; i < nf; i++)	/* Read in data of all the fields. */
      {
          order[i] = w->order[i];

          /* Set offset for the next element. */
          off[i] = DFKNTsize(w->type[i] | DFNT_NATIVE);
          nattrs[i] = VSfnattrs(vd, i);
          if (FAIL == nattrs[i])
            {
                fprintf(stderr,"dumpvd: VSfnattrs failed for vd = %d \n",(int)vd);
                ret_value = FAIL;
                goto done;
            }

          /* Display the header of a vdata if the user didn't specify the
             data-only option. */
          if (!data_only)
          { 
             if(ft==DASCII)
             {
                if ((dumpallfields) || (flds_indices[x] == i))
                {
                   fprintf(fp, "- field index %d: [%s], type=%d, order=%d\n",
                          (int) i, w->name[i], w->type[i], w->order[i]);
                   x++;
                }
             }
             /* display attributes - BMR moved this block inside if(!data_only) 
		to keep the attributes from being printed - bug #231*/
             if (FAIL == dumpattr(vd, i, 1, ft, fp))
             {
                fprintf(stderr,"dumpvd: dumpattr() failed for vd = %d \n",(int)vd);
                ret_value = FAIL;
                goto done;
             }

          }	/* if !data_only */

          /* Choose a function for displaying a piece of data of a 
             particular type. */
          switch (w->type[i] & 0xff)
            {
            case DFNT_CHAR:
	    case DFNT_UCHAR:
                vfmtfn[i] = fmtchar;
                break;

            case DFNT_UINT8:
                vfmtfn[i] = fmtuint8;
                break;

            case DFNT_INT8:
                vfmtfn[i] = fmtint8;
                break;

            case DFNT_UINT16:
                vfmtfn[i] = fmtuint16;
                break;

            case DFNT_INT16:
                vfmtfn[i] = fmtint16;
                break;

            case DFNT_UINT32:
                vfmtfn[i] = fmtuint32;
                break;

            case DFNT_INT32:
                vfmtfn[i] = fmtint32;
                break;

            case DFNT_FLOAT32:
                vfmtfn[i] = fmtfloat32;
                break;

            case DFNT_FLOAT64:
                vfmtfn[i] = fmtfloat64;
                break;

            default:
                fprintf(stderr, "sorry, type [%d] not supported\n", (int) w->type[i]);
                ret_value = FAIL;
                goto done;

            }	/* switch */
      }		/* for */

    cn = 0;
    done = count = 0;
    
    if(ft==DASCII)
      { 

          /* If not just the data will be dumped out, then put an address-type
             column on the left so that the user can recognize which record 
             he/she is looking at. */
          if (!data_only)
            {
                temp = nv / 10;
                address = 0;
                addr_width = num_digits = 1;
                while (temp != 0)
                  {
                      if (temp != 1)
                          addr_width++;
                      temp = temp / 10;
                  }
                fprintf(fp, "Loc.");
                for (j = 0; j < addr_width - 3; j++)
                    fprintf(fp, " ");
                fprintf(fp, "     Data\n");

                /* The address of the first record is 0. Also, fill in the extra 
                   space on the left with 0's. */
                while (num_digits <= addr_width)
                  {
                      fprintf(fp, "0");
                      num_digits++;
                      cn++;
                  }
                fprintf(fp, "      ");
                cn += 6;
                if (addr_width == 2)
                  {
                      fprintf(fp, " ");
                      cn++;
                  }
                else if (addr_width == 1)
                  {
                      fprintf(fp, "  ");
                      cn += 2;
                  }
            }		/* while */

          nfields = VSgetfields(vd, flds);
          if (FAIL == nfields )
            {
                fprintf(stderr,"dumpvd: VSgetfields failed for vd = %d \n",(int)vd);
                ret_value = FAIL;
                goto done;
            }

          cnt1 = 0;
          cnt2 = 0;
          while (done != nv)
            {
                /* Determine the amount of data to be read this time. */
                if ((nv - done) > chunk)
                    count = chunk;
                else
                    count = nv - done;

                /* read and update bookkeeping */
                if (FAIL == VSread(vd, bb, count, interlace))
                  {
                      fprintf(stderr,"dumpvd: VSread failed for vd = %d \n",(int)vd);
                      ret_value = FAIL;
                      goto done;
                  }

                done += count;
                b = bb;

                /* Display the data. */
                for (j = 0; j < count; j++)	/* each iteration causes one record 
                                               to be printed */
                  {
                      cnt1++;
                      x = 0;
                      for (i = 0; i < nf; i++)	/* display all fields in one record */
                        {
                            if ((!dumpallfields) && (flds_indices[x] != i))
                                display = 0;
                            else
                              {
                                  display = 1;
                                  x++;
                              }

                            for (t = 0; t < order[i]; t++)
                              {
                                  if(display)
                                      cn+=(vfmtfn[i]) (b, ft, fp);
                                  b += off[i];
                                  if (display)
                                    {
                                        fprintf(fp, " ");
                                        cn++;
                                        cnt2++;
                                    }
                              }
                            if (display)
                              {
                                  fprintf(fp, " ");
                                  cn++;
                                  cnt2++;
                              }
                        }		/* for i to nf-1 */

	   
                      if (cnt2 > 0)
                        {
                            address++;
                            /* "separator" is the symbol used for separating 
                               different records. */
                            fprintf(fp, "%s ", separater);
                        }

                      if (!data_only)
                        {
                            if ((cnt1 * cnt2) > 30)
                              {
                                  cnt1 = 0;
                                  cnt2 = 0;
                                  fprintf(fp, "\n");
                                  cn = 0;

                                  /* As long as there is data to be displayed,
                                     fill in the extra space with 0's on the left
                                     of each address. */
                                  if (j < (count - 1))
                                    {
                                        temp = address;
                                        num_digits = 1;
                                        while ((temp = temp / 10) != 0)
                                            num_digits++;
                                        while (num_digits < addr_width)
                                          {
                                              fprintf(fp, "0");
                                              num_digits++;
                                              cn++;
                                          }
                                        fprintf(fp, "%d      ", (int)address);
                                        cn += 6 + num_digits;
                                        if (addr_width == 2)
                                          {
                                              fprintf(fp, " ");
                                              cn++;
                                          }
                                        else if (addr_width == 1)
                                          {
                                              fprintf(fp, "  ");
                                              cn += 2;
                                          }
                                    }		/* if (!data_only) */
                              }
                        }
                      else
                          fprintf(fp, "\n");
                  }	/* for (j=0; j<count; j++) */
            }		/* while (done != nv) */

          /* ============================================ */

          HDfree((VOIDP) bb);
          bb = NULL;

          fprintf(fp, "\n\n");
      }  /*  for DASCII  */

    else
      {       /*  binary file  */
          nfields = VSgetfields(vd, flds);
          if (FAIL == nfields )
            {
                fprintf(stderr,"dumpvd: VSgetfields failed for vd = %d \n",(int)vd);
                ret_value = FAIL;
                goto done;
            }

          cnt1 = 0;
          cnt2 = 0; 
          while (done != nv)
            {
                /* Determine the amount of data to be read this time. */
                if ((nv - done) > chunk)
                    count = chunk;
                else
                    count = nv - done;

                /* read and update bookkeeping */
                if (FAIL == VSread(vd, bb, count, interlace))
                  {
                      fprintf(stderr,"dumpvd: VSread failed for vd = %d \n",(int)vd);
                      ret_value = FAIL;
                      goto done;
                  }

                done += count;
                b = bb;

                /* Display the data. */
                for (j = 0; j < count; j++)	/* each iteration causes one record 
                                               to be printed */
                  {
                      cnt1++;
                      x = 0;
                      for (i = 0; i < nf; i++)	/* display all fields in one record */
                        {
                            if ((!dumpallfields) && (flds_indices[x] != i))
                                display = 0;
                            else
                              {
                                  display = 1;
                                  x++;
                              }

                            for (t = 0; t < order[i]; t++)
                              {
                                  if(display)
                                      cn+=(vfmtfn[i]) (b, ft, fp);
                                  b += off[i];
                                  if (display)
                                    {
                           
                                        cn++;
                                        cnt2++;
                                    }
                              }
                            if (display)
                              {
                           
                                  cn++;
                                  cnt2++;
                              }
                        }		/* for i to nf-1 */      
                      if (cnt2 > 0)
                        {
                            address++;
                            /* "separator" is the symbol used for separating
                               different records. */
                        }

                  }	/* for (j=0; j<count; j++) */
            }		/* while (done != nv) */

          /* ============================================ */

          HDfree((VOIDP) bb);     
          bb = NULL;
      }   /* binary file */

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
          if (bb != NULL)
              HDfree((VOIDP)bb);
      }
    /* Normal cleanup */
#if defined (MAC) || defined (macintosh) || defined (SYMANTEC_C) || defined(__APPLE__)
   if(fields != NULL)
   {
      HDfree(fields);
      fields = NULL;
    } 
   if(flds != NULL)
   {
      HDfree(flds);
      fields = NULL;
    } 
#endif /* macintosh */
    
    return ret_value;
}	/* dumpvd */


/* 
 * dumps attributes of vdata for vgroup
 */
intn 
dumpattr(int32 vid, 
         int32 findex, 
         intn isvs,
         file_type_t ft, 
         FILE *fp)
{
    intn          i, k;
    intn          cn = 0;
    intn          nattrs;
    intn          alloc_flag = 0;
    int32         i_type;
    int32         i_count;
    int32         i_size, e_size;
    int32         off;
    uint8         *buf = NULL;
    uint8         *ptr = NULL;
    intn (*vfmtfn)(VOIDP, file_type_t ft, FILE *);
    intn          status;
    intn          ret_value = SUCCEED;

#if defined (MAC) || defined (macintosh) || defined (SYMANTEC_C) || defined(__APPLE__)
	/* macintosh cannot handle >32K locals */
    char *name = (char *)HDmalloc((FIELDNAMELENMAX+1) * sizeof(char));
    uint8 *attrbuf = (uint8 *)HDmalloc((BUFFER) * sizeof(uint8));

    /* check if allocations fail, terminate hdp */
    CHECK_ALLOC( name, "name", "dumpattr" );
    CHECK_ALLOC( attrbuf, "attrbuf", "dumpattr" );

#else /* !macintosh */
    char          name[FIELDNAMELENMAX+1];
    uint8         attrbuf[BUFFER];
#endif /* !macintosh */    

    /* vdata or vgroup? */
    if (isvs) 
        nattrs = VSfnattrs(vid, findex);
    else
        nattrs = Vnattrs(vid);

    if (FAIL == nattrs)
      {
          fprintf(stderr,">>>dumpattr: Failed to get number of attributes for vid %d \n",(int)vid);
          ret_value = FAIL;
          goto done;
      }

    fprintf(fp, "   number of attributes = %d \n", nattrs);

    /* loop for number of attributes to process */
    for (i = 0; i < nattrs; i++) 
      {
          /* get attribute infor of vdata/vgroup */
          if (isvs)
              status = VSattrinfo(vid, findex, i, name, &i_type, &i_count, &e_size);
          else
              status = Vattrinfo(vid, i, name, &i_type,&i_count, &e_size);

          if (status == FAIL) 
            {
                fprintf(stderr,">>>dumpattr: failed in getting %d'th attr info.\n",i);
                ret_value = FAIL;
                goto done;
            }

          /* get attribute hdfsize of vdata/vgroup */
          if (isvs)
              status = VSattrhdfsize(vid, findex, i, &i_size);
          else
              status = Vattrhdfsize(vid, i, &i_size);

          if (status == FAIL) 
            {
                fprintf(stderr,">>>dumpattr: failed in getting %d'th attr hdfsize.\n",i);
                ret_value = FAIL;
                goto done;
            }

          fprintf(fp,"    attr%d: name=%s type=%d count=%d size=%d\n",
                  i, name, (int)i_type, (int)i_count, (int)i_size);

          /* we have two buffer sizes? */
          if (e_size > BUFFER) 
            {
                if (NULL == (buf = HDmalloc(e_size)))  
                  {
                      fprintf(stderr,">>>dumpattr:can't allocate buf for %d'th attribute.\n",i);
                      ret_value = FAIL;
                      goto done;  /* do we want exit here? */
                  }

                alloc_flag = 1;

                /* get attribute itself */
                if (isvs) 
                    status = VSgetattr(vid, findex, i, (VOIDP)buf);
                else
                    status = Vgetattr(vid, i, (VOIDP)buf);

                if (status == FAIL) 
                  {
                      fprintf(stderr,">>>dympattr: failed in getting %d'th attr .\n",i);
                      ret_value = FAIL;
                      goto done;
                  }
            }
          else
            {
                /* get attribute itself */
                if (isvs) 
                    status = VSgetattr(vid, findex, i, (VOIDP)attrbuf);
                else
                    status = Vgetattr(vid, i, (VOIDP)attrbuf);

                if (status == FAIL) 
                  {
                      fprintf(stderr,">>>dympattr: failed in getting %d'th attr.\n",i);
                      ret_value = FAIL;
                      goto done;
                  }
            }

          /* format output */
          switch (i_type & 0xff)  
            {
            case DFNT_CHAR:
            case DFNT_UCHAR:
                vfmtfn = fmtchar;
                break;
            case DFNT_UINT8:
                vfmtfn = fmtuint8;
                break;
            case DFNT_INT8:
                vfmtfn = fmtint8;
                break;
            case DFNT_UINT16:
                vfmtfn = fmtuint16;
                break;
            case DFNT_INT16:
                vfmtfn = fmtint16;
                break;
            case DFNT_UINT32:
                vfmtfn = fmtuint32;
                break;
            case DFNT_INT32:
                vfmtfn = fmtint32;
                break;
            case DFNT_FLOAT32:
                vfmtfn = fmtfloat32;
                break;
            case DFNT_FLOAT64:
                vfmtfn = fmtfloat64;
                break;
            default:
                fprintf(stderr,">>>dumpattr: sorry, type [%d] not supported\n",
                        (int) i_type);
                ret_value = FAIL;
                goto done;
            }

          /* find offset */
          off = DFKNTsize(i_type | DFNT_NATIVE);

          /* which buffer are we using? */
          ptr = (alloc_flag) ? buf : attrbuf;

          putchar('\t');
          cn = 0;
          for (k = 0; k < i_count; k++)  
            {
                cn += vfmtfn((uint8 *)ptr, ft, fp);
                ptr += off;
                putchar(' ');
                cn++;
                if (cn > 55)  
                  {
                      putchar('\n');
                      putchar('\t');
                      cn = 0;
                  }
            }

          if (cn) 
              putchar('\n');

          /* free allocated space if any */
          if (alloc_flag) 
            {
                if ( buf != NULL)
                    HDfree(buf);
                alloc_flag = 0;
                buf = NULL;
            }
      }  /* for i */

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
          if (buf != NULL)
              HDfree(buf);
      }
    /* Normal cleanup */
#if defined (MAC) || defined (macintosh) || defined (SYMANTEC_C) || defined(__APPLE__)
   if(name != NULL)
   {
      HDfree(name);
      name = NULL;
    } 
   if(attrbuf != NULL)
   {
      HDfree(attrbuf);
      attrbuf = NULL;
    } 
#endif /* macintosh */    

    return ret_value;
}
