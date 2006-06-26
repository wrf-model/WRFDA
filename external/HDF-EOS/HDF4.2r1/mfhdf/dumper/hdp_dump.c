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
static char RcsId[] = "$Revision: 1.19 $";
#endif

/* $Id: hdp_dump.c,v 1.19 2000/12/19 20:57:12 koziol Exp $ */

#include <stdio.h>
#include "mfhdf.h"
#include "hdp.h"
#include <ctype.h>
#ifndef MIPSEL
#include <math.h>
#endif /* MIPSEL */

#define	CARRIAGE_RETURN	13
#define	LINE_FEED	10
#define	HORIZONTAL_TAB	9
/* 
 * printing functions copied from vshow.c and used by sdsdumpfull(). 
 *
 * Please pay attention to the data types used in the print/output routines.
 * Make sure the data type being dumped matches arguments in 'fwrite()' .etc.
 *
 */

intn 
fmtbyte(unsigned char *x, /* assumption: byte is the same as unsigned char */
        file_type_t    ft, 
        FILE          *ofp)
{
    unsigned char s;

    if(ft == DASCII)
      return (fprintf(ofp, "%02x ", (unsigned) *x));
    else
      { 
          s = (unsigned char) *x;
          return(fwrite(&s, sizeof(unsigned char),1,ofp));
      }
}

intn 
fmtint8(VOIDP       x,  /* assumption: int8 is same as signed char */
        file_type_t ft, 
        FILE       *ofp)
{
    int8 s;

    if(ft == DASCII)
        return (fprintf(ofp, "%d", (int) *((signed char *) x)));
    else
      {
          s = (int8) *((signed char *) x);
          return(fwrite(&s, sizeof(int8), 1, ofp));
      }
}

intn 
fmtuint8(VOIDP       x, /* assumption: uint8 is same as unsigned char */
         file_type_t ft, 
         FILE       *ofp)
{
    uint8 s;

    if(ft == DASCII)
        return (fprintf(ofp, "%u", (unsigned) *((unsigned char *) x)));
    else
      { 
          s = (uint8) *((unsigned char *) x);
          return(fwrite(&s, sizeof(uint8), 1, ofp));
      } 
}

intn 
fmtint16(VOIDP       x, 
         file_type_t ft, 
         FILE       *ofp)
{
    int16 s;

    HDmemcpy(&s, x, sizeof(int16));

    if(ft == DASCII)
        return (fprintf(ofp, "%d", (int) s));
    else
        return(fwrite(&s, sizeof(int16), 1, ofp));
}

intn 
fmtuint16(VOIDP       x, 
          file_type_t ft, 
          FILE       *ofp)
{
    uint16      s;

    HDmemcpy(&s, x, sizeof(uint16));

    if(ft == DASCII)
        return (fprintf(ofp, "%u", (unsigned) s));
    else
        return(fwrite(&s, sizeof(uint16), 1, ofp));
}

intn 
fmtchar(VOIDP       x, 
        file_type_t ft, 
        FILE       *ofp)
{
   if (isprint(*(unsigned char *) x))
   {
      putc(*((char *) x), ofp);
      return (1);
   }
   else
   {
      putc('\\', ofp);
      return (1 + fprintf(ofp, "%03o", *((uchar8 *) x)));
   }		
}

intn 
fmtuchar8(VOIDP       x, /* assumption: uchar8 is same as unsigned char */
          file_type_t ft, 
          FILE       *ofp)
{   
    uchar8 s;

    if(ft == DASCII) 
	/* replace %o with %d by Elena's suggestion: it doesn't make
	   sense to print in octal - BMR 06/23/00 */
        return (fprintf(ofp, "%d", *((uchar8 *) x)));
    else
      { 
          s = (uchar8) *((unsigned char *)x);
          return(fwrite(&s, sizeof(uchar8),1, ofp));
      }
}

intn 
fmtint(VOIDP       x, /* assumption: int is same as 'intn' */
       file_type_t ft, 
       FILE       *ofp)
{
    intn        i;

    HDmemcpy(&i, x, sizeof(intn));

    if(ft == DASCII) 
        return (fprintf(ofp, "%d", (int) i));
    else
        return(fwrite(&i, sizeof(intn), 1, ofp));
}

#define FLOAT32_EPSILON ((float32)1.0e-20)
intn 
fmtfloat32(VOIDP       x, 
           file_type_t ft, 
           FILE       *ofp)
{
    float32     fdata;

    HDmemcpy(&fdata, x, sizeof(float32));

    if(ft == DASCII)
      {
          if (fabs(fdata - FILL_FLOAT) <= FLOAT32_EPSILON)
              return (fprintf(ofp, "FloatInf"));
          else
              return (fprintf(ofp, "%f", fdata));
      }
    else
      {
          return(fwrite(&fdata, sizeof(float32), 1, ofp));
      }
}

intn 
fmtint32(VOIDP       x, 
         file_type_t ft, 
         FILE       *ofp)
{
    int32       l;

    HDmemcpy(&l, x, sizeof(int32));

    if(ft == DASCII)
        return (fprintf(ofp, "%ld", (long) l));
    else
        return(fwrite(&l, sizeof(int32), 1, ofp));
}

intn 
fmtuint32(VOIDP       x, 
          file_type_t ft, 
          FILE       *ofp)
{
    uint32      l;

    HDmemcpy(&l, x, sizeof(uint32));

    if(ft == DASCII)
        return (fprintf(ofp, "%lu", (unsigned long) l));
    else
        return(fwrite(&l, sizeof(uint32), 1, ofp));
}

intn 
fmtshort(VOIDP       x, 
         file_type_t ft, 
         FILE       *ofp)
{
    short s;

    HDmemcpy(&s, x, sizeof(short));

    if(ft == DASCII)
        return (fprintf(ofp, "%d", (int) s));
    else
        return(fwrite(&s, sizeof(short), 1, ofp));
}

#define FLOAT64_EPSILON ((float64)1.0e-20)
intn 
fmtfloat64(VOIDP       x, 
           file_type_t ft, 
           FILE       *ofp)
{
    float64     d;

    HDmemcpy(&d, x, sizeof(float64));

    if(ft == DASCII)
      {
          if (fabs(d - FILL_DOUBLE) <= FLOAT64_EPSILON)
              return (fprintf(ofp, "DoubleInf"));
          else
             return (fprintf(ofp, "%f", d));
      }
    else
      {
          return(fwrite(&d, sizeof(float64), 1, ofp));
      }
}

typedef intn (*fmtfunct_t) (VOIDP, file_type_t, FILE *);
fmtfunct_t select_func(
		int32 nt )
{
   switch (nt & 0xff )
   {
      case DFNT_CHAR:
          return( fmtchar );
          break;
      case DFNT_UCHAR:
          return( fmtuchar8 );
          break;
      case DFNT_UINT8:
          return( fmtuint8 );
          break;
      case DFNT_INT8:
          return( fmtint8 );
          break;
      case DFNT_UINT16:
          return( fmtuint16 );
          break;
      case DFNT_INT16:
          return( fmtint16 );
          break;
      case DFNT_UINT32:
          return( fmtuint32 );
          break;
      case DFNT_INT32:
          return( fmtint32 );
          break;
      case DFNT_FLOAT32:
          return( fmtfloat32 );
          break;
      case DFNT_FLOAT64:
          return( fmtfloat64 );
          break;
      default:
          fprintf(stderr, "HDP does not support type [%d].  Use signed character printing function.\n", (int) nt);
	  return( fmtchar );
   }		/* end switch */
}  /* select_func */

intn 
dumpfull(int32       nt, 
	 dump_info_t* dump_opts,
         int32       cnt,     /* number of items in 'databuf' ? */
         VOIDP       databuf, 
         FILE       *ofp,
	 intn	indent,		/* indentation on the first line */
	 intn	cont_indent )	/* indentation on the continuous lines */
{
   intn    i;
   VOIDP   bufptr = NULL;
   fmtfunct_t fmtfunct = NULL;
   int32   off;
   intn    cn;
   file_type_t ft = dump_opts->file_type;
   intn    ret_value = SUCCEED;

   /* check inputs */
   if( NULL == databuf )
      ERROR_GOTO_1( "in %s: Data buffer to be dumped is NULL", "dumpfull" );
   if( NULL == ofp )
      ERROR_GOTO_1( "in %s: Output file pointer is NULL", "dumpfull" );
    
   /* select the appropriate function to print data elements depending
      on the data number type */
   fmtfunct = select_func( nt );

   /* assign to variables used in loop below (?)*/
   bufptr = databuf;
   off = DFKNTsize(nt | DFNT_NATIVE); /* what is offset for data type */
   if (off == FAIL)
      ERROR_GOTO_2("in %s: Failed to find native size of type [%d]", 
			"dumpfull", (int)nt );

   cn = cont_indent; /* current column number, cont_indent because that's
			where the data actually starts */

   /* check if we're dumping data in ASCII or Binary mode. */
   if(ft == DASCII)
   {
      /* print spaces in front of data on the first line */
      for (i = 0; i < indent; i++)
	 putc(' ', ofp);

      if (nt != DFNT_CHAR)
      {
         for (i = 0; i < cnt && bufptr != NULL; i++)
         {
            cn += fmtfunct(bufptr, ft, ofp); /* dump item to file */
            bufptr = (char *) bufptr + off;
            putc(' ', ofp);
            cn++;

            /* temporary fix bad alignment algo in dumpfull by
               adding i < cnt-1 to remove extra line - BMR 4/10/99 */
	    if( !dump_opts->as_stream ) /* add \n after MAXPERLINE chars */
               if (cn > MAXPERLINE && i < cnt-1 )
               {
                  putc('\n', ofp);

		  /* print spaces in front of data on the continuous line */
                  for (cn = 0; cn < cont_indent; cn++)
                      putc(' ', ofp);
               }	/* end if */
         }	 /* end for every item in buffer */
      }		
      else /* DFNT_CHAR */
      {
         for (i = 0; i < cnt && bufptr != NULL; i++)
         {
            cn += fmtfunct(bufptr, ft, ofp); /* dump item to file */
            bufptr = (char *) bufptr + off;
	    if( !dump_opts->as_stream ) /* add \n after MAXPERLINE chars */
               if (cn > MAXPERLINE )
               {
                  putc('\n', ofp);

		  /* print spaces in front of data on the continuous line */
                  for (cn = 0; cn < cont_indent; cn++)
                      putc(' ', ofp);
               }		/* end if */
         }	/* end for every item in buffer */
      }		/* end else DFNT_CHAR */

      putc('\n', ofp); /* newline after a dataset or attribute */

   } /* end DASCII  */
   else /*  Binary   */
   {
      for (i = 0; i < cnt && bufptr != NULL; i++)
      {
         cn += fmtfunct(bufptr, ft, ofp); /* dump item to file */
         bufptr = (char *) bufptr + off; /* increment by offset? */
         /* cn++; I don't see any reason of this increment being here 9/4/00*/
      } /* end for all items in buffer */
   }

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
    /* Normal cleanup */

    return ret_value;
} /* dumpfull */

intn 
dumpclean(int32       nt, 
	 dump_info_t* dump_opts,
         int32       cnt,     /* number of items in 'databuf' ? */
         VOIDP       databuf, 
         FILE       *ofp)
{
   intn    i;
   VOIDP   bufptr = NULL;
   int32   off;
   intn    cn;		/* # of characters being printed on a line */
   intn small_attr = TRUE;    /* data buffer of the attribute is small */
   intn    is_null;	/* TRUE if current character is a null  */
   char* tempptr;	/* used in finding CR or LF in data buffer */
   intn    ret_value = SUCCEED;

   /* check inputs */
   if( NULL == databuf )
      ERROR_GOTO_1( "in %s: Data buffer to be dumped is NULL", "dumpclean" );
   if( NULL == ofp )
      ERROR_GOTO_1( "in %s: Output file pointer is NULL", "dumpclean" );
    
   /* assign to variables used in loop below (?)*/
   bufptr = databuf;
   off = DFKNTsize(nt | DFNT_NATIVE); /* what is offset for data type */
   if (off == FAIL)
      ERROR_GOTO_2("in %s: Failed to find native size of type [%d]", 
			"dumpclean", (int)nt );

   /* set char counter to the col. #, where the first attr value will be
      printed in the case it is printed on the same line with "Value =" */
   cn = ATTR_CONT_INDENT;  /* this is the default */

   is_null = FALSE; /* no null character is reached yet */

   /***********************************************************************
    * Requirement for printing attribute data (BMR - Oct 5, 2000):
    * if the attribute is large, print all data at the left most column; 
    * otherwise (small attribute), print the first line of the data 
    * next to the title, i.e. "Value = ", and indent the succeeding lines 
    * ATTR_CONT_INDENT spaces.
    * Large attribute: buffer size is >= MAXPERLINE and the buffer 
    * contains at least one \n (LF) or \r (CR).
    * Small attribute: buffer size is < MAXPERLINE or the buffer doesn't
    * contain any \n (LF) or \r (CR) among the data. 
    ***********************************************************************/

   /* Setting variables to prepare for the printing */

   /* check the size of the buffer first, if it's shorter than MAXPERLINE
      then set flag small_attr.  If the buffer size is larger, then 
      proceed to the next segment which determines whether the attribute
      is small or large using the space char. criteria. */

   if( cnt < MAXPERLINE )
      small_attr = TRUE;

   /* if the buffer contains at least one \n (LF) or \r (CR), reset
      flag small_attr to indicate the attribute is considred large. */
   else /* space char. criteria */
   {
      tempptr = strchr( (char *) bufptr, '\n'); /* find the first linefeed */
      if( tempptr != NULL) /* if an LF is found within the data buffer */
      {
         putc('\n', ofp); /* start first line of data on the next line */
         small_attr = FALSE; /* indicate data buffer contains CRs or LFs */
      }
      else    /* no LF, maybe CR is there */
      {
         tempptr = strchr( (char *) bufptr, '\r');
         if( tempptr != NULL) /* if a CR is found within the data buffer */
         {
            putc('\n', ofp); /* start first line of data on the next line */
            small_attr = FALSE; /* indicate data buffer contains CRs or LFs */
         }
      }
   }  /* space char. criteria */

   /* for each character in the buffer, print it accordingly */
   for (i = 0; i < cnt; i++)
   {
      /* if number of characters printed on the current line reaches
         the max defined and the data buffer doesn't contain any LF or
         CR, print a new line and indent appropriately.
	 Note: this statement is at the top here is to prevent the
	 extra line and indentation when the last line of the attribute
	 data just reached MAXPERLINE */
      if (cn >= MAXPERLINE && small_attr )
      {
         putc('\n', ofp);
         for (cn = 0; cn < ATTR_CONT_INDENT; cn++)
            putc(' ', ofp);
      }	/* end if */

      /* if the current character is printable */
      if (isprint(*(unsigned char *) bufptr))
      {
	 /* if there has been null characters before this non-null char,
	    print "..." */
	 if( is_null )
	 {
            cn = cn + fprintf( ofp, " ... " );
	    is_null = FALSE; /* reset flag */
	 }

	 /* then print the current non-null character */
         putc(*((char *) bufptr), ofp);
         cn++;  /* increment character count */
      }

      /* when a \0 is reached, do not print it, set flag for its existence,
	 so when a non-null char is reached, "..." can be printed */
      else if( *(unsigned char *) bufptr == '\0')
         is_null = TRUE;

      /* when a space character, such as LF, CR, or tab, is reached, print
         it and increment the character counter accordingly */
      else if( isspace(*(unsigned char *) bufptr))
      {
         /* when either LF or CR exists in the data buffer, character
            counter, cn, is no longer needed since we don't need to keep
            track of the number of chars being printed on a line anymore.
            Yet, for logical purpose, reset it here just as a new line 
	    of data starts */
         if( *(unsigned char *) bufptr == CARRIAGE_RETURN 
            || *(unsigned char *) bufptr == LINE_FEED )
	 {
            putc('\n', ofp); /* print \n for both CR and LF, otherwise, CR=^M*/
            cn = 0;  /* indicating that next data element will be printed
			   at column 1 */
	 }
         else if( *(unsigned char *) bufptr == HORIZONTAL_TAB )
         {
            putc(*((char *) bufptr), ofp);
            cn = cn + 8;   /* compensate for the tab, at most 8 chars */
         }

         /* keep this else here to take care of other isspace cases, fill in
	    more cases as need; if all cases are taken care of, remove else */
         else
         {
            putc('\\', ofp);
            cn = cn + fprintf(ofp, "%03o", *((uchar8 *) bufptr));
         }		
      }
      else
      {
/* this should be printed as binary intstead of \digits */
         putc('\\', ofp);
         cn = cn + fprintf(ofp, "%03o", *((uchar8 *) bufptr));
      }		

      /* advance the buffer pointer */
      bufptr = (char *) bufptr + off;

       /* Move here to avoid internal compiler error on Cray J90 -QAK */
       if(bufptr==NULL)
           break;
   }	/* end for every item in buffer */

   putc('\n', ofp); /* newline */

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
    /* Normal cleanup */

    return ret_value;
} /* dumpclean */

