/**********************************************************************
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
static char RcsId[] = "@(#)Revision";
#endif

/* $Id: hdp_sds.c,v 1.42 2003/12/10 21:14:01 epourmal Exp $ */

#include <stdio.h>
#include "mfhdf.h"
#include "hdp.h"
#ifndef MIPSEL
#include <math.h>
#endif /* MIPSEL */

void 
dumpsds_usage(intn argc, 
              char *argv[])
{
    printf("Usage:\n");
    printf("%s dumpsds [-a|-i <indices>|-r <refs>|-n <names>] [-cdhvs] [-o <filename>] [-bx] <filelist>\n", argv[0]);
    printf("\t-a\tDump all SDSs in the file (default)\n");
    printf("\t-i <indices>\tDump the SDSs at positions listed in <indices>\n");
    printf("\t-r <refs>\tDump the SDSs with reference number listed in <refs>\n");
    printf("\t-n <names>\tDump the SDSs with name listed in <names>\n");
    printf("\t-d\tDump data only, no tag/ref, formatted to input to hp2hdf\n");
    printf("\t-h\tDump header only, no annotation for elements nor data\n");
    printf("\t-v\tDump everything including all annotations (default)\n");
    printf("\t-c\tPrint space characters as they are, not \\digit\n");
    printf("\t-g\tDo not print data of file (global) attributes\n");
    printf("\t-l\tDo not print data of local attributes\n");
    printf("\t-s\tDo not add carriage return to a long line - dump it as a stream\n");
    printf("\t-o <filename>\tOutput to file <filename>\n");
    printf("\t-b\tBinary format of output\n");
    printf("\t-x\tAscii text format of output (default)\n");
    printf("\t<filelist>\tList of hdf file names, separated by spaces\n");
}	/* end list_usage() */

intn 
parse_dumpsds_opts(dump_info_t *dumpsds_opts, 
                   intn *curr_arg, 
                   intn argc,
                   char *argv[])
{
   intn ret_value = SUCCEED;

   /* traverse the command and process each option */
#if defined(WIN386) || defined(DOS386)
   while ((*curr_arg < argc) && ((argv[*curr_arg][0] == '-') ||
                                 (argv[*curr_arg][0] == '/')))
#else
   while ((*curr_arg < argc) && (argv[*curr_arg][0] == '-'))
#endif /* for the use of / as option on PC */
   {
      switch (argv[*curr_arg][1])
      {
         case 'a':	/* dump all, default */
            dumpsds_opts->filter = DALL;

            /* indicate that no specific SDS requested, will dump all */
            dumpsds_opts->num_chosen = NO_SPECIFIC;
            (*curr_arg)++;
            break;

         case 'i':	/* dump by index */
            dumpsds_opts->filter |= DINDEX;  /* set bit DINDEX */
            (*curr_arg)++;

            /* parse and store the given indices in structure by_index */
            parse_number_opts( argv, curr_arg, &dumpsds_opts->by_index);
            (*curr_arg)++;
            break;

         case 'r':	/* dump by reference */
            dumpsds_opts->filter |= DREFNUM;  /* set bit DREFNUM */
            (*curr_arg)++;

            /* parse and store the given ref numbers in structure by_ref */
            parse_number_opts( argv, curr_arg, &dumpsds_opts->by_ref);
           (*curr_arg)++;
            break;

         case 'n':	/* dump by names */
            dumpsds_opts->filter |= DNAME;   /* set bit DNAME */
            (*curr_arg)++;

            /* parse and store the given names in structure by_name */
            parse_string_opts( argv, curr_arg, &dumpsds_opts->by_name);
            (*curr_arg)++;
            break;

         case 'd':	/* dump data only */
                dumpsds_opts->contents = DDATA;
                (*curr_arg)++;
                break;

         case 'h':	/* no annotations nor data */
                dumpsds_opts->contents = DHEADER;
                (*curr_arg)++;
                break;

         case 'v':	/* dump all info */
                dumpsds_opts->contents = DVERBOSE;
                (*curr_arg)++;
                break;

         case 's':	/* do not add carriage returns to output data lines */
                dumpsds_opts->as_stream = TRUE;
                (*curr_arg)++;
                break;

         case 'c':	/* print space characters as they are, not \digit */
                dumpsds_opts->clean_output = TRUE;
                (*curr_arg)++;
                break;

         case 'g':	/* suppress file (global) attr data, print its header */
                dumpsds_opts->no_gattr_data = TRUE;
                (*curr_arg)++;
                break;

         case 'l':	/* suppress local attr data, only print its header */
                dumpsds_opts->no_lattr_data = TRUE;
                (*curr_arg)++;
                break;

         case 'o':   /* specify output file */
                dumpsds_opts->dump_to_file = TRUE;

                /* Get file name */
                HDstrcpy(dumpsds_opts->file_name, argv[++(*curr_arg)]);

                (*curr_arg)++;
                break;

         case 'b':   /* dump data in binary */
                dumpsds_opts->file_type = DBINARY;
                (*curr_arg)++;
                break;

         case 'x':   /* dump data in ascii, also default */
                dumpsds_opts->file_type = DASCII;
                (*curr_arg)++;
                break;

         default:    /* invalid dumpsds option */
                printf("HDP ERROR>>> Invalid dumpsds option %s\n", argv[*curr_arg]);
                HGOTO_DONE( FAIL );
            }	/* end switch */
      }		/* end while */

   /* add the number of datasets requested by index, by ref#, and by name
      to have a total number of requested datasets */
   dumpsds_opts->num_chosen = dumpsds_opts->by_index.num_items +
                             dumpsds_opts->by_ref.num_items +
                             dumpsds_opts->by_name.num_items;

done:
   if (ret_value == FAIL)
   { /* Failure cleanup */
      /* free the lists for given indices, ref#s, and names if
         they had been allocated */
      free_num_list(dumpsds_opts->by_index.num_list );
      free_num_list(dumpsds_opts->by_ref.num_list );
      free_str_list(dumpsds_opts->by_name.str_list, dumpsds_opts->by_name.num_items);
   }
   /* Normal cleanup */
   return (ret_value);
}	/* end parse_dumpsds_opts */

/* sdsdumpfull prints a single SDS */
int32 
sdsdumpfull( int32        sds_id, 
             dump_info_t *dumpsds_opts,
             int32        rank, 
             int32        dimsizes[], 
             int32        nt, 
             FILE        *fp)
{
	/* "rank" is the number of dimensions and 
	   "dimsizes[i]" is size of dimension "i". */
   int32    j, i;
   VOIDP    buf = NULL;		/* holds one row of data */
   int32    numtype;
   int32    eltsz;
   int32    read_nelts;		/* number of elements in one row */
   int32    done;			/* number of rows we have done */
   int32   *left = NULL;
   int32   *start = NULL;
   int32   *edge = NULL;
   intn     emptySDS = TRUE;
   file_type_t ft;
   intn     status;
   int32    status32;
   int32    ret_value = SUCCEED;

   /* temp. names for ease of use */
   ft = dumpsds_opts->file_type;

   /* Compute the number of the bytes for each value. */
   numtype = nt & DFNT_MASK;
   eltsz = DFKNTsize(numtype | DFNT_NATIVE);

   read_nelts = dimsizes[rank - 1];

   /* make sure we are not allocating 0 elements */
   CHECK_POS( read_nelts, "read_nelts", "sdsdumpfull" );
   CHECK_POS( eltsz, "eltsz", "sdsdumpfull" );
   CHECK_POS( rank, "rank", "sdsdumpfull" );

   buf = (VOIDP) HDmalloc(read_nelts * eltsz);
   CHECK_ALLOC( buf, "buf", "sdsdumpfull" );

   left = (int32 *) HDmalloc(rank * sizeof(int32));
   CHECK_ALLOC( left, "left", "sdsdumpfull" );

   start = (int32 *) HDmalloc(rank * sizeof(int32));
   CHECK_ALLOC( start, "start", "sdsdumpfull" );

   edge = (int32 *) HDmalloc(rank * sizeof(int32));
   CHECK_ALLOC( edge, "edge", "sdsdumpfull" );

/* BMR - how come this doesn't have stride as for GR? */
    for (i = 0; i < rank; i++)
   {
          start[i] = 0;		/* Starting location to read the data. */
          left[i] = dimsizes[i];
          edge[i] = 1;	/* Number of values to read in each dimension. */
   }

   /* so that the last edge has many elements as the last dimension??? */
   edge[rank - 1] = dimsizes[rank - 1];

   /* check if the SDS has data before proceeding */
   status32 = SDcheckempty( sds_id, &emptySDS );
   if( status32 == FAIL )
      ERROR_GOTO_2( "in %s: SDcheckempty failed for sds_id(%d)",
			"sdsdumpfull", (int) sds_id );
   if( emptySDS )
   {
      if( ft == DASCII ) /* what about binary??? - BMR */
         fprintf( fp, "                No data written.\n" );
      HGOTO_DONE( SUCCEED );  /* because the dump for this SDS is */
      			/* successful although it's empty -> next SDS */
   }
   if (rank == 1)
   { /* If there is only one dimension, then dump the data
               and the job is done. */
      if (FAIL == SDreaddata(sds_id, start, NULL, edge, buf))
         ERROR_GOTO_2( "in %s: SDreaddata failed for sds_id(%d)",
			"sdsdumpfull", (int)sds_id );

      /* if printing data only, print with no indentation */
      if( dumpsds_opts->contents == DDATA )
         status = dumpfull(numtype, dumpsds_opts, read_nelts, buf, fp,
				0, 0 );
      else
         status = dumpfull(numtype, dumpsds_opts, read_nelts, buf, fp,
				DATA_INDENT, DATA_CONT_INDENT );
      if( FAIL == status )
         ERROR_GOTO_2( "in %s: dumpfull failed for sds_id(%d)",
                        "sdsdumpfull", (int)sds_id );
   }
   else if (rank > 1)
   {
      done = 0;

      /* In each iteration, a row in dumped and "left[]" is modified 
	accordingly(?) */
      while (!done)
      {
         status = SDreaddata(sds_id, start, NULL, edge, buf);
         if( FAIL == status )
            ERROR_GOTO_2( "in %s: SDreaddata failed for sds_id(%d)",
                        "sdsdumpfull", (int)sds_id );

         /* if printing data only, print with no indentation */
         if( dumpsds_opts->contents == DDATA )
            status = dumpfull(numtype, dumpsds_opts, read_nelts, buf, fp,
				0, 0 );
         else
            status = dumpfull(numtype, dumpsds_opts, read_nelts, buf, fp,
				DATA_INDENT, DATA_CONT_INDENT );

         if( FAIL == status )
            ERROR_GOTO_2( "in %s: dumpfull failed for sds_id(%d)",
                        "sdsdumpfull", (int)sds_id );

         /* Modify the values for "start[]" and "left[]" that are to be used
            for dumping the next row. */

         /* The following index variable "j" starts from "rank-2" because:
               (1) the range is from 0 to rank-1
               (2) each element in dimension rank-1 is just an element in a row
            which is read in each time, and so we don't have to compute
            the "start" of it. */

         for (j = rank - 2; j >= 0; j--)
         {		/* Examine each dimension. */
            if (--left[j] > 0)
	    {  /* Proceed in the same dimension; as long as there are
	       elements in this dimension, this loop breaks here after the
	       last element in the current dimension has been subtracted,
	       we substract one for the next lower dimension and reset
	       "left[j]" to be the size of dimension j. */
                 start[j]++;
                 break;
            }
            else
            {  /* Nothing left in the current dimension.  So, subtract one
               from the (j-1)th dimension and reset the value of "left[j]". */

               left[j] = dimsizes[j];
               start[j] = 0;
               if (j == 0)
                  done = 1;

   	       /* someone added an extra line b/w two dims of data for nice format;
		  this causes 1 extra line at the end of the output but I still
		  don't understand the logic here so I left it alone; just 
		  removed the spaces attempting to line up the data. BMR 7/13/00 */
               /*if( ft==DASCII && !dumpsds_opts->as_stream )*/
               if( ft==DASCII )
                  if (j == rank - 2)
                     fprintf(fp, "\n");
            }
         }  /* for j */
      }	 /* while   */
   }  /* else */

   /* add an extra line between two datasets for pretty format 
      this also causes 1 extra line at the end of the output! */
    /*if (ft == DASCII && !dumpsds_opts->as_stream )*/
    if (ft == DASCII )
        fprintf(fp, "\n");
done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
    /* Normal cleanup */
    if (edge != NULL)
        HDfree((VOIDP) edge);
    if (start != NULL)
        HDfree((VOIDP) start);
    if (left != NULL)
        HDfree((VOIDP) left);
    if (buf != NULL)
        HDfree((VOIDP) buf);    

    return ret_value;
}	/* sdsdumpfull */

/* compose the list of indices of the requested SDSs although some
   SDSs are requested by ref# or name.
   The routine returns:
        - the number of SDSs to be processed, or
        - NO_SPECIFIC if all SDSs are to be processed, or
        - 0 if none.
   If any failure occurs, the parameter index_error will be set to TRUE
*/
intn get_SDSindex_list( 
        int32 sd_id, 
        dump_info_t *dumpsds_opts,
        int32 **sds_chosen,/* array of indices of SDSs to be processed */
        intn *index_error )
{
   intn     i;
   int32    index,          /* index of an SDS */
            sds_count = 0,  /* number of SDSs to be processed */
            num_sds_chosen = dumpsds_opts->num_chosen;
   filter_t filter = dumpsds_opts->filter; /* temporary name */
   intn     ret_value = 0;  /* assume that no SDS will be processed */

   /* if no specific datasets are requested, return the SDS count as 
      NO_SPECIFIC (-1) to indicate that all datasets are to be dumped */
   if( filter == DALL )
      HGOTO_DONE( NO_SPECIFIC );

   /* if specific datasets were requested, allocate space for the array
      of indices */
   if (num_sds_chosen > 0)
      alloc_index_list( sds_chosen, num_sds_chosen );

   /* else, no chosen SDSs but filter is not DALL, it shouldn't be this
      combination, return SDS count as NO_SPECIFIC to dump all */
   else
      HGOTO_DONE( NO_SPECIFIC );

   /* if there are some SDSs requested by index, store the indices in
      the array sds_chosen */
   if( filter & DINDEX )
         /* Note: Don't replace this with HDmemcpy unless you change 
                  the sizes of the objects correctly -QAK */
      for (i = 0; i < dumpsds_opts->by_index.num_items; i++)
      {
         (*sds_chosen)[sds_count] = dumpsds_opts->by_index.num_list[i];
         sds_count++;
      }

   /* if there are some SDSs requested by ref#, convert the ref#s to indices
      and store the indices in the array provided by the caller, sds_chosen */
   if( filter & DREFNUM )
      for (i = 0; i < dumpsds_opts->by_ref.num_items; i++)
      {
         index = SDreftoindex(sd_id, dumpsds_opts->by_ref.num_list[i]);
         if (index == FAIL)
         {
            printf( "SDS with reference number %d: not found\n",
                               (int)dumpsds_opts->by_ref.num_list[i]);
            *index_error = TRUE; /* error */
         }
         else
         {
            (*sds_chosen)[sds_count] = index;
            sds_count++;
         }
      }

   /* if there are some SDSs requested by name, convert the names to indices 
      and store the indices in the array provided by the caller, sds_chosen */
   if( filter & DNAME )
      for (i = 0; i < dumpsds_opts->by_name.num_items; i++)
      {
         index = SDnametoindex(sd_id, dumpsds_opts->by_name.str_list[i]);
         if (index == FAIL)
         {
            printf( "SDS with name %s: not found\n",
                              dumpsds_opts->by_name.str_list[i]);
            *index_error = TRUE; /* error */
         }
         else
         {
            (*sds_chosen)[sds_count] = index;
            sds_count++;
         }
      }

   ret_value = sds_count;
done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
    /* Normal cleanup */
    return ret_value;
} /* end of get_SDSindex_list */

/* Displays all SD file attributes */
intn
print_SDattrs( int32 sd_id,
               FILE *fp,
               int32 n_file_attrs,
	       dump_info_t* dumpsds_opts )
{
   int32 attr_index,
         attr_count,
         attr_nt,
         attr_buf_size;
   char  attr_name[MAXNAMELEN],
        *attr_nt_desc = NULL;
   VOIDP attr_buf=NULL;
   intn  printed = FALSE; /* whether file attr title has been printed */
   intn  status,          /* status from a called routine */
         ret_value = SUCCEED;

   /* for each file attribute, print its info and values */
   for (attr_index = 0; attr_index < n_file_attrs; attr_index++)
   {
      /* get the current attr's name, number type, and number of values */
      status = SDattrinfo(sd_id, attr_index, attr_name, &attr_nt, &attr_count);
      if( status == FAIL )
         ERROR_CONT_2( "in %s: SDattrinfo failed for %d'th attribute", 
			"print_SDattrs", (int)attr_index );
     
      /* get number type description of the attribute */
      attr_nt_desc = HDgetNTdesc(attr_nt);
      if (attr_nt_desc == NULL)
         ERROR_CONT_2( "in %s: HDgetNTdesc failed for %d'th attribute", 
			"print_SDattrs", (int)attr_index );

      /* print a title line for file attributes if it's not printed
         yet and set flag so it won't be printed again */
      if( !printed )
      {
         fprintf( fp, "\nFile attributes:\n" );
         printed = TRUE;
      }

      /* display the attribute's information */
      fprintf(fp,"\t Attr%i: Name = %s\n", (int) attr_index, attr_name);
      fprintf(fp,"\t\t Type = %s \n\t\t Count= %i\n", attr_nt_desc, (int) attr_count);
      resetBuff(( VOIDP *) &attr_nt_desc );

      /* display the attribute's values unless user chose to suppress them */
      if( dumpsds_opts->no_gattr_data == FALSE )
      {
         /* to be sure that attr_buf is free before reuse since sometimes we
            have to break the current loop and continue to the next item */
         resetBuff( &attr_buf );

	 /* calculate the buffer size of the attribute using the number of
	    values in the attribute and its value size */
	 attr_buf_size = DFKNTsize(attr_nt) * attr_count;

	 /* make sure we are not allocating 0 elements */
	 CHECK_POS( attr_buf_size, "attr_buf_size", "print_SDattrs" );

	 /* allocate space for the attribute's values */
	 attr_buf = (VOIDP) HDmalloc(attr_buf_size);

	 /* if allocation fails, handle the failure */
	 CHECK_ALLOC( attr_buf, "attr_buf", "print_SDattrs" );

	 /* read the values of the attribute into the buffer attr_buf */
	 status = SDreadattr(sd_id, attr_index, attr_buf);
	 if( status == FAIL )
	    ERROR_CONT_2( "in %s: SDreadattr failed for %d'th attribute", 
			"print_SDattrs", (int)attr_index );

	 fprintf( fp,"\t\t Value = ");

	 /* if the user wishes to have clean output, i.e. option -c is 
	    selected - Note that this option is only applicable to DFNT_CHAR 
	    type, the option will be ignored for other types */
	 if( dumpsds_opts->clean_output && attr_nt == DFNT_CHAR )
	 {
	    status = dumpclean(attr_nt, dumpsds_opts, attr_count, attr_buf, fp);
	    if( status == FAIL )
		ERROR_CONT_2( "in %s: dumpclean failed for %d'th attribute", 
			"print_SDattrs", (int)attr_index );
	 }
	 else  /* show tab, lf, null char... in octal as \011, \012, \000... */
	 {
	    status = dumpfull(attr_nt, dumpsds_opts, attr_count, attr_buf, fp,
				ATTR_INDENT, ATTR_CONT_INDENT );
	    if( status == FAIL )
		ERROR_CONT_2( "in %s: dumpfull failed for %d'th attribute", 
			"print_SDattrs", (int)attr_index );
	 }
      }  /* end of if no file attributes */
   }  /* for each file attribute */

   return( ret_value );
}   /* end of print_SDattrs */

intn
print_SDSattrs( int32 sds_id,
                int32 nattrs,
                FILE *fp,
	        dump_info_t *dumpsds_opts)
{
   int32 attr_index,
         attr_count,
         attr_nt,
         attr_buf_size;
   char  attr_name[MAXNAMELEN],
        *attr_nt_desc = NULL;
   VOIDP attr_buf=NULL;
   intn  status,   /* status returned from a called routine */
         ret_value = SUCCEED; /* returned value of print_SDSattrs */

   /* for each attribute, display its info and data */
   for (attr_index = 0; attr_index < nattrs; attr_index++)
   {
      /* get the current attr's name, number type, and number of values */
      status = SDattrinfo(sds_id, attr_index, attr_name, &attr_nt, &attr_count);
      if (status == FAIL)
         ERROR_CONT_2( "in %s: SDattrinfo failed for %d'th attribute", 
			"print_SDSattrs", (int)attr_index );

      /* calculate the buffer size of the attribute using the number of
         values in the attribute and its value size */
      attr_buf_size = DFKNTsize(attr_nt|DFNT_NATIVE) * attr_count;

      /* make sure we are not allocating 0 elements */
      CHECK_POS( attr_buf_size, "attr_buf_size", "print_SDSattrs" );

      /* get number type description of the attribute */
      attr_nt_desc = HDgetNTdesc(attr_nt);
      if (attr_nt_desc == NULL)
         ERROR_CONT_2( "in %s: HDgetNTdesc failed for %d'th attribute", 
			"print_SDSattrs", (int)attr_index );

      /* display the attribute's information */
      fprintf(fp, "\t Attr%d: Name = %s\n", (int) attr_index, attr_name);
      fprintf(fp, "\t\t Type = %s \n\t\t Count= %d\n", 
			attr_nt_desc, (int) attr_count);

      /* free buffer and reset it to NULL */
      resetBuff((VOIDP *) &attr_nt_desc );

      /* display the attribute's values unless user chose to suppress them */
      if( dumpsds_opts->no_lattr_data == FALSE )
      {
         /* to be sure that attr_buf is free before reuse since sometimes we
            have to break the current loop and continue to the next item */
         resetBuff( &attr_buf );

	 /* allocate space for attribute's values */
	 attr_buf = (VOIDP) HDmalloc(attr_buf_size);
	 CHECK_ALLOC( attr_buf, "attr_buf", "print_SDSattrs" );

	 /* read the values of the attribute into buffer attr_buf */
	 status = SDreadattr(sds_id, attr_index, attr_buf);
	 if (status == FAIL)
            ERROR_CONT_2( "in %s: SDreadattr failed for %d'th attribute", 
			"print_SDSattrs", (int)attr_index );

	 fprintf(fp, "\t\t Value = ");

	 /* if the user wishes to have clean output, i.e. option -c is 
	    selected - Note that this option is only applicable to DFNT_CHAR 
	    type, the option will be ignored for other types */
	 if( dumpsds_opts->clean_output && attr_nt == DFNT_CHAR )
	 {
            status = dumpclean(attr_nt, dumpsds_opts, attr_count, attr_buf, fp);
            if( status == FAIL )
                ERROR_CONT_2( "in %s: dumpclean failed for %d'th attribute", 
			"print_SDSattrs", (int)attr_index );
	 }
	 else  /* show tab, lf, null char... in octal as \011, \012, \000... */
	 {
            status = dumpfull(attr_nt, dumpsds_opts, attr_count, attr_buf, fp,
				ATTR_INDENT, ATTR_CONT_INDENT );
            if( status == FAIL )
                ERROR_CONT_2( "in %s: dumpfull failed for %d'th attribute", 
			"print_SDSattrs", (int)attr_index );
	 }
      }  /* end of if no local attributes */
   } /* for each attribute */

   return( ret_value );
} /* end of print_SDSattrs */

void
resetSDS(
	int32 *sds_id,
	int32  sds_index,
	char  *curr_file_name )
{
   if( *sds_id != FAIL )
   {
      if( FAIL == SDendaccess( *sds_id ))
         fprintf(stderr,"SDendaccess failed for %d'th SDS in file %s\n",
                    (int)sds_index, curr_file_name );
      *sds_id = FAIL;
   }
}  /* end of resetSDS */

/* printSDS_ASCII (yea, not SD_ASCII) prints all of the requested SDSs in
   the file
*/
intn printSD_ASCII( 
	int32 sd_id,
	dump_info_t *dumpsds_opts,
	int32 ndsets,         /* # of datasets in the file */
	int32 *sds_chosen,    /* list of indices of SDSs */
	int32 num_sds_chosen, /* # of items in sds_chosen */
	FILE *fp )
{
   int32 sds_id = FAIL, /* SDS id, always reset to FAIL when not used */
         sds_ref,       /* ref# of an SDS */
         sds_count,     /* count of SDSs being printed */
         sds_index,     /* index of SDSs in the file */
         dim_id = FAIL, /* id of an SDS dimension */
         rank,          /* number of dimensions of an SDS */
         nt,            /* number type of an SDS */
         nattrs,        /* # of attributes assigned to an SDS */
         dimsizes[MAXRANK],     /* SDS dimensions */
         dimNT[MAXRANK],        /* number type of dimension */
         dimnattr[MAXRANK];     /* # of attributes of a dim */
   char  dim_nm[MAXNAMELEN],    /* dimension name */
         name[MAXNAMELEN],      /* SDS name */
         *nt_desc = NULL,       /* SDS's or dim's num type description */
         *attr_nt_desc = NULL,  /* attr's nt description */
         curr_file_name[MAXFNLEN]; /* curr hdf file name */
   intn  isdimvar,      /* TRUE if curr SDS is used for a dim */
         j,
         dumpall = FALSE,    /* TRUE if all SDSs are to be dumped */
         status,             /* status returned from a routine */
         ret_value = SUCCEED;/* returned value of printSD_ASCII */

   /* temp. name for curr input file name for ease of use */
/* curr_file_name can be removed from this routine after changing resetSDS API */
   HDstrcpy( curr_file_name, dumpsds_opts->ifile_name );

   /* when there are no SDS specified, dumper dumps all datasets */
   if (num_sds_chosen == (NO_SPECIFIC))  /* NO_SPECIFIC = -1 */
      dumpall = TRUE;
   else		/* otherwise, sort index list */
      sort(sds_chosen, num_sds_chosen);

   /* for each valid index, if the user request to dump all SDSs or if
      there are more requested SDSs to be processed, process the curr SDS */
   sds_count = 0;   /* no SDS has been processed yet */
   for (sds_index = 0; sds_index < ndsets  /* validate index */
        && (dumpall                /* either all datasets are dumped or */
        || sds_count < num_sds_chosen); /* or more requested datasets */
        sds_index++)
   {
      /* if the user neither requests dump all nor the current dataset */
      if ((!dumpall) && (sds_index != sds_chosen[sds_count]))
         continue; /* skip */

      sds_count++;   /* count the # of datasets being processed */

      /* Reset variables. */
      HDmemset(dimsizes, 0, sizeof(int32) * MAXRANK);
      HDmemset(dimNT, 0, sizeof(int32) * MAXRANK);
      HDmemset(dimnattr, 0, sizeof(int32) * MAXRANK);

      /* get access to the current dataset */
      sds_id = SDselect(sd_id, sds_index);
      if (sds_id == FAIL)
         ERROR_CONT_3( "in %s: %s failed for %d'th SDS",
                      "printSD_ASCII", "SDselect", (int)sds_index );

      /* get dataset's information */
      status = SDgetinfo(sds_id, name, &rank, dimsizes, &nt, &nattrs);
      if( status == FAIL )
      {
         resetSDS( &sds_id, sds_index, curr_file_name );
         ERROR_CONT_3( "in %s: %s failed for %d'th SDS", 
                       "printSD_ASCII", "SDgetinfo", (int)sds_index );
      }

      /* BMR: it seems like this whole block of code is to get number
         type of dim0, all the other info will be discarded */
      isdimvar = (SDiscoordvar(sds_id)) ? TRUE : FALSE;
      if (isdimvar) 
      { /* use dim0 nt instead of dimvar nt, because when no dim values 
           dimvar nt was set to float32 by default */
         int32 size, num_attrs;

         /* get dimension id for accessing */
         dim_id = SDgetdimid(sds_id, 0);
         if( dim_id == FAIL )
         {
            resetSDS( &sds_id, sds_index, curr_file_name );
            ERROR_CONT_3( "in %s: %s failed for %d'th SDS", 
                       "printSD_ASCII", "SDgetdimid", (int)sds_index );
         }

         /* get information of current dimension */
         if( SDdiminfo(dim_id, NULL, &size, &nt, &num_attrs) == FAIL )
         {
            resetSDS( &sds_id, sds_index, curr_file_name );
            ERROR_CONT_3( "in %s: %s failed for %d'th SDS", 
                       "printSD_ASCII", "SDdiminfo", (int)sds_index );
         }
      }                                                       
      /* print the current SDS's as specified by user's options */
      switch (dumpsds_opts->contents)
      {
         case DVERBOSE:
         case DHEADER:
            nt_desc = HDgetNTdesc(nt);
            if (nt_desc == NULL)
            {
               ERROR_BREAK_3( "in %s: %s failed for %d'th SDS", 
			"printSD_ASCII", "HDgetNTdesc", (int)sds_index, FAIL );
               /* did this one fail on allocation and need exit(1)? */
            }

            /* Note: a variable can be used to hold an SDS or a dimension. */
            /* display dimension info if the variable is used to contain a
               dimension */
            if (isdimvar)
            {
               fprintf(fp, "\nDimension Variable Name = %s\n\t ", name);
               fprintf(fp, "Index = %d\n\t Scale Type= %s\n", 
						(int)sds_index, nt_desc);
            }
            /* display the SDS info, otherwise */
            else
            {
               fprintf(fp, "\nVariable Name = %s\n\t Index = ", name);
               fprintf(fp, "%d\n\t Type= %s\n", (int)sds_index, nt_desc);
            }

            resetBuff(( VOIDP *) &nt_desc );  /* done with nt_desc */

            /* get SDS's ref# from its id */
            if ((sds_ref = SDidtoref(sds_id)) == FAIL)
               ERROR_BREAK_3( "in %s: %s failed for %d'th SDS", 
			"printSD_ASCII", "SDidtoref", (int)sds_index, FAIL );

            fprintf(fp, "\t Ref. = %d\n", (int) sds_ref);
            fprintf(fp, "\t Rank = %d\n\t Number of attributes = %d\n",
                                        (int) rank, (int) nattrs);

            /* print each dimension of the current SDS */
            for (j = 0; j < rank; j++)
            {
               int32 size;    /* size of the current dimension */

               /* get current dimension id for access */
               if (FAIL == (dim_id = SDgetdimid(sds_id, j)))
                  ERROR_BREAK_3( "in %s: %s failed for %d'th SDS", 
			"printSD_ASCII", "SDgetdimid", (int)sds_index, FAIL );

               /* get information of current dimension */
               ret_value = SDdiminfo(dim_id,dim_nm,&size,&(dimNT[j]),&(dimnattr[j]));
               if (FAIL == ret_value )
                  ERROR_BREAK_4( "in %s: %s failed for %d'th dimension of %d'th SDS", 
			"printSD_ASCII", "SDdiminfo", j, (int)sds_index, FAIL );

               fprintf(fp, "\t Dim%d: Name=%s\n", (int) j, dim_nm);
               if (size == 0)
               {
                  fprintf(fp, "\t\t Size = UNLIMITED ");
                  fprintf(fp, "(currently %d)\n", (int) dimsizes[j]);
               }
               else
                  fprintf(fp, "\t\t Size = %d\n", (int) dimsizes[j]);

               /* don't print type and # of attrs for dim var */
               if (isdimvar == 0)
               {  
                  attr_nt_desc = HDgetNTdesc(dimNT[j]);
                  if (attr_nt_desc == NULL)
                     ERROR_BREAK_4( "in %s: %s failed for %d'th dimension of %d'th SDS", 
		     "printSD_ASCII", "HDgetNTdesc", j, (int)sds_index, FAIL);

                  fprintf(fp, "\t\t Scale Type = %s\n", attr_nt_desc);
                  fprintf(fp, "\t\t Number of attributes = %d\n", (int) dimnattr[j]);
                  resetBuff(( VOIDP *) &attr_nt_desc);
               }
            } /* end each for dimension */

            /* print dataset's attributes */
            status = print_SDSattrs(sds_id, nattrs, fp, dumpsds_opts);
            if( status == FAIL )
               ERROR_BREAK_3( "in %s: %s failed for %d'th SDS",
		"printSD_ASCII", "print_SDSattrs", (int)sds_index, FAIL );

            /* header only, don't go into case DDATA */
            if (dumpsds_opts->contents == DHEADER)
               break;

            /* case DDATA doesn't need this */
            fprintf(fp, "\t Data : \n");

         case DDATA:
            if (rank > 0 && dimsizes[0] != 0)
            {
               if (!isdimvar || nt != 0)
               { /* no dump if dimvar w/o scale values */
                  status = sdsdumpfull( sds_id, dumpsds_opts, rank, dimsizes, nt, fp);
                  if( FAIL == status )
                     ERROR_BREAK_3( "in %s: %s failed for %d'th SDS", 
			"printSD_ASCII", "sdsdumpfull", (int)sds_index, FAIL );
               }
            }
            break;
         default:
            printf("Output format must be either -d, -h, or -v only.\n");
      }	 /* switch  */

      resetSDS( &sds_id, sds_index, curr_file_name ); /* end access to current SDS */

   }	/* for each SDS in the file */

   return( ret_value );
} /* end of printSD_ASCII() */

intn
printSD_BINARY( int32 sd_id,
             FILE *fp,
             dump_info_t *dumpsds_opts,
             int32 *sds_chosen,
             int32 num_sds_chosen,
             int32 ndsets )
{
   int32 sds_id,
         sds_index,
         sds_count,
         dimsizes[MAXRANK],
         dimNT[MAXRANK],
         dimnattr[MAXRANK],
         rank,
         nt,
         nattrs;
   char  name[MAXNAMELEN],
         curr_file_name[MAXFNLEN];
   intn  dumpall = FALSE,
         status,
         ret_value = SUCCEED;
   
   /* temp. names for file type and curr input file name for ease of use */
   HDstrcpy( curr_file_name, dumpsds_opts->ifile_name );

   /* when there are no datasets specified, dumper dumps all datasets */
   if (num_sds_chosen == NO_SPECIFIC )
      dumpall = TRUE;
   else
      sort(sds_chosen, num_sds_chosen);

   /* for each valid index, if the user request to dump all datasets or
      if there are more requested datasets to be processed, process the
      indexed dataset */
   sds_count = 0;   /* no requested dataset has been processed yet */
   for (sds_index = 0; sds_index < ndsets  /* validate index */
        && (dumpall                /* either all datasets are dumped or */
        || sds_count < num_sds_chosen); /* more requested datasets */
        sds_index++)
   {
      /* if the user neither requests dump all nor the current dataset */
      if ((!dumpall) && (sds_index != sds_chosen[sds_count]))
         continue; /* skip */

      sds_count++;   /* count the # of datasets have been processed */

      /* Reset variables. */
      HDmemset(dimsizes, 0, sizeof(int32) * MAXRANK);
      HDmemset(dimNT, 0, sizeof(int32) * MAXRANK);
      HDmemset(dimnattr, 0, sizeof(int32) * MAXRANK);

      sds_id = SDselect(sd_id, sds_index);
      if (sds_id == FAIL)
         ERROR_CONT_3( "in %s: %s failed for %d'th SDS",
			"printSD_BINARY", "SDselect", (int)sds_index );

      status = SDgetinfo(sds_id, name, &rank, dimsizes, &nt, &nattrs);
      if( FAIL == status )
      {
         resetSDS( &sds_id, sds_index, curr_file_name );
         ERROR_CONT_3( "in %s: %s failed for %d'th SDS",
			"printSD_BINARY", "SDgetinfo", (int)sds_index );
      }

      /* output data to binary file   */
      if (rank > 0 && dimsizes[0] != 0)
      {
         status = sdsdumpfull(sds_id, dumpsds_opts, rank, dimsizes, nt, fp);
         if( FAIL == status )
            ERROR_CONT_3( "in %s: %s failed for %d'th SDS", 
			"printSD_BINARY", "sdsdumpfull", (int)sds_index );
      }
      resetSDS( &sds_id, sds_index, curr_file_name );
   }  /* for each dataset in file */

   return( ret_value );
} /* end of printSD_BINARY */

intn 
dsd(dump_info_t *dumpsds_opts, 
    intn         curr_arg, 
    intn         argc, 
    char        *argv[])
{
   int32  sd_id = FAIL,
         *sds_chosen = NULL,
          num_sds_chosen,
          ndsets,
          n_file_attrs;
   char   file_name[MAXFNLEN];
   FILE  *fp = NULL;
   file_type_t ft = dumpsds_opts->file_type;
   intn   index_error = 0,
          status,
          ret_value = SUCCEED;

   /* check for missing input file name */
   if( curr_arg >= argc )
   {
      fprintf( stderr, "Missing input file name.  Please try again.\n");
      return( FAIL ); /* nothing to be cleaned up at this point */
   }

   /* going through each input file, open the file, try to compose the list
      of indices of the SDSs in the file that are requested, then read and
      display information and data of each SDS in the specified manner */
   while (curr_arg < argc)
   {
      intn isHDF = TRUE;  /* FALSE, if current file is not HDF file */

      HDstrcpy(file_name, argv[curr_arg]);
      HDstrcpy( dumpsds_opts->ifile_name, file_name ); /* record file name */
      curr_arg++;

      /* Print an informative message and skip this file if it is not
         an HDF file */
      isHDF = Hishdf(file_name);
      if (isHDF == FALSE)
      {
         /* if there are no more files to be processed, print error
            message, then returns with FAIL */
	 if( curr_arg == argc )
	    {ERROR_GOTO_1( "in dsd: %s is not an HDF file", file_name);}
         else /* print message, then continue processing the next file */
            {ERROR_CONT_1( "in dsd: %s is not an HDF file", file_name);}
      }

      /* open current hdf file with error check, if fail, go to next file */
      sd_id = SDstart(file_name, DFACC_RDONLY);
      if (sd_id == FAIL)
      {
	 /* if there are no more files to be processed, print error 
	    message, then returns with FAIL */
	 if( curr_arg == argc )
            {ERROR_GOTO_1( "in dsd: Failure in opening file %s", file_name);}
	 else /* print message, then continue processing the next file */
            ERROR_CONT_1( "in dsd: Failure in opening file %s", file_name );
      }

      /* compose the list of indices of SDSs to be processed in the current 
         file: sds_chosen is the list and return the number of items in it */
      num_sds_chosen = get_SDSindex_list( sd_id, dumpsds_opts, &sds_chosen, &index_error );

      /* if there are errors with the given indices, ref#s, or names of the
         requested datasets, and so the input yields no valid datasets,
         then close the interface and the input file, and move on to the
         next file */
      if (index_error && num_sds_chosen == 0)
      {
         if(sds_chosen!=NULL)
         {
            HDfree(sds_chosen);
            sds_chosen=NULL;
         } /* end if */
         if( FAIL == SDend(sd_id))
            fprintf( stderr, "in dsd: SDend failed in closing file %s\n",
			file_name );
         continue; /* to the next file */
      } /* end if */

      /* obtain number of datasets in the file and number of file attributes,
         ndsets will be used to process the datasets, n_file_attrs will be
         used to print file attributes */
      status = SDfileinfo(sd_id, &ndsets, &n_file_attrs);
      if (status == FAIL)
         ERROR_GOTO_2( "in dsd: %s failed for file %s", "SDfileinfo", file_name);

      fp = stdout;                /* assume no output file given */

      /* ASCII or binary dump? */
      switch( ft )
      {
         case DASCII:       /* ASCII file */
            /* open output file for ASCII or direct to standard output */
            if (dumpsds_opts->dump_to_file)
               fp = fopen(dumpsds_opts->file_name, "w");

            /* display the name and global attributes of the current file if
               data only option is not selected */
            if (dumpsds_opts->contents != DDATA)
            {
               fprintf(fp, "File name: %s \n", file_name);

               /* print SD file attributes */
               status = print_SDattrs( sd_id, fp, n_file_attrs, dumpsds_opts );
               if( status == FAIL )
                  ERROR_CONT_2( "in dsd: %s failed for file %s", file_name, "print_SDattrs" );
            }
            status = printSD_ASCII( sd_id, dumpsds_opts, ndsets, sds_chosen, num_sds_chosen, fp );
            if( status == FAIL )
               fprintf( stderr, "HDP ERROR>>> in dsd: %s failed for file %s\n", 
				"printSD_ASCII", file_name );

            /* close output file only if option -o is given */
            if (dumpsds_opts->dump_to_file)
               fclose(fp);                       
            break;

         case DBINARY:       /* binary file */
             /* get output file name  */
             if (dumpsds_opts->dump_to_file)
                 fp = fopen(dumpsds_opts->file_name, "wb");
            status = printSD_BINARY( sd_id, fp, dumpsds_opts, sds_chosen, num_sds_chosen, ndsets );
            if( status == FAIL )
               fprintf( stderr, "HDP ERROR>>> in dsd: %s failed for file %s\n", 
			"printSD_BINARY", file_name );

            /* close output file only if option -o is given */
            if (dumpsds_opts->dump_to_file)
               fclose(fp);                       
            break;

         default:
            printf("Output file type must be either ascii or binary only\n");
      } /* switch for output file */

      if (sds_chosen != NULL)
      {
         HDfree(sds_chosen);
         sds_chosen = NULL;
      } /* end if */

      if (FAIL == SDend(sd_id))
         ERROR_CONT_1( "in dsd: SDend failed for file %s", file_name );
      sd_id = FAIL; /* reset */
   }  /* while processing files  */

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
          if (sd_id != FAIL)
              SDend(sd_id);

          if (sds_chosen != NULL)
            {
                HDfree(sds_chosen);
                sds_chosen=NULL;
            } /* end if */
      }
    /* Normal cleanup */

    return ret_value;
}	/* dsd */

/* Exported */
intn
do_dumpsds(intn  curr_arg, 
           intn  argc, 
           char *argv[], 
           intn  help )
{
    dump_info_t dumpsds_opts;	/* dumpsds options */
    intn status, ret_value = SUCCEED;

    /* initialize the structure that holds user's options and inputs */                                     init_dump_opts(&dumpsds_opts);        

   /* command line: hdp help */
   if (help == TRUE)
   {
      dumpsds_usage(argc, argv);
      goto done;
   }  /* end if */

   /* incomplete command */
   if( curr_arg >= argc )
   {
      dumpsds_usage(argc, argv);
      ERROR_GOTO_0( "in do_dumpsds: command is incomplete");
   }            /* end if */

   /* parse the user's command and store the inputs in dumpsds_opts */
   status = parse_dumpsds_opts( &dumpsds_opts, &curr_arg, argc, argv );
   if( status == FAIL )
   {
      dumpsds_usage(argc, argv);
      ret_value = FAIL; /* so caller can be traced in debugging */
      goto done; /* skip dsd */
   }

   /* display data and information as specified in dumpsds_opts */
   status = dsd( &dumpsds_opts, curr_arg, argc, argv );
   if( status == FAIL )
      ERROR_GOTO_0( "in do_dumpsds: dsd failed" );

  done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
    /* Normal cleanup */

   /* free the lists for given indices, ref#s, and names if
      they had been allocated */
   free_num_list(dumpsds_opts.by_index.num_list );
   free_num_list(dumpsds_opts.by_ref.num_list );
   free_str_list(dumpsds_opts.by_name.str_list, dumpsds_opts.by_name.num_items);

   return ret_value;
}	/* end do_dumpsds() */

