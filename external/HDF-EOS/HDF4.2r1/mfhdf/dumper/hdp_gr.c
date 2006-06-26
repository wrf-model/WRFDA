
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

/* $Id: hdp_gr.c,v 1.21 2003/12/10 21:14:01 epourmal Exp $ */

#include <stdio.h>
#include "mfhdf.h"
#include "mfgr.h"
#include "hdp.h"
#ifndef MIPSEL
#include <math.h>
#endif /* MIPSEL */

#define  N_ENTRIES      256     /* number of elements of each color */

void
dumpgr_usage(intn  argc,
             char *argv[])
{
    printf("Usage:\n");
    printf("%s dumpgr [-a|-i <indices>|-r <refs>|-n <names>] [-m <interlace>] [-dhvcs] [-p|-pd] [-o <filename>] [-bx] <filelist>\n", argv[0]);
    printf("\t-a\tDump all RIs in the file (default)\n");
    printf("\t-i <indices>\tDump the <indices>th RIs in the file \n");
    printf("\t-r <refs>\tDump the RIs with reference number <refs>\n");
    printf("\t-n <names>\tDump the RIs with name <names>\n");
    printf("\t-m <interlace>\tDump data in interlace mode <interlace= 0, 1, or 2>\n");
    printf("\t-d\tDump data only, no tag/ref, formatted to input to hp2hdf\n");
    printf("\t-h\tDump header only, no data - exclusive with -p and -pd\n");
    printf("\t-v\tDump everything including all annotations (default)\n");
    printf("\t-c\tPrint space characters as they are, not \\digit\n");
    printf("\t-g\tDo not print data of file (global) attributes\n"); 
    printf("\t-l\tDo not print data of local attributes\n"); 
    printf("\t-s\tDo not add carriage return to a long line - dump it as a stream\n");
    printf("\t-p\tDump palette's information and data - exclusive with -h\n");
    printf("\t-pd\tDump palette's data only - exclusive with -h\n");
    printf("\t-o <filename>\tOutput to file <filename>\n");
    printf("\t-b\tBinary format of output\n");
    printf("\t-x\tAscii text format of output (default)\n");
    printf("\t<filelist>\tList of hdf file names, separated by spaces\n");
}       /* end list_usage() */
intn 
parse_dumpgr_opts(dump_info_t *dumpgr_opts, 
                  intn        *curr_arg, 
                  intn         argc, 
                  char        *argv[] )
{
   gr_interlace_t user_interlace; /* temporary store user's interlace mode */
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
             dumpgr_opts->filter = DALL;

             /* indicate that no specific images requested, will dump all */
             dumpgr_opts->num_chosen = NO_SPECIFIC;
             (*curr_arg)++;
             break;
            
         case 'i':	/* dump by index */
             dumpgr_opts->filter |= DINDEX;  /* set bit DINDEX */
             (*curr_arg)++;

             /* parse and store the given indices in structure by_index */
             parse_number_opts( argv, curr_arg, &dumpgr_opts->by_index);
             (*curr_arg)++;
             break;

         case 'r':	/* dump by reference */
             dumpgr_opts->filter |= DREFNUM; /* set bit DREFNUM */
             (*curr_arg)++;

             /* parse and store the given ref numbers in structure by_ref */
             parse_number_opts( argv, curr_arg, &dumpgr_opts->by_ref);
             (*curr_arg)++;
             break;

         case 'n':	/* dump by names */
             dumpgr_opts->filter |= DNAME;   /* set bit DNAME */
             (*curr_arg)++;

             /* parse and store the given names in structure by_name */
             parse_string_opts( argv, curr_arg, &dumpgr_opts->by_name);
             (*curr_arg)++;
             break;

         case 'm':	/* dump data in different interlace than at creation */
             (*curr_arg)++;  /* move forward to interlace option input */
             user_interlace = atoi( argv[*curr_arg] );
	     if( user_interlace == MFGR_INTERLACE_PIXEL || 
		 user_interlace == MFGR_INTERLACE_LINE || 
		 user_interlace == MFGR_INTERLACE_COMPONENT )
		dumpgr_opts->interlace = user_interlace; /* store interlace */
	     else
	     {
                printf("Invalid input for interlace option %s\n", argv[*curr_arg]);
		HGOTO_DONE( FAIL );
	     }
             (*curr_arg)++;  /* move forward to next option */
             break;

         case 'd':	/* dump data only */
             dumpgr_opts->contents = DDATA;
             (*curr_arg)++;
             break;

         case 'h':	/* no annotations nor data */
	     /* make sure -p is not also given */
	     if( dumpgr_opts->print_pal )
		ERROR_GOTO_0( "Option -h must not be used together with either -p or -pd" );

             dumpgr_opts->contents = DHEADER;
             (*curr_arg)++;
             break;

         case 'v':	/* dump all info, default */
             dumpgr_opts->contents = DVERBOSE;
             (*curr_arg)++;
             break;

         case 's':      /* do not add carriage returns to output data lines */
             dumpgr_opts->as_stream = TRUE;
             (*curr_arg)++;
             break; 

         case 'c':      /* print space characters as they are, not \\digit */
             dumpgr_opts->clean_output = TRUE;
             (*curr_arg)++;
             break; 

         case 'g':      /* suppress file (global) attr data, print its header */
                dumpgr_opts->no_gattr_data = TRUE;
                (*curr_arg)++;
                break;

         case 'l':      /* suppress local attr data, only print its header */
                dumpgr_opts->no_lattr_data = TRUE;
                (*curr_arg)++;
                break;

         case 'o':   /* specify output file */
             dumpgr_opts->dump_to_file = TRUE;

             /* Get file name */
             HDstrcpy(dumpgr_opts->file_name, argv[++(*curr_arg)]);

             (*curr_arg)++;
             break;

         case 'b':   /* dump data in binary */
             dumpgr_opts->file_type = DBINARY;
             (*curr_arg)++;
             break;

         case 'x':   /* dump data in ascii, also default */
             dumpgr_opts->file_type = DASCII;
             (*curr_arg)++;
             break;

         case 'p':   /* dump palette data */
	     /* make sure -h is not also given */
	     if( dumpgr_opts->contents == DHEADER )
		ERROR_GOTO_0( "Option -h must not be used together with either -p or -pd" );

             dumpgr_opts->print_pal = TRUE;

	     /* if the current option is -pd, then pal data only is requested */
	     if( argv[*curr_arg][2] == 'd' ) /* \0 if only -p */
	     {
		dumpgr_opts->contents = DDATA;
	     }
             (*curr_arg)++;

             break;

         default:	/* invalid dumpgr option */
             printf("HDP ERROR>>> Invalid dumpgr option %s\n", argv[*curr_arg]);
	     HGOTO_DONE( FAIL );
         }   /* end switch */
      }	 /* end while */
   
   /* add the number of images requested by index, by ref#, and by name
      to have a total number of requested images */
   dumpgr_opts->num_chosen = dumpgr_opts->by_index.num_items +
                             dumpgr_opts->by_ref.num_items +
                             dumpgr_opts->by_name.num_items;
done:
   if (ret_value == FAIL)
   { /* Failure cleanup */
      /* free the lists for given indices, ref#s, and names if
         they had been allocated */
      free_num_list(dumpgr_opts->by_index.num_list );
      free_num_list(dumpgr_opts->by_ref.num_list );
      free_str_list(dumpgr_opts->by_name.str_list, dumpgr_opts->by_name.num_items);
   }
   /* Normal cleanup */

   return ret_value;
}	/* end parse_dumpgr_opts */

intn 
grdumpfull(int32        ri_id, 
           dump_info_t  *dumpgr_opts,
           int32        ncomps,	/* "ncomps" is the number of components 
                                   in each element of the data set */
           int32        dimsizes[], /*  size of dimension "i". */
           int32        nt, 
           FILE        *fp)
{
   VOIDP  buf = NULL;
   int32  numtype,
          eltsz,
          read_nelts,
         *start = NULL,  /* starting location to be read */
         *edge = NULL,   /* # of values to be read in each dim */
         *stride = NULL; /* # of values to be skipped b/w readings */
   intn   status, ret_value = SUCCEED;

   /* Compute the number of the bytes for each value. */
   numtype = nt & DFNT_MASK;
   eltsz = DFKNTsize(numtype | DFNT_NATIVE)*ncomps;
    
   read_nelts = dimsizes[0]*dimsizes[1];

   /* make sure we are not allocating 0 elements, ie. number of 
      elements is positive */
   CHECK_POS( read_nelts, "read_nelts", "grdumpfull" );
   CHECK_POS( eltsz, "eltsz", "grdumpfull" );
   CHECK_POS( ncomps, "ncomps", "grdumpfull" );

   buf = (VOIDP) HDmalloc(read_nelts * eltsz);
   CHECK_ALLOC( buf, "buf", "grdumpfull" );

   start = (int32 *) HDmalloc(ncomps * sizeof(int32));
   CHECK_ALLOC( start, "start", "grdumpfull" );

   edge = (int32 *) HDmalloc(ncomps * sizeof(int32));
   CHECK_ALLOC( edge, "edge", "grdumpfull" );

   stride = (int32 *) HDmalloc(ncomps * sizeof(int32));
   CHECK_ALLOC( stride, "stride", "grdumpfull" );

   start[0]=start[1]=0;
   edge[0]=dimsizes[0];
   edge[1]=dimsizes[1];
   stride[0]=1;
   stride[1]=1;
 
   /* if the user requests that the data is printed in a different 
      interlace mode from that at the creation of the image, set the 
      interlace mode of the image to be stored in memory when read */
   if( dumpgr_opts->interlace != NO_SPECIFIC )
   {
      status = GRreqimageil( ri_id, dumpgr_opts->interlace );
      if( status == FAIL )
         ERROR_GOTO_2( "in %s: GRreqimageil failed for ri_id(%d)",
                  "grdumpfull", (int)ri_id );
   }

   status = GRreadimage(ri_id, start, stride, edge, buf);
   if ( status == FAIL )
      ERROR_GOTO_2( "in %s: GRreadimage failed for ri_id(%d)",
                  "grdumpfull", (int)ri_id );

   /* if printing data only, print with no indentation */
   if( dumpgr_opts->contents == DDATA )
      status = dumpfull( nt, dumpgr_opts, read_nelts*ncomps, buf, fp, 0, 0);
   else 
      status = dumpfull( nt, dumpgr_opts, read_nelts*ncomps, buf, fp,
				DATA_INDENT, DATA_CONT_INDENT );
   if( status == FAIL )
      ERROR_GOTO_2( "in %s: dumpfull failed for ri_id(%d)",
                  "grdumpfull", (int)ri_id );

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
    /* Normal cleanup */
    if (edge != NULL)
        HDfree((VOIDP) edge);
    if (start != NULL)
        HDfree((VOIDP) start);
    if (stride != NULL)
        HDfree((VOIDP) stride);
    if (buf != NULL)
        HDfree((VOIDP) buf);

    return ret_value;
}	/* grdumpfull */

/* compose the list of indices of the requested vgroups although some
   vgroups are requested by ref# or name.
   The routine returns:
        - the number of vgroups to be processed, or
        - NO_SPECIFIC if all vgroups are to be processed, or
        - 0 if none.
   If there are any errors, the parameter index_error will return TRUE */
int32
get_RIindex_list(
	int32 gr_id,
	dump_info_t *dumpgr_opts, 
	int32 **gr_chosen,/* array of indices of RIs to be processed */
	intn *index_error )
{
   int32    index,         /* index of an image */
            ri_count = 0;  /* number of RIs to be processed */
   filter_t filter = dumpgr_opts->filter; /* temporary name */
   int32    num_ri_chosen = dumpgr_opts->num_chosen;
   intn     i, 
            ret_value = 0;

   /* if no specific images are requested, return the image count as 
      NO_SPECIFIC (-1) to indicate that all images are to be dumped */
   if( filter == DALL )
      HGOTO_DONE( NO_SPECIFIC );

   /* if specific images were requested, allocate space for the array 
      of indices */
   if (num_ri_chosen > 0)
      alloc_index_list( gr_chosen, num_ri_chosen );

   /* else, no chosen images but filter is not DALL, it shouldn't be this
      combination, return image count as NO_SPECIFIC to dumpall */
   else
      HGOTO_DONE( NO_SPECIFIC );

   /* if there are some images requested by index, store the indices
      in the array gr_chosen */
   if( filter & DINDEX )
      for (i = 0; i < dumpgr_opts->by_index.num_items; i++)
      {
         /* Note: Don't replace this with HDmemcpy unless you change the 
            sizes of the objects correctly -QAK */
         (*gr_chosen)[ri_count] = dumpgr_opts->by_index.num_list[i];
         ri_count++;
      }

   /* if there are some images requested by ref#, convert the ref#s
      to indices and store them in the array gr_chosen */
   if( filter & DREFNUM ) 
      for (i = 0; i < dumpgr_opts->by_ref.num_items; i++)
      {
         index = GRreftoindex(gr_id, dumpgr_opts->by_ref.num_list[i]);
                
         if (index == FAIL)
         {
            printf("Image with reference number %d: not found\n", 
				(int)dumpgr_opts->by_ref.num_list[i]);
            *index_error = TRUE;     
         }
         else
         {
            (*gr_chosen)[ri_count] = index;
            ri_count++;
         }
      }             

   /* if there are some images requested by name, convert the names to
      indices and store them in the array gr_chosen */
   if( filter & DNAME )
      for (i = 0; i < dumpgr_opts->by_name.num_items; i++)
      {
         index = GRnametoindex(gr_id, dumpgr_opts->by_name.str_list[i]);
         if (index == FAIL)
         {
            printf("Image with name %s: not found\n", dumpgr_opts->by_name.str_list[i]);
            *index_error = TRUE;
         }
         else
         {
            (*gr_chosen)[ri_count] = index;
            ri_count++;
         }
      }

   ret_value = ri_count;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
    /* Normal cleanup */

    return ret_value;
} /* end of get_RIindex_list */

/* prints all GR file attributes in the current file */
intn
print_GRattrs( 
	int32 gr_id,
	int32 n_file_attrs,
	FILE *fp,
	dump_info_t *dumpgr_opts )
{
   int32 attr_index,
         attr_count,
         attr_nt,
         attr_buf_size;
   char  attr_name[MAXNAMELEN],
        *attr_nt_desc = NULL;
   VOIDP attr_buf = NULL;
   intn  printed = FALSE;  /* whether file attr title has been printed */
   intn  status,           /* status from called routine */
         ret_value = SUCCEED;

   /* for each file attribute, print its info and values */
   for (attr_index = 0; attr_index < n_file_attrs; attr_index++)
   {
      /* get the current attr's name, number type, and number of values */
      status = GRattrinfo(gr_id, attr_index, attr_name, &attr_nt, &attr_count);
      if (FAIL == status ) /* to the next attribute */
         ERROR_CONT_2( "in %s: GRattrinfo failed for %d'th attribute", 
			"print_GRattrs", (int)attr_index );

      /* get number type description of the attribute */
      attr_nt_desc = HDgetNTdesc(attr_nt);
      if (NULL == attr_nt_desc)
         ERROR_CONT_2( "in %s: HDgetNTdesc failed for %d'th attribute", 
		"print_GRattr", (int)attr_index );

      /* print a title line for file attributes if it's not printed
         yet and set flag so it won't be printed again */
      if( !printed )
      {
         fprintf(fp, "\n    File attributes:\n");
         printed = TRUE;
      }

      /* display the attribute's information then free buffer */
      fprintf(fp, "\t Attr%d: Name = %s\n", (int) attr_index, attr_name);
      fprintf(fp, "\t\t Type = %s \n\t\t Count= %d\n", 
			attr_nt_desc, (int) attr_count);
      resetBuff(( VOIDP *) &attr_nt_desc ); 

      /* display the attribute's values unless user chose to suppress them */
      if( dumpgr_opts->no_gattr_data == FALSE )
      {
         /* to be sure that attr_buf is free before reuse since sometimes we
            have to break the current loop and continue to the next item */
         resetBuff( &attr_buf );

         /* calculate the buffer size of the attribute using the number of
            values in the attribute and its value size */
         attr_buf_size = DFKNTsize(attr_nt) * attr_count;

         /* make sure we are not allocating 0 elements */
         CHECK_POS( attr_buf_size, "attr_buf_size", "print_GRattrs" );

         /* allocate space for the attribute's values */
         attr_buf = (VOIDP) HDmalloc(attr_buf_size);
         CHECK_ALLOC( attr_buf, "attr_buf", "print_GRattrs" );

         /* read the values of the attribute into the buffer */
         status = GRgetattr( gr_id, attr_index, attr_buf );
         if (status == FAIL)
            ERROR_CONT_2( "in %s: GRgetattr failed for %d'th attribute", 
		"print_GRattr", (int)attr_index );

         /* display the attribute's values */
         fprintf(fp, "\t\t Value = ");

         /* if the user wishes to have clean output, i.e. option -c is
            selected - Note that this option is only applicable to DFNT_CHAR
            type, the option will be ignored for other types */
         if( dumpgr_opts->clean_output && attr_nt == DFNT_CHAR )
         {
            status = dumpclean(attr_nt, dumpgr_opts, attr_count, attr_buf, fp);
            if( status == FAIL )
               ERROR_CONT_2( "in %s: dumpclean failed for %d'th attribute",
                   "print_GRattr", (int)attr_index );

         }
         else  /* show tab, lf, null char... in octal as \011, \012, \000... */
         {
            status = dumpfull(attr_nt, dumpgr_opts, attr_count, attr_buf, fp,
			ATTR_INDENT, ATTR_CONT_INDENT );
            if( status == FAIL )
               ERROR_CONT_2( "in %s: dumpfull failed for %d'th attribute",
                   "print_GRattr", (int)attr_index );
         }
         /* free buffer and reset it to NULL */
      }  /* end of if no file attributes */
   } /* for all attributes of GR */

   return ret_value;
} /* end of print_GRattrs */

intn
print_RIattrs( 
	int32 ri_id,
	intn ri_index,
	int32 nattrs,
	FILE *fp,
	dump_info_t* dumpgr_opts )
{
   int32 attr_index,
         attr_count,
         attr_nt,
         attr_buf_size;
   char  attr_name[MAXNAMELEN],
        *attr_nt_desc = NULL;
   VOIDP attr_buf=NULL;
   intn  status,   /* status returned from a called routine */
         ret_value = SUCCEED;  /* returned value of print_RIattrs */

   /* for each attribute, display its info and data */
   for (attr_index = 0; attr_index < nattrs; attr_index++)
   {
      /* get the current attr's name, number type, and number of values */
      status = GRattrinfo(ri_id, attr_index, attr_name, &attr_nt, &attr_count);
      if (FAIL == status) /* go to next attribute */
         ERROR_CONT_3( "in %s: GRattrinfo failed for %d'th attribute of %d'th RI", 
			"print_RIattrs", (int)attr_index, (int)ri_index );

      /* get number type description of the attribute */
      attr_nt_desc = HDgetNTdesc(attr_nt);
      if (NULL == attr_nt_desc)  /* go to the next attribute */
         ERROR_CONT_3( "in %s: HDgetNTdesc failed for %d'th attribute of %d'th RI", 
			"print_RIattrs", (int)attr_index, (int)ri_index );

      /* display the attribute's information then free buffer */
      fprintf(fp, "\t Attr%d: Name = %s\n", (int) attr_index, attr_name);
      fprintf(fp, "\t\t Type = %s \n\t\t Count= %d\n", attr_nt_desc, (int) attr_count);

      /* free buffer and reset it to NULL */
      resetBuff((VOIDP *) &attr_nt_desc );

      /* display the attribute's values unless user chose to suppress them */
      if( dumpgr_opts->no_lattr_data == FALSE )
      {
         /* to be sure that attr_buf is free before reuse since sometimes we
            have to break the current loop and continue to the next item */
         resetBuff( &attr_buf );

         /* calculate the buffer size of the attribute using the number of 
            values in the attribute and its value size */
         attr_buf_size = DFKNTsize(attr_nt) * attr_count;

         /* make sure we are not allocating 0 elements */
         CHECK_POS( attr_buf_size, "attr_buf_size", "print_RIattrs" );

         /* allocate space for attribute's values */
         attr_buf = (VOIDP) HDmalloc(attr_buf_size);
         CHECK_ALLOC( attr_buf, "attr_buf", "print_RIattrs" );

         /* read the values of the attribute into buffer attr_buf */
         status = GRgetattr( ri_id, attr_index, attr_buf );
         if (status == FAIL)  /* go to the next attribute */
            ERROR_CONT_3( "in %s: GRgetattr failed for %d'th attribute of %d'th RI", 
			"print_RIattrs", (int)attr_index, (int)ri_index );

         /* display the attribute's values then free buffer */
         fprintf(fp, "\t\t Value = ");

         /* if the user wishes to have clean output, i.e. option -c is selected */
         /* Note that this option is only applicable to DFNT_CHAR type, the
            option will be ignored for other types */
         if( dumpgr_opts->clean_output && attr_nt == DFNT_CHAR )
         {
            status = dumpclean(attr_nt, dumpgr_opts, attr_count, attr_buf, fp);
            if( status == FAIL )
               ERROR_CONT_3( "in %s: dumpclean failed for %d'th attribute of %d'th RI", 
			"print_RIattrs", (int)attr_index, (int)ri_index );
         }
         else  /* show tab, lf, null char... in octal as \011, \012, \000... */
         {
            status = dumpfull(attr_nt, dumpgr_opts, attr_count, attr_buf, fp,
			ATTR_INDENT, ATTR_CONT_INDENT );
            if( status == FAIL )
               ERROR_CONT_3( "in %s: dumpfull failed for %d'th attribute of %d'th RI", 
			"print_RIattrs", (int)attr_index, (int)ri_index );
         }
      }  /* end of if no local attributes */
   } /* for all attributes of an RI */

   return ret_value;
} /* end of print_RIattrs */

/* Displays the palette information only.  Note that HDF supports only
   256 colors.  Each color is defined by its 3 components. Therefore,
   verifying the value of n_entries and n_comps is not necessary and
   the buffer to hold the palette data can be static.  However,
   if more values or colors are added to the model, these parameters
   must be checked to allocate sufficient space when reading a palette
   and the palette number should be printed */
intn print_PaletteInfo(
        int32 ri_id,
        int32 num_pals, /* number of palettes, currently only 1 */
        FILE *fp )
{
   int32 pal_id = FAIL,
         pal_index,
         data_type,
         n_comps,
         n_entries,
         interlace_mode;
   intn  status, ret_value = SUCCEED;

   /* Check the number of palettes */
   if((num_pals=GRgetnluts(ri_id))<0)
         ERROR_GOTO_2( "in %s: GRgetnluts failed for raster image ID #%d",
                        "print_PaletteInfo", (int)ri_id);

   /* if there are no palette data, print message for both cases:
      header-only and verbose (data+header) */
   if(num_pals==0) {
        fprintf( fp, "\t No palette\n");
   } /* end if */
   else {
      /* display each palette of an RI */
      for( pal_index = 0; pal_index < num_pals; pal_index++ )
      {
          /* Get the identifier of the palette attached to the image. */
         pal_id = GRgetlutid (ri_id, pal_index);
         if( pal_id == FAIL ) /* continue to the next palette */
             ERROR_CONT_2( "in %s: GRgetlutid failed for palette #%d",
                            "print_PaletteInfo", (int)pal_index);

         /* Obtain and display information about the palette. */
         status = GRgetlutinfo (pal_id, &n_comps, &data_type, &interlace_mode,
                                &n_entries);
         if( status == FAIL ) /* continue to the next palette */
             ERROR_CONT_2( "in %s: GRgetlutinfo failed for palette #%d",
                            "print_PaletteInfo", (int)pal_index);

         /* if there are palette data, print header info */
         fprintf (fp, "\t Palette: %d components; %d entries\n",
                        (int)n_comps, (int)n_entries);

      } /* end of for each palette */
   } /* end else */

done:
   if (ret_value == FAIL)
   { /* Failure cleanup */
   }
   return( ret_value );
}  /* end of print_PaletteInfo */

/* Displays the palette data.  Note that HDF supports only 256 colors.
   Each color is defined by its 3 components. Therefore,
   verifying the value of n_entries and n_comps is not necessary and
   the buffer to hold the palette data can be static.  However,
   if more values or colors are added to the model, these parameters
   must be checked to allocate sufficient space when reading a palette */
intn
print_Palette(
	int32 ri_id, 
	int32 num_pals, /* number of palettes, currently only 1 */
	FILE *fp,
	dump_info_t *dumpgr_opts )
{
   int32 pal_id = FAIL,
         pal_index,
         data_type,
         n_comps,
         n_entries,
         interlace_mode;
   uint8 palette_data[N_ENTRIES][3];  /* static because of fixed size */
   uint16 ri_ref;
   intn  status, ret_value = SUCCEED;

   /* Get the ref# of the image, for displaying output */
   ri_ref = GRidtoref(ri_id);

   /* Check the number of palettes */
   if((num_pals=GRgetnluts(ri_id))<0)
         ERROR_GOTO_2( "in %s: GRgetnluts failed for raster image with ref#=%d",
                        "print_Palette", (int)ri_ref);

   /* if there are no palette data, print message for both cases:
      header-only and verbose (data+header) */
   if(num_pals==0) {
      if( dumpgr_opts->contents != DDATA )
         fprintf( fp, "\n\t Raster Image Ref. = %d\n\t No palette data\n", 
			  (int)ri_ref);
   } /* end if */
   else {
      /* display each palette of an RI */
      for( pal_index = 0; pal_index < num_pals; pal_index++ )
      {
         /* Get the identifier of the palette attached to the image. */
         pal_id = GRgetlutid (ri_id, pal_index);
         if( pal_id == FAIL ) /* continue to the next palette */
             ERROR_CONT_2( "in %s: GRgetlutid failed for palette #%d",
                            "print_Palette", (int)pal_index);

         /* Read the palette data. */
         status = GRreadlut (pal_id, (VOIDP)palette_data);
         if( status == FAIL ) { /* continue to the next palette */
             ERROR_CONT_2( "in %s: GRreadlut failed for palette #%d",
                            "print_Palette", (int)pal_index); }

         /* Obtain and display information about the palette. */
         status = GRgetlutinfo (pal_id, &n_comps, &data_type, 
				&interlace_mode, &n_entries);
         if( status == FAIL ) /* continue to the next palette */
             ERROR_CONT_2( "in %s: GRgetlutinfo failed for palette #%d",
                           "print_Palette", (int)pal_index);

         /* if there are palette data, print header info when not 
             data-only and print palette data when not header-only */
	 switch (dumpgr_opts->contents)
	 {
	    /* Note that case DHEADER is not presented because option -h
	       is not allowed with -p or -pd */
	    case DVERBOSE:
		 fprintf (fp, "\n\t Raster Image Ref. = %d\n", (int)ri_ref);
		 fprintf (fp, "\t Palette: %d components; %d entries\n", 
                         (int)n_comps, (int)n_entries);

		 /* display the palette data with the title line and 
		    indent the data with DATA_INDENT and DATA_CONT_INDENT */
		 fprintf (fp, "\t Palette Data: \n");
		 status = dumpfull(data_type, dumpgr_opts, n_entries*n_comps,
		    	  palette_data, fp, DATA_INDENT, DATA_CONT_INDENT );
                 if( status == FAIL )
                    ERROR_GOTO_2( "in %s: dumpfull failed for palette #%d", 
			     "print_Palette", (int)pal_index);
		 break;

	    case DDATA:
		 /* print palette data with no title and indentation */
		 status = dumpfull(data_type, dumpgr_opts, 
			  n_entries*n_comps, palette_data, fp, 0, 0 );
                 if( status == FAIL )
                    ERROR_GOTO_2( "in %s: dumpfull failed for palette #%d",
                         "print_Palette", (int)pal_index );
		 break;
	    default:
		 printf("Unknown output type option \n" );
	  } /* end switch */
      } /* end for each palette */
   } /* end else */

done:
   if (ret_value == FAIL)
   { /* Failure cleanup */
   }
   /* Normal cleanup */
   return ret_value;
}  /* end of print_Palette */

char*
Il_mode_text( gr_interlace_t interlace_mode )
{
   char* il_text;
   switch( interlace_mode )
   {
	case MFGR_INTERLACE_PIXEL:
	   il_text = "PIXEL";
	   break;
	case MFGR_INTERLACE_LINE:
	   il_text = "LINE";
	   break;
	case MFGR_INTERLACE_COMPONENT: 
           il_text = "COMPONENT";
	   break;
	default:
	   il_text = "INVALID";
   } /* end switch */
   return( il_text );
}

intn
printGR_ASCII( 
	int32 gr_id,
	dump_info_t *dumpgr_opts,
	int32 ndsets,        /* number of images in the file */
	int32 *gr_chosen,    /* list of images' indices */
	int32 num_ri_chosen, /* number of indices in gr_chosen */
	FILE *fp )
{
   int32 ri_id = FAIL,       /* image id, always reset to FAIL when not used */
         ri_ref,             /* ref# of an image */
         ri_index,           /* index of images in the file */
         ri_count,           /* count of images being printed */
         nt,                 /* number type of an image */
         il,                 /* interlace mode of an image */
         ncomps,             /* number of components in an image */
         dimsizes[MAXRANK],  /* dimension sizes of an image */
         nattrs;             /* number of attributes assigned to an image */
   char  name[MAXNAMELEN],   /* name of an image */
         curr_file_name[MAXFNLEN], /* current input (hdf) file's name */
        *nt_desc = NULL;     /* ??? */
   intn  dumpall = FALSE,    /* TRUE when no specific images requested */
         status,             /* status returned from a routine */
         ret_value = SUCCEED;/* returned value of printGR_ASCII */

   /* temp. name for curr input file name for ease of use */
   HDstrcpy( curr_file_name, dumpgr_opts->ifile_name );

   /* when there are no images specified, dumper dumps all images */
   if (num_ri_chosen == NO_SPECIFIC)  /* NO_SPECIFIC = -1 */
      dumpall = TRUE;
   else
      sort(gr_chosen, num_ri_chosen); /* otherwise, sort the list */

   /* for each valid index, if the user requests to dump all images or 
      if there are more requested images to be processed, process the 
      indexed image */
   ri_count = 0;    /* no images has been processed yet */
   for (ri_index = 0; ri_index < ndsets /* validate index */
        && (dumpall                     /* either all images are dumped */
        || ri_count < num_ri_chosen);   /* or more requested images */
        ri_index++)
   {
      /* if the user neither requests dump all nor the current image */
      if ((!dumpall) && (ri_index != gr_chosen[ri_count]))
         continue; /* go to the next image in the file  */

      ri_count++;  /* count the # of images being processed */

      /* Reset variables. */
      HDmemset(dimsizes, 0, sizeof(int32) * MAXRANK);

      /* get access to the current image */
      ri_id = GRselect(gr_id, ri_index);
      if (ri_id == FAIL) /* to the next image */
         ERROR_CONT_2( "in %s: GRselect failed for %d'th RI", 
			"printGR_ASCII", (int)ri_index );

      /* get image's information */
      status = GRgetiminfo(ri_id, name, &ncomps, &nt, &il, dimsizes, &nattrs);
      if( FAIL == status )
      {
         fprintf(stderr,"in %s: GRgetiminfo failed for %d'th RI",
			"printGR_ASCII", (int)ri_index );

         /* end access to the current image before going on to the next */
         if (FAIL == GRendaccess(ri_id))
            fprintf( stderr,"in %s: GRendaccess failed for %d'th RI",
			"printGR_ASCII", (int)ri_index );

         ri_id = FAIL;  /* reset image id */
         continue; /* to the next image */
      }

      /* print image palette's info and data or data only depending on
         the content's option (-pd or -p, taken care by print_Palette) */
      if( dumpgr_opts->print_pal )  /* set when -p or -pd is given */
      {
         /* Note: currently only 1 pal assigned to an image, 2nd arg. */
         status = print_Palette( ri_id, 1, fp, dumpgr_opts );
	 if( status == FAIL )
	    ERROR_BREAK_2("in %s: Printing image's palette failed for RI #%d",
			"printGR_ASCII", (int)ri_index, FAIL );
      }
      else /* only happen when neither -p nor -pd were given */
      {

         /* print the current image as specified by user's options */
         switch (dumpgr_opts->contents)
         {
            case DVERBOSE:
            case DHEADER:
               nt_desc = HDgetNTdesc(nt);
               if (NULL == nt_desc)
                  ERROR_BREAK_2( "in %s: HDgetNTdesc failed for %d'th RI",
      			"printGR_ASCII", (int)ri_index, FAIL );

               /* display image's info then free the buffer no longer needed */
               fprintf(fp, "\n\t Image  Name = %s\n\t Index = ", name);
               fprintf(fp, "%d\n\t Type= %s\n", (int)ri_index, nt_desc);

               resetBuff(( VOIDP *) &nt_desc ); 

               /* get the image's ref# from its id */
               if ((ri_ref = GRidtoref(ri_id)) == FAIL)
                  ERROR_BREAK_2( "in %s: GRidtoref failed for %d'th RI",
			"printGR_ASCII", (int)ri_index, FAIL );

               /* print more image's info */
               fprintf(fp, "\t width=%d; height=%d\n", (int) dimsizes[0], (int) dimsizes[1]);
               fprintf(fp, "\t Ref. = %d\n", (int) ri_ref);
               fprintf(fp, "\t ncomps = %d\n\t Interlace mode= %s\n",
				(int) ncomps, Il_mode_text(il) );

	       /* print the palette info now so it won't be lost after the 
	          image data; currently, only 1 palette per image (2nd arg.) */
	       status = print_PaletteInfo( ri_id, 1, fp );
               if( status == FAIL )
		   ERROR_BREAK_2( "in %s: Printing image's palette information failed for %d'th RI",
			"printGR_ASCII", (int)ri_index, FAIL );

               /* Print image attributes */
               fprintf(fp, "\t Number of attributes = %d\n", (int) nattrs );
               status = print_RIattrs(ri_id, ri_index, nattrs, fp, dumpgr_opts );
               if( status == FAIL )
		   ERROR_BREAK_2( "in %s: Printing image's attributes failed for %d'th RI",
			"printGR_ASCII", (int)ri_index, FAIL );

               if (dumpgr_opts->contents == DHEADER)
                  break; /* break out for header only */

            case DDATA:
               if (dumpgr_opts->contents != DDATA)
                  fprintf(fp, "\t Data : \n");

               if (ncomps > 0 && dimsizes[0] != 0)
               {
                  /* print the current image's data */
                  status = grdumpfull( ri_id, dumpgr_opts, ncomps, dimsizes, nt, fp);
                  if ( status == FAIL )
		     ERROR_BREAK_2( "in %s: Printing image's data failed for %d'th RI",
			"printGR_ASCII", (int)ri_index, FAIL );
               }
               else
               {
                  fprintf(fp, "\t\t No data written.\n");
               }
               break; /* data section */
            default:
               printf("Unknown output type option \n" );
         } /* switch  on contents */
      } /* end if when neither -p nor -pd given */

      /* end access to the current image */
      if (FAIL == GRendaccess(ri_id))    
         fprintf(stderr,"in %s: GRendaccess failed for %d'th RI",
			"printGR_ASCII", (int)ri_index );
      ri_id = FAIL;  /* reset image id */
   }	/* for ndsets  */

   /* Normal cleanup */
   resetBuff(( VOIDP *) &nt_desc ); 

   return ret_value; /* status of calls */
} /* end of printGR_ASCII */

intn printGR_BINARY(
		int32 gr_id,
		dump_info_t *dumpgr_opts,
		int32 num_ri_chosen,     /* # of indices in gr_chosen */
		int32 ndsets,            /* # of images in the file */
		int32 *gr_chosen,        /* list of images' indices */
		FILE *fp )
{
   intn  dumpall = FALSE;    /* TRUE when no specific images requested */
   int32 ri_index,           /* index of images in the file */
         ri_count,           /* count of images being printed */
         nt,                 /* number type of an image */
         il,                 /* interlace mode of an image */
         ncomps,             /* number of components in an image */
         dimsizes[MAXRANK],  /* dimension sizes of an image */
         ri_id = FAIL,       /* image id, always reset to FAIL when not used */
         nattrs;             /* number of attributes assigned to an image */
   char  name[MAXNAMELEN],   /* name of an image */
         curr_file_name[MAXFNLEN]; /* current input (hdf) file's name */
   intn  status,             /* status returned from a routine */
         ret_value = SUCCEED;/* return value of printGR_ASCII */

   /* temp. name for curr input file name for ease of use */
   HDstrcpy( curr_file_name, dumpgr_opts->ifile_name );

   /* default content option to DDATA when output is binary because
      no header info will be printed in binary format */
   dumpgr_opts->contents = DDATA;

   /* when there are no images specified, dumper dumps all images */
   if (num_ri_chosen == (NO_SPECIFIC))
      dumpall = TRUE;
   else
      sort(gr_chosen, num_ri_chosen); /* otherwise, sort the list */

   /* for each valid index, display the image's data
      if the user request to dump all images or if there are more
      requested images to be processed */
   ri_count = 0;  /* no requested image has been processed yet */
   for (ri_index = 0; ri_index < ndsets /* validate index */
        && ( dumpall                /* either all images are dumped */
        || ri_count < num_ri_chosen); /* or more requested images */
        ri_index++)
   {
      /* if the user neither requests dump all nor the current images */
      if ((!dumpall) && (ri_index != gr_chosen[ri_count]))
         continue; /* go to the next image in the file */

      ri_count++;  /* count the # of images being processed */

      /* Reset variables. */
      HDmemset(dimsizes, 0, sizeof(int32) * MAXRANK);

      /* get access to the current image */
      ri_id = GRselect(gr_id, ri_index);
      if (ri_id == FAIL) /* to the next image */
         ERROR_CONT_2( "in %s: GRselect failed for %d'th RI", 
			"printGR_BINARY", (int)ri_index );

      /* get image's information */
      status = GRgetiminfo(ri_id, name, &ncomps, &nt, &il, dimsizes, &nattrs);
      if( status == FAIL )
      {
         fprintf( stderr, "in %s: GRgetiminfo failed for %d'th RI",
			"printGR_BINARY", (int)ri_index );

	 /* end access to the current image before going to the next one */
         if( GRendaccess(ri_id) == FAIL )    
            fprintf( stderr, "in %s: GRendaccess failed for %d'th RI",
			"printGR_BINARY", (int)ri_index );
         ri_id = FAIL;  /* reset image id */
         continue; /* to the next image */
      }

      /* print image palette's data */
      if( dumpgr_opts->print_pal )
      {
         /* Note: currently only 1 pal assigned to an image, 2nd arg. */
         status = print_Palette( ri_id, 1, fp, dumpgr_opts );
         if ( status == FAIL )
            ERROR_BREAK_2( "in %s: Printing image's palette failed for %d'th RI",
			"printGR_BINARY", (int)ri_index, FAIL );
      }

      /* output data in binary format if palette printing is not requested  */
      else if (ncomps > 0 && dimsizes[0] != 0)
      {
         /* print the current image's data */
         status = grdumpfull(ri_id, dumpgr_opts, ncomps, dimsizes, nt, fp);
         if ( status == FAIL )
            ERROR_BREAK_2( "in %s: Printing image's data failed for %d'th RI",
			"printGR_BINARY", (int)ri_index, FAIL );
      }

      /* end access to the current image */
      if (FAIL == GRendaccess(ri_id))
         fprintf(stderr,"in %s: GRendaccess failed for %d'th RI",
			"printGR_BINARY", (int)ri_index );
      ri_id = FAIL;  /* reset image id */
   }  /* for ndsets */

   return ret_value;
} /* end of printGR_BINARY */

/* closeGR combines the processes of GRend, Hclose, freeing the list
   of numbers, and resetting all ids after validating the ids first.
   When either GRend or Hclose fails, closeGR prints an informative
   message then resetting the ids as normal since these failures are
   highly unlikely and since the files are opened as read-only, it's 
   safe to go on. */
void closeGR(
    int32 *file_id,     /* will be returned as a FAIL */
    int32 *gr_id,       /* will be returned as a FAIL */
    int32 **gr_chosen ) /* will be returned as a NULL */
{
   if( *gr_id != FAIL )
   {
      if (FAIL == GRend(*gr_id))
         fprintf(stderr,"in closeGR: GRend failed for the current file\n" );
      *gr_id = FAIL; /* reset */
   }

   if( *file_id != FAIL )
   {
      if (FAIL == Hclose(*file_id))
         fprintf(stderr,"in closeGR: Hclose failed for the current file\n" );
      *file_id = FAIL; /* reset */
   }

   if( *gr_chosen != NULL )
   {
      HDfree( *gr_chosen );
      *gr_chosen = NULL;
   } /* end if */

} /* end of closeGR */

intn 
dgr(	dump_info_t *dumpgr_opts, 
	intn         curr_arg, 
	intn         argc, 
	char        *argv[] )
{
    int32 file_id = FAIL,	/* current hdf file id */
          gr_id = FAIL,		/* interface id */
         *gr_chosen = NULL,	/* index list of requested images */
          num_ri_chosen,	/* # of requested images */
          nglb_attrs,		/* # of file attributes */
          ndsets;		/* # of images in the file */
    FILE *fp;			/* output file pointer */
    char  file_name[MAXFNLEN];	/* current hdf file name */
    intn  index_error=FALSE,	/* indicate an error in getting index list */
          status,		/* status returned from a called routine */
          ret_value = SUCCEED;	/* returned value of dgr */

   /* check for missing input file name */
   if( curr_arg >= argc )
      ERROR_GOTO_0( "Missing input file name.  Please try again" );

   /* going through each input file, open the file, try to compose the list
      of indices of the images in the file that are requested, then read and
      display information and data of each image in the specified manner */
   while (curr_arg < argc)
   {
      intn isHDF = TRUE;  /* FALSE, if current file is not HDF file */

      HDstrcpy(file_name, argv[curr_arg]);   /* get file name */
      HDstrcpy( dumpgr_opts->ifile_name, file_name ); /* record file name */
      curr_arg++;   /* forward the pointer to the current argument */

      /* ensure that file_id, gr_id, and gr_chosen are all reset before
         using because sometimes we have to break out a cycle; this'll
         help avoiding the chore at every one of those instances */ 
      closeGR( &file_id, &gr_id, &gr_chosen ); 

      /* Print an informative message and skip this file if it is not 
         an HDF file */
      isHDF = Hishdf(file_name);
      if (isHDF == FALSE)
      {
         /* if there are no more files to be processed, print error
            message, then returns with FAIL */
         if( curr_arg == argc )
            {ERROR_GOTO_1( "in dgr: %s is not an HDF file", file_name);}
         else /* print message, then continue processing the next file */
            {ERROR_CONT_1( "in dgr: %s is not an HDF file", file_name);}
      }

      /* open current hdf file for processing */
      file_id = Hopen(file_name, DFACC_RDONLY, 0);
      if (file_id == FAIL)
      {
         /* if there are no more files to be processed, print error
            message, then returns with FAIL */
         if( curr_arg == argc )
            {ERROR_GOTO_1( "in dgr: Failure in opening file %s", file_name);}
         else /* print message, then continue processing the next file */
            ERROR_CONT_1( "in dgr: Failure in opening file %s", file_name );
      }

      /* initiate GR interface, if fail, probably something fatal, returns
         with FAIL */
      gr_id = GRstart(file_id);
      if (FAIL == gr_id)
         ERROR_GOTO_1( "in dgr: GRstart failed for file %s", file_name);

      /* BMR: compose the list of indices of RIs to be processed in the current
         file: gr_chosen is the list and return the number of items in it */
      num_ri_chosen = get_RIindex_list(gr_id, dumpgr_opts, &gr_chosen, &index_error);

      /* if there are errors with the given indices, ref#s, or names of
         the requested images, and yields no valid images, then close the
         interface and the input file, and move on to the next file */ 
      if (index_error && num_ri_chosen==0)
         continue;	/* to the next file */

      /* obtain number of images in the file and number of file attributes,
         ndsets will be used to process the images, nglb_attrs will be 
         used to print file attributes */
      status = GRfileinfo(gr_id, &ndsets, &nglb_attrs);
      if (status == FAIL) /* to the next file */
         ERROR_CONT_1( "in dgr: GRfileinfo failed for file %s", file_name);

      fp = stdout;  /* assume that output option is not given */

      /* display images in requested format for the current file */
      switch(dumpgr_opts->file_type)
      {
         case DASCII:       /* ASCII file */
            /* open output file for ASCII or direct to standard output */
            if (dumpgr_opts->dump_to_file)
            {
               fp = fopen( dumpgr_opts->file_name, "w");
               if( fp == NULL )
               {
                  fprintf( stderr, "Unable to open output file %s\n", 
                           dumpgr_opts->file_name );
                  exit(1);          /* terminate hdp */
               }
            }
            /* display the name and global attributes of the current file if 
               data only option is not selected */
            if (dumpgr_opts->contents != DDATA)
            {
               fprintf(fp, "File name: %s \n", file_name );
         
               /* print GR file attributes */
               if( nglb_attrs > 0 )  /* save overhead */
               { 
                  status = print_GRattrs( gr_id, nglb_attrs, fp, dumpgr_opts );
                  if( status == FAIL )
                     ERROR_BREAK_1( "in dgr: print_GRattrs failed for file %s",
                              file_name, FAIL );
               } 
            }
            /* print RIs'data and information as requested */
            if( ndsets > 0 )
            {
               status = printGR_ASCII( gr_id, dumpgr_opts, ndsets, 
                                       gr_chosen, num_ri_chosen, fp );
               if( status == FAIL ) /* to the next file */
                  ERROR_BREAK_1( "in dgr: printGR_ASCII failed for file %s", 
                                    file_name, FAIL );
            }
            else
               if( dumpgr_opts->contents != DDATA )
                  fprintf( stderr, "File %s has no images\n", file_name );

            break; /* ASCII */

         case DBINARY:       /* binary file */
            /* open output file for ASCII or print to screen */
            if (dumpgr_opts->dump_to_file)
            {
               fp = fopen(dumpgr_opts->file_name, "wb");
               if( fp == NULL )
               {
                  fprintf( stderr, "Unable to open output file %s\n", 
                           dumpgr_opts->file_name );
                  exit(1);          /* terminate hdp */
               }
            }

            status = printGR_BINARY( gr_id, dumpgr_opts, num_ri_chosen, ndsets, 
                               gr_chosen, fp );
            if( status == FAIL )
               ERROR_BREAK_1( "in dgr: printGR_BINARY failed for file %s", 
                                 file_name, FAIL );
            break; /* BINARY */

         default:
            printf("Output file type must be either ascii or binary only\n" );
         } /* switch for output format */
                       
      /* cleaning up before going on to the next file */
      closeGR( &file_id, &gr_id, &gr_chosen ); 
   }  /* while more files to process */

   /* close output file only if option -o is given */
   if (dumpgr_opts->dump_to_file)
      fclose(fp);

done:
    if (ret_value == FAIL)
      { /* Failure cleanup  */
      }
    /* Normal cleanup */
    closeGR( &file_id, &gr_id, &gr_chosen );

    return ret_value;
}	/* dgr */

intn 
do_dumpgr(intn        curr_arg, 
          intn        argc, 
          char       *argv[], 
          intn        help)
{
   dump_info_t dumpgr_opts;	/* dumpgr options */
   intn status, ret_value = SUCCEED;

   /* initialize the structure that holds user's options and inputs */
   init_dump_opts(&dumpgr_opts);

   /* command line: hdp help */
   if( help == TRUE )
   {
      dumpgr_usage(argc, argv);
      goto done;
   }		/* end if */

   /* incomplete command */
   if( curr_arg >= argc )
   {
      dumpgr_usage(argc, argv);
      ERROR_GOTO_0( "in do_dumpgr: command is incomplete");
   }		/* end if */

   /* parse the user's command and store the inputs in dumpgr_opts */
   status = parse_dumpgr_opts( &dumpgr_opts, &curr_arg, argc, argv );
   if( status == FAIL )
   {
      dumpgr_usage(argc, argv);
      ERROR_GOTO_0( "in do_dumpgr: parse_dumpgr_opts is unable to parse command");
   }

   /* display data and information as specified in dumpgr_opts */
   status = dgr( &dumpgr_opts, curr_arg, argc, argv );
   if( status == FAIL )
      ERROR_GOTO_0( "in do_dumpgr: dgr failed" );

done:
   if (ret_value == FAIL)
   { /* Failure cleanup */
   }
    /* Normal cleanup */

   /* free the lists for given indices, ref#s, and names if
      they had been allocated */
   free_num_list(dumpgr_opts.by_index.num_list );
   free_num_list(dumpgr_opts.by_ref.num_list );
   free_str_list(dumpgr_opts.by_name.str_list, dumpgr_opts.by_name.num_items);

   return ret_value;
}  /* end do_dumpgr() */
