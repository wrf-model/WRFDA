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
static char RcsId[] = "@(#)$Revision: 1.51 $";
#endif

/* $Id: hdp_vd.c,v 1.51 2003/12/10 21:14:02 epourmal Exp $ */

#include "hdp.h"
#ifndef MIPSEL
#include <math.h>
#endif /* MIPSEL */

void 
dumpvd_usage(intn argc, 
             char *argv[])
{
   printf("Usage:\n");
   printf("%s dumpvd [-a|-i <indices>|-r <refs>|-n <names>|-c <classes>|-f <f1, f2,..>] [-dhv] [-o <filename>] [-bx] <filelist>\n", argv[0]);
   printf("\t-a\tDump all VDs in the file (default)\n");
   printf("\t-i <indices>\tDump the VDs at positions listed in <indices>\n");
   printf("\t-r <refs>\tDump the VDs with reference number listed in <refs>\n");
   printf("\t-n <names>\tDump the VDs with name listed in <names>\n");
   printf("\t-c <classes>\tDump the VDs with class listed in <classes>\n");
   printf("\t-f <f1, f2,..> \tDump based on fields in vdata header\n");
   printf("\t-d\tDump data only, no tag/ref, formatted to input to hp2hdf\n");
   printf("\t-h\tDump header only, no annotation for elements nor data\n");
   printf("\t-v\tDump everything including all annotations (default)\n");
   printf("\t-o <filename>\tOutput to file <filename>\n");
   printf("\t-b\tBinary format of output\n");
   printf("\t-x\tAscii text format of output (default)\n");
   printf("\t<filelist>\tList of hdf file names, separated by spaces\n");
}	/* end dumpvd_usage() */

intn 
parse_dumpvd_opts(dump_info_t *dumpvd_opts, 
                  intn *curr_arg, 
                  intn  argc,
                  char *argv[], 
                  char *flds_chosen[MAXCHOICES],
                  int  *dumpallfields)
{
   int32       i, lastItem;
   char       *tempPtr, *ptr;

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
             dumpvd_opts->filter = DALL;

             /* indicate that no specific vdata requested, will dump all */
             dumpvd_opts->num_chosen = NO_SPECIFIC;
             (*curr_arg)++;
             break;

         case 'i':	/* dump by index */
             dumpvd_opts->filter |= DINDEX;  /* set bit DINDEX */
             (*curr_arg)++;

             /* parse and store the given indices in structure by_index */
             parse_number_opts( argv, curr_arg, &dumpvd_opts->by_index);
             (*curr_arg)++;
             break;

         case 'r':	/* dump by reference */
             dumpvd_opts->filter |= DREFNUM; /* set bit DREFNUM */
             (*curr_arg)++;

             /* parse and store the given ref numbers in structure
by_ref */
             parse_number_opts( argv, curr_arg, &dumpvd_opts->by_ref);
             (*curr_arg)++;
             break;

         case 'n':	/* dump by names */
             dumpvd_opts->filter |= DNAME;   /* set bit DNAME */
             (*curr_arg)++;

             /* parse and store the given names in structure by_name */
             parse_string_opts( argv, curr_arg, &dumpvd_opts->by_name);
             (*curr_arg)++;
             break;

         case 'c':	/* dump by class */
             dumpvd_opts->filter |= DCLASS;   /* set bit DCLASS */
             (*curr_arg)++;

             /* parse and store the given classes in structure by_class */
             parse_string_opts( argv, curr_arg, &dumpvd_opts->by_class);
             (*curr_arg)++;
             break;

         case 'f':	/* dump a subset of the fields */
             if (dumpvd_opts->filter == DALL) /* not necessary to set this */
                dumpvd_opts->filter = DFIELDS;/* leave it here anyway */
             *dumpallfields = 0;  /*???*/
             (*curr_arg)++;

             lastItem = 0;
             ptr = argv[*curr_arg];
             for (i = 0; !lastItem; i++)
             {
                tempPtr = HDstrchr(ptr, ',');
                if (tempPtr == NULL)
                   lastItem = 1;
                else
                   *tempPtr = '\0';
                flds_chosen[i] = (char *) HDmalloc(sizeof(char) * (HDstrlen(ptr)+1));
		CHECK_ALLOC( flds_chosen[i], "flds_chosen[i]", "parse_dumpvd_opts" );

/*
                if (flds_chosen[i] == NULL)
                {
                   fprintf(stderr,"Failure in parse_dumpvd_opts: Not enough memory!\n");
                   exit(1);
                }
*/
                HDstrcpy(flds_chosen[i], ptr);
                ptr = tempPtr + 1;
             }
             flds_chosen[i] = NULL;
             (*curr_arg)++;
             break;

         case 'd':	/* dump data only */
                dumpvd_opts->contents = DDATA;
                (*curr_arg)++;
                break;

         case 'h':	/* no annotations nor data */
                dumpvd_opts->contents = DHEADER;
                (*curr_arg)++;
                break;

         case 'v':	/* dump all info */
                dumpvd_opts->contents = DVERBOSE;
                (*curr_arg)++;
                break;

         case 'o':	/* specify output file */
             dumpvd_opts->dump_to_file = TRUE;

             /* Get file name */
             HDstrcpy(dumpvd_opts->file_name, argv[++(*curr_arg)]);

             (*curr_arg)++;
             break;

         case 'b':   /* dump data in binary */
             dumpvd_opts->file_type = DBINARY;
             (*curr_arg)++;
             break;

         case 'x':   /* dump data in ascii, also default */
             dumpvd_opts->file_type = DASCII;
             (*curr_arg)++;
             break;

         default:	/* invalid dumpvd option */
                printf("Warning: Invalid dumpvd option %s\n", argv[*curr_arg]);
                return (FAIL);
            }	/* end switch */
      }		/* end while */

   /* add the number of vdatas requested by index, by ref#, and by name
      to have a total number of requested vdatas */
   dumpvd_opts->num_chosen = dumpvd_opts->by_index.num_items +
                             dumpvd_opts->by_ref.num_items +
                             dumpvd_opts->by_name.num_items +
                             dumpvd_opts->by_class.num_items;

    return (SUCCEED);
}	/* end parse_dumpvd_opts */

/* VSref_index returns the index of a vdata given the vdata's ref# 
   or returns FAIL, if the ref# is not found */
int32 
VSref_index(int32 file_id, 
            int32 vd_ref)
{
   int32  find_ref = -1;
   int    index    = 0; /* index is zero based? */
   int32  ret_value = FAIL;   

   while ((find_ref = VSgetid(file_id, find_ref)) != FAIL)
   {
      if (find_ref == vd_ref)
      {
         ret_value = index;
         goto done; /* found, done */
      }
      index++;
   }

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
    /* Normal cleanup */
    
    return ret_value;
}	/* VSref_index */

/* VSstr_index returns the index of a vdata given the vdata's name or
   class, if the name is not found, and returns FAIL, otherwise.  */
int32 
VSstr_index(int32 file_id, 
            char *filter_str, /* searched vd's name or class */
            int   is_name, /* TRUE if searching for name, FALSE if class */
            int32 *find_ref, /* current ref#, will return next found */
            int32 *index)  /* index of the vdata w/ref# *find_ref */
{
   int32  vdata_id = FAIL;
   char   vdata_name[MAXNAMELEN];
   int32  ret_value = SUCCEED;

   /* starting from the ref# *find_ref, search for the vdata having a
      name or class the same as the given string filter_str; when no
      more vdata to search, return FAIL */
   while ((*find_ref = VSgetid(file_id, *find_ref)) != FAIL)
   {
      vdata_id = VSattach(file_id, *find_ref, "r");
      if (FAIL == vdata_id)
	 ERROR_GOTO_2( "in %s: VSattach failed for vdata with ref#=%d",
		"VSstr_index", (int)*find_ref );

      /* if the string searched is a vdata's name */
      if (is_name)
      {
         if (FAIL == VSgetname(vdata_id, vdata_name))
	    ERROR_GOTO_2( "in %s: VSgetname failed for vdata with ref#=%d",
		"VSstr_index", (int)*find_ref );
      }
      /* or the string searched is a vdata's class */
      else
      {
         if (FAIL == VSgetclass(vdata_id, vdata_name))
	 ERROR_GOTO_2( "in %s: VSgetclass failed for vdata with ref#=%d",
		"VSstr_index", (int)*find_ref );
      }

      if (FAIL == VSdetach(vdata_id))
	 ERROR_GOTO_2( "in %s: VSdetach failed for vdata with ref#=%d",
		"VSstr_index", (int)*find_ref );

      /* if the vd's name or vd's class is the given string, return the
         index of the vdata found */
      if (HDstrcmp(vdata_name, filter_str) == 0)
      {
             /* store the current index to return first */
         ret_value = (*index);
            /* then increment index for next vdata - same class vdatas*/
         (*index)++;
         goto done; /* succeeded */
      }
      else
	 ret_value = FAIL;

      /* Note: in either case, increment the index for the next vdata */
      (*index)++;
   } /* end while getting vdatas */

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
    /* Normal cleanup */
    
    return ret_value;
} /* VSstr_index() */

/* compose the list of indices of the requested vdatas although some
   vdatas are requested by ref# or name.
   The routine returns:
        - the number of vdatas to be processed, or
        - NO_SPECIFIC if all vdatas are to be processed, or
        - 0 if none.
   If there are any errors, the parameter index_error will return TRUE */
int32
choose_vd(dump_info_t * dumpvd_opts, 
          int32 **vd_chosen,
          int32 file_id, 
          int *index_error)
{
   int32  i,
          index,
          find_ref,
          number,
          num_vd_chosen = dumpvd_opts->num_chosen,
          vd_count = 0; 
   filter_t filter = dumpvd_opts->filter; /* temporary name */
   intn   ret_value = 0;

   /* if no specific vdatas are requested, return vdata count as 
      NO_SPECIFIC (-1) */
   if( filter == DALL )
      HGOTO_DONE( NO_SPECIFIC );

   /* if specific vdatas were requested, allocate space for the array
      of indices */
   if (num_vd_chosen > 0)
      alloc_index_list( vd_chosen, num_vd_chosen );

   /* else, no chosen vdatas but filter is not DALL, it shouldn't be this
      combination, return vdata count as NO_SPECIFIC to dump all */
   else
      HGOTO_DONE( NO_SPECIFIC );

   /* if there are some vdatas requested by index, store the indices in
      the array provided by the caller */
   if( filter & DINDEX )
      for (i = 0; i<dumpvd_opts->by_index.num_items; i++)
      {
         (*vd_chosen)[vd_count] = dumpvd_opts->by_index.num_list[i];
         vd_count++;
      }
          
   /* if there are some vdatas requested by ref#, store the indices in
      the array provided by the caller */
   if( filter & DREFNUM )
      for (i = 0; i<dumpvd_opts->by_ref.num_items; i++)
      {
         index = VSref_index(file_id, dumpvd_opts->by_ref.num_list[i]);
         if (index == FAIL)
         {
            printf("Vdata with reference number %d: not found\n", 
                                   (int)dumpvd_opts->by_ref.num_list[i]);
            *index_error = TRUE; /* index error */
         }
         else
         {
            (*vd_chosen)[vd_count] = index;
            vd_count++;
         }
      }	/* for */

   /* if there are some vdatas requested by name, store the indices in
      the array provided by the caller */
   if( filter & DNAME )
      for (i = 0; i<dumpvd_opts->by_name.num_items; i++)
      {
         find_ref = -1;
         number = 0;
         index = VSstr_index(file_id, dumpvd_opts->by_name.str_list[i], 1, &find_ref, &number);
         if (index == FAIL)
         {
            printf("Vdata with name %s: not found\n", 
                         dumpvd_opts->by_name.str_list[i]);
            *index_error = TRUE;
         }
         else
         {
            (*vd_chosen)[vd_count] = index;
            vd_count++;
         }
      }	/* for */

   /* if there are some vdatas requested by class, store the indices in
      the array provided by the caller */
   if( filter & DCLASS )
      for (i = 0; i < dumpvd_opts->by_class.num_items; i++)
      {
         int32       found = 0;

         find_ref = -1;
         number = 0;
         while ((index = VSstr_index(file_id, dumpvd_opts->by_class.str_list[i], 0, &find_ref, &number)) != FAIL)
         {
            if (vd_count < num_vd_chosen)
               (*vd_chosen)[vd_count] = index;
            else
            {
               *vd_chosen = (int32 *) HDrealloc(*vd_chosen,sizeof(int32) * (num_vd_chosen+1));
               if (*vd_chosen == NULL)
               {
                  fprintf(stderr,"Failure in choose_vd: Memory re-allocation error\n");
                  exit(1);
               }		/* end if */
               (*vd_chosen)[vd_count] = index;
               num_vd_chosen++;
            }
            vd_count++;
            found = 1;
         }
         if (!found)
         {
            printf("Vdata with class %s: not found\n", dumpvd_opts->by_class.str_list[i]);
            *index_error = TRUE; /* index error */
         }
      }	/* for */

      if( filter == DFIELDS )	/* Don't have to worry about which chosen fields yet. */
      {}

   ret_value = vd_count; /* actual number of vdatas to be processed; might
    be different from dumpvd_opts->num_chosen because of the non-unique
    class name */
done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
    /* Normal cleanup */

    return ret_value;
}	/* choose_vd */

void printHeader( 
		FILE* fp,
		char* fldstring,
		char* fields,
		vd_info_t* curr_vd )
{ 
   fprintf(fp, "Vdata: %d\n", (int) curr_vd->index );
   if( curr_vd->tag == FAIL )	/* print vdata tag */
      fprintf(fp, "   tag = <Undefined>; ");
   else
      fprintf(fp, "   tag = %d; ", (int) curr_vd->tag);

   /* print reference number without checking because it's from
      VSgetid and has been checked by the calling routine */
   fprintf(fp, "reference = %d;\n", (int) curr_vd->ref);

   if( curr_vd->nvf == FAIL ) /* print number of records in the vdata */
      fprintf(fp, "   number of records = <Undefined>; ");
   else
      fprintf(fp, "   number of records = %d;", (int) curr_vd->nvf);

   if( curr_vd->interlace == FAIL ) /* print interlace mode */
      fprintf(fp, "   interlace = <Undefined>;\n");
   else
      if( curr_vd->interlace == 0 )
	  fprintf(fp, " interlace = FULL_INTERLACE (0);\n");
      else if( curr_vd->interlace == 1 )
	  fprintf(fp, "   interlace = NO_INTERLACE;\n");
      else
	  fprintf(fp, "   interlace = <Unknown interlace mode (%d)>;\n", 
				(int) curr_vd->interlace);
                     
   /* print the list of field names of the vdata if it's available */
   /* The list of field names can be very long and would look very 
      messy when being displayed if it were to be dumped out at once. 
      print_fields displays a list in a nice way even if the list is 
      long.  The second parameter specifies how the field name list 
      begins; it's needed because dumpvg also uses this routine and 
      has different indentation format than dumpvd */
   print_fields( fields, "   fields = ", fp );

   fprintf(fp, "   record size (in bytes) = %d;\n", (int)curr_vd->vsize);

   if( curr_vd->name[0] == '\0' ) /* print vdata name */
      fprintf(fp, "   name = <Undefined>; ");
   else
      fprintf(fp, "   name = %s;", curr_vd->name);

   /* print class name - Note that vdclass can be NULL */
   if( curr_vd->class[0] == '\0' || curr_vd->class == NULL )
      fprintf(fp, " class = <Undefined>;\n");
   else
      fprintf(fp, " class = %s;\n", curr_vd->class);

} /* end of printHeader */

intn getFieldIndices( 
		char* fields, 
		char *flds_chosen[MAXCHOICES],
		int32* flds_indices )
{
   int32 lastItem = 0;  /* whether the last field in the list 'fields' is reached */
   int32 fld_name_idx;  /* index for the list of field names */
   int32 idx = 0;	      /* index for the array of field indices */
   char  *tempPtr = NULL;  /* temp ptr to mark the end of a field name in the list */
   char  *ptr = NULL;	/* used to forward to next field name in the field name list */
   char  fldstring[MAXNAMELEN]; /* holds a field name extracted from field name list */
   int32 i;
   intn  flds_match = 0;
   intn  ret_value = SUCCEED;
#if defined (MAC) || defined (macintosh) || defined (SYMANTEC_C) || defined(__APPLE__)
	/* macintosh cannot handle >32K locals */
   char *tempflds = (char *)HDmalloc(VSFIELDMAX*FIELDNAMELENMAX* sizeof(char));
   CHECK_ALLOC( tempflds, "tempflds", "getFieldIndices" );
#else /* !macintosh */
   char   tempflds[VSFIELDMAX*FIELDNAMELENMAX];
#endif /* !macintosh */    

   /* make copy of the field name list retrieved by VSinquire to use 
      in processing the field names */
   HDstrcpy(tempflds, fields);

   ptr = tempflds;		/* used to forward the field names */

   i = (-1);	/* dummy? */
   HDmemfill(flds_indices, &i, sizeof(int32), MAXCHOICES);

   /* Extract each field name from the list of fields of the 
      current record. */
   for (fld_name_idx = 0; !lastItem; fld_name_idx++)
   {
      /* look for a comma in the fields */
      tempPtr = HDstrchr(ptr, ','); 

      /* if no comma is found, that means the last field name is reached */
      if (tempPtr == NULL)
         lastItem = 1;	/* set flag */

      /* otherwise, add null to end the extracted field name */
      else
         *tempPtr = '\0';

      /* extract that field into fldstring */
      HDstrcpy(fldstring, ptr);

      /* forward the pointer to the next field name in the list */
      ptr = tempPtr + 1;

      /* Compare the extracted field name with each of the names
         of the fields having been chosen. */
      for( i = 0; flds_chosen[i] != NULL; i++)
      {
         if (!HDstrcmp(flds_chosen[i], fldstring))
         {
            flds_indices[idx] = fld_name_idx;
            idx++;
            flds_match = 1;
         }
      }		/* for (i...) */
   }	/* for (fld_name_idx...) */

/* free dynamic space if on MAC */
#if defined (MAC) || defined (macintosh) || defined (SYMANTEC_C) || defined(__APPLE__)
   if(tempflds != NULL)
   {
      HDfree(tempflds);
      tempflds = NULL;
   } 
#endif /* macintosh */

   /* return the flag indicating whether any given fields exist */
   ret_value = flds_match;

   return ret_value;
} /* end of getFieldIndices */

intn
dumpvd_ascii(dump_info_t * dumpvd_opts, 
             int32 file_id,
             const char  *file_name,
	     FILE* fp,
             int32 num_vd_chosen, 
             char *flds_chosen[MAXCHOICES],
             int32 *vd_chosen,
             int dumpallfields)
{
   int32       flds_indices[MAXCHOICES];
   int32       i;
   int32       vd_chosen_idx;	/* index for vd_chosen array */
   int32       nvf;
   int32       interlace;
   int32       vsize;
   int32       vdata_ref = -1;
   int32       vdata_tag;
   char        vdclass[VSNAMELENMAX];
   char        vdname[VSNAMELENMAX];
   char        fldstring[MAXNAMELEN];
   intn        dumpall = 0;
   file_type_t ft = DASCII;
   vd_info_t   curr_vd;
   int32       vd_id = FAIL;
   int32       an_handle   = FAIL;
   intn        status = SUCCEED, ret_value = SUCCEED;
#if defined (MAC) || defined (macintosh) || defined (SYMANTEC_C) || defined(__APPLE__)
	/* macintosh cannot handle >32K locals */
   char *fields = (char *)HDmalloc(VSFIELDMAX*FIELDNAMELENMAX* sizeof(char));
   CHECK_ALLOC( fields, "fields", "dumpvd_ascii" );

#else /* !macintosh */
   char   fields[VSFIELDMAX*FIELDNAMELENMAX];
#endif /* !macintosh */    

   if (dumpvd_opts->contents != DDATA)
   {
      fprintf(fp, "File name: %s \n\n", file_name);

      /* print file annotations */
      status = print_file_annotations( file_id, file_name );
      if( status == FAIL )
      ERROR_GOTO_2( "in %s: Failure in printing file annotations for file %s",
			"dumpvd_ascii", file_name );
   }
   vd_chosen_idx = 0;	/* start at the beginning of vd_chosen */

   /* Determine if all VDs are to be dumped out. */
   if (num_vd_chosen <= 0)	/* If so, set the corresponding flag. */
       dumpall = 1;
   else
       /* Otherwise, sort the indices of the chosen VDs in increasing 
          order so that they will be dumped out in such order */
      sort(vd_chosen,num_vd_chosen);

   /* Examine each VD, which is identified by its ref# returned by VSgetid */
   for (i = 0; 
	   (vdata_ref = VSgetid(file_id, vdata_ref)) != FAIL
	&& (dumpall!=0 || vd_chosen_idx < num_vd_chosen); 
	i++)
   {
      intn  data_only;	    /* indicates whether to print data only */
      intn  flds_match = 0;  /* indicates whether any requested fields exist,
		or if no field requested, set to 1 means dump all fields */
      char sep[2];	    /* the character that is used to separate 2 fields */

      /* Only dump the info of the chosen VDs or all of the VDs if none
         has been selected. */
      if ((!dumpall) && (i != vd_chosen[vd_chosen_idx]))
          continue; /* skip */

      vd_chosen_idx++;	/* One vdata has been located; so go to the next one in 
                   the array. */
      vd_id = VSattach(file_id, vdata_ref, "r");
      if (vd_id == FAIL) /* continue to the next vdata */
	 ERROR_CONT_2( "in %s: VSattach failed for vdata_ref#=%d", 
                        "dumpvd_ascii", (int) vdata_ref );

      /* Note: each of the parameters retuned by a query routine below 
	 must be checked before being used */

      /* Retrieves general information about the vdata.  Note that NULL is
	 passed in for vdata's record size because it is not needed and 
	 attempting to retrieve it causes failure when the vdata doesn't
	 have any fields defined.  (bug #626) - BMR Mar 12, 02 */
      if (FAIL == VSinquire( vd_id, &nvf, &interlace, fields, NULL, vdname ))
	 ERROR_CONT_END( "in %s: VSinquire failed for vdata with ref#=%d", 
                        "dumpvd_ascii", (int) vdata_ref, vd_id );

      /* Get the HDF size of the specified fields of the vdata; VShdfsize 
	 returns 0 if there are no fields previously defined */
         vsize = VShdfsize( vd_id, fields );
         if (vsize == FAIL)
             ERROR_CONT_END( "in %s: VShdfsize failed for vdata with ref#=%d",
			 "dumpvd_ascii", (int) vdata_ref, vd_id );

      if (FAIL == (vdata_tag = VSQuerytag(vd_id)))
         ERROR_CONT_END( "in %s: VSQuerytag failed for vdata with ref#=%d", 
                        "dumpvd_ascii", (int) vdata_ref, vd_id );

      if (FAIL == VSgetclass(vd_id, vdclass))
         ERROR_CONT_END( "in %s: VSgetclass failed for vdata with ref#=%d",
                        "dumpvd_ascii", (int) vdata_ref, vd_id );

      /* If one or more fields were specified by the user, then find out
         what they were, determine their corresponding indices in 
         "fields", and store these indices in the array "flds_indices" so
         that they can be used to determine whether a field should be
         dumped later on. */
      if (flds_chosen[0] != NULL)
	 flds_match = getFieldIndices( fields, flds_chosen, flds_indices );

      /* If no fields were chosen, all fields are to be dumped out, and
         so all fields match. */
      else /* if (flds_chosen[0] == NULL) */
         flds_match = 1;

      if (flds_match)
      {
         switch (dumpvd_opts->contents)
         {
             case DVERBOSE: /* dump all information */
             case DHEADER:	 /* header only, no attributes, annotations 
                                    or data */
		/* store the vdata info into the vd_info_t struct for convenience */
		curr_vd.index = i;        	/* vdata index */
		curr_vd.nvf = nvf;        	/* number of records in the vdata */
		curr_vd.interlace = interlace;  /* interlace mode of the vdata */
		curr_vd.vsize = vsize;    	/* record size of the vdata */
		curr_vd.ref = vdata_ref;  	/* vdata ref# */
		curr_vd.tag = vdata_tag;  	/* vdata tag */
		HDstrcpy( curr_vd.class, vdclass );   /* vdata class */
		HDstrcpy( curr_vd.name, vdname );     /* vdata name */
		printHeader( fp, fldstring, fields, &curr_vd );

                /* proceed to printing data if not printing header only */
                if (dumpvd_opts->contents != DHEADER)
                {
                   /* dump vdata attributes */
                   status = dumpattr(vd_id, _HDF_VDATA, 1, ft, fp);
                   if( FAIL == status )
                      ERROR_BREAK_3( "in %s: %s failed for attributes of vdata with ref#=%d",
                                    "dumpvd_ascii", "dumpattr", (int) vdata_ref, FAIL );

                   /* dump all the annotations for this vdata */
                   status = print_data_annots( file_id, file_name, vdata_tag, vdata_ref );
                   if( FAIL == status )
                      ERROR_BREAK_3( "in %s: %s failed for attributes of vdata with ref#=%d",
                           "dumpvd_ascii", "print_data_annots", (int) vdata_ref, FAIL );

                   /* BMR - 6/30/98 to fix bug #236: if no fields are defined or 
		      no data is written, break out and don't fall through */
                   if ( fields[0] == '\0' || nvf == 0 )
                   {
                      fprintf(fp, "   No data written\n\n");
                      break;
                   }
                }
                else /* only header, no attributes, annotations or data */
                    break; /* break out and don't fall through */

                      /* note that we fall through if not printing header only */
             case DDATA:	/* data only */
                if (dumpvd_opts->contents == DDATA)
                {
                   data_only = 1;
                   HDstrcpy(sep, "");
                }
                else
                {
                   data_only = 0;
                   HDstrcpy(sep, ";");
                }

                /* Only the chosen or all fields will be dumped out. */
                status = dumpvd(vd_id, ft, data_only, fp, sep, flds_indices, dumpallfields);
                if( FAIL == status )
                   ERROR_BREAK_2( "in %s: dumpvd failed for vdata with ref#=%d",
                                 "dumpvd_ascii", (int) vdata_ref, FAIL );
		break; /* out of DDATA */

             default:
                printf("dumping vdata in file %s, unknown option %d\n",
			file_name, dumpvd_opts->contents);
         }   /* switch */
      }

      if (FAIL == VSdetach(vd_id))
         fprintf(stderr,"in %s: VSdetach failed on vdata with ref#=%d", 
                        "dumpvd_ascii", (int) vdata_ref );
      /* just simply goes to the next vdata */

      vd_id = FAIL; /* reset */

   }	/* for each vdata */

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
          if (an_handle != FAIL)
              ANend(an_handle);

          if (vd_id != FAIL)
              VSdetach(vd_id);
      }
    /* Normal cleanup */
#if defined (MAC) || defined (macintosh) || defined (SYMANTEC_C) || defined(__APPLE__)
   if(fields != NULL)
   {
      HDfree(fields);
      fields = NULL;
    } 
#endif /* macintosh */

    return ret_value;
} /* dumpvd_ascii() */


intn
dumpvd_binary(dump_info_t * dumpvd_opts, 
              int32 file_id,
              const char  *file_name,
	      FILE* fp,
              int32 num_vd_chosen, 
              char *flds_chosen[MAXCHOICES],
              int32 *vd_chosen,
              int dumpallfields)
{
   int32       flds_indices[MAXCHOICES];
   int32       i, vd_chosen_idx;
   int32       nvf;
   int32       interlace;
   int32       vdata_ref = -1;
   char        vdname[VSNAMELENMAX];
   intn        dumpall = 0;
   file_type_t ft = DBINARY;
   int32       vd_id = FAIL;
   intn        status;
   intn        ret_value = SUCCEED;
#if defined (MAC) || defined (macintosh) || defined (SYMANTEC_C) || defined(__APPLE__)
	/* macintosh cannot handle >32K locals */
   char *fields = (char *)HDmalloc(VSFIELDMAX*FIELDNAMELENMAX* sizeof(char));

   CHECK_ALLOC( fields, "fields", "dumpvd_binary" );

#else /* !macintosh */
    char        fields[VSFIELDMAX*FIELDNAMELENMAX]; 
#endif /* !macintosh */    

    vd_chosen_idx = 0;	/* "vd_chosen_idx" is used to index the array of "vd_chosen". */

    /* Determine if all VDs are to be dumped out. */
    if (num_vd_chosen <= 0)	/* If so, set the corresponding flag. */
        dumpall = 1;
    else
      {
        /* Otherwise, sort the indices of the chosen VDs in increasing 
                       order so that they will be dumped out in such order. */
        sort(vd_chosen,num_vd_chosen);
      }

    /* Examine each VD. */
   for (i = 0; 
         (vdata_ref = VSgetid(file_id, vdata_ref)) != -1 
         && (dumpall != 0 || vd_chosen_idx < num_vd_chosen); 
         i++)
   {
      int  data_only, flds_match = 0;
      char sep[2];	/* character used to separate fields */

      /* Only dump the info of the chosen VDs or all of the VDs if none
         has been selected. */
      if ((!dumpall) && (i != vd_chosen[vd_chosen_idx]))
         continue; /* skip */

      vd_chosen_idx++;	/* One vdata has been located; so go to the next one in 
                   the array.  ???*/

      /* Select the next vdata for processing */
      vd_id = VSattach(file_id, vdata_ref, "r");
      if (vd_id == FAIL)  /* continue to the next vdata */
         ERROR_CONT_2( "in %s: VSattach failed for vdata with ref#=%d", 
                        "dumpvd_binary", (int) vdata_ref );

      status = VSinquire(vd_id, &nvf, &interlace, fields, NULL, vdname);
      if( FAIL == status ) /* end access to vd_id and cont. to next vdata */
         ERROR_CONT_END( "in %s: VSinquire failed for vdata with ref#=%d", 
                        "dumpvd_binary", (int) vdata_ref, vd_id );

      /* BMR - 7/1/98 realized while fixing bug #236.
         Skip binary printing if the vdata is empty */
      if (fields[0] == '\0' || nvf == 0 )
	 fprintf(stderr,"in %s: Vdata with ref#=%d is empty.\n",
                        "dumpvd_binary", (int) vdata_ref );

      else /* vdata is not empty */
      {
	 /* removed calls to VSQuerytag and VSgetclass - not useful in
	    dumping by binary - BMR 8/23/00) */

         /* If one or more fields were specified by the user, then find out
            what they were, determine their corresponding indices in 
            "fields", and store these indices in the array "flds_indices" so
            that they can be used to determine whether a field should be
            dumped later on. */
         if (flds_chosen[0] != NULL)
	    flds_match = getFieldIndices( fields, flds_chosen, flds_indices );

         /* If no fields were chosen, all fields are to be dumped out, and
            so all fields match. */
         else /* if (flds_chosen[0] == NULL) */
            flds_match = 1;

         if (flds_match)
         {
	    /* BMR: removed the if statement to determine if data_only
	       should be set; set data_only in either case */
            data_only = 1;
            HDstrcpy(sep, "");

            /* Only the chosen or all fields will be dumped out. */
            if (FAIL == dumpvd(vd_id, ft, data_only, fp, sep, flds_indices, dumpallfields))
               ERROR_CONT_END( "in %s: Failure in dumping data for vdata with ref#=%d", 
                              "dumpvd_binary", (int) vdata_ref, vd_id );
         }
      } /* end of if (fields[0] == '\0' || nvf == 0 ) */
                      
      if (FAIL == VSdetach(vd_id))
         fprintf(stderr,"in %s: VSdetach failed on vdata with ref#=%d", 
                        "dumpvd_binary", (int) vdata_ref );

      vd_id = FAIL; /* reset */

   }	/* for each vdata */

   /* Normal cleanup */
#if defined (MAC) || defined (macintosh) || defined (SYMANTEC_C) || defined(__APPLE__)
   if(fields != NULL)
   {
      HDfree(fields);
      fields = NULL;
    } 
#endif /* macintosh */

   return( ret_value );
} /* dumpvd_binary */

/* closeVD combines the processes of Vend, Hclose, freeing the list
   of numbers, and resetting all ids, after validating the ids first.
   When either Vend or Hclose fails, closeVD prints an informative
   message then resetting the ids as normal since these failures are
   highly unlikely and since the files are opened as read-only, it's
   safe to go on. */
void
closeVD(
    int32 *file_id,     /* will be returned as a FAIL */
    int32 **vd_chosen,  /* will be returned as a NULL */
    const char  *curr_file_name )
{
   if( *file_id != FAIL )
   {
      if (FAIL == Vend(*file_id))
         fprintf(stderr,"Failure in closeVD: Vend failed for file %s\n",
                         curr_file_name );
      if (FAIL == Hclose(*file_id))
         fprintf(stderr,"Failure in closeVD: Hclose failed for file %s\n",
                         curr_file_name );
      *file_id = FAIL; /* reset */
   }

   if( *vd_chosen != NULL )
   {
      HDfree( *vd_chosen );
      *vd_chosen = NULL;
   } /* end if */

} /* end of closeVD */

intn 
dvd(dump_info_t * dumpvd_opts, 
    intn curr_arg,
    intn argc, 
    char *argv[], 
    char *flds_chosen[MAXCHOICES], 
    int dumpallfields)
{
    int32       file_id = FAIL;
    char        file_name[MAXFNLEN];
    int32      *vd_chosen = NULL;
    FILE       *fp = NULL;
    int32       num_vd_chosen;
    intn        index_error = 0;
    file_type_t ft;
    intn        status;
    intn        ret_value = SUCCEED;

   /* check for missing input file name */
   if( curr_arg >= argc ) /* goto done with FAIL */
      ERROR_GOTO_0( "Missing input file name.  Please try again.\n" );

   while (curr_arg < argc)
   {	/* Loop until all specified files have been processed */

      intn isHDF = TRUE;  /* FALSE, if current file is not HDF file */

      /* get file name */
      HDstrcpy(file_name, argv[curr_arg]); 

      /* record for later use */
      HDstrcpy( dumpvd_opts->ifile_name, file_name );
      curr_arg++;

      closeVD( &file_id, &vd_chosen, file_name );

      /* Print an informative message and skip this file if it is not
         an HDF file */
      isHDF = Hishdf(file_name);
      if (isHDF == FALSE)
      {
         /* if there are no more files to be processed, print error
            message, then returns with FAIL */
         if( curr_arg == argc )
            {ERROR_GOTO_1( "in dvd: %s is not an HDF file", file_name);}
         else /* print message, then continue processing the next file */
            {ERROR_CONT_1( "in dvd: %s is not an HDF file", file_name);}
      }

      /* open current hdf file with error check, if fail, go to next file */
      file_id = Hopen(file_name, DFACC_READ, 0);
      if (file_id == FAIL)
      {
         /* if there are no more files to be processed, print error
            message, then returns with FAIL */
         if( curr_arg == argc )
            {ERROR_GOTO_1( "in dvd: Failure in opening file %s", file_name);}
         /* otherwise, print message, then continue processing the next file */
         else
            ERROR_CONT_1( "in dvd: Failure in opening file %s", file_name );
      }

      /* initiate VG interface; if fail, probably something fatal, returns
         with FAIL */
      if (FAIL == Vstart(file_id))
         ERROR_GOTO_1( "in dvd: Vstart failed for file %s\n", file_name);

      /* Find out which VDs have been chosen. */
      num_vd_chosen = choose_vd(dumpvd_opts, &vd_chosen, file_id, &index_error);

      /* if there are no valid indices, move on to the next file */
      if (index_error && num_vd_chosen == 0)
         continue;   /* to the next file, closeVG before opening next file
                        takes care of Vend, Hclose, and free vg_chosen */

      ft = dumpvd_opts->file_type;
      fp = stdout;	/* default file pointer to the standard output */
      switch(ft)
      {
          case DASCII:  /*    ASCII file   */

	     /* set output file */
	     if (dumpvd_opts->dump_to_file)
	        fp = fopen(dumpvd_opts->file_name, "w");

             status = dumpvd_ascii(dumpvd_opts, file_id, file_name, fp,
                      num_vd_chosen, flds_chosen, vd_chosen, dumpallfields);
             if( FAIL == status )
                ERROR_BREAK_0( "in dvd: dumpvd_ascii returned failure", FAIL );
             break;
          case DBINARY:   /*  binary file, not fully tested yet  */

    	     /* Get output file name.  */
    	     if (dumpvd_opts->dump_to_file)
                fp = fopen(dumpvd_opts->file_name, "wb");

             status = dumpvd_binary(dumpvd_opts, file_id, file_name, fp,
                      num_vd_chosen, flds_chosen, vd_chosen, dumpallfields);
             if( FAIL == status )
                ERROR_BREAK_0( "in dvd: dumpvd_binary returned failure", FAIL );

             break;
          default:
             printf("dumping vdata, unknown ouput file option \n");
             ret_value = FAIL;
      }    /* switch for output file   */

      if(vd_chosen != NULL)
      {
         HDfree(vd_chosen);
         vd_chosen = NULL;
      } 

      if (dumpvd_opts->dump_to_file)
         fclose(fp);

      if (FAIL == Vend(file_id))
         ERROR_CONT_1( "in dvd: Vend failed on file %s\n", file_name);

      if (FAIL == Hclose(file_id))
         ERROR_CONT_1( "in dvd: Hclose failed on file %s\n", file_name);

      file_id = FAIL; /* reset */

   }	/* while processing files  */

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
          if (file_id != FAIL)
            {
                Vend(file_id);
                Hclose(file_id);
            }

          if(vd_chosen != NULL)
            {
                HDfree(vd_chosen);
                vd_chosen = NULL;
            } 
      }
    /* Normal cleanup */
    
    return ret_value;
}	/* dvd */

/* exported */
intn
do_dumpvd(intn curr_arg, 
          intn argc, 
          char *argv[], 
          intn help)
{
    dump_info_t dumpvd_opts;	/* dumpvd options */
    char       *flds_chosen[MAXCHOICES];
    int         dumpallfields;
    intn status, ret_value = SUCCEED;

    flds_chosen[0] = NULL;
    dumpallfields = 1;

   /* initialize the structure that holds user's options and inputs */
    init_dump_opts(&dumpvd_opts);

    if (help == TRUE)
      {
          dumpvd_usage(argc, argv);
          goto done;
      }		

   /* incomplete command */
   if( curr_arg >= argc )
   {
      dumpvd_usage(argc, argv);
      ERROR_GOTO_0( "in do_dumpvd: command is incomplete");
   }            /* end if */

   /* parse the user's command and store the inputs in dumpvd_opts */
   status = parse_dumpvd_opts(&dumpvd_opts, &curr_arg, argc, argv, flds_chosen, &dumpallfields);
   if( status == FAIL )
   {
      dumpvd_usage(argc, argv);
      ret_value = FAIL; /* return status to caller */
      goto done;  /* skip dvd */
   }

   /* display data and information as specified in dumpvd_opts */
   status = dvd(&dumpvd_opts, curr_arg, argc, argv, flds_chosen, dumpallfields);
   if( status == FAIL )
      ERROR_GOTO_0( "in do_dumpvd: dvd failed" );

  done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
    /* Normal cleanup */

   /* free the lists for given indices, ref#s, names, and classes if
      they had been allocated */
   free_num_list(dumpvd_opts.by_index.num_list );
   free_num_list(dumpvd_opts.by_ref.num_list );
   free_str_list(dumpvd_opts.by_name.str_list, dumpvd_opts.by_name.num_items);
   free_str_list(dumpvd_opts.by_class.str_list, dumpvd_opts.by_class.num_items);

   return ret_value;
}	/* end do_dumpvd() */

