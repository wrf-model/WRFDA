/***************************************************************************e
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
static char RcsId[] = "@(#)$Revision: 1.41 $";
#endif

/* $Id: hdp_vg.c,v 1.41 2003/12/10 21:14:03 epourmal Exp $ */

#include "hdp.h"
#ifndef MIPSEL
#include <math.h>
#endif /* MIPSEL */
#include "vg.h"

/* display the usage of command dumpvg */
void 
dumpvg_usage(intn argc, 
             char *argv[])
{
    printf("Usage:\n");
    printf("%s dumpvg [-a|-i <indices>|-r <refs>|-n <names>|-c <classes>] [-hv] [-o <filename>] <filelist>\n", argv[0]);
    printf("\t-a\tDump all VGs in the file (default)\n");
    printf("\t-i <indices>\tDump the VGs at position listed in <indices>\n");
    printf("\t-r <refs>\tDump the VGs with reference number listed in <refs>\n");
    printf("\t-n <names>\tDump the VGs with name listed in <names>\n");
    printf("\t-c <class>\tDump the VGs with class listed in <classes>\n");
/*  printf("\t-d\tDump data only, no tag/ref, formatted to input to hp2hdf\n");
*/  printf("\t-h\tDump header only, no annotation for elements nor data\n");
    printf("\t-v\tDump everything including all annotations (default)\n");
    printf("\t-o <filename>\tOutput to file <filename>\n");
/*  printf("\t-x\tAscii text format of output (default)\n"); */
    printf("\t<filelist>\tList of hdf file names, separated by spaces\n");
}   /* end dumpvg_usage() */

intn 
parse_dumpvg_opts(dump_info_t *dumpvg_opts, 
                  intn *curr_arg, 
                  intn  argc,
                  char *argv[]) 
{

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
             dumpvg_opts->filter = DALL;

             /* indicate that no specific images requested, will dump all */
             dumpvg_opts->num_chosen = NO_SPECIFIC;
             (*curr_arg)++;
             break;

         case 'i':	/* dump by index */
             dumpvg_opts->filter |= DINDEX;  /* set bit DINDEX */
             (*curr_arg)++;

             /* parse and store the given indices in structure by_index */
             parse_number_opts( argv, curr_arg, &dumpvg_opts->by_index);
             (*curr_arg)++;
             break;

         case 'r':	/* dump by reference */
             dumpvg_opts->filter |= DREFNUM; /* set bit DREFNUM */
             (*curr_arg)++;

             /* parse and store the given ref numbers in structure by_ref */
             parse_number_opts( argv, curr_arg, &dumpvg_opts->by_ref);
             (*curr_arg)++;
             break;

         case 'n':	/* dump by names */
             dumpvg_opts->filter |= DNAME;   /* set bit DNAME */
             (*curr_arg)++;

             /* parse and store the given names in structure by_name */
             parse_string_opts( argv, curr_arg, &dumpvg_opts->by_name);
             (*curr_arg)++;
             break;

         case 'c':	/* dump by class */
             dumpvg_opts->filter |= DCLASS;   /* set bit DCLASS */
             (*curr_arg)++;

             /* parse and store the given classes in structure by_class */
             parse_string_opts( argv, curr_arg, &dumpvg_opts->by_class);
             (*curr_arg)++;
             break;

         case 'd':	/* dump data only */
             dumpvg_opts->contents = DDATA;
             (*curr_arg)++;
             printf("Warning>>> option -d is being removed from dumpvg.\n");
             printf("Please contact hdfhelp@ncsa.uiuc.edu if you would like to keep it.\n\n");
	     exit(1);

         case 'h':	/* no annotations nor data */
             dumpvg_opts->contents = DHEADER;
             (*curr_arg)++;
             break;

         case 'v':	/* dump all info */
             dumpvg_opts->contents = DVERBOSE;
             (*curr_arg)++;
             break;

         case 'o':	/* specify output file */
             dumpvg_opts->dump_to_file = TRUE;

             /* Get file name */
             HDstrcpy(dumpvg_opts->file_name, argv[++(*curr_arg)]);

             (*curr_arg)++;
             break;

         case 'b':   /* dump data in binary - should be removed */ 
             printf("Warning>>> option -b has been removed from dumpvg.\n");
             printf("Please contact hdfhelp@ncsa.uiuc.edu for further assistance.\n");
	     exit(1);

         case 'x':   /* dump data in ascii, also default */
             dumpvg_opts->file_type = DASCII;
             (*curr_arg)++;
             break;

         default:	/* invalid dumpvg option */
             printf("Warning>>> Invalid dumpvg option %s\n", argv[*curr_arg]);
                return (FAIL);
            }	/* end switch */
      }		/* end while */

   /* add the number of vgroups requested by index, by ref#, and by name
      to have a total number of requested vgroups */
   dumpvg_opts->num_chosen = dumpvg_opts->by_index.num_items +
                             dumpvg_opts->by_ref.num_items +
                             dumpvg_opts->by_name.num_items +
                             dumpvg_opts->by_class.num_items;

    return (SUCCEED);
}	/* end parse_dumpvg_opts */

/* Given a ref#, Vref_index searches for the vgroup that has this ref#
and returns the vgroup's index or FAIL */
int32 
Vref_index(int32 file_id, 
           int32 vg_ref)
{
    int32  find_ref = -1;
    int    index = 0;
    int32  ret_value = FAIL;   

   while ((find_ref = Vgetid(file_id, find_ref)) != FAIL)
   {
      if (find_ref == vg_ref)
      {
         ret_value = index;
         goto done; /* found , done */
      }
      index++;
   }

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
    /* Normal cleanup */
    
    return ret_value;
}	/* Vref_index */

/* Vstr_index searches for a vgroup that has a given name or class and
returns the vgroup's index or FAIL. */
int32 
Vstr_index(int32 file_id, 
           char filter_str[MAXNAMELEN], /* vg's name or vg's class */
           int is_name,   /* TRUE if searching vg's name, FALSE if class */
           int32 *find_ref, /* current ref#, will return next one */
           int32 *index)  /* index of the vgroup w/ref# *find_ref */
{
    int32  vg_id = FAIL;
    char   vg_name[MAXNAMELEN];
    int32  ret_value = FAIL;

   /* starting from the ref# *find_ref, search for the vgroup having a
      name or class the same as the given string filter_str; when no 
      more vgroups to search, return FAIL */
   while ((*find_ref = Vgetid(file_id, *find_ref)) != FAIL)
   {
      vg_id = Vattach(file_id, *find_ref, "r");
      if (FAIL == vg_id)
         ERROR_GOTO_2( "in %s: Vattach failed for vgroup with ref#(%d)",
                "Vstr_index", (int)*find_ref );

      /* if the string searched is a vg's name */
      if (is_name)
      {
         if (FAIL == Vgetname(vg_id, vg_name))
            ERROR_GOTO_2( "in %s: Vgetname failed for vgroup with ref#(%d)",
                "Vstr_index", (int)*find_ref );
      }

      /* or the string searched is a vg's class */
      else
      {
         if (FAIL == Vgetclass(vg_id, vg_name))
         ERROR_GOTO_2( "in %s: Vgetclass failed for vgroup with ref#(%d)",
                "Vstr_index", (int)*find_ref );
      }

      if (FAIL == Vdetach(vg_id))
         ERROR_GOTO_2( "in %s: Vdetach failed for vgroup with ref#(%d)",
                "Vstr_index", (int)*find_ref );

      /* if the vg's name or vg's class is the given string, return the
         index of the vgroup found */
      if (HDstrcmp(vg_name, filter_str) == 0)
      {
             /* store the current index to return first */
         ret_value = (*index);
            /* then increment index for next vgroup - same class vgroups*/
         (*index)++;
         goto done;
      }
      /* Note: in either case, vg or vs, increment the index for the 
	 next vgroup */
      (*index)++;
   } /* end while getting vgroups */

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
    /* Normal cleanup */
    
    return ret_value;
} /* Vstr_index() */

/* resetVG calls Vdetach to end access to a vgroup with error checking
   and resets the vgroup id to FAIL.  If failure occurs, resetVG 
   print an error message in the stderr to help debugging */
void
resetVG( int32 *vg_id,
         const char  *curr_file_name )
{
   if( *vg_id != FAIL )
   {
      if( FAIL == Vdetach( *vg_id ))
         fprintf(stderr,"in resetVG: Vdetach failed for vg_id(%d) in file %s\n",
                    (int)*vg_id, curr_file_name );
      *vg_id = FAIL;
   }
}  /* end of resetVG */

/* recursively display a vgroup with subtrees.  The parameter root_index
is the index of the node of which the initial display is called.  The
recursion is continued until this node is visited again; this way, infinite
loop will be eliminated; or until all of its children are displayed */
void 
display(vg_info_t *ptr, 
        int32       level,  /* level of decendants - for indentation */
        vg_info_t **list, 
        int32       num_nodes, 
        int32       root_index, /* index of the node starting the tree */
        int32       firstchild,
        FILE       *fp )
{
    char *name = NULL;
    int   i, k, x, z;
    int   num = 1;

   level++;
   if (!firstchild)   /* take care of the indentations */
   {
      for (k = 0; k < level; k++)
      {
         for (z = 0; z < num; z++)
            fprintf( fp, "\t");
      }
   }
   else
      for (z = 0; z < num; z++)
         fprintf( fp, "\t");

   if (level > 0)  /* only print -- on subtrees */
      fprintf( fp, "-- ");

   /* every vg is printed here, except the one that indicates a loop */
   fprintf( fp, "vg%d ", (int)ptr->index);

   /* if this vgroup does not have any element, go to next line */
   if (ptr->children[0] == NULL)
      fprintf( fp, "\n");

   /* if this vgroup has not been displayed before, print it and its
      subtree */
   if (!ptr->displayed)
   {
      ptr->displayed = TRUE;  /* so this item will not be printed again */
      for (i = 0; ptr->children[i] != NULL; i++)
      {
         if (i == 0)
            firstchild = TRUE;
         else
            firstchild = FALSE;

         name = ptr->children[i];

         if ((HDstrcmp(ptr->type[i], "vd")) 
                    && (HDstrcmp(ptr->children[i], "***")))
         {
            x = 0;
            while (HDstrcmp(name, list[x]->name))
            {
               x++;
            }

            /* BMR: stop when the current node is the same as the one 
               that starts this whole graph, i.e, loop starts */
            if( list[x]->index != root_index ) 
               display(list[x], level, list, num_nodes, root_index,firstchild,fp);

            /* BMR: if the loop has started, only print the node's name 
               and an '*' to indicate a loop */
            else
            {
               if( i > 0)
               {
                  for (k = 0; k < level+1; k++)
                  {
                     for (z = 0; z < num; z++)
                        fprintf( fp, "\t");
                  }
                  fprintf( fp, "-- vg%d (*)\n", (int)list[x]->index);
               }
               else
                  fprintf( fp, " -- vg%d (*)\n", (int)list[x]->index);
            }
         }		
         else  /* this node is a vdata */
         {
            if (i > 0)
            {
               for (k = 0; k < level + 1; k++)
               {
                  for (z = 0; z < num; z++)
                     fprintf( fp, "\t");
               }
            }

            if (firstchild)
            {
               for (z = 0; z < num; z++)
                  fprintf( fp, "\t");
            }

            fprintf( fp, "-- ");
                          /* fprintf( fp, "%s%d ", ptr->type[i], i); */
            fprintf( fp, "%s  \n", ptr->type[i]);
         }
      }	/* for (i...) */
   }  /* if (!ptr->displayed) */
   else
      fprintf( fp, "\n");

   /* BMR: reset for the next node in the list - 01/16/99 */
   ptr->displayed = FALSE;
}  /* display */

/* get_VGandInfo attaches to vgroup with ref# vg_ref, and retrieves the 
   vgroup's id.  If attaching successful, the routine reads the vgroup's 
   tag, name, class, and number of entries; otherwise, it will set the
   vgroup's id to FAIL and returns to the caller with status FAIL.
   If any other failure occurs, get_VGandInfo will return the status
   as FAIL after completing its processing. */
intn
get_VGandInfo( int32 *vg_id,
               int32  file_id,
               int32  vg_ref,
               const  char *file_name,
               int32 *n_entries,
               char  *vgname,
               char  *vgclass )
{
   intn status, ret_value = SUCCEED;

   /* detach the current vgroup if it's attached to cover the case 
      where a library routine fails and must continue to the next vgroup
      without detaching the failed vgroup */
   resetVG( vg_id, file_name );

   *vg_id = Vattach(file_id, vg_ref, "r");
   if( *vg_id == FAIL) /* go to done and return a FAIL */
      ERROR_GOTO_2( "in %s: Vattach failed for vgroup ref=%d", 
		"get_VGandInfo", (int) vg_ref );

   status = Vinquire(*vg_id, n_entries, vgname);
   if (FAIL == status) /* go to done and return a FAIL */
   {
      /* stuff values to these variables so they can be printed */
      *n_entries = -1;
      HDstrcpy( vgname, "<Unknown>" );

      ERROR_GOTO_2( "in %s: Vinquire failed for vg ref=%d",
		"get_VGandInfo", (int) vg_ref );
   }
   else if( HDstrcmp( vgname, "" ) == 0) 
      HDstrcpy( vgname, "<Unknown>" );

   status = Vgetclass(*vg_id, vgclass);
   if( FAIL == status ) /* go to done and return a FAIL */
   {
      /* stuff values to the class so it can be printed */
      HDstrcpy( vgclass, "<Unknown>" );

      ERROR_GOTO_2( "in %s: Vgetclass failed for vgroup ref#=%d",
		"get_VGandInfo", (int) vg_ref );
   }
   else if( HDstrcmp( vgclass, "" ) == 0) 
      HDstrcpy( vgclass, "<Unknown>" ); 
done:
   if( ret_value == FAIL )
   {
   }
   /* Normal cleanup */

   return( ret_value );
} /* end of get_VGandInfo */

intn 
print_data_annots( int32 file_id,
                const char *file_name,
                int32 tag,
                int32 ref )
{
   int32  an_id = FAIL;
   intn   ret_value = SUCCEED;

   if ((an_id = ANstart(file_id)) == FAIL)
      ERROR_GOTO_2( "in %s: ANstart failed for file %s\n", 
		"print_data_annots", file_name);
                                  
   /* print labels of vgroup if any */
   if (FAIL == print_data_labels(file_name, an_id, tag, ref))
      ERROR_GOTO_3( "in %s: print_data_labels failed for vg_ref(%d) in file %s\n", 
		"print_data_annots", (int) ref, file_name);

   /* print descriptions of vgroup if any */
   if (FAIL == print_data_descs(file_name, an_id, tag, ref))
      ERROR_GOTO_3( "in %s: print_data_descs failed for vg-ref(%d) in file %s\n", 
		"print_data_annots", (int) ref, file_name);

   /* close annotation interface */
   if (FAIL == ANend(an_id))
      ERROR_GOTO_2( "in %s: ANend failed for file %s\n", 
		"print_data_annots", file_name);

done:
   if( ret_value == FAIL )
   {
      if (an_id != FAIL)
         ANend(an_id);
   }
   /* Normal cleanup */
   return( ret_value );
} /* end of print_data_annots */

/* alloc_list_of_strings allocates a list of num_entries char pointers 
   and initializes the pointers to NULL.  If allocation fails, 
   alloc_list_of_strings simply terminates hdp. */
char **
alloc_list_of_strings(
	int32 num_entries )
{
   char **ptr;
   intn i;

   /* I don't know why +1 here and only i<num_entries at for loop - BMR*/
   /* probably, +1 so that HDmalloc won't fail when num_entries = 0. */
   ptr = (char **) HDmalloc(sizeof(char *) * (num_entries+1));

   /* If allocation fails, alloc_list_of_string simply terminates hdp. */
   CHECK_ALLOC( ptr, "ptr", "alloc_list_of_strings" );

   /* Init. all strings to NULL */
   for (i = 0; i < num_entries; i++)
      ptr[i] = NULL;

   return( ptr );
}  /* end of alloc_list_of_strings */

char *
alloc_strg_of_chars(
        char *strg )
{
   char *ptr;

   ptr = (char *) HDmalloc(sizeof(char) * (HDstrlen(strg)+1));

   /* If allocation fails, alloc_list_of_string simply terminates hdp. */
   CHECK_ALLOC( ptr, "ptr", "alloc_strg_of_chars" );

   HDstrcpy(ptr, strg);  /* copy given string */

   return( ptr );
}

/* print_fields displays the vdata's fields in an aligned format,
   particularly when there are many fields and/or fields names are
   lengthy */
void print_fields( char *fields,
	char *field_title,	/* */
	FILE *fp )
{
   int32  lastItem = 0, i,
          count = 0;
   char  *ptr, *tempPtr,
          fldname[MAXNAMELEN],
#if defined (MAC) || defined (macintosh) || defined (SYMANTEC_C) || defined(__APPLE__) 

   /* Lets allocate space for tmpflds */
   *tempflds = (char *)HDmalloc(VSFIELDMAX * FIELDNAMELENMAX * sizeof(char *));
   CHECK_ALLOC( tempflds, "*tempflds", "print_fields" );

#else /* !macintosh */
   tempflds[VSFIELDMAX*FIELDNAMELENMAX];
#endif /* !macintosh */

   /* if fields are not defined by VSsetfields and VSfdefine */
   if( fields[0] == '\0' || fields == NULL )
      fprintf( fp, "%s <Undefined>;\n", field_title );

   else
   { /* there are fields to print */
      fprintf(fp, "%s[", field_title );
      HDstrcpy(tempflds, fields);	/* tempflds can be manipulated */
      ptr = tempflds;			/* traverse tempflds with ptr */

      /* traverse the temporary fieldname list to obtain and print each
       * field name; use ',' to locate individual field names, and each 
       * line should not exceed 50 characters beside the alignment */
      for (i = 0; !lastItem; i++)
      {
         tempPtr = HDstrchr(ptr, ',');  /* locate next separator */
         if (tempPtr == NULL)
            lastItem = 1;	/* set flag for end of list */
         else
            *tempPtr = '\0';	/* change ',' to null to obtain field name */
         HDstrcpy(fldname, ptr);	/* obtain current field name */
         count += HDstrlen(fldname);	/* increment current # of chars on line */
         if (count > 50)
         {
	    /* print alignment for the subsequent lines */
            fprintf(fp, "\n\t          ");

	    /* include the skipped field from previous line */
            count = HDstrlen(fldname);  
         }
         fprintf(fp, "%s", fldname);  /* print the current field name */
         if (!lastItem)
            fprintf(fp, ", ");	/* print a comma if it's not the last field name */
         ptr = tempPtr + 1;	/* move ptr beyond last field name */
      }  /* end of for loop */
      fprintf(fp, "];\n");
   }  /* there are fields to print */
   
#if defined (MAC) || defined (macintosh) || defined (SYMANTEC_C) || defined(__APPLE__)
   if(tempflds != NULL)
   {
      HDfree(tempflds);
      tempflds = NULL;
    } 
#endif /* macintosh */ 

}  /* end of print_fields */

/* compose the list of indices of the requested vgroups although some
   vgroups are requested by ref# or name.
   The routine returns:
	- the number of vgroups to be processed, or
	- NO_SPECIFIC if all vgroups are to be processed, or 
	- 0 if none.
   If there are any errors, the parameter index_error will return TRUE */
int32
get_VGindex_list( 
	int32 file_id,
	dump_info_t *dumpvg_opts,
	int32 **vg_chosen,
	intn *index_error )
{
   intn     i;
   int32    index,
            find_ref,
            vg_count = 0,
            num_vg_chosen = dumpvg_opts->num_chosen,
            number;
   filter_t filter = dumpvg_opts->filter; /* temporary name */
   intn     ret_value = 0;

   /* if no specific vgroups are requested, return vgroup count 
      as NO_SPECIFIC (-1) */
   if( filter == DALL )
   {
      ret_value = NO_SPECIFIC;
      goto done;
   }

   /* if specific vgroups were requested, allocate space for the array
      of indices */
   if (num_vg_chosen > 0)
      alloc_index_list( vg_chosen, num_vg_chosen );

   /* else, no chosen vgroups but filter is not DALL, it shouldn't be this
      combination, return vgroup count as NO_SPECIFIC to dump all */
   else
   {
      ret_value = NO_SPECIFIC;
      goto done;

   }

   /* if there are some vgroups requested by index, store the indices in
      the array provided by the caller */
   if( filter & DINDEX )
      for (i = 0; i<dumpvg_opts->by_index.num_items; i++)
      {
         (*vg_chosen)[vg_count] = dumpvg_opts->by_index.num_list[i];
         vg_count++;
      }

   /* if there are some vgroups requested by ref#, store the indices in
      the array provided by the caller */
   if( filter & DREFNUM )
      for (i = 0; i<dumpvg_opts->by_ref.num_items; i++)
      {
         index = Vref_index(file_id, dumpvg_opts->by_ref.num_list[i]);
         if (index == FAIL)
         {
            printf( "Vgroup with reference number %d: not found\n", 
                           (int)dumpvg_opts->by_ref.num_list[i]);
            *index_error = 1; /* error */
         }
         else
         {
            (*vg_chosen)[vg_count] = index;
            vg_count++;
         }
      }

   /* if there are some vgroups requested by name, store the indices in
      the array provided by the caller */
   if( filter & DNAME )
      for (i = 0; i<dumpvg_opts->by_name.num_items; i++)
      {
         find_ref = (-1);
         number = 0;
         index = Vstr_index(file_id, dumpvg_opts->by_name.str_list[i], 1, &find_ref, &number);
         if (index == FAIL)
         {
            printf( "Vgroup with name %s: not found\n", 
                           dumpvg_opts->by_name.str_list[i]);
            *index_error = 1; /* error */
         }
         else
         {
            (*vg_chosen)[vg_count] = index;
            vg_count++;
         }
      }

   /* if there are some vgroups requested by class, store the indices in
      the array provided by the caller */
   if( filter & DCLASS )
      for (i = 0; i<dumpvg_opts->by_class.num_items; i++)
      {
         int32 found = 0;
         char sear_class[MAXNAMELEN];

         number = 0;
         find_ref = (-1);
         HDstrcpy( sear_class, dumpvg_opts->by_class.str_list[i] );
         while ((index = Vstr_index(file_id, sear_class, 0, &find_ref, &number)) != -1)
         {
            if (vg_count < num_vg_chosen)
               (*vg_chosen)[vg_count] = index;
            else
            {
               /* reallocate the array vg_chosen to hold multiple
                  vgroups since class is not unique b/w vgroups */
               *vg_chosen = (int32 *)HDrealloc(*vg_chosen, sizeof(int32)*(num_vg_chosen+1));
               if( *vg_chosen == NULL)
               {
                  fprintf(stderr,"Failure in get_VGindex_list: Not enough memory!\n");
                  exit(1);
               }  /* end if */

               (*vg_chosen)[vg_count] = index;
               num_vg_chosen++;
            }
            found = 1;
            vg_count++;
         }
         if (!found)
         {
            printf( "Vgroup with class %s: not found\n", 
                           dumpvg_opts->by_class.str_list[i]);
            *index_error = 1; /* error */
         }
      }
   ret_value = vg_count; /* actual number of vgroups to be processed; might
                            be different from dumpvg_opts->num_chosen 
                            because of the non-unique class name */

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
    /* Normal cleanup */

    return ret_value; 
} /* get_VGindex_list */

/* print_file_annotations manage the AN interface and simply calls the
   two routines print_all_file_labels and print_file_descs defined in
   hdp_list.c to display the file annotations of the current file */
intn
print_file_annotations( int32 file_id, const char* file_name )
{
   int32 an_id = FAIL;
   intn  status = SUCCEED, ret_value = SUCCEED;

   /* initiate the annotation interface */
   an_id = ANstart(file_id);
   if (FAIL == an_id)
      ERROR_GOTO_0( "in print_file_annotations: ANstart failed");

   /* print file labels if any */
   status = print_all_file_labels( file_name, an_id );
   if( status == FAIL )
      ERROR_GOTO_0( "in print_file_annotations: print_all_file_labels failed");

   /* print file descriptions if any */
   status = print_file_descs( file_name, an_id );
   if( status == FAIL )
      ERROR_GOTO_0( "in print_file_annotations: print_file_descs failed");

   /* close annotation interface */
   if (FAIL == ANend( an_id ))
      ERROR_GOTO_0( "in print_file_annotations: ANend failed" );

done:
   if( ret_value == FAIL )
   {
      if (an_id != FAIL)
         if (FAIL == ANend( an_id ))
            fprintf(stderr,"ANend failed for an_id(%d) for file %s\n",
                                    (int)an_id, file_name);
   }
   /* Normal cleanup */
   return( ret_value );
}  /* end of print_file_annotations */

/* closeVG combines the processes of Vend, Hclose, freeing the list
   of numbers, and resetting all ids, after validating the ids first.
   When either Vend or Hclose fails, closeVG prints an informative
   message then resetting the ids as normal since these failures are
   highly unlikely and since the files are opened as read-only, it's
   safe to go on. */
void
closeVG(
    int32 *file_id,     /* will be returned as a FAIL */
    int32 **vg_chosen,  /* will be returned as a NULL */
    const char  *curr_file_name )
{
   if( *file_id != FAIL )
   {
      if (FAIL == Vend(*file_id))
         fprintf(stderr,"Failure in closeVG: Vend failed for file %s\n",
                         curr_file_name );
      if (FAIL == Hclose(*file_id))
         fprintf(stderr,"Failure in closeVG: Hclose failed for file %s\n",
                         curr_file_name );
      *file_id = FAIL; /* reset */
   }

   if( *vg_chosen != NULL )
   {
      HDfree( *vg_chosen );
      *vg_chosen = NULL;
   } /* end if */

} /* end of closeVG */

intn
vgBuildGraph(int32        vg_id, 
           int32        file_id, 
           int32        num_entries, 
	   const char* file_name,
           vg_info_t *aNode,
           intn         *skipfile)
{
    int32  vs;
    char   vsname[MAXNAMELEN];
    char   vgname[VGNAMELENMAX];
    char   vgclass[VGNAMELENMAX];
    char  *name = NULL;
    int32  vgt = FAIL;
    int32  entry_num;
    int32  elem_ref = FAIL;
    int32  elem_n_entries;
    int32  elem_tag;
    int32  vg_ref;
    intn   status, ret_value = SUCCEED;

   /* allocate and init memory for storing children's and type's info */
   aNode->children = alloc_list_of_strings( num_entries );
   aNode->type = alloc_list_of_strings( num_entries );

   /* get the current vgroup's ref# for error message */
   vg_ref = VQueryref( vg_id );
   if( vg_ref == FAIL )
      ERROR_GOTO_2( "in %s: VQueryref failed for vgroup with id=%d",
		"vgBuildGraph", (int) vg_id );

   for (entry_num = 0; entry_num < num_entries; entry_num++)
   {
      status = Vgettagref(vg_id, entry_num, &elem_tag, &elem_ref);
      if( FAIL == status )
         ERROR_CONT_2( "in %s: Vgettagref failed for the %d'th entry", 
                "vgBuildGraph", (int) entry_num );

      if (elem_tag == DFTAG_VG)
      { /* vgroup */

         /* get the current vgroup and its information */
         status = get_VGandInfo( &vgt, file_id, elem_ref, file_name, 
				 &elem_n_entries, vgname, vgclass );
         if( status == FAIL )
            ERROR_NOTIFY_3( "in %s: %s failed in getting vgroup with ref#=%d",
		"vgBuildGraph", "get_VGandInfo", (int)vg_ref );

         /* although get_VGandInfo failed to get a valid id for this vgroup,
	    the node for the vgroup is not affected, so build it anyway */
         if( vgt == FAIL )
         {
            *skipfile = TRUE; /* severe failure, skip this file */
	    ERROR_NOTIFY_3( "in %s: %s failed to return a valid vgroup id for the %d'th entry",
			"vgBuildGraph", "get_VGandInfo", (int) entry_num );
         }

	 /* just in case vgroup name is null */
         if (HDstrlen(vgname) == 0)
            HDstrcat(vgname, "NoName");

         resetVG( &vgt, file_name );

         /* add the name and type of this element to the current graph */
         aNode->children[entry_num] = alloc_strg_of_chars( vgname );
         aNode->type[entry_num] = alloc_strg_of_chars( "vg" );

      }	/* if current element is vgroup */
      else if (elem_tag == VSDESCTAG)
      { /* vdata */
	 /* add type of this element to the current graph */
         aNode->type[entry_num] = alloc_strg_of_chars( "vd" );

         vs = VSattach(file_id, elem_ref, "r");
         if (vs == FAIL)
         {
            /* add Invalid for the name of this element to the current graph */
            aNode->children[entry_num] = alloc_strg_of_chars( "<Invalid>" );

            ERROR_CONT_2( "in %s: VSattach failed for vdata with ref#=%d", 
			"vgBuildGraph", (int) elem_ref );
         }
         else /* VSattach didn't fail */
         {
               /* get vdata's name for graphical rep. preparation only */
               if (FAIL == VSgetname(vs, vsname))
               {
                  ERROR_NOTIFY_2( "in %s: VSgetname failed for vdata with ref#=%d", 
			"vgBuildGraph", (int) elem_ref );

                  /* set vdata's name to undefined */
      		  HDstrcpy( vgname, "<Invalid>" );
               }

            if (FAIL == VSdetach(vs))
               ERROR_NOTIFY_2( "in %s: VSdetach failed for vdata with ref#=%d", 
			"vgBuildGraph", (int) elem_ref );
         }  /* if VSattach doesn't fail */

         /* vdata's name might be "" - is this same thing as <Unknown>? */
         if (HDstrlen(vsname) == 0)
            HDstrcat(vsname, "NoName");

         /* add the name of this element to the current graph */
         aNode->children[entry_num] = alloc_strg_of_chars( vsname );

      }  /* if current element is a vdata */
      else /* else something else */
      {
         name = HDgettagsname((uint16) elem_tag);
         if (!name)
            name = HDstrdup("Unknown Tag");

         /* add the name and type of this element to the current graph */
         aNode->children[entry_num] = alloc_strg_of_chars( "***" );

         if (!strcmp(name, "Unknown Tag"))
         {
            aNode->type[entry_num] = alloc_strg_of_chars( "Unknown Object");
         }
         else
            aNode->type[entry_num] = name;

      }  /* something else */
   }  /* for */

   aNode->children[num_entries] = NULL;

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
          if (aNode->children != NULL)
            {
                for (entry_num = 0; entry_num < num_entries; entry_num++)
                   if (aNode->children[entry_num] != NULL)
                       HDfree(aNode->children[entry_num]);
                HDfree( aNode->children );
            }
          if (aNode->type != NULL)
            {
                for (entry_num = 0; entry_num < num_entries; entry_num++)
                   if (aNode->type[entry_num] != NULL)
                       HDfree(aNode->type[entry_num]);
                HDfree( aNode->type );
            }
      }
    /* Normal cleanup */
    
    return ret_value;
}	/* vgBuildGraph */

intn
vgdumpfull(int32        vg_id, 
           dump_info_t *dumpvg_opts,
           int32        file_id, 
           int32        num_entries, 
           FILE        *fp,  
           vg_info_t *aNode, 
           intn         *skipfile)
{
    int32  vgt = FAIL;
    int32  vg_ref;
    int32  entry_num;
    int32  elem_tag;
    int32  elem_ref = FAIL;
    int32  elem_n_entries;
    int    found = 0;
    int32  vs;
    int32  nv;
    int32  interlace;
    int32  vsize;
    char   vsname[MAXNAMELEN];
    char   vsclass[VSNAMELENMAX];
    char   vgname[VGNAMELENMAX];
    char   vgclass[VGNAMELENMAX];
    char  *name = NULL;
    int32  i;
    char  *file_name = dumpvg_opts->ifile_name;
    intn   status, ret_value = SUCCEED;

#if defined (MAC) || defined (macintosh) || defined (SYMANTEC_C) || defined(__APPLE__)
	/* macintosh cannot handle >32K locals */
   char *fields = (char *)HDmalloc(VSFIELDMAX*FIELDNAMELENMAX* sizeof(char));
   CHECK_ALLOC( fields, "*fields", "vgdumpfull" );

#else /* !macintosh */
    char   fields[VSFIELDMAX*FIELDNAMELENMAX];
#endif /* !macintosh */    

   /* allocate and init memory for storing children's and type's info */
   aNode->children = alloc_list_of_strings( num_entries );
   aNode->type = alloc_list_of_strings( num_entries );

   /* get the current vgroup's ref# for error message */
   vg_ref = VQueryref( vg_id );
   if( vg_ref == FAIL )
      ERROR_GOTO_2( "in %s: VQueryref failed for vgroup with id=%d",
		"vgdumpfull", (int) vg_id );

   for (entry_num = 0; entry_num < num_entries; entry_num++)
   {
      status = Vgettagref(vg_id, entry_num, &elem_tag, &elem_ref);
      if( FAIL == status )
         ERROR_CONT_2( "in %s: Vgettagref failed for the %d'th entry", 
                "vgdumpfull", (int) entry_num );

      found = 1;
      if( elem_ref == 0 )  /* taken care of ref=0 in tdfr8f and tdf24 for now */
      {
         fprintf(fp, "     #%d (Vgroup)\n", (int) entry_num );
         fprintf(fp, "\ttag = %d;", (int) elem_tag);
         fprintf(fp, "reference = %d;\n", (int) elem_ref );
      }

      else if (elem_tag == DFTAG_VG)
      { /* vgroup */

         /* get the current vgroup and its information */
         status = get_VGandInfo( &vgt, file_id, elem_ref, file_name, &elem_n_entries, vgname, vgclass );
         if( status == FAIL )
	    ERROR_GOTO_3( "in %s: %s failed in getting vgroup with ref#=%d",
                "vgdumpfull", "get_VGandInfo", (int) vg_ref );

         /* since the succeeding processing depends on this vg id, we 
            decided to just skip the current file.  Note that elem_n_entries 
            is not checked here since it does not effect the following 
            processing as in the case of the parent's vgroup */
         if( vgt == FAIL )
         {
            /* return to caller to go to next file */
            *skipfile = TRUE;
	    ERROR_GOTO_3( "in %s: %s failed to return a valid vgroup id for the %d'th entry",
                "vgdumpfull", "get_VGandInfo", (int) entry_num );
         }

	 /* just in case vgroup name is null */
         if (HDstrlen(vgname) == 0)
            HDstrcat(vgname, "NoName");

         /* add the name and type of this element to the current graph */
         aNode->children[entry_num] = alloc_strg_of_chars( vgname );
         aNode->type[entry_num] = alloc_strg_of_chars( "vg" );

         /* print the entry's info */ 
         fprintf(fp, "     #%d (Vgroup)\n", (int) entry_num );
         fprintf(fp, "\ttag = %d;", (int) elem_tag);
         fprintf(fp, "reference = %d;\n", (int) elem_ref );
         fprintf(fp, "\tnumber of entries = %d;\n", (int) elem_n_entries);
         fprintf(fp, "\tname = %s; class = %s\n", vgname, vgclass);

         /* dump attributes for vgroup */
         status = dumpattr(vgt, 0, 0, dumpvg_opts->file_type, fp);
         if( FAIL == status )
            ERROR_CONT_3( "in %s: %s failed to dump attributes for vgroup with ref#=%d",
		"vgdumpfull", "dumpattr", (int) elem_ref );

         /* dump all of the annotations for this vgroup */
         status = print_data_annots( file_id, file_name, elem_tag, elem_ref );
         if( FAIL == status )
            ERROR_CONT_3( "in %s: %s failed to dump annotations for vgroup with ref#=%d",
		"vgdumpfull", "print_data_annots", (int) elem_ref );

         resetVG( &vgt, file_name );

      }	/* if current element is vgroup */
      else if (elem_tag == VSDESCTAG)
      { /* vdata */

         vs = VSattach(file_id, elem_ref, "r");
         if (vs == FAIL)
            ERROR_CONT_2( "in %s: VSattach failed for vdata with ref#=%d", 
                                 "vgdumpfull", (int) elem_ref );

         /* get and print vdata's information */
         status = VSinquire(vs, &nv, &interlace, fields, &vsize, vsname);
         if( FAIL == status )
            ERROR_CONT_2( "in %s: VSinquire failed for vdata with ref#=%d", 
                                 "vgdumpfull", (int) elem_ref );
   
         vsize = VShdfsize(vs, fields);
         if (vsize == FAIL)
            ERROR_CONT_2( "in %s: VShdfsize failed for vdata with ref#=%d", 
                                 "vgdumpfull", (int) elem_ref );

	 /* just in case vdata name is null */
         if (HDstrlen(vsname) == 0)
               HDstrcat(vsname, "NoName");
   
         if (FAIL == VSgetclass(vs, vsclass))
               ERROR_CONT_2( "in %s: VSgetclass failed for vdata with ref#=%d", 
                                 "vgdumpfull", (int) elem_ref );

         if (HDstrlen(vsclass) == 0)
            HDstrcpy( vsclass, "<Unknown>" );

         fprintf(fp, "     #%d (Vdata)\n", (int) entry_num);
         fprintf(fp, "\ttag = %d; ", (int) elem_tag);
         fprintf(fp, "reference = %d; \n", (int) elem_ref);
         fprintf(fp, "\tnumber of records = %d; ", (int) nv);
         fprintf(fp, "interlace = %d;\n", (int) interlace);

         /* The list of field names can be very long and would  
               look very messy when being displayed if it were to 
               be dumped out at once. print_fields displays a list 
	       in a nice way even if the list is long. 
	       The second parameter specifies how the field name list 
	       begins; it's needed because dumpvd also uses this
	       routine and has different indentation format than dumpvg */
         print_fields( fields, "\tfields = ", fp );
         fprintf(fp, "\trecord size (in bytes) = %d;\n", (int)vsize);
         fprintf(fp, "\tname = %s; class = %s;\n", vsname, vsclass);
         fprintf(fp, "\ttotal number of attributes = %d.\n", 
				VSnattrs(vs));

         if (FAIL == VSdetach(vs))
               ERROR_CONT_2( "in %s: VSdetach failed for vdata with ref#=%d", 
                                 "vgdumpfull", (int) elem_ref );

         /* vdata's name might be "" - is this same thing as <Unknown>? */
         if (HDstrlen(vsname) == 0)
            HDstrcat(vsname, "NoName");

         /* add the name and type of this element to the list node */
         aNode->children[entry_num] = alloc_strg_of_chars( vsname );
         aNode->type[entry_num] = alloc_strg_of_chars( "vd" );

      }  /* if current element is a vdata */
      else /* else something else */
      {
         name = HDgettagsname((uint16) elem_tag);
         if (!name)
            name = HDstrdup("Unknown Tag");

            fprintf(fp, "     #%d (%s)\n", (int) entry_num, name);
            fprintf(fp, "\ttag = %d; reference = %d;\n", (int) elem_tag, (int) elem_ref);

         /* add the name and type of this element to the current graph */
         aNode->children[entry_num] = alloc_strg_of_chars( "***" );

         if (!strcmp(name, "Unknown Tag"))
            aNode->type[entry_num] = alloc_strg_of_chars( "Unknown Object");
         else
            aNode->type[entry_num] = name;

      }  /* something else */
   }  /* for */

   aNode->children[num_entries] = NULL;

   if( !found )
      printf("     None.\n");

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
          if (aNode->children != NULL)
            {
                for (i = 0; i < num_entries; i++)
                   if (aNode->children[i] != NULL)
                       HDfree(aNode->children[i]);
                HDfree( aNode->children );
            }
          if (aNode->type != NULL)
            {
                for (i = 0; i < num_entries; i++)
                   if (aNode->type[i] != NULL)
                       HDfree(aNode->type[i]);
                HDfree( aNode->type );
            }
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
}	/* vgdumpfull */


intn 
dvg(dump_info_t *dumpvg_opts, 
    intn         curr_arg, 
    intn         argc, 
    char        *argv[])
{
    int32       file_id = FAIL;
    int32       vg_id = FAIL;
    int32      *vg_chosen = NULL;
    int32       vg_count;
    int32       num_vg_chosen;
    int32       i, j;
    int32       vg_ref = -1;
    int32       vg_tag;
    int32       n_entries;
    int32       level;
    int32       max_vgs=0;
    int32       num_nodes = 0;
    int         index_error = 0;
    int         dumpall = 0;
    char        file_name[MAXFNLEN];
    char        vgclass[VGNAMELENMAX];
    char        vgname[VGNAMELENMAX];
    FILE       *fp = NULL;
    vg_info_t **list = NULL;
    vg_info_t *ptr = NULL;
    intn        status, ret_value = SUCCEED;

   /* check for missing input file name */
   if( curr_arg >= argc )
   {
      fprintf( stderr, "Missing input file name.  Please try again.\n");
      return( FAIL ); /* nothing to be cleaned up at this point */
   }

   /* going through each input file, look for the requested vgroups 
      and display them */
   while (curr_arg < argc)
   {
      intn isHDF = TRUE;  /* FALSE, if current file is not HDF file */
      intn skipfile = FALSE;  /* skip the current file when some severe */
           /* failure occurs; otherwise, the list of nodes is not */
           /* completely prepared and will cause a crash in display */

      HDstrcpy(file_name, argv[curr_arg]);  /* get current input file name */
      HDstrcpy( dumpvg_opts->ifile_name, file_name ); /* record for later use */
      curr_arg++;  /* forward the current argument pointer */

      /* there are times a failure causes continuation without proper
         cleanup, so closeVG ensures of that */
      closeVG( &file_id, &vg_chosen, file_name );

      /* Print an informative message and skip this file if it is not
         an HDF file */
      isHDF = Hishdf(file_name);
      if (isHDF == FALSE)
      {
         /* if there are no more files to be processed, print error
            message, then returns with FAIL */
         if( curr_arg == argc )
            {ERROR_GOTO_1( "in dvg: %s is not an HDF file", file_name);}
         else /* print message, then continue processing the next file */
            {ERROR_CONT_1( "in dvg: %s is not an HDF file", file_name);}
      }

      /* open current hdf file with error check, if fail, go to next file */
      file_id = Hopen(file_name, DFACC_READ, 0);
      if (file_id == FAIL)
      {
         /* if there are no more files to be processed, print error
            message, then returns with FAIL */
         if( curr_arg == argc )
            {ERROR_GOTO_1( "in dvg: Failure in opening file %s", file_name);}
         /* otherwise, print message, then continue processing the next file */
         else
            ERROR_CONT_1( "in dvg: Failure in opening file %s", file_name );
      }

      /* initiate VG interface; if fail, probably something fatal, returns 
	 with FAIL */
      if (FAIL == Vstart(file_id))
         ERROR_GOTO_1( "in dvg: Vstart failed for file %s\n", file_name);

      /* compose the list of indices of vgroups to be processed in the current
      file and return the number of items in the list */
      num_vg_chosen = get_VGindex_list( file_id, dumpvg_opts, &vg_chosen, &index_error);

      /* if there are no valid indices, move on to the next file */
      if (index_error && num_vg_chosen == 0) /* to the next file */
         ERROR_CONT_1( "in dvg: Invalid vgroups given for file %s", file_name );

      /* open output file for ASCII or direct to standard output */
      if (dumpvg_opts->dump_to_file)
         fp = fopen(dumpvg_opts->file_name, "w");
      else
         fp = stdout;

      /* print input file name and file annotations */
      fprintf(fp, "File name: %s \n", file_name);
      status = print_file_annotations( file_id, file_name );
      if( status == FAIL )
         ERROR_CONT_1( "in dvg: print_file_annotations failed for file %s",
                        file_name );

      /* when no vgroups specified, dump all vgroups */
      if (num_vg_chosen == NO_SPECIFIC)
         dumpall = TRUE;
      /* otherwise, sort the list of indices */
      else
         sort(vg_chosen, num_vg_chosen);

      /* allocate space for the list of nodes to be printed in the 
         Graphical Representation part */
      max_vgs = NUM_VGS;
      list = (vg_info_t **) HDmalloc(sizeof(vg_info_t *) * max_vgs);
      CHECK_ALLOC( list, "list", "dvg" );

      for (j = 0; j < max_vgs; j++)  /* init that list */
         list[j] = NULL;

      /* init number of nodes for the graphical representation of the
         current file */
      num_nodes = 0;

      vg_count = 0; /* no vgroups processed yet */

      vg_ref = -1;  /* searching at the beginning of the file */

      /* for each vgroup: go thru each vgroup in the file or until the 
         number of vgs being printed reaches the number of vgs chosen */
      for (i = 0; (vg_ref = Vgetid(file_id, vg_ref)) != FAIL 
                   && (dumpall || vg_count < num_vg_chosen); i++)
      {
         int32       skipvg = FALSE;
         intn isvdata; /* TRUE if a vdata being processed, FALSE if vg */

         /* if not to dump all vgroups but the current vgroup is not
            one of the selected ones */
         if ((!dumpall) && (i != vg_chosen[vg_count]))
            skipvg = TRUE;  /* skip printing this vg's info and data but
                             include it in the graphical representation */

         /* attaches the current vgroup and gets its tag, name, and class */ 
         status = get_VGandInfo( &vg_id, file_id, vg_ref, file_name, 
                                 &n_entries, vgname, vgclass );
         if( status == FAIL )
            ERROR_CONT_2( "in dvg: %s failed in getting vgroup with ref#=%d",
                        "get_VGandInfo", (int)vg_ref );

         /* since the succeeding processing depends heavily on these
            we decided to just skip the current file */
         if( vg_id == FAIL || n_entries == -1 )
         { 
            skipfile = TRUE;  /* so Graphical Rep won't be printed */
            break;  /* to get out of this current file */
         }

         if (!skipvg)
            vg_count++;

         num_nodes++;	/* one more node to the list */

         /* if more vgs to be printed than originally allocated space for,
            reallocate the list of nodes to the proper amount */
         if (num_nodes > max_vgs)
         {
            max_vgs += NUM_VGS;
            list = HDrealloc(list, (uint32) sizeof(vg_info_t) * max_vgs);
            CHECK_ALLOC( list, "list", "dvg" );
         }

         list[i] = (vg_info_t *) HDmalloc(sizeof(vg_info_t));
         CHECK_ALLOC( list[i], "list[i]", "dvg" );

         /* if this vgroup is to be skipped, do not print the info here; 
            go to the data part to add the vgroup to the node list for 
            the graphical rep. */
         if( !skipvg )
         {
	    if (FAIL == (vg_tag = VQuerytag(vg_id)))
               ERROR_NOTIFY_3("in dvg: %s failed on vgroup with ref=%d in file %s", 
		         "VQuerytag", (int) vg_ref, file_name);
            fprintf(fp, "\n");
            fprintf(fp, "\nVgroup:%d\n", (int) i);
            if( vg_tag == DFTAG_VG )
               /* when have time, change to this one, not now because
                  it takes time to fix the testfiles */
               /* fprintf(fp, "     tag = DFTAG_VG(%d);", ); */
               fprintf(fp, "     tag = %d;", (int) vg_tag );
            else
               fprintf(fp, "     tag = Invalid tag (%d);", (int) vg_tag );
            fprintf(fp, " reference = %d;\n", (int) vg_ref);
            fprintf(fp, "     name = %s; class = %s;\n", vgname, vgclass);
            fprintf(fp, "     number of entries = %d;\n", (int) n_entries);

            /* dump attributes of vgroup; the second argument is ignored 
               in this call, it's there as an index when dumping attributes
               of a vdata field */
            isvdata = FALSE;
            status = dumpattr(vg_id, 0, isvdata, dumpvg_opts->file_type, fp);
            if (FAIL == status )
               ERROR_NOTIFY_3("in dvg: %s failed on vgroup with ref=%d in file %s", 
		         "dumpattr", (int) vg_ref, file_name);
  
            /* Read in all of the annotations. */
            /* Re-vamped annotation handling to use new ANxxx interface 
             *  -georgev 6/11/97 */
            status = print_data_annots( file_id, file_name, vg_tag, vg_ref );
            if (FAIL == status )
               ERROR_NOTIFY_3("in dvg: %s failed on vgroup with ref=%d in file %s", 
		         "dumpattr", (int) vg_ref, file_name);
         } /* not skipped */

         if( skipvg || dumpvg_opts->contents == DHEADER )
         {
            status = vgBuildGraph(vg_id, file_id, n_entries, file_name, list[i], &skipfile );
            if( status == FAIL )
               ERROR_NOTIFY_3( "in dvg: %s failed for vgroup with ref#=%d in file %s", 
			     "vgBuildGraph", (int) vg_ref, file_name );
         }
         else
         {
            fprintf(fp, "Entries:-\n");
            status = vgdumpfull(vg_id, dumpvg_opts, file_id, n_entries,
                                  fp, list[i], &skipfile );
            if( FAIL == status )
            {
               ERROR_NOTIFY_3( "in dvg: %s failed for vgroup with ref#=%d in file %s", 
			      "vgdumpfull", (int) vg_ref, file_name);

               /* do not continue so list[i] can be set */
            }
         } /* neither skipped nor header only */

         /* if the current file is to be skipped due to some severe
            error, break out of for loop and the partial list is freed */
         if( skipfile )
            ERROR_BREAK_1( "in dvg: Severe failure in file %s.  Go to next file", 
				file_name, FAIL );

         /* done using this vgroup id, reset it */
         resetVG( &vg_id, file_name );
  
         /* fill the graph. rep. node for this vgroup */
         list[i]->index = i;
         HDstrcpy(list[i]->name, vgname);
         list[i]->displayed = FALSE;
         list[i]->treedisplayed = FALSE;  /* BMR - 01/16/99 */
      }	/* for all vgroups */

      /* print the graphical representation part */
      if( !skipfile )
      {
         int32 node_num;

         fprintf( fp, "\n\nGraphical representation of the file:-\n");
         fprintf( fp, "(vg#: vgroup;   vd: vdata)\n\n");
         for (node_num = 0; node_num < num_nodes; node_num++)
         {
            int32       firstchild = FALSE;

            level = -1;
            ptr = list[node_num];
            fprintf( fp, "   ");

            /* print tree */
            display(ptr, level, list, num_nodes, ptr->index, firstchild, fp);
            ptr->treedisplayed = TRUE; /* so this tree won't be shown again */
            fprintf( fp, "\n");
          }		/* for */
       } /* if the file is not to be skipped */

      /* free the list of vg_info_t nodes */
      list = free_vginfo_list( list, max_vgs );

      /* free vg_chosen, and terminate access to and close the input file */
      closeVG( &file_id, &vg_chosen, file_name );

      /* close the output file if there is one */
      if (dumpvg_opts->dump_to_file)
          fclose(fp);

   } /* while (more file to process) */

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
          closeVG( &file_id, &vg_chosen, file_name );
          resetVG( &vg_id, file_name );
          list = free_vginfo_list( list, max_vgs );
      }
    /* Normal cleanup */

    return ret_value;
}	/* dvg */

/* main routine in hdp_vg.c; called by hdp.c/main to process the command
hdp dumpvg... */
intn
do_dumpvg(intn  curr_arg, 
          intn  argc, 
          char *argv[], 
          intn  help )
{
    dump_info_t dumpvg_opts;	/* dumpvg options */
    intn status, ret_value = SUCCEED;

   /* initialize the structure that holds user's options and inputs */
   init_dump_opts(&dumpvg_opts);

   if (help == TRUE)
   {
      dumpvg_usage(argc, argv);
      goto done;
   }

   /* incomplete command */
   if( curr_arg >= argc )
   {
      dumpvg_usage(argc, argv);
      ERROR_GOTO_0( "in do_dumpvg: command is incomplete");
   }            /* end if */

   /* parse the user's command and store the inputs in dumpvg_opts */
   status = parse_dumpvg_opts(&dumpvg_opts, &curr_arg, argc, argv );
   if( status == FAIL )
   {
      dumpvg_usage(argc, argv);
      ERROR_GOTO_0( "in do_dumpvg: parse_dumpvg_opts is unable to parse command");
   }

   /* display data and information as specified in dumpvg_opts */
   status = dvg(&dumpvg_opts, curr_arg, argc, argv);
   if( status == FAIL )
      ERROR_GOTO_0( "in do_dumpvg: dvg failed");

  done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
    /* Normal cleanup */

   /* free the lists for given indices, ref#s, names, and classes if
      they had been allocated */
   free_num_list( dumpvg_opts.by_index.num_list );
   free_num_list( dumpvg_opts.by_ref.num_list );
   free_str_list( dumpvg_opts.by_name.str_list, dumpvg_opts.by_name.num_items);
   free_str_list( dumpvg_opts.by_class.str_list, dumpvg_opts.by_class.num_items );

    return ret_value;
}	/* end do_dumpvg() */

