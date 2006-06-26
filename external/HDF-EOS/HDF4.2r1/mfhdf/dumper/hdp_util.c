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
static char RcsId[] = "@(#)1.1";
#endif

/* hdp_util.c,v 1.1 1994/04/18 15:49:18 georgev Exp */

#include "hdp.h"

const char *unknown_tag = "Unknown Tag";

char       *
tagnum_to_name(intn num)
{
    char       *ret;

    ret = HDgettagsname(num);
    if (ret == NULL)
        ret = HDstrdup(unknown_tag);
    return (ret);
}	/* end tagnum_to_name() */

intn 
tagname_to_num(const char *name)
{
    return (HDgettagnum(name));
}	/* end tagname_to_num() */

/*
 *  Routines to create a list of file names from the command line
 */
/* assumes that curr_arg is pointing to the first file name */
filelist_t *
make_file_list(intn curr_arg, intn argc, char *argv[])
{
    intn        i;
    filelist_t *ret;

    if (curr_arg > argc)	/* consistency check */
        return (NULL);

   ret = (filelist_t *) HDmalloc(sizeof(filelist_t));
    if (ret == NULL)
      {
          fprintf(stderr, "make_file_list: space allocation failed\n");
          return (NULL);
      }
    ret->file_arr = (char **) HDmalloc(sizeof(char *) * ((argc - curr_arg) + 1));
    if (ret->file_arr == NULL)
      {
          fprintf(stderr, "make_file_list: space allocation failed\n");
          HDfree(ret);
          return (NULL);
      }		/* end if */

    ret->max_files = (argc - curr_arg);
    ret->curr_file = 0;
    for (i = 0; curr_arg < argc; i++, curr_arg++)
        ret->file_arr[i] = HDstrdup(argv[curr_arg]);
    return (ret);
}	/* end make_file_list() */

char       *
get_next_file(filelist_t * f_list, intn advance)
{
    if (advance)
        f_list->curr_file++;
    if (f_list->curr_file >= f_list->max_files)
        return (NULL);
    return (f_list->file_arr[f_list->curr_file]);
}	/* end get_next_file() */

/* free_struct_list use HDfree to free the list of vgroup info structs */
vg_info_t ** free_vginfo_list( 
		vg_info_t **nodelist,
		int32 num_items )
{
   intn i;

   /* if the list is not NULL, free each node then reset the list to NULL */
   if( nodelist != NULL)
   {
      for( i = 0; i < num_items; i++ )
         if( nodelist[i] != NULL )
            HDfree( nodelist[i] );
      HDfree( nodelist );
   }
   return( NULL );
}  /* end of free_vginfo_list */

/* free_str_list use HDfree to free the list of strings of characters */
char** free_str_list( char **str_list,
               int32 num_items )
{
   intn i;

   if( str_list != NULL)
   {
      for( i = 0; i < num_items; i++ )
         free_char_list( str_list[i] );
      HDfree( str_list );
   }
   return( NULL );
}  /* end of free_str_list */

/* free_num_list use HDfree to free the string of characters; this routine
   is short but can be used in many different places and very convenient */
char* free_char_list( char *char_list )
{
   if( char_list != NULL)
      HDfree( char_list );
   return( NULL );
}  /* end of free_char_list */

/* free_num_list use HDfree to free the list of integers; this routine
   is short but can be used in many different places and very convenient */
int32* free_num_list( int32 *num_list )
{
   if( num_list != NULL)
      HDfree( num_list );
   return( NULL );
}  /* end of free_num_list */

void 
free_file_list(filelist_t * f_list)
{
    intn        i;

    for (i = 0; i < f_list->max_files; i++)
        HDfree(f_list->file_arr[i]);
    HDfree(f_list->file_arr);
    HDfree(f_list);
}	/* end free_file_list() */

/*
 *  Routines to manipulate group lists
 */
groupinfo_t *
make_group_list(int32 fid, uint16 tag, uint16 ref)
{
    intn        nobj;
    intn        i;
    groupinfo_t *ret;
    int32       gid;

    if (tag == DFTAG_RIG || tag == DFTAG_SDG || tag == DFTAG_NDG)
      {
          if ((gid = DFdiread(fid, tag, ref)) == FAIL)
              return (NULL);
          if ((nobj = DFdinobj(gid)) == FAIL)
              return (NULL);
          if ((ret = (groupinfo_t *) HDmalloc(sizeof(groupinfo_t))) == NULL)
            {
              fprintf(stderr, "make_group_list: space allocation failed\n");
                return (NULL);
            }
          ret->max_dds = nobj;
          ret->curr_dd = 0;
          if (nobj > 0)
            {
                if ((ret->dd_arr = (DFdi *) HDmalloc(sizeof(DFdi) * nobj)) == NULL)
                  {
                  fprintf(stderr, "make_group_list: space allocation failed\n");
                      HDfree(ret);
                      return (NULL);
                  }		/* end if */
                for (i = 0; i < nobj; i++)
                  {
                      if (DFdiget(gid, &ret->dd_arr[i].tag, &ret->dd_arr[i].ref) == FAIL)
                        {
                            HDfree(ret->dd_arr);
                            HDfree(ret);
                            return (NULL);
                        }	/* end if */
                  }		/* end for */
            }	/* end if */
          else
            {	/* paranoia sets in... */
                ret->max_dds = ret->curr_dd = 0;
                ret->dd_arr = NULL;
            }	/* end else */
      }		/* end if */
    else
      {		/* check for Vgroup? */
          int32       vkey;

/* Yes, I know this wastes time, but at least it allows uniform access */
/* to both types of groups in HDF files... */
          if (vinit_done == FALSE)
            {	/* check whether we've already init'ed Vsets */
                vinit_done = TRUE;
                Vinitialize(fid);
            }	/* end if */
          if ((vkey = Vattach(fid, ref, "r")) != FAIL)
            {
                if ((nobj = Vntagrefs(vkey)) != FAIL)
                  {
		   if( nobj > 0 ) { /* Albert fixed */
                      int32      *temp_tag;
                      int32      *temp_ref;

                      if ((temp_tag = (int32 *) HDmalloc(sizeof(int32) * nobj)) == NULL)
                        {
                  fprintf(stderr, "make_group_list: space allocation failed\n");
                            Vdetach(vkey);
                            return (NULL);
                        }	/* end if */
                      if ((temp_ref = (int32 *) HDmalloc(sizeof(int32) * nobj)) == NULL)
                        {
                  fprintf(stderr, "make_group_list: space allocation failed\n");

                            Vdetach(vkey);
                            HDfree(temp_tag);
                            return (NULL);
                        }	/* end if */

                      if (Vgettagrefs(vkey, temp_tag, temp_ref, nobj) == FAIL)
                        {
                            Vdetach(vkey);
                            HDfree(temp_tag);
                            HDfree(temp_ref);
                            return (NULL);
                        }	/* end if */

                      if ((ret = (groupinfo_t *) HDmalloc(sizeof(groupinfo_t))) == NULL)
                        {
                  fprintf(stderr, "make_group_list: space allocation failed\n");

                            Vdetach(vkey);
                            HDfree(temp_tag);
                            HDfree(temp_ref);
                            return (NULL);
                        }	/* end if */
                      ret->max_dds = nobj;
                      ret->curr_dd = 0;
                      if ((ret->dd_arr = (DFdi *) HDmalloc(sizeof(DFdi) * nobj)) == NULL)
                        {
                  fprintf(stderr, "make_group_list: space allocation failed\n");

                            Vdetach(vkey);
                            HDfree(temp_tag);
                            HDfree(temp_ref);
                            HDfree(ret);
                            return (NULL);
                        }	/* end if */

/*
printf("make_group_list for tag/ref = %d/%d\n", tag, ref );
*/
                      for (i = 0; i < nobj; i++)
                        {
                            ret->dd_arr[i].tag = (uint16) temp_tag[i];
                            ret->dd_arr[i].ref = (uint16) temp_ref[i];
/*
printf("element %d: tag/ref = %d/%d\n", i, temp_tag[i], temp_ref[i] );
*/
                        }	/* end for */

                      HDfree(temp_tag);
                      HDfree(temp_ref);
		    } /* if nobj > 0 */
                  /* BMR: 7/28/00 must add this one, otherwise, HDfree fails later */
                  else /* nobj <= 0 */
                     return( NULL );
                  }		/* end if */
                else	/* bad vkey? */
                    return (NULL);
                Vdetach(vkey);	/* release the Vgroup */
            }	/* end if */
          else	/* failed to attach */
              return (NULL);
      }		/* end else */
    return (ret);
}	/* end make_group_list() */

DFdi       *
get_next_group(groupinfo_t * g_list, intn advance)
{
    if (advance)
        g_list->curr_dd++;
    if (g_list->curr_dd >= g_list->max_dds)
        return (NULL);
    return (&g_list->dd_arr[g_list->curr_dd]);
}	/* end get_next_group() */

int32 
get_group_max(groupinfo_t * g_list)
{
    if (g_list != NULL)
        return (g_list->max_dds);
    return (FAIL);
}	/* end get_group_max() */

void 
free_group_list(groupinfo_t * g_list)
{
   if( g_list != NULL )
   {
      if( g_list->dd_arr != NULL )
         HDfree(g_list->dd_arr); 
      HDfree(g_list);
   }
}	/* end free_group_list() */

/*
 *  Routines to manipulate tag/ref lists
 */

objlist_t  *
make_obj_list(int32 fid, uint32 options)
{
    intn        nobj;		/* number of DDs in the file */
    int32       status;		/* status of various HDF calls */
    int32       aid;		/* temporary AID to use while getting DD info */
    int16       tmp_spec;	/* temporary storage for special status */
    objlist_t  *obj_ret;	/* pointer to the dd list to return */
    objinfo_t  *obj_ptr;	/* temporary pointer to a working DD object */
    sp_info_block_t info;	/* temp. storage for special elem. info */
    intn        n, m;		/* local counting variable */

   /* get the number of all objects in the file */
    nobj = Hnumber(fid, DFTAG_WILDCARD);
    if (nobj == FAIL || nobj <= 0 )  /* BMR: added check for nobj<=0 */
        return (NULL);

   /* allocate space for the object list - exit at failure??? */
    if ((obj_ret = (objlist_t *) HDmalloc(sizeof(objlist_t))) == NULL)
      {
      fprintf(stderr, "make_obj_list: space allocation failed\n");
          return (NULL);
      }

    obj_ret->max_obj = nobj;	/* set the number of objects */
    obj_ret->curr_obj = 0;
    obj_ret->raw_obj_arr = (objinfo_t *) HDmalloc(sizeof(objinfo_t) * nobj);

/* should it exit on failure ??? */
    if( obj_ret->raw_obj_arr == NULL)
      {
      fprintf(stderr, "make_obj_list: space allocation failed\n");
          HDfree(obj_ret);
          return (NULL);
      }		/* end if */

   /* Clear array of dd/object information */
   HDmemset(obj_ret->raw_obj_arr, 0, sizeof(objinfo_t) * nobj);

   /*
    * Read all the tag/ref's in the file into an array 
   */
   /* start the reading of an access element */
   aid = Hstartread(fid, DFTAG_WILDCARD, DFREF_WILDCARD);
   if (aid == FAIL)
   {
      HEprint(stderr, 0);
      HDfree(obj_ret->raw_obj_arr);
      HDfree(obj_ret);
      return (NULL);
   }		/* end if */

   /* for each element */
   for (n = 0, status = SUCCEED; (n < nobj) && (status != FAIL); n++)
   {
      Hinquire(aid, NULL, &(obj_ret->raw_obj_arr[n].tag),
               &(obj_ret->raw_obj_arr[n].ref), &(obj_ret->raw_obj_arr[n].length),
               &(obj_ret->raw_obj_arr[n].offset), NULL, NULL, &tmp_spec);
      if (options & CHECK_SPECIAL)
      {	/* are we looking for spec. elem. ? */
         obj_ret->raw_obj_arr[n].is_special = (tmp_spec != 0);
         if (obj_ret->raw_obj_arr[n].is_special)
         {		/* get the special info. */
            if ((status = HDget_special_info(aid, &info)) == FAIL)
            {
               obj_ret->raw_obj_arr[n].is_special = 0;
                        }	/* end if */
            else
            {	/* copy over special information we found */
               obj_ret->raw_obj_arr[n].spec_info = (sp_info_block_t *) HDmalloc(sizeof(sp_info_block_t)); 
               if( obj_ret->raw_obj_arr[n].spec_info == NULL)
               {
                   fprintf(stderr, "make_obj_list: space allocation failed\n");
                   obj_ret->raw_obj_arr[n].is_special = 0;
               }
               else
                  HDmemcpy(obj_ret->raw_obj_arr[n].spec_info, &info, sizeof(sp_info_block_t));
            }	/* end else */
         }  /* end if */
      }	 /* end if */
      status = Hnextread(aid, DFTAG_WILDCARD, DFREF_WILDCARD, DF_CURRENT);
   }  /* end for */

   if (Hendaccess(aid) == FAIL)
   {
      HEprint(stderr, 0);
      HDfree(obj_ret->raw_obj_arr);
      HDfree(obj_ret);
      return (NULL);
   }  /* end if */

   /* Post-process the list of dd/objects, adding more information */
   /*  Also set up the pointers for the sorted list to be manipulated later */

   obj_ret->srt_obj_arr = (objinfo_t **) HDmalloc(sizeof(objinfo_t *) * nobj);
   if( obj_ret->srt_obj_arr == NULL )
   {
      fprintf(stderr, "make_obj_list: space allocation failed\n");
      HDfree(obj_ret->raw_obj_arr);
      HDfree(obj_ret);
      return (NULL);
   }  /* end if */

   /* Loop for more information */
   for (n = 0; n < nobj; n++)
   {
      obj_ptr = obj_ret->srt_obj_arr[n] = &obj_ret->raw_obj_arr[n];

      /* set the index value to a flag for later */
      obj_ptr->index = (-1);

      /* check for a group */
      if (options & CHECK_GROUP)
      {	/* are we looking for groups ? */
         if (obj_ptr->tag == DFTAG_RIG || obj_ptr->tag == DFTAG_SDG
             || obj_ptr->tag == DFTAG_NDG || obj_ptr->tag == DFTAG_VG)
         {
            obj_ptr->is_group = TRUE;
            obj_ptr->group_info = make_group_list(fid, obj_ptr->tag, obj_ptr->ref);
            if( obj_ptr->group_info == NULL )
            {
	    /* do not free these because even this element has no group
	       list, it still can be displayd */
/*
               HDfree(obj_ret->raw_obj_arr);
               HDfree(obj_ret);
               return (NULL); 
*/
            }	/* end if */
         }		/* end if */
      }	/* end if */
   }		/* end for */

   /* Loop once more to figure out the index information */
   for (n = 0, obj_ptr = &obj_ret->raw_obj_arr[0]; n < nobj; n++, obj_ptr++)
   {
      if (obj_ptr->index == (-1))
      {	/* first object of this type in the file */
         int32       temp_index = 0;
         objinfo_t  *temp_ptr;	/* temporary pointer to a working DD object */

	 /* the object gets index of 0 */
         obj_ptr->index = 0;

         /* look for other objects of this tag */
         for (m = n, temp_ptr = obj_ptr + 1; m+1 < nobj; m++, temp_ptr++)
         {
            if (temp_ptr->tag == obj_ptr->tag)
               temp_ptr->index = ++temp_index;	/* set next index */
         } 		/* end for */
      }	/* end if */
   }		/* end for */

   obj_ret->options = options;
   return (obj_ret);
}  /* end make_dd_list() */

objinfo_t* get_next_obj(
		objlist_t * o_list, intn advance )
{
   if( advance )
      o_list->curr_obj++;
   if( o_list->curr_obj >= o_list->max_obj )
      return (NULL);
   return( o_list->srt_obj_arr[o_list->curr_obj] );
}	/* end get_next_obj() */

objinfo_t* goto_nth_obj( 
		objlist_t * o_list, intn n )
{
   if( n >= 0 && n < o_list->max_obj )
      o_list->curr_obj = n;
   return( o_list->srt_obj_arr[o_list->curr_obj] );
}  /* end goto_nth_obj() */

void reset_obj_list( 
		objlist_t * o_list )
{
   if( o_list != NULL )
      o_list->curr_obj = 0;
}  /* end reset_obj_list() */

void free_obj_list( 
		objlist_t * o_list )
{
   intn        i;	/* local counting variable */
   objinfo_t  *obj_ptr;	/* temporary pointer to a working DD object */

   /* BMR: verify that o_list is not nil before accessing */
   if( o_list != NULL )
   { 
      for (i = 0, obj_ptr = o_list->raw_obj_arr; i < o_list->max_obj; 
							i++, obj_ptr++)
      {
         /* group_info can be NULL while is_group is set, how to handle 
	    this one??? BMR 8/1/2000 
	    if( obj_ptr->is_group && obj_ptr->group_info != NULL ) */
         if( obj_ptr->is_group )
            free_group_list( obj_ptr->group_info );
         if( obj_ptr->is_special )
            HDfree( obj_ptr->spec_info );
      }		/* end for */
      HDfree(o_list->srt_obj_arr);
      HDfree(o_list->raw_obj_arr);
      HDfree(o_list);
   }
   else
       fprintf(stderr, ">>>free_obj_list failed - attempting to free a NULL list \n");
}	/* end free_obj_list() */

int sort_obj_list_by_tag(const void *p1, const void *p2)
{
   const objinfo_t *a = (const objinfo_t *) *((const void **) p1);
   const objinfo_t *b = (const objinfo_t *) *((const void **) p2);

   if (a->tag > b->tag)
      return (1);
   if (a->tag < b->tag)
      return (-1);
   if (a->ref > b->ref)
      return (1);
   if (a->ref < b->ref)
      return (-1);
   return (0);
}	/* end sort_obj_info_by_tag() */

#if 0 /* No longer possible since objects can have more than one label 
       * -GV 6/12/97 */
int sort_obj_list_by_name(const void *p1, const void *p2)
{
    const objinfo_t *a = (const objinfo_t *) *((void **) p1);
    const objinfo_t *b = (const objinfo_t *) *((void **) p2);

	/* Any label has priority over no label, else sort alphabetically */
    if (a->has_label)
      {
          if (b->has_label)
              return (HDstrcmp(a->lab_info, b->lab_info));
          else
              return (1);
      }		/* end if */
    else
      {
          if (b->has_label)
              return (-1);
          else
              return (0);
      }		/* end else */
}	/* end sort_obj_info_by_tag() */
#endif

void sort_obj_list(objlist_t * o_list, sort_t sort_type)
{
   switch (sort_type)
   {
#if 0 /* No longer possible since objects can have more than one label 
       * -GV 6/12/97 */
      case ONAME:	/* sort by name order */
          qsort(o_list->srt_obj_arr, o_list->max_obj, sizeof(objinfo_t *), sort_obj_list_by_name);
          break;
#endif
      case OGROUP:		/* sort by group order */
          break;	/* not currently implemented */

      case OTAG:	/* sort by tag order */
          qsort(o_list->srt_obj_arr, o_list->max_obj, sizeof(objinfo_t *), sort_obj_list_by_tag);
          break;

      case OFILE:	/* sort by file order */
      default:
          break;
   }  /* end switch() */
}  /* end sort_obj_list() */

/* Misc. utility functions */
int int32_compare(const void *a, const void *b)
{
   if (*(const int32 *) a > *(const int32 *) b)
      return (1);
   else if (*(const int32 *) a < *(const int32 *) b)
      return (-1);
   else
      return (0);
}  /* end int32_compare() */

void sort(int32 *chosen, int32 choices)
{
    qsort((void *) chosen, choices, sizeof(int32), int32_compare);
}

/* resetBuff frees the passed-in pointer and resets it to NULL,
   if it is not NULL.  Its purpose is to make cleaning up simpler 
   throughout the entire dumper */
void resetBuff( VOIDP *ptr )
{
   if( *ptr != NULL )
   {
      HDfree(*ptr);
      *ptr = NULL;
   }
}

/* parse_number_opts take a list of numbers separated by commas then 
   retrieves the numbers and stores them in the structure provided by
   the caller.  This routine is used by all the routines
   parse_dumpxx_opts to parse the index or ref list that accompanies
   option -i or -r */
void
parse_number_opts( char *argv[],
                   int *curr_arg, 
                   number_filter_t *filter)
{
   int32 numItems = 0, i;
   char *tempPtr = NULL;
   char *ptr = NULL;

   /* put a temp ptr at the beginning of the given list of numbers, 
      separated by commas, for example, 1,2,3 */
   ptr = argv[*curr_arg];

   /* check if it's the end of the command */
   if( ptr == NULL )
   {
      printf("Missing values for option\n");
      exit(1);
   }

   /* then traverse the list and count the number of items in it */
   while ((tempPtr = HDstrchr(ptr, ',')) != NULL)
   {
      numItems++;       /* count number of items in the list */
      ptr = tempPtr + 1;/* forward pointer to next item, after a comma */
   }  /* end while */
   if (*ptr != '\0')	/* count the last item */
      numItems++;

   /* allocate space to hold all the items in the list */
   filter->num_list = (int32 *) HDmalloc(sizeof(intn) * numItems);
   CHECK_ALLOC( filter->num_list, "filter->num_list", "parse_number_opts" );

   /* go back to the beginning of the list and read in the numbers */
   ptr = argv[*curr_arg];
   i = 0;  /* index of the list */
   while ( i < numItems )
   {
      tempPtr = HDstrchr(ptr, ',');
      if( tempPtr != NULL )
         *tempPtr = '\0';  /* end the string of digits */
      filter->num_list[i] = atoi(ptr);  /* convert string to digits */
      ptr = tempPtr + 1;
      i++;
   }
   filter->num_items = numItems;   /* save the number of items */
}  /* parse_number_opts */

/* parse_string_opts take a list of strings separated by commas then 
   retrieves the strings and stores them in the structure provided by
   the caller.  This routine is used by all the routines 
   parse_dumpxx_opts to parse the name or class list that accompanies
   option -n or -c */
void
parse_string_opts( char *argv[],
                   int *curr_arg, 
                   char_filter_t *filter)
{
   int32 numItems = 0, i;
   char *tempPtr = NULL;
   char *ptr = NULL;

   /* put a temp pointer at the beginning of the list of strings,
      separated by commas */
   ptr = argv[*curr_arg];

   /* check if it's the end of the command */
   if( ptr == NULL )
   {
      printf("Missing values for option\n");
      exit(1);
   }

   /* then traverse the list and count the number of strings in it */
   while ((tempPtr = HDstrchr(ptr, ',')) != NULL)
   {
      numItems++;
      ptr=tempPtr+1;
   }  /* end while */
   if (*ptr != '\0')	/* count the last item */
      numItems++;

   /* allocate space to hold pointers that will point to the given strings */
   filter->str_list = (char **) HDmalloc(sizeof(char *) * numItems);
   CHECK_ALLOC( filter->str_list, "filter->str_list", "parse_string_opts" );

   /* go back to the beginning of the list and read in the given strings */
   ptr = argv[*curr_arg];
   i = 0;  /* init the index of the list */
   while ( i < numItems )
   {
      tempPtr = HDstrchr(ptr, ','); /* find the end of a string */
      if( tempPtr != NULL )
         *tempPtr = '\0';  /* end the string with a NULL char */

      /* allocate space for each string */
      filter->str_list[i] = (char *)HDmalloc(sizeof(char) * (HDstrlen(ptr)+1));
      CHECK_ALLOC( filter->str_list[i], "filter->str_list[i]", "parse_string_opts" );
      HDstrcpy(filter->str_list[i], ptr);  /* get the current string */
      ptr = tempPtr + 1;  /* move pointer to next item or end of list */
      i++;
   }  /* end while */

   filter->num_items = numItems;	/* save the number of items */

} /* parse_string_opts */

/* validate_pos makes sure that number is > 0 so we are not going to
   allocate 0 elements
   This routine is replaced by the macro called CHECK_POS just because
   the error checkings are being done that way! 7/27/00 
*/

/* if there are any specific datasets requested, alloc_index_list
   allocates space for the list of indices of these requested items */
void
alloc_index_list(
        int32 **index_list,
        int32 num_chosen )
{
   int32 i = -1;        /* used to pass into HDmemfill as dummmy? */

   *index_list = (int32 *) HDmalloc(sizeof(int32) * num_chosen);
   CHECK_ALLOC( *index_list, "index_list", "alloc_index_list" );

   i = (-1);
   HDmemfill(*index_list, &i, sizeof(int32), num_chosen);
}  /* end of alloc_index_list */

