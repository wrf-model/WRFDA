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
static char RcsId[] = "@(#)$Revision: 1.30 $";
#endif

/* $Id: hdp_list.c,v 1.30 2002/03/08 05:58:23 bmribler Exp $ */

#include "mfhdf.h"
#include "hdp.h"
#include <ctype.h>

#define NUM_FIELD_WIDTH 5
#define TAGNAME_FIELD_WIDTH 20
#define TAG_FIELD_WIDTH 8
#define REF_FIELD_WIDTH 8
#define INDEX_FIELD_WIDTH 12
#define OFFSET_FIELD_WIDTH 12
#define LENGTH_FIELD_WIDTH 12
#define LABEL_FIELD_WIDTH 15
#define DESC_FIELD_WIDTH 15

static void 
list_usage(intn argc, 
           char *argv[])
{
    printf("Usage:\n");
    printf("%s list [-acensldg] [-o<f|g|t|n>] [-t tag] <filelist>\n", argv[0]);
    printf("\t-a\tPrint annotations of items (sets long output)\n");
    printf("\t-c\tPrint classes of items (sets long output)\n");
    printf("\t-n\tPrint names or labels of items (sets long output)\n");
    printf("\t-e\tPrint special element information of items (sets long output)\n");
    printf("\t-s\tShort output (default)\n");
    printf("\t-l\tLong output\n");
    printf("\t-d\tDebugging output\n");
    printf("\t-g\tPrint groups only\n");
    printf("\t-t <number>\tPrint items of with a given tag number\n");
    printf("\t-t <name>\tPrint items of with a given tag name\n");
    printf("\t-of\tPrint items in the order found in the file\n");
    printf("\t-og\tPrint items in group order\n");
    printf("\t-ot\tPrint items in tag order (default)\n");
#if 0 /* No longer possible since objects can have more than one label 
       * -GV 6/12/97 */
    printf("\t-on\tPrint items in name or label order\n");
#endif
    printf("\t<filelist>\tList of hdf file names, separated by spaces\n");
}	/* end list_usage() */

static void 
init_list_opts(list_info_t * list_opts)
{
    list_opts->order = OTAG;	/* default ordering is by tag */
    list_opts->verbosity = VSHORT;	/* default verbosity is a short list */
    list_opts->limit = LNONE;	/* default is all the tag/refs */
    list_opts->class = FALSE;	/* don't dump class information */
    list_opts->name = FALSE;	/* don't dump name information */
    list_opts->desc = FALSE;	/* don't dump annotation information */
    list_opts->spec = FALSE;	/* don't dump special element information */
    list_opts->group = FALSE;	/* don't dump group information */
    list_opts->limit_tag = 0;	/* initialize... */
}	/* end init_list_opts() */

static intn 
parse_list_opts(list_info_t * list_opts, 
                intn curr_arg, 
                intn argc, 
                char *argv[])
{
    intn        ret = SUCCEED;

    for (; curr_arg < argc; curr_arg++)
      {
#if defined(WIN386) || defined(DOS386)
          if (argv[curr_arg][0] == '-' || argv[curr_arg][0] == '/')
#else
          if (argv[curr_arg][0] == '-' )
#endif /* for DOS/WINDOWS */
            {
                ret++;
                switch (argv[curr_arg][1])
                  {
                  case 'a':	/* print annotations */
                      list_opts->desc = TRUE;	/* dump description information */
                      if (list_opts->verbosity == VSHORT)
                          list_opts->verbosity = VLONG;		/* verbosity is a long list */
                      break;

                  case 'c':	/* print classes */
                      list_opts->class = TRUE;	/* dump class information */
                      if (list_opts->verbosity == VSHORT)
                          list_opts->verbosity = VLONG;		/* verbosity is a long list */
                      break;

                  case 'n':	/* print names */
                      list_opts->name = TRUE;	/* dump name/label information */
                      if (list_opts->verbosity == VSHORT)
                          list_opts->verbosity = VLONG;		/* verbosity is a long list */
                      break;

                  case 'e':	/* print special element info */
                      list_opts->spec = TRUE;	/* dump special element information */
                      if (list_opts->verbosity == VSHORT)
                          list_opts->verbosity = VLONG;		/* verbosity is a long list */
                      break;

                  case 's':	/* short output */
                      list_opts->verbosity = VSHORT;	/* verbosity is short */
                      break;

                  case 'l':	/* long output */
                      list_opts->verbosity = VLONG;		/* verbosity is long */
                      break;

                  case 'd':	/* debugging output */
                      list_opts->verbosity = VDEBUG;	/* verbosity is debug */
                      break;

                  case 'g':	/* print only groups */
#ifdef LATER
                      list_opts->limit = LGROUP;	/* limit to group output */
#endif
                      list_opts->group = TRUE;	/* dump group info */
                      if (list_opts->verbosity == VSHORT)
                          list_opts->verbosity = VLONG;		/* verbosity is long */
                      break;

                  case 't':	/* print only items of one tag */
                      curr_arg++;
                      ret++;
                      if (isdigit(argv[curr_arg][0]))
                        {
                            list_opts->limit = LTAGNUM;		/* limit to tag name output */
                            list_opts->limit_tag = atoi(argv[curr_arg]);
                            list_opts->limit_name = tagnum_to_name(list_opts->limit_tag);
                        }	/* end if */
                      else
                        {	/* must be a tag name */
                            list_opts->limit = LTAGNAME;	/* limit to tag name output */
                            list_opts->limit_name = HDstrdup(argv[curr_arg]);
                            list_opts->limit_tag = tagname_to_num(list_opts->limit_name);
                            if (list_opts->limit_tag == DFTAG_NULL)
                              {
                                  printf("ERROR: invalid tag name: %s\n", list_opts->limit_name);
                                  return (FAIL);
                              }		/* end if */
                        }	/* end else */
                      break;

                  case 'o':	/* order the items in some way */
                      switch (argv[curr_arg][2])
                        {
                        case 'g':
                            list_opts->order = OGROUP;	/* ordering is by group */
                            break;

                        case 't':
                            list_opts->order = OTAG;	/* ordering is by tag */
                            break;

                        case 'f':
                            list_opts->order = OFILE;	/* ordering is by file */
                            break;

#if 0 /* No longer possible since objects can have more than one label 
       * -GV 6/12/97 */
                        case 'n':
                            list_opts->order = ONAME;	/* ordering is by name */
                            break;
#endif
                        default:
                            printf("ERROR: Invalid list ordering!\n");
                            return (FAIL);
                        }	/* end switch */
                      break;

                  default:		/* invalid list option */
                      printf("ERROR: Invalid list option!\n");
                      return (FAIL);
                  }		/* end switch */
            }	/* end if */
      }		/* end for */

    return (ret);
}	/* end parse_list_opts */

static void 
print_list_header(list_info_t * list_opts)
{
    switch (list_opts->verbosity)
      {
      case VSHORT:		/* short output */
              /* no header */
          break;

      case VLONG:	/* long output */
          printf("%*s%*s%*s%*s%*s\n",
                 NUM_FIELD_WIDTH, "no",
                 TAGNAME_FIELD_WIDTH, "tagname",
                 TAG_FIELD_WIDTH, "tag", REF_FIELD_WIDTH, "ref",
                 INDEX_FIELD_WIDTH, "  index_by_tag");
          break;

      case VDEBUG:		/* debugging output */
          printf("%*s%*s%*s%*s%*s%*s%*s\n",
                 NUM_FIELD_WIDTH, "no",
                 TAGNAME_FIELD_WIDTH, "tagname",
                 TAG_FIELD_WIDTH, "tag", REF_FIELD_WIDTH, "ref",
                 INDEX_FIELD_WIDTH, "  index_by_tag",
                 OFFSET_FIELD_WIDTH, "offset",
                 LENGTH_FIELD_WIDTH, "length");
          break;
      }		/* end switch() */
}	/* end print_list_header() */

/* Exported
 * print all data annotations, which are either data labels or data
   descriptions, for object with tag/ref
   This routine is used by print_data_labels and print_data_descs
   for common code */
intn
print_annots_by_object(
		const char *fname,
		int32 an_id, 
		ann_type annot_type,
		uint16 tag, 
		uint16 ref)
{
   intn  i;
   char  *buf = NULL;
   int32  ann_num;
   int32  ann_length;
   int32  ann_id = FAIL;
   int32 *ann_list = NULL;
   char *annot_type_text = "invalid";	/* "label" or "description" */
   char *func_name = "print_annots_by_object"; /* used to print error msg */
   char error_item[256];	/* holds tag, ref, and fname for error msg */
   intn   ret_value = SUCCEED;

   /* stores the current values tag, ref, and file name in error_item,
	just to simplify the error printing statement */
   sprintf( error_item, "object tag=%d, ref=%d, in file %s", tag, ref, fname ); 

   /* validate annotation type before processing */
   if( annot_type == AN_DATA_LABEL )
      annot_type_text = "label";
   else if( annot_type == AN_DATA_DESC )
      annot_type_text = "description";
   else
      ERROR_GOTO_2("%s: invalid annotation type for\n             %s\n",
		  				func_name, error_item );

   /* find number of labels for object with tag/ref */
   ann_num = ANnumann(an_id, annot_type, tag, ref);
   if (FAIL == ann_num)
      ERROR_GOTO_2("%s: ANnumann failed for object %s\n",
	 					func_name, error_item );

   if (ann_num > 0 )
   { /* print data annotation */

      /* allocate space for all label/description id's for data object */
      ann_list = HDmalloc(ann_num*sizeof(int32));
      CHECK_ALLOC( ann_list, "ann_list", func_name );

      /* retrieve all the data objects label/description handles and 
	 store them in the buffer ann_list */
      if (FAIL == ANannlist( an_id, annot_type, tag, ref, ann_list ))
         ERROR_GOTO_2("%s: ANannlist failed for %s\n",
	 	func_name, error_item );

      /* for every data label/description */
      for(i = 0; i < ann_num; i++)
      {
         ann_id = ann_list[i];  /* get next annotation id */

         ann_length =  ANannlen(ann_id); /* get annotation length */
         if (FAIL == ann_length)
	    ERROR_GOTO_4( "%s: ANannlen failed for\n               %d'th data %s for %s\n", 
		func_name, i, annot_type_text, error_item );

	 /* allocate space for the data annotation */
         buf = HDcalloc((ann_length+1) * sizeof(char),1);
         CHECK_ALLOC( buf, "buf", func_name );

         buf[ann_length] = '\0';

	 /* read annotation data */
         if (FAIL == ANreadann(ann_id, buf, ann_length+1))
            ERROR_GOTO_4( "%s: ANreadann failed on the %d'th data %s for\n               %s\n", func_name, i, annot_type_text, error_item );

	 /* print the annotation with title depending on annotation type */
	 if( annot_type == AN_DATA_LABEL )
            printf("%*s%s\n", LABEL_FIELD_WIDTH, " Name/Label=", buf);
	 else /* annot_type == AN_DATA_DESC -already checked for else */
            printf("%*s%s\n", LABEL_FIELD_WIDTH, "  Description=", buf);

         /* end access */
         if (FAIL == ANendaccess(ann_id))
            ERROR_GOTO_4( "%s: ANendaccess failed on the %d'th data %s \n              for %s\n", func_name, i, annot_type_text, error_item );

         /* reset id and free space for data label/description */
         ann_id = FAIL;
         HDfree(buf);
         buf = NULL;
      } /* end for every data label/description */
        
      /* cleanup */
      HDfree(ann_list);
      ann_list = NULL;
   }  /* end if num_ann > 0 */

  done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
          if (ann_id != FAIL)
              ANendaccess(ann_id);        
          if (buf != NULL)
              HDfree(buf);
      }
    /* Normal cleanup */

    return ret_value;
} /* print_annots_by_object() */

/* print all data labels for object with tag/ref */
intn
print_data_labels(
		const char *fname,
		int32 an_id, 
		uint16 tag, 
		uint16 ref)
{
   intn ret_value = SUCCEED;

   ret_value = print_annots_by_object(fname, an_id, AN_DATA_LABEL, tag, ref);
   if( ret_value == FAIL )
      ERROR_GOTO_0( "in print_data_labels\n" );

  done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
    /* Normal cleanup */

    return ret_value;
} /* end print_data_labels */

/* print all data descriptions for object with tag/ref */
intn
print_data_descs(
		const char *fname,
		int32 an_id, 
		uint16 tag, 
		uint16 ref)
{
   intn ret_value = SUCCEED;

   ret_value = print_annots_by_object(fname, an_id, AN_DATA_DESC, tag, ref);
   if( ret_value == FAIL )
      ERROR_GOTO_0( "in print_data_descs\n" );

  done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
    /* Normal cleanup */

    return ret_value;
} /* end print_data_descs */

/* Prints all annotations in the file.
   This routine is used by print_all_data_labels, print_all_data_descs, 
print_all_file_labels, and print_all_file_descs for the common code. */
intn
print_annots_in_file(
		int32 an_id,
		const char* fname,
		int32 n_annotations,
		ann_type annot_type )
{
   intn i;
   int32 len;
   char* annotation = NULL;
   char* func_name = "print_annots_in_file";
   int32 ann_id = FAIL;
   char *annot_type_text = "invalid";	/* "label" or "description" */
   intn   ret_value = SUCCEED;

   /* validate annotation type before processing */
   if( annot_type == AN_DATA_LABEL )
      annot_type_text = "Data Label";
   else if( annot_type == AN_DATA_DESC )
      annot_type_text = "Data Description";
   else if( annot_type == AN_FILE_LABEL )
      annot_type_text = "File Label";
   else if( annot_type == AN_FILE_DESC )
      annot_type_text = "File Description";
   else
      ERROR_GOTO_2("%s: invalid annotation type for file %s\n",
		  				func_name, fname );

   /* for all annot_type annotations in the file */
   for(i = 0; i< n_annotations; i++) 
   {  
      /* select i'th annotation */
      ann_id = ANselect(an_id, i, annot_type );
      if (FAIL == ann_id)
         ERROR_GOTO_4("%s: ANselect failed on the %d'th %s for file %s\n",
			func_name, i, annot_type_text, fname);

      /* get length of i'th annotation */
      len = ANannlen(ann_id);
      if (FAIL == len)
         ERROR_GOTO_4("%s: ANannlen failed on the %d'th %s for file %s\n",
			func_name, i, annot_type_text, fname);

      /* allocate space for an annotation */
      annotation = (char *) HDcalloc(len + 1,1);
      CHECK_ALLOC( annotation, "annotation", func_name );

      /* read in annotation and print it */
      if(ANreadann(ann_id, annotation, len+1)!= FAIL)
          printf("%s #%ld: %s\n", annot_type_text, (long)i, annotation);
      else
         ERROR_GOTO_4("%s: ANreadann failed on the %d'th %s for file %s\n", 
			func_name, i, annot_type_text, fname);

      /* end access */
      if (FAIL == ANendaccess(ann_id))
         ERROR_GOTO_4("%s: ANendaccess failed on the %d'th %s for file %s\n", 
			func_name, i, annot_type_text, fname);

      /* reset id and free space for annotation */
      ann_id = FAIL;
      HDfree(annotation);
      annotation = NULL;
   } /* end for every annotation in file */ 

  done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
          if (ann_id != FAIL)
              ANendaccess(ann_id);
          if (annotation != NULL)
              HDfree(annotation);
      }
    /* Normal cleanup */
    
    return ret_value;
} /* end print_annots_in_file */

/* Exported
 * prints all data labels in file */
intn
print_all_data_labels(const char *fname, 
                      int32 an_id)
{
   int32 n_file_label;
   int32 n_data_label;
   int32 n_file_desc;
   int32 n_data_desc;
   intn  ret_value = SUCCEED;

   /* find out how many file labels/descs and data labels/descs in file */
   if (FAIL == ANfileinfo(an_id, &n_file_label, &n_file_desc, &n_data_label, 
                          &n_data_desc))
      ERROR_GOTO_1("print_all_data_labels: ANfileinfo failed for file %s\n",
					fname);

   /* prints all the data labels in the file */
   ret_value = print_annots_in_file( an_id, fname, n_data_label, AN_DATA_LABEL );
   if( ret_value == FAIL )
      ERROR_GOTO_0("in print_all_data_labels\n" );

  done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
    /* Normal cleanup */

    return ret_value;
} /* print_all_data_labels() */

/* Exported
 * prints all data descriptions in file */
intn
print_all_data_descs(const char *fname, 
                     int32 an_id)
{
    int32 len;
    char *desc = NULL;
    int32 ann_id = FAIL;
    intn i;
    int32 n_file_label;
    int32 n_file_desc;
    int32 n_data_label;
    int32 n_data_desc;
    intn  ret_value = SUCCEED;

    /* find out how many file labels/descs and data labels/descs in file */
    if (FAIL == ANfileinfo(an_id, &n_file_label, &n_file_desc, &n_data_label, 
                           &n_data_desc))
      {
          fprintf(stderr,"ANfileinfo failed for file %s\n", fname);
          ret_value = FAIL;
          goto done;
      }

    /* for all data descs */
    for(i = 0; i< n_data_desc; i++) 
      {  
          /* select i'th data desc */
          ann_id = ANselect(an_id, i, AN_DATA_DESC);
          if (FAIL == ann_id)
            {
                fprintf(stderr,"ANselect failed for %d'th data description in file %s\n", i, fname);
                ret_value = FAIL;
                goto done;
            }

          /* get length of i'th data desc */
          len = ANannlen(ann_id);
          if (FAIL == len)
            {
                fprintf(stderr,"ANannlen failed for %d'th data description in file %s\n", i, fname);
                ret_value = FAIL;
                goto done;
            }

          /* allocate room for a data desc */
          desc = (char *) HDcalloc(len + 1,1);
          CHECK_ALLOC( desc, "desc", "print_all_data_descs" );

          /* read in data desc and print it */
          if(ANreadann(ann_id, desc, len+1)!= FAIL)
              printf("Data ID Annotation #%ld: %s\n", (long)i, desc);
          else
            {
                fprintf(stderr,"ANreadann failed for %d'th data description in file %s\n",i, fname);
                ret_value = FAIL;
                goto done;
            }

          /* end access */
          if (FAIL == ANendaccess(ann_id))
            {
                fprintf(stderr,"ANendaccess failed for %d'th data description in file %s\n",i, fname);
                ret_value = FAIL;
                goto done;
            }

          /* reset id and free space for desc */
          ann_id = FAIL;
          HDfree(desc);
          desc = NULL;
      } /* end for every data desc */ 

  done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
    /* Normal cleanup */
    if (ann_id != FAIL)
        ANendaccess(ann_id);        
    if (desc != NULL)
        HDfree(desc);

    return ret_value;
} /* print_all_data_descs() */

intn
print_all_file_labels(const char *fname, 
                      int32 an_id)
{
    int32 len;
    char *label = NULL;
    intn i;
    int32 ann_id = FAIL;
    int32 n_file_label;
    int32 n_file_desc;
    int32 n_data_label;
    int32 n_data_desc;
    intn  ret_value = SUCCEED;

    /* find out how many file labels/descs and data labels/descs in file */
    if (FAIL == ANfileinfo(an_id, &n_file_label, &n_file_desc, &n_data_label, 
                      &n_data_desc))
      {
          fprintf(stderr,"ANfileinfo failed for file %s\n", fname);
          ret_value = FAIL;
          goto done;
      }

    /* for all file labels */
    for(i = 0; i< n_file_label; i++) 
      {  
          /* select i'th file label */
          ann_id = ANselect(an_id, i, AN_FILE_LABEL);
          if (FAIL == ann_id)
            {
                fprintf(stderr,"ANselect failed for %d'th label for file %s\n", i, fname);
                ret_value = FAIL;
                goto done;
            }

          /* get length of i'th file label */
          len = ANannlen(ann_id);
          if (FAIL == len)
            {
                fprintf(stderr,"ANannlen failed for %d'th label for file %s\n",i, fname);
                ret_value = FAIL;
                goto done;
            }

          /* allocate room for the file label */
          label = (char *) HDcalloc(len + 1,1); 
          CHECK_ALLOC( label, "label", "print_all_data_labels" );

          /* read in file label and print it */
          if(ANreadann(ann_id, label, len+1)!= FAIL)
              printf("File Label #%ld: %s\n", (long)i, label);
          else
            {
                fprintf(stderr,"ANreadann failed for %d'th label for file %s\n", i, fname);
                ret_value = FAIL;
                goto done;
            }

          /* end access */
          if (FAIL == ANendaccess(ann_id))
            {
                fprintf(stderr,"ANendaccess failed for %d'th label for file %s\n",i, fname);
                ret_value = FAIL;
                goto done;
            }

          /* reset id and free space for label */
          ann_id = FAIL;
          HDfree(label);
          label = NULL;
      } /* end for every file label */ 

  done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
    /* Normal cleanup */
    if (ann_id != FAIL)
        ANendaccess(ann_id);        
    if (label != NULL)
        HDfree(label);

    return ret_value;
}	/* end print_all_file_labels() */

intn 
print_all_file_descs(const char *fname, 
		     list_info_t* list_opts, /* for print_SDattrs */
                     int32 an_id)
{
    /* file desc */
    int32       len; 
    char       *desc = NULL;
    int32       ann_id = FAIL;
    intn       i;
    int32       n_file_label;
    int32       n_file_desc;
    int32       n_data_label;
    int32       n_data_desc;
    /* SDS */
    int32       sd_fid = FAIL;
    int32       ndsets, nattrs;
    char       *attr_nt_desc = NULL;
    VOIDP       attr_buf = NULL;
    intn        ret_value = SUCCEED;

    /* find out how many file labels/descs and data labels/descs in file */
    if (FAIL == ANfileinfo(an_id, &n_file_label, &n_file_desc, &n_data_label, 
                           &n_data_desc))
      {
          fprintf(stderr,"ANfileinfo failed for file %s \n",fname);
          ret_value = FAIL;
          goto done;
      }

    /* for all file descs */
    for(i = 0; i< n_file_desc; i++) 
      {  
          /* select i'th file desc */
          ann_id = ANselect(an_id, i, AN_FILE_DESC);
          if (FAIL == ann_id)
            {
                fprintf(stderr,"ANselect failed for %d'th desc for file %s \n",i, fname);
                ret_value = FAIL;
                goto done;
            }

          /* get length of i'th file desc */
          len = ANannlen(ann_id);
          if (FAIL == len)
            {
                fprintf(stderr,"ANannlen failed for %d'th desc for file %s \n",i,fname);
                ret_value = FAIL;
                goto done;
            }

          /* allocate room for the file desc */
          desc = (char *) HDcalloc(len + 1,1);
          CHECK_ALLOC( desc, "desc", "print_all_file_descs" );

          /* read in file desc and print it */
          if(ANreadann(ann_id, desc, len+1)!= FAIL)
              printf("File description #%ld: %s\n", (long)i, desc);
          else
            {
                fprintf(stderr,"ANreadann failed for %d'th desc for file %s \n",i,fname);
                ret_value = FAIL;
                goto done;
            }

          /* end access */
          if (FAIL == ANendaccess(ann_id))
            {
                fprintf(stderr,"ANendaccess failed for %d'th desc for file %s \n",i,fname);
                ret_value = FAIL;
                goto done;
            }

          /* reset id and free space for label */
          ann_id = FAIL;
          HDfree(desc);
          desc = NULL;
      } /* end for every file desc */ 

    /* all SDS global attributes are considered file descriptions */
    if ((sd_fid = SDstart(fname, DFACC_READ)) != FAIL)
      { /* SD global attributes */
dump_info_t dump_opts;
init_dump_opts( &dump_opts );
          if (SDfileinfo(sd_fid, &ndsets, &nattrs) != FAIL)
	  {
	     /* BMR: installed input file name to opts for dumpfull 
                in print_SDattrs to use - 6/16/2000 */
             print_SDattrs( sd_fid, stdout, nattrs, &dump_opts );
               /* temporary use stdout until fixing hdp_list to print
                  to a FILE *fp */
	  }
          else
            {
                fprintf(stderr,"Failure in SDfileinfo for file %s\n",
fname);
                ret_value = FAIL;
                goto done;
            }
          if (FAIL == SDend(sd_fid))
             fprintf(stderr, "SDend failed for the current file\n" );
          sd_fid = FAIL; /* reset */
      }         /* end if  SDstart */

  done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
          if (ann_id != FAIL)
              ANendaccess(ann_id);        
          if (desc != NULL)
              HDfree(desc);
          if (attr_nt_desc != NULL)
              HDfree(attr_nt_desc);
          if (attr_buf != NULL)
              HDfree((VOIDP) attr_buf);
      }
    /* Normal cleanup */

    return ret_value;
}	/* end print_all_file_descs() */

/* BMR: use part of print_all_file_descs for this routine to just print
   the file annotations because print_all_file_descs also prints SD
   file attributes.  Probably will separate SD file attributes when
   adding GR file attributes */
intn 
print_file_descs(const char *f_name,
                 int32 an_id ) 
{
    /* file desc */
    int32       len; 
    char       *desc = NULL;
    int32       ann_id = FAIL;
    intn       i;
    int32       n_file_label;
    int32       n_file_desc;
    int32       n_data_label;
    int32       n_data_desc;
    intn        ret_value = SUCCEED;

    /* find out how many file labels/descs and data labels/descs in file */
    if (FAIL == ANfileinfo(an_id, &n_file_label, &n_file_desc, &n_data_label, 
                           &n_data_desc))
      {
          fprintf(stderr,"ANfileinfo failed for file %s \n",f_name);
          ret_value = FAIL;
          goto done;
      }

    /* for all file descs */
    for(i = 0; i< n_file_desc; i++) 
      {  
          /* select i'th file desc */
          ann_id = ANselect(an_id, i, AN_FILE_DESC);
          if (FAIL == ann_id)
            {
                fprintf(stderr,"ANselect failed for %d'th desc for file %s \n",i, f_name);
                ret_value = FAIL;
                goto done;
            }

          /* get length of i'th file desc */
          len = ANannlen(ann_id);
          if (FAIL == len)
            {
                fprintf(stderr,"ANannlen failed for %d'th desc for file %s \n",i,f_name);
                ret_value = FAIL;
                goto done;
            }

          /* allocate room for the file desc */
          desc = (char *) HDcalloc(len + 1,1);
          CHECK_ALLOC( desc, "desc", "print_file_descs" );

          /* read in file desc and print it */
          if(ANreadann(ann_id, desc, len+1)!= FAIL)
              printf("File description #%ld: %s\n", (long)i, desc);
          else
            {
                fprintf(stderr,"ANreadann failed for %d'th desc for file %s \n",i,f_name);
                ret_value = FAIL;
                goto done;
            }

          /* end access */
          if (FAIL == ANendaccess(ann_id))
            {
                fprintf(stderr,"ANendaccess failed for %d'th desc for file %s \n",i,f_name);
                ret_value = FAIL;
                goto done;
            }

          /* reset id and free space for label */
          ann_id = FAIL;
          HDfree(desc);
          desc = NULL;
      } /* end for every file desc */ 

  done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
          if (ann_id != FAIL)
              ANendaccess(ann_id);        
          if (desc != NULL)
              HDfree(desc);
      }
    /* Normal cleanup */

    return ret_value;
}	/* end print_all_file_descs() */

/* prints all relevant information that an HDF object can have
   including annotations */
static intn
print_list_obj(const char *fname,
               list_info_t *l_opts, 
               objinfo_t * o_info, 
               intn o_num, 
               int32 an_id)
{
    int32  i;
    char  *s = NULL;
    char  *buf = NULL;
    intn   ret_value = SUCCEED;

    switch (l_opts->verbosity)
      {
      case VSHORT:		/* short output */
          /* handled elsewhere */
          break;

      case VLONG:	/* long output */
          printf("%*d%*s%*d%*d%*ld\n",
                 NUM_FIELD_WIDTH, o_num,
                 TAGNAME_FIELD_WIDTH,
                 ((s = HDgettagsname(o_info->tag)) == NULL ? HDstrdup("Unknown") : s),
                 TAG_FIELD_WIDTH, o_info->tag, REF_FIELD_WIDTH, o_info->ref,
                 INDEX_FIELD_WIDTH, (long) o_info->index);
          HDfree(s);	/* free tagname string */
          s = NULL;
          break;

      case VDEBUG:		/* debugging output */
          printf("%*d%*s%*d%*d%*ld%*ld%*ld\n",
                 NUM_FIELD_WIDTH, o_num,
                 TAGNAME_FIELD_WIDTH,
                 ((s = HDgettagsname(o_info->tag)) == NULL ? HDstrdup("Unknown") : s),
                 TAG_FIELD_WIDTH, o_info->tag, REF_FIELD_WIDTH, o_info->ref,
                 INDEX_FIELD_WIDTH, (long) o_info->index,
                 OFFSET_FIELD_WIDTH, (long) o_info->offset,
                 LENGTH_FIELD_WIDTH, (long) o_info->length);
          HDfree(s);	/* free tagname string */
          s = NULL;
          break;
      }		/* end switch */

   /* find data labels for object if any */
   if (l_opts->name == TRUE)
      if (FAIL == print_data_labels(fname, an_id, o_info->tag, o_info->ref))
         ERROR_GOTO_0("in print_list_obj\n");

   if (l_opts->class == TRUE)
      {
      }		/* end if */

   /* find data descs for object if any */
   if (l_opts->desc == TRUE )
      if (FAIL == print_data_descs(fname, an_id, o_info->tag, o_info->ref))
         ERROR_GOTO_0("in print_list_obj\n");

   if (l_opts->spec == TRUE && o_info->is_special)
   {
      switch (o_info->spec_info->key)
      {
         case SPECIAL_LINKED:
             printf("\tLinked Block: first %ld standard %ld per unit %ld\n",
                   (long) o_info->spec_info->first_len,
                   (long) o_info->spec_info->block_len,
                   (long) o_info->spec_info->nblocks);
             break;

         case SPECIAL_EXT:
             printf("\tExternal File: path %s  offset %ld\n",
                   o_info->spec_info->path, (long) o_info->spec_info->offset);
             break;

         case SPECIAL_COMP:
             printf("\tCompressed Element: compression type: %s  modeling type %s\n",
                   (o_info->spec_info->comp_type == COMP_CODE_NONE ? "None" :
                   (o_info->spec_info->comp_type == COMP_CODE_RLE ? "Run-Length" :
                   (o_info->spec_info->comp_type == COMP_CODE_NBIT ? "N-Bit" : "Unknown"))),
                   (o_info->spec_info->model_type == COMP_MODEL_STDIO ? "Standard" : "Unknown"));
             break;

         case SPECIAL_CHUNKED:
             printf("\tChunked element: chunk size %d, ndims %d, [",
                   (intn)o_info->spec_info->chunk_size,  (intn)o_info->spec_info->ndims);
             for (i = 0; i < o_info->spec_info->ndims; i++)
             {
                 printf("%d",(intn)o_info->spec_info->cdims[i]);
                 if(i != (o_info->spec_info->ndims -1 ))
                     printf(",");
             }
             printf("]\n");
             break;

         default:
             printf("\t Do not understand special element type %d \n",
                   o_info->spec_info->key);
             break;
         }	/* end switch */
      }	 /* end if */

/* BMR 8/1/00: added no_element test to make sure group_info is not NULL */
/*
   if (l_opts->group == TRUE && o_info->is_group && o_info->no_element != FALSE)
*/
   if (l_opts->group == TRUE && o_info->is_group )
   {
      DFdi       *g_obj = NULL;
      int32       num;

      if ((num = get_group_max(o_info->group_info)) != FAIL)
      {
         printf("\tContents: (%ld objects)\n", (long) num);
         g_obj = get_next_group(o_info->group_info, 0);
         while (g_obj != NULL)
         {
            printf("\t\t%-30s: (tag=%6d) ref=%d\n",
                  ((s = HDgettagsname(g_obj->tag)) == NULL ? HDstrdup("Unknown") : s),
            g_obj->tag, g_obj->ref);
            HDfree(s);	/* free tagname string */
            s = NULL;
            g_obj = get_next_group(o_info->group_info, 1);
         }		/* end while */
      }	/* end if */
   }		/* end if */

  done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
          if (s != NULL)
              HDfree(s);
          if (buf != NULL)
              HDfree(buf);
      }
    /* Normal cleanup */

    return ret_value;
}/* print_list_obj() */


/* print the library version of the file */
static void 
printfilever(int32 file_id)
{
    uint32 major, minor, release;
    char string[LIBVSTR_LEN+1];

    if (Hgetfileversion(file_id, &major, &minor, &release, string) == SUCCEED)
      {
        string[LIBVSTR_LEN] = '\0';	/* make it a null terminated string */
        printf("Last modified with %s\n\n", string);
      }
    else
        printf("(Has no library version information)\n\n");
}

/* low level object listing routine for HDF file */
intn 
do_list(intn curr_arg, 
        intn argc, 
        char *argv[], 
        intn  help )
{
   list_info_t list_opts;	/* list options */
   filelist_t *f_list = NULL;	/* list of files to dump */
   objlist_t  *o_list = NULL;	/* list of DD objects in a file */
   objinfo_t  *o_info = NULL;	/* pointer to a DD object */
   char       *f_name = NULL;	/* current file name to list */
   int32       fid = FAIL;	/* HDF file ID */
   intn        obj_num;		/* number of the object we are displaying */
   intn        status;		/* status from various function calls */
   char       *s = NULL;	/* temporary character pointer */
   int32       an_id = FAIL;	/* annotation interface handle */
   intn        ret_value = SUCCEED;

   if (help == TRUE )
   {
      list_usage(argc, argv);
      goto done;
   }  /* end if */

   /* incomplete command */
   if( curr_arg >= argc )
   {
      list_usage(argc, argv);
      ret_value = FAIL; /* so caller can be traced in debugging */
      goto done;
   }

   init_list_opts(&list_opts);
   if ((status = parse_list_opts(&list_opts, curr_arg, argc, argv)) == FAIL)
   {
      list_usage(argc, argv);
      ret_value = FAIL;
      goto done;
   }  /* end if */

   curr_arg += status;
   if (curr_arg >= argc || (f_list = make_file_list(curr_arg, argc, argv)) == NULL)
   {
      fprintf(stderr,"ERROR: No files to dump!\n");
      list_usage(argc, argv);
      ret_value = FAIL;
      goto done;
   }  /* end if */

   /* process each file */
   f_name = get_next_file(f_list, 0);
   while (f_name != NULL)
   {
      int label_flag, desc_flag;
      vinit_done = FALSE; 	/* reset global Vset variable */
      obj_num = 0;	/* number of the object we are displaying */
      fid = FAIL;
      an_id = FAIL;

      if ((fid = Hopen(f_name, DFACC_READ, 0)) != FAIL)
      {
         an_id = ANstart(fid);
         if (FAIL == an_id)
	    ERROR_GOTO_1( "do_list: ANstart failed for file %s \n", f_name );

	 label_flag = desc_flag = 0;
         if( list_opts.name == TRUE )
	    label_flag = CHECK_LABEL;
         if( list_opts.desc == TRUE )
	    desc_flag = CHECK_DESC;

         /* make list of all objects in file */
         o_list = make_obj_list(fid, label_flag | desc_flag |
                                   CHECK_GROUP | CHECK_SPECIAL);

         /* if there are any object in the file, print annotations if
	    requested, then the object information as requested */
         if (o_list != NULL)
         {
            /* print out filename, etc. */
            printf("File: %s\n", f_name);
            printfilever(fid);

            /* print file labels if requested */
            if (list_opts.name == TRUE)
               if (FAIL == print_all_file_labels(f_name, an_id))
                  ERROR_GOTO_0( "in do_list\n" );

            /* print file descriptions if requested */
            if (list_opts.desc == TRUE)
               if (FAIL == print_all_file_descs(f_name, &list_opts, an_id))
                  ERROR_GOTO_0( "in do_list\n" );

            /* sort list of objects in requested order */
            sort_obj_list(o_list, list_opts.order);

            /* print out list header according to options */
            print_list_header(&list_opts);

            /* Special case for short output */
            if (list_opts.verbosity == VSHORT)
            {
               uint16      last_tag = 0;

               o_info = get_next_obj(o_list, 0);  /* get first DD object */
               while (o_info != NULL)
               {
                  if( (list_opts.limit == LGROUP || list_opts.limit == LNONE) 
                      || list_opts.limit_tag == o_info->tag)
                  {
                     if (o_info->tag != last_tag)
                     {
                        s = HDgettagsname(o_info->tag);
                        if( s == NULL )
                           s = HDstrdup("Unknown");

                        printf("%s%-*s: (tag %d)\n", 
				(last_tag == 0 ? "" : "\n"),
                                TAGNAME_FIELD_WIDTH, s, o_info->tag);
                        last_tag = o_info->tag;
                        printf("\tRef nos: ");
                        HDfree(s);	/* free tagname string */
                        s = NULL; /* reset */
                     }		/* end if */
                     printf("%d ", o_info->ref);
                  }	/* end if */

                  /* advance to the next DD object */
                  o_info = get_next_obj(o_list, 1);
               }  /* end while o_info */
               printf("\n");
            }	/* end if verbosity */

            else /* must be verbose output */
            {
                /* Loop through all the objects in the file */
                o_info = get_next_obj(o_list, 0);  /* get first DD object */
                while (o_info != NULL)
                {
                   switch (list_opts.limit)
                   {
                      default:
                      case LNONE:
                      case LGROUP:
                          if (FAIL == print_list_obj(f_name, &list_opts, 
						o_info, obj_num,an_id))
                             ERROR_GOTO_0("in do_list\n");
                          break;

                       case LTAGNUM:
                       case LTAGNAME:
                           if (list_opts.limit_tag == o_info->tag)
                              if (FAIL == print_list_obj(f_name, &list_opts, 
						o_info, obj_num, an_id))
                           	 ERROR_GOTO_0("in do_list\n");
                           break;
                   }	/* end switch */
                   obj_num++;
                   o_info = get_next_obj(o_list, 1); /* advance to next DD object */
                }		/*end while */
             }	/* end else */

             /* free the object list */
             free_obj_list(o_list);
             o_list = NULL;
         }	/* end if o_list */

          /* cleanup section */
          if (vinit_done == TRUE)
             if (FAIL == Vfinish(fid))
                ERROR_GOTO_1("do_list: Vfinish failed for file %s\n", f_name);

          if (FAIL == ANend(an_id))
             ERROR_GOTO_1("do_list: ANend failed for file %s\n", f_name);
          an_id = FAIL; /* reset */

          if (Hclose(fid) == FAIL)
             ERROR_GOTO_1("do_list: Hclose failed for file %s\n", f_name);
          fid = FAIL; /* reset */

       }	/* end if */
       else
          ERROR_GOTO_1("in do_list: Hopen failed - possible invalid file name: %s", f_name);

       /* next file processing */
       /* get next file to process */
       f_name = get_next_file(f_list, 1);
   }		/* end while processing files*/
  done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
          if (fid != FAIL) /* check if file open still */
            {
                Hclose(fid);
                fid = FAIL;
            }
          if (an_id != FAIL) /* check if annotation handle still open */
            {
                ANend(an_id);
                an_id = FAIL;
            }
          if (s != NULL)
              HDfree(s);
          if (o_list != NULL)
              free_obj_list(o_list);
      }
    /* Normal cleanup */
    if (f_list != NULL)
        free_file_list(f_list);

    return ret_value;
}	/* end do_list() */
