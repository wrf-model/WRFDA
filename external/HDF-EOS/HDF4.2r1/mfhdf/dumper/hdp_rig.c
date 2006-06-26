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
static char RcsId[] = "@(#)$Revision: 1.27 $";
#endif

/* $Id: hdp_rig.c,v 1.27 2000/09/05 14:28:11 bmribler Exp $ */

#include <stdio.h>
#include "mfhdf.h"
#include "hdp.h"
#ifndef MIPSEL
#include <math.h>
#endif /* MIPSEL */

#define IMAGE 1

static void 
dumprig_usage(intn argc, 
              char *argv[])
{
    printf("Usage:\n");
#ifdef LATER
    printf("%s dumprig [-a|-i <indices>|-n <names>|-r <refs>] [-dhvc] [-o <filename> [-bx]] <filelist>\n", argv[0]);
#endif
    printf("%s dumprig [-a|-i <indices>|-m <n>|-r <refs>] [-dhv] [-o <filename> [-bx]] <filelist>\n", argv[0]);
    printf("\t-a\tDump all RIGs in the file (default)\n");
    printf("\t-i <indices>\tDump the RIGs at positions listed in <indices>\n");
#ifdef LATER
    printf("\t-n <names>\tDump the RIGs with name listed in <names>\n");
#endif
    printf("\t-m <n>\tDump the 8- or 24-bit RIGs only, <n> may be 8 or 24 \n");
    printf("\t-r <refs>\tDump the RIGs with reference number listed in <refs>\n");
    printf("\t-d\tDump data only, no tag/ref, formatted to input to hp2hdf\n");
    printf("\t-h\tDump header only, no annotation for elements nor data\n");
    printf("\t-v\tDump everything including all annotations (default)\n");
    printf("\t-c\tDo not add a carriage return to a long data line\n");
    printf("\t-o <filename>\tOutput to file <filename>\n");
    printf("\t-b\tBinary format of output\n");
    printf("\t-x\tAscii text format of output (default)\n");
    printf("\t<filelist>\tList of hdf file names, separated by spaces\n");
}	/* end list_usage() */

static void 
init_dumprig_opts(dump_info_t * dumprig_opts)
{
    dumprig_opts->filter = DALL;	/* default dump all RIGs */
    dumprig_opts->filter_num = NULL;	/* not by reference nor by index */
    dumprig_opts->filter_str = NULL;	/* no strings */
    dumprig_opts->num_chosen = (-1);	/* default dump all items */
    dumprig_opts->contents = DVERBOSE;	/* default dump all information */
    dumprig_opts->dump_to_file = FALSE;		/* don't dump to output file */
    dumprig_opts->file_type = DASCII;	/* default output is ASCII file */
    dumprig_opts->as_stream = FALSE; /* print output aligned, using carriage returns */
    dumprig_opts->print_pal = FALSE;     /* GR only, don't print palette */

    /* print output aligned, using carriage returns */
    dumprig_opts->as_stream = FALSE;

    /* print space characters (LF, FF, CR, space, tabs...) in \digit format */
    dumprig_opts->clean_output = FALSE;

    /* print data starting at column 5 unless reset otherwise */
    dumprig_opts->firstln_indent = 5;  /* Note: only for dump_rig, so 
					testfiles won't need to be changed */

    /* print data on a continuous line starting at column 5 unless
       reset otherwise */
    dumprig_opts->contln_indent = 5;  /* Note: only for dump_rig, so 
					testfiles won't need to be changed */

    /* GR only, print data using interlace at creation */
    dumprig_opts->interlace = NO_SPECIFIC;

    /* GR & SD only, print data of global attributes unless -g is given */
    dumprig_opts->no_gattr_data = FALSE;

    /* GR & SD only, print data of local attributes unless -l is given */
    dumprig_opts->no_lattr_data = FALSE;

    HDstrcpy(dumprig_opts->file_name, "\0");
}	/* end init_list_opts() */

static intn 
parse_dumprig_opts(dump_info_t *dumprig_opts, 
                   intn        *curr_arg,
                   intn         argc, 
                   char        *argv[], 
                   int         *model)
{
    int32  i;
    int32  numItems;
    char  *tempPtr = NULL;
    char  *ptr = NULL;

    if (*curr_arg >= argc)
        return (FAIL);

    while ((*curr_arg < argc) && (argv[*curr_arg][0] == '-'))
      {
          switch (argv[*curr_arg][1])
            {
            case 'a':	/* dump all, default */
                dumprig_opts->filter = DALL;
                (*curr_arg)++;
                break;

            case 'm':	/* dump the rigs with model specified as 8-bit or 24-bit */
                (*curr_arg)++;
                *model = atoi(argv[*curr_arg]);
                if (((*model) != 8) && ((*model) != 24))
                  {
                      printf("%s-bit raster model: not available.\n", argv[*curr_arg]);
                      exit(1);
                  }
                (*curr_arg)++;
                break;

            case 'i':	/* dump by index */
            case 'r':	/* dump by reference */
                if ((argv[*curr_arg][1]) == 'i')
                    dumprig_opts->filter = DINDEX;
                else
                    dumprig_opts->filter = DREFNUM;
                (*curr_arg)++;

                ptr = argv[*curr_arg];

                /* check if it's the end of the command */
                if( ptr == NULL )
                {
                   printf("Missing values for option\n");
                   exit(1);
                }
                numItems = 0;
                while ((tempPtr = HDstrchr(ptr, ',')) != NULL)
                  {
                      numItems++;
                      ptr=tempPtr+1;
                  }		/* end while */
                if (*ptr != '\0')	/* count the last item */
                    numItems++;

                dumprig_opts->filter_num = (intn *) HDmalloc(sizeof(intn) * numItems);
                if (dumprig_opts->filter_num == NULL)
                  {
                      printf("Not enough memory!\n");
                      exit(-1);
                  }

                ptr = argv[*curr_arg];
                i = 0;
                while ((tempPtr = HDstrchr(ptr, ',')) != NULL)
                  {
                      *tempPtr = '\0';
                      dumprig_opts->filter_num[i] = atoi(ptr);
                      ptr = tempPtr + 1;
                      i++;
                  }
                dumprig_opts->filter_num[i] = atoi(ptr);	/* get the last item */
                dumprig_opts->num_chosen = numItems;	/* save the number of items */

                (*curr_arg)++;
                break;

            case 'd':	/* dump data only */
                dumprig_opts->contents = DDATA;
                (*curr_arg)++;
                break;

            case 'h':	/* no annotations nor data */
                dumprig_opts->contents = DHEADER;
                (*curr_arg)++;
                break;

            case 'v':	/* dump all info */
                dumprig_opts->contents = DVERBOSE;
                (*curr_arg)++;
                break;

            case 'c':   /* do not add carriage returns to output data lines */
                dumprig_opts->as_stream = TRUE;
                (*curr_arg)++;
                break;    

            case 'o':   /* specify output file */
                dumprig_opts->dump_to_file = TRUE;

                /* Get file name */
                HDstrcpy(dumprig_opts->file_name, argv[++(*curr_arg)]);

                (*curr_arg)++;
                break;

            case 'b':   /* dump data in binary */
                dumprig_opts->file_type = DBINARY;
                (*curr_arg)++;
                break;

            case 'x':   /* dump data in ascii, also default */
                dumprig_opts->file_type = DASCII;
                (*curr_arg)++;
                break;

            default:	/* invalid dumprig option */
                printf("Warning: Invalid dumprig option %s\n", argv[*curr_arg]);
                return (FAIL);
            }	/* end switch */
      }		/* end while */
    return (SUCCEED);
}	/* end parse_dumprig_opts */


static intn 
drig(dump_info_t *dumprig_opts, 
     intn         curr_arg, 
     intn         argc,
     char        *argv[], 
     int          model)
{
    int32      *rig_chosen = NULL;
    int32       num_rig_chosen;
    int32       width, height;
    int32       ndsets;
    int32       temp;
    intn        i, k, x;
    char        file_name[MAXFNLEN];
    FILE       *fp = NULL;
    VOIDP       image = NULL;
    int         dumpall = 0;
    int         ncomps;
    int         il;
    file_type_t ft;
    intn        ret_value = SUCCEED;

    while (curr_arg < argc)
      {		/* Examine all files. */
          HDstrcpy(file_name, argv[curr_arg]);
          curr_arg++;

          num_rig_chosen = dumprig_opts->num_chosen;
          if (num_rig_chosen > 0)
            {
                if ((rig_chosen = (int32 *) HDmalloc(sizeof(int32) * num_rig_chosen)) == NULL)
                  {
                      fprintf(stderr,"Memory allocation error\n");
                      ret_value = FAIL;
                      goto done;
                  }		/* end if */

                k = (-1);
                HDmemfill(rig_chosen, &k, sizeof(int32), num_rig_chosen);
            }	/* end if */

          /* Determine which RIGs are to be displayed. */
          if (dumprig_opts->filter == DINDEX)
            {
                for (i = 0; i < dumprig_opts->num_chosen; i++)
                    rig_chosen[i] = dumprig_opts->filter_num[i];
                sort(rig_chosen, num_rig_chosen);  /* DREFNUM doesn't need this */ 
            }	/* end if */

          /* ASCII or Binary? */
          ft = dumprig_opts->file_type;
          switch(ft)
            {
            case DASCII:   /*  ASCII  file   */

                /* Get the name of the output file. */
                if (dumprig_opts->dump_to_file)
                    fp = fopen(dumprig_opts->file_name, "w");
                else
                    fp = stdout;
                if (dumprig_opts->contents != DDATA)
                    fprintf(fp, "File name: %s \n\n", file_name);

                /* Determine the number of images in a file. */
                if (model == 8)
                  { /* raster 8 */
                      if ((ndsets = DFR8nimages(file_name)) == FAIL)
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                  }
                else if (model == 24)
                  { /* raster 24 */
                      if ((ndsets = DF24nimages(file_name)) == FAIL)
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                  }
                else /* try both 8 and 24 */
                  {
                      if ((temp = DFR8nimages(file_name)) == FAIL)
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                      if ((ndsets = DF24nimages(file_name)) == FAIL)
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                      ndsets += temp;
                  }

                if (num_rig_chosen == -1)/* If all RIGs will be dumped, set the flag. */
                    dumpall = 1;

                x = 0;	/* Used as the index of the array of "rig_chosen[x]". */

                /* is this normal output here or debugging? leave for now */
                printf("ndsets=%d, dumpall=%d, num_chosen=%d\n",
                       (int)ndsets,(int)dumpall,(int)dumprig_opts->num_chosen);

                /* can only check index range here */
                for(i=0; 
                    i < dumprig_opts->num_chosen && dumprig_opts->filter == DINDEX; 
                    i++)
                  {
                      if((dumprig_opts->filter_num[i] > ndsets)
                         ||(dumprig_opts->filter_num[i] < 0))
                        {
                            fprintf(stderr,"\nThe index number %d is out of range\n",
                                    dumprig_opts->filter_num[i]);
                            ret_value = FAIL;
                            goto done;
                        }
                  }

                for (i = 0; 
                     i < ndsets && (dumpall!=0 || x < dumprig_opts->num_chosen); 
                     i++)
                  {	/* Examine all RIGs. */
                      int     compressed;
                      int     has_pal;
                      int32   eltsz;
                      int32   read_nelts;
                      uint16  rig_ref;
                      uint16  compr_type;

                      /* get dimensions of ri */
                      if (FAIL == DFGRgetimdims(file_name, &width, &height, &ncomps, &il))
                        {
                            ret_value = FAIL;
                            goto done;
                        }

                      /* Determine the size of each element; "ncomps" is 1 for an
                         8-bit image or 3 for a 24-bit image. */
                      eltsz = DFKNTsize(DFNT_UINT8 | DFNT_NATIVE) * ncomps;
                      read_nelts = width * height;	/* Number of elements to be read in. */
                      if ((image = (VOIDP) HDmalloc(read_nelts * eltsz)) == NULL)
                        {
                            fprintf(stderr,"Not enough memory!\n");
                            ret_value = FAIL;
                            goto done;
                        }

/*DFGRreqimil( 1 );*/
                      if (DFGRIgetimlut((const char *) file_name, image, width,
                                               height, IMAGE, 0, &compressed, &compr_type, &has_pal) == -1)
                        {
                            fprintf(stderr,"DFGRIgetimlut: Read error for file %s\n",file_name);
                            ret_value = FAIL;
                            goto done;
                        }

                      rig_ref = DFGRIlastref();	/* Determine the reference of the image just read. */

                      /* If the user has specificed the reference option, then 
                         something has to be done. 
                         Note: the reason why the following part was not done  
                         inside the above "switch" statement is that the reference 
                         number of a raster image cannot be appropriately retrieved
                         before actually reading in a raster image. */
                      if (dumprig_opts->filter == DREFNUM)
                        {
                            int         ref_found = 0, m;

                            /* Determine if the image just read has the reference specified by the user. */
                            for (m = 0; m < dumprig_opts->num_chosen; m++)
                              {
                                if (dumprig_opts->filter_num[m] == rig_ref)
                                    ref_found = 1; /* found image */
                              }

                            if (!ref_found)
                              {	/* If no match, then the current image is
                                   not what the user wants and so skip it. */
                                  HDfree((VOIDP) image);
                                  image = NULL; /* reset */
                                  continue;
                              }
                        }

                      /* If not all images are to be dumped out and the current image
                         is not what the user wants or if the user has specified a 
                         model and the model of the current image is not that one, then
                         skip the current image. */
                      if (((dumprig_opts->filter == DINDEX) 
                           && (i != rig_chosen[x])) 
                          || (((ncomps * 8) != model) && (model != 0)))
                        {
                            HDfree((VOIDP) image);
                            image = NULL; /* reset */
                            continue;
                        }

                      /* Determine what to be dumped out. */
                      switch (dumprig_opts->contents)
                        {
                        case DVERBOSE:
                        case DHEADER:
                            fprintf(fp, "Data model: %d-bit raster image ", ncomps * 8);
                            if (has_pal)
                                fprintf(fp, "with palette.\n");
                            else
                                fprintf(fp, "without palette.\n");
                            fprintf(fp, "\twidth=%d;  height=%d\n", (int) width, (int) height);
                            fprintf(fp, "\treference=%d\n", (int) rig_ref);
                            /* check compression if any */
                            if (compressed)
                              {
                                  fprintf(fp, "\t*data is compressed and the compression scheme is ");
                                  switch (compr_type)
                                    {
                                    case DFTAG_RLE:
                                        fprintf(fp, "RLE compression.\n");
                                        break;
                                    case DFTAG_IMCOMP:
                                        fprintf(fp, "IMCOMP conmpression.\n");
                                        break;
                                    case DFTAG_JPEG:
                                        fprintf(fp, "JPEG conmpression (24-bit data).\n");
                                        break;
                                    case DFTAG_GREYJPEG:
                                        fprintf(fp, "JPEG conmpression (8-bit data).\n");
                                        break;
                                    default:
                                        break;
                                    }		/* switch */
                                  fprintf(fp, "\n");
                              }	/* if (compressed) */
                            else
                                fprintf(fp, "\t*data is not compressed.\n");

                            if (dumprig_opts->contents == DHEADER)
                                break;
                        case DDATA:
                            if (dumprig_opts->contents != DDATA)
                                fprintf(fp, "\tData : \n");
                            if (FAIL == dumpfull(DFNT_UINT8, dumprig_opts, read_nelts*eltsz, image, fp, 5, 5))
                              {
                                  fprintf(stderr,"dumpfull: failed to dump %d'th image data for file %s",
                                          i,file_name);
                                  ret_value = FAIL;
                                  goto done;
                              }
                            HDfree((VOIDP) image);
                            image = NULL; /* reset */
                            break;
                        default:
                            printf("dumping RIG, unknown option \n");
                            ret_value = FAIL;
                            goto done;
                        }		/* switch  */

                      if(dumpall!=1 && i == rig_chosen[x])
                          x++;

                  }	/* for every image in file  */

                break; /* ASCII */

            case DBINARY:       /* binary  file  */
                /* Get the name of the output file. */
                if (dumprig_opts->dump_to_file)
                    fp = fopen(dumprig_opts->file_name, "wb");
                else
                    fp = stdout;

                /* Determine the number of images in a file. */
                if (model == 8)
                  { /* raster 8 */
                      if ((ndsets = DFR8nimages(file_name)) == FAIL)
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                  }
                else if (model == 24)
                  { /* raster 24 */
                      if ((ndsets = DF24nimages(file_name)) == FAIL)
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                  }
                else /* try both 8 and 24 */
                  {
                      if ((temp = DFR8nimages(file_name)) == FAIL)
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                      if ((ndsets = DF24nimages(file_name)) == FAIL)
                        {
                            ret_value = FAIL;
                            goto done;
                        }
                      ndsets += temp;
                  }

                if (num_rig_chosen == -1)		/* If all RIGs will be dumped, set the flat. */
                    dumpall = 1;

                x = 0;	/* Used as the index of the array of "rig_chosen[x]". */

                /* why was this left here uncommented out for binary dump? */
#if 0
                printf("ndsets=%d, dumpall=%d, num_chosen=%d\n",(int)ndsets,(int)dumpall,(int)dumprig_opts->num_chosen);
#endif

                /* can only check index range here */
                for(i=0; i < dumprig_opts->num_chosen && dumprig_opts->filter == DINDEX; i++)
                  { 
                      if((rig_chosen[i] > ndsets)||(rig_chosen[i]<0))
                        {
                            fprintf(stderr,"\nThe index %d is out of range\n",(int)rig_chosen[i]);
                            ret_value = FAIL;
                            goto done;
                        }
                  }

                for (i = 0; 
                     i < ndsets && (dumpall!=0 || x<dumprig_opts->num_chosen); 
                     i++)
                  {	/* Examine all RIGs. */
                      int    compressed;
                      int    has_pal;
                      int32  eltsz;
                      int32  read_nelts;
                      uint16 rig_ref;
                      uint16 compr_type;

                      if (FAIL == DFGRgetimdims(file_name, &width, &height, &ncomps, &il))
                        {
                            ret_value = FAIL;
                            goto done;
                        }


                      /* Determine the size of each element; "ncomps" is 1 for an
                         8-bit image or 3 for a 24-bit image. */
                      eltsz = DFKNTsize(DFNT_UINT8 | DFNT_NATIVE) * ncomps;
                      read_nelts = width * height;	/* Number of elements to be read in. */
                      if ((image = (VOIDP) HDmalloc(read_nelts * eltsz)) == NULL)
                        {
                            fprintf(stderr,"Not enough memory!\n");
                            ret_value = FAIL;
                            goto done;
                        }
                      if (DFGRIgetimlut((const char *) file_name, image, width,
                                               height, IMAGE, 0, &compressed, &compr_type, &has_pal) == -1)
                        {
                            fprintf(stderr,"DFGRIgetimlut: Read error for file %s\n",file_name);
                            ret_value = FAIL;
                            goto done;
                        }

                      rig_ref = DFGRIlastref();	/* Determine the reference of the image just read. */

                      /* If the user has specificed the reference option, then 
                         something has to be done. 
                         Note: the reason why the following part was not done  
                         inside the above "switch" statement is that the reference 
                         number of a raster image cannot be appropriately retrieved
                         before actually reading in a raster image. */
                      if (dumprig_opts->filter == DREFNUM)
                        {
                            int         ref_found = 0, m;

                            /* Determine if the image just read has the reference specified by the user. */
                            for (m = 0; m < dumprig_opts->num_chosen; m++)
                              {
                                if (dumprig_opts->filter_num[m] == rig_ref)
                                    ref_found = 1; /* found it */
                              }

                            if (!ref_found)
                              {	/* If no match, then the current image is
                                   not what the user wants and so skip it. */
                                  HDfree((VOIDP) image);
                                  image = NULL; /* reset */
                                  continue;
                              }
                        }

                      /* If not all images are to be dumped out and the current image
                         is not what the user wants or if the user has specified a 
                         model and the model of the current image is not that one, then
                         skip the current image. */
                      if (((dumprig_opts->filter == DINDEX) && (i != rig_chosen[x])) || 
                          (((ncomps * 8) != model) && (model != 0)))
                        {
                            HDfree((VOIDP) image);
                            image = NULL; /* reset */
                            continue;
                        }

                                 
                      if (FAIL == dumpfull(DFNT_UINT8, dumprig_opts, read_nelts*ncomps, image, fp, 5, 5))
                        {
                            fprintf(stderr,"dumpfull: failed to dump %d'th image data for file %s",
                                    i,file_name);
                            ret_value = FAIL;
                            goto done;
                        }
                      HDfree((VOIDP) image);
                      image = NULL; /* reset */
                 
                      if(dumpall!=1 && i == rig_chosen[x])
                          x++;
                  }	/* for every image in file  */

                break; /* BINARY */
            default:
                printf("dumping RIG, unknown output file option \n");
                ret_value = FAIL;
                goto done;
            }   /* switch for output file   */

          if (rig_chosen != NULL)
            {
                HDfree(rig_chosen);
                rig_chosen = NULL;
            } /* end if */

          if (dumprig_opts->dump_to_file)
              fclose(fp);
      }		/* while processing files  */

done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
          if (image != NULL)
              HDfree(image);
          if (rig_chosen != NULL)
              HDfree(rig_chosen);
      }
    /* Normal cleanup */

    return ret_value;
}	/* drig */

intn 
do_dumprig(intn  curr_arg, 
           intn  argc, 
           char *argv[], 
           intn  help)
{
    dump_info_t dumprig_opts;	/* dumprig options */
    int         model = 0;
    intn        ret_value = SUCCEED;

    if (help == TRUE)
      {
          dumprig_usage(argc, argv);
          goto done;
      }		/* end if */

   /* initialize the structure that holds user's options and inputs */
    init_dumprig_opts(&dumprig_opts);


    if (parse_dumprig_opts(&dumprig_opts, &curr_arg, argc, argv, &model) == FAIL)
      {
          dumprig_usage(argc, argv);
          ret_value = FAIL;
          goto done;
      }		/* end if */

    if (drig(&dumprig_opts, curr_arg, argc, argv, model) == FAIL)
      {
          fprintf(stderr,"Failure in drig.\n");
          ret_value = FAIL;
          goto done;
      }

  done:
    if (ret_value == FAIL)
      { /* Failure cleanup */
      }
    /* Normal cleanup */

    if(dumprig_opts.filter_num != NULL)
      HDfree(dumprig_opts.filter_num);

    return ret_value;
}	/* end do_dumprig() */

