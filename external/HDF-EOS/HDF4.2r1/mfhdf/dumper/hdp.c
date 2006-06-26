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
static char RcsId[] = "@(#)$Revision: 1.25 $";
#endif

/* $Id: hdp.c,v 1.25 2005/01/09 20:54:34 mcgrath Exp $ */
#if defined __MWERKS__
#include <console.h>
#endif

#define HDP_MASTER
#define VSET_INTERFACE
#include "hdp.h"

/* Print the usage message about this utility */
static void 
usage(intn argc, char *argv[])
{
    printf("%s, %s\n\n", argv[0], LIBVER_STRING );
    printf("Usage: hdp [-H] command [command options] <filelist>\n");
    printf("\t -H  Display usage information about the specified command.\n");
    printf("\t     If no command is specified, -H lists all commands.\n");
    printf("\t Commands:\n");
    printf("\t     list \tlists contents of files in <filelist>\n");
    printf("\t     dumpsds\tdisplays data of SDSs in <filelist>\n");
    printf("\t     dumpvd\tdisplays data of vdatas in <filelist>. \n");
    printf("\t     dumpvg\tdisplays data of vgroups in <filelist>. \n");
    printf("\t     dumprig\tdisplays data of RIs in <filelist>. \n");
    printf("\t     dumpgr\tdisplays data of RIs in <filelist>. \n");
    printf("\t <filelist>\tlist of hdf file names, separated by spaces.\n");
}

void
init_dump_opts(dump_info_t * dump_opts)
{
    dump_opts->filter = DALL; /* default dump all GRs */
    dump_opts->by_index.num_list = NULL; /* no index given */
    dump_opts->by_index.num_items = 0;
    dump_opts->by_ref.num_list = NULL; /* no ref# given */
    dump_opts->by_ref.num_items = 0;
    dump_opts->by_name.str_list = NULL; /* no name given */
    dump_opts->by_name.num_items = 0;
    dump_opts->by_class.str_list = NULL; /* no class given */
    dump_opts->by_class.num_items = 0;
    dump_opts->contents = DVERBOSE;   /* default dump all information */
    dump_opts->dump_to_file = FALSE;          /* don't dump to output file */
    dump_opts->file_type = DASCII;    /* default output is ASCII file */
    dump_opts->print_pal = FALSE;     /* GR only, don't print palette */

    /* no specific dataset requested, default to dump all datasets */
    dump_opts->num_chosen = NO_SPECIFIC;

    /* print output aligned, using carriage returns */
    dump_opts->as_stream = FALSE;

    /* print space characters (LF, FF, CR, space, tabs...) in \digit format */
    dump_opts->clean_output = FALSE;

    /* print data starting at column 16 unless reset otherwise */
    dump_opts->firstln_indent = 16;

    /* print data on a continuous line starting at column 16 unless 
       reset otherwise */
    dump_opts->contln_indent = 16;

    /* GR only, print data using interlace at creation */
    dump_opts->interlace = NO_SPECIFIC;

    /* GR & SD only, print data of global attributes unless -g is given */
    dump_opts->no_gattr_data = FALSE;

    /* GR & SD only, print data of local attributes unless -l is given */
    dump_opts->no_lattr_data = FALSE;

    HDstrcpy(dump_opts->file_name, "\0");
}       /* end init_dump_opts() */


int 
main(int argc, char *argv[])
{
    command_t   cmd;			/* command to perform */
    intn        curr_arg;		/* current cmd line argument */
    dump_opt_t  glob_opts;		/* global options for all commands */
    intn        j;				/* local counting variables */

#if defined __MWERKS__
    argc = ccommand(&argv);
#endif

    HDmemset(&glob_opts, 0, sizeof(dump_opt_t));

    if (argc < 2)
      {
          usage(argc, argv);
          exit(1);
      }		/* end if */

    curr_arg = 1;
/*  printf("Argument 0: %s\n",argv[0]);
    printf("Argument 1: %s\n",argv[1]);
    */
    while (curr_arg < argc && (argv[curr_arg][0] == '-'))
      {
              /*  while(curr_arg<argc && (argv[curr_arg][0]=='-' || argv[curr_arg][0]=='/')) {  */
          switch (argv[curr_arg][1])
            {
            case 'H':
                    /*     case 'h':  *//*    Print help for a given command */
                if (curr_arg < argc - 1)
                  {
                      glob_opts.help = TRUE;	/* for displaying options. */
                      break;
                  }
            default:
                usage(argc, argv);	/* Display the general usage. */
                exit(1);
            }	/* end switch */
          curr_arg++;
      }		/* end while */

    for (j = 0, cmd = HELP; j < (sizeof(commands) / sizeof(const char *)); j++, cmd++)
      {
          if (HDstrcmp(argv[curr_arg], commands[j]) == 0)
              break;
      }		/* end for */

/* printf("cmd=%d\n",(int)cmd);
   printf("command=%s\n",argv[curr_arg]);
   */
    curr_arg++;

	/* must be a legit command */
    switch (cmd)
      {
      case LIST:
          if (FAIL == do_list(curr_arg, argc, argv, glob_opts.help))
              exit(1);
          break;

      case DUMPSDS:
          if (FAIL == do_dumpsds(curr_arg, argc, argv, glob_opts.help))
              exit(1);
          break;

      case DUMPRIG:
	  /* BMR: retire dumprig, have dumpgr do the work */
          if (FAIL == do_dumprig(curr_arg, argc, argv, glob_opts.help)) 
/*
	  fprintf( stderr, ">>> Please make a note that dumprig is no longer available.\n");
	  fprintf( stderr, "    The command dumpgr is and should be used in its place.\n" );
          if (FAIL == do_dumpgr(curr_arg, argc, argv, glob_opts.help)) */
              exit(1);
          break;

      case DUMPVG:
          if (FAIL == do_dumpvg(curr_arg, argc, argv, glob_opts.help))
              exit(1);
          break;

      case DUMPVD:
          if (FAIL == do_dumpvd(curr_arg, argc, argv, glob_opts.help))
              exit(1);
          break;

      case DUMPGR:
          if (FAIL == do_dumpgr(curr_arg, argc, argv, glob_opts.help))
              exit(1);
          break;

      case HELP:
      case NONE:
          usage(argc, argv);
          break;

      default:
          printf("Invalid command!, cmd=%d\n", (int) cmd);
          exit(1);
          break;
      }		/* end switch */

    return (0);
}

/* -----------------------------------------------------------------
NAME
   VShdfsize - computes the byte size of the field(s) of a vdata.

DESCRIPTION
   The size is the byte size of the fields of a vdata in a hdf file.
   This routine is very similar to the HDF API routine VSsizeof except
   it uses (struct vdata_desc).wlist.isize to compute the field size
   instead of (struct vdata_desc).wlist.esize as VSsizeof.

RETURNS
   The byte size of the field(s), positive integer, on success; 
   otherwise, returns FAIL.
----------------------------------------------------------------- */
int32 
VShdfsize(int32 vkey,   /* IN vdata key */
         char *fields  /* IN: Name(s) of the fields to check size of */ )
{
    int32       totalsize;
    int32       i, j;
    int32       found;
    int32       ac;
    char        **av = NULL;
    vsinstance_t *w  = NULL;
    VDATA        *vs = NULL;
    int32        ret_value = SUCCEED;
    CONSTR(FUNC, "VShdfsize");

    /* check key is valid vdata */
    if (HAatom_group(vkey)!=VSIDGROUP)
        HGOTO_ERROR(DFE_ARGS, FAIL);

    /* get vdata instance */
    if (NULL == (w = (vsinstance_t *) HAatom_object(vkey)))
        HGOTO_ERROR(DFE_NOVS, FAIL);

    /* get vdata itself and check it */
    vs = w->vs;
    if (vs == NULL)
        HGOTO_ERROR(DFE_ARGS, FAIL);

    totalsize = 0;
    if (fields == NULL) /* default case? */
      {   /* count all field sizes in vdata */
        for (j = 0; j < vs->wlist.n; j++)	
            totalsize += vs->wlist.isize[j];
      }		
    else if (fields[0] != '\0')  /* implies: return 0 for empty 'fields' */
      {  /* parse field string */
        if ((scanattrs(fields, &ac, &av) < 0) || (ac < 1))
            HGOTO_ERROR(DFE_ARGS, FAIL);

        for (i = 0; i < ac; i++)
          {   /* check fields in vs */
            for (found = 0, j = 0; j < vs->wlist.n; j++)	
                if (!HDstrcmp(av[i], vs->wlist.name[j]))
                  {
                    totalsize += vs->wlist.isize[j];
                    found = 1;
                    break;
                  }

            if (!found)
                HGOTO_ERROR(DFE_ARGS, FAIL);
          }	/* end for */
      }		/* end else */

    /* return total size of vdata fields specified */
    ret_value = totalsize;

done:
  if(ret_value == FAIL)   
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */
  return ret_value;
}   /* VShdfsize */


/* ------------- VSattrhdfsize --------------------------
NAME
       VSattrhdfsize -- get hdfsize of a vdata attribute
USAGE
      intn VSattrhdfsize(int32 vsid, int32 findex, intn attrindex, int32 *size);
      int32 vsid;      IN: vdata id
      int32 findex;    IN: field index. _HDF_VDATA (-1) for the vdata
      intn attrindex;  IN: which attr of the field/vdata 
                           attrindex is 0-based
      int32 *size;     OUT: size of the attr values in hdf files.
RETURNS
        Returns SUCCEED when successful, FAIL otherwise.
DESCRIPTION
        size can be NULL if which is not interested.
--------------------------------------------------- */
intn VSattrhdfsize(int32 vsid, int32 findex, intn attrindex, int32 *size)
{

     CONSTR(FUNC, "VSattrhdfsize");
     VDATA *vs, *attr_vs;
     vs_attr_t *vs_alist;
     vsinstance_t *vs_inst, *attr_inst;
     int32 attr_vsid;
     int32 ret_value = SUCCEED;
     intn i, nattrs, a_index, found;
     DYN_VWRITELIST *w;

     HEclear();
     if (HAatom_group(vsid) != VSIDGROUP)
        HGOTO_ERROR(DFE_ARGS, FAIL);
     /* locate vs' index in vstab */
     if (NULL == (vs_inst = (vsinstance_t *)HAatom_object(vsid)))
        HGOTO_ERROR(DFE_NOVS, FAIL);
     if (NULL == (vs = vs_inst->vs))
        HGOTO_ERROR(DFE_NOVS, FAIL);
     if ((findex >= vs->wlist.n || findex < 0) && (findex != _HDF_VDATA))
        HGOTO_ERROR(DFE_BADFIELDS, FAIL);
     nattrs = vs->nattrs;
     if (attrindex <0 || attrindex >= nattrs)
        HGOTO_ERROR(DFE_ARGS, FAIL);
     vs_alist = vs->alist;
     if (nattrs == 0 || vs_alist == NULL)
          /* no attrs or bad attr list */
            HGOTO_ERROR(DFE_ARGS, FAIL);
    found = 0;
    a_index = -1; 
    for (i=0; i<nattrs; i++)  {
        if (vs_alist->findex == findex)  {
           a_index++; 
           if (a_index == attrindex) {
              found = 1;
              break;
           }
        }
        vs_alist++;
    }
    if (!found)
        HGOTO_ERROR(DFE_ARGS, FAIL);
    /* found. get attr info */
    if (FAIL == (attr_vsid = VSattach(vs->f, (int32)vs_alist->aref, "r")))
        HGOTO_ERROR(DFE_CANTATTACH, FAIL);
    if (NULL == (attr_inst = (vsinstance_t *)HAatom_object(attr_vsid)))
        HGOTO_ERROR(DFE_NOVS, FAIL);
    if (NULL == (attr_vs = attr_inst->vs) ||
          HDstrcmp(attr_vs->vsclass,  _HDF_ATTRIBUTE) != 0)
        HGOTO_ERROR(DFE_BADATTR, FAIL);
    w = &(attr_vs->wlist);
    /* this vdata has 1 field */
    if (w->n != 1 || HDstrcmp(w->name[0], ATTR_FIELD_NAME))
        HGOTO_ERROR(DFE_BADATTR, FAIL);
    if (size)
        *size = w->order[0] * (DFKNTsize(w->type[0]));
    if (FAIL == VSdetach(attr_vsid))
        HGOTO_ERROR(DFE_CANTDETACH, FAIL);
done:
    if (ret_value == FAIL)
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */

  return ret_value;
}  /* VSattrhdfsize */

/* ----------   Vattrhdfsize ----------------------
NAME
       Vattrhdfsize -- get hdfsize of a vgroup attribute
USAGE
        intn Vattrinfo(int32 vgid, intn attrindex, int32 *size)
        int32 vgid;      IN: vgroup id
        intn attrindex;  IN: which attr's info we want
                             attrindex is 0-based
        int32 *size;     OUT: size of the attr values in hdf files.

RETURNS
        Returns SUCCEED when successful, FAIL otherwise.
DESCRIPTION
        size can be NULL if which is not interested.
--------------------------------------------------- */
intn Vattrhdfsize(int32 vgid, intn attrindex, int32 *size)
{
    CONSTR(FUNC, "Vattrhdfsize");
    VGROUP *vg;
    VDATA *vs;
    DYN_VWRITELIST  *w;
    vginstance_t *v;
    vsinstance_t *vs_inst;
    int32 fid, vsid;
    int32 ret_value = SUCCEED;

    HEclear();
    if (HAatom_group(vgid) != VGIDGROUP)
       HGOTO_ERROR(DFE_ARGS, FAIL);
    /* locate vg's index in vgtab */
    if (NULL == (v = (vginstance_t *)HAatom_object(vgid)))
       HGOTO_ERROR(DFE_VTAB, FAIL);
    vg = v->vg;
    fid = vg->f;
    if (vg == NULL)
       HGOTO_ERROR(DFE_BADPTR, FAIL);
    if (vg->otag != DFTAG_VG)
       HGOTO_ERROR(DFE_ARGS, FAIL);
    if (vg->nattrs <= attrindex || vg->alist == NULL) 
         /* not that many attrs or bad attr list */
            HGOTO_ERROR(DFE_ARGS, FAIL);
    
    if ((vsid = VSattach(fid, (int32)vg->alist[attrindex].aref, "r")) == FAIL)
        HGOTO_ERROR(DFE_CANTATTACH, FAIL);
    if (HAatom_group(vsid) != VSIDGROUP)
        HGOTO_ERROR(DFE_ARGS, FAIL);
    if (NULL == (vs_inst = (vsinstance_t *)HAatom_object(vsid)))
        HGOTO_ERROR(DFE_NOVS, FAIL);
    if (NULL == (vs = vs_inst->vs) ||
          HDstrcmp(vs->vsclass,  _HDF_ATTRIBUTE) != 0)
        HGOTO_ERROR(DFE_BADATTR, FAIL);
    w = &(vs->wlist);
    /* this vdata has 1 field */
    if (w->n != 1 || HDstrcmp(w->name[0], ATTR_FIELD_NAME))  
/*    if (w->n != 1 )   */
        HGOTO_ERROR(DFE_BADATTR, FAIL);
    if (size)
       *size = w->order[0] * (DFKNTsize(w->type[0]));
    if (FAIL == VSdetach(vsid))
        HGOTO_ERROR(DFE_CANTDETACH, FAIL);
done:
    if (ret_value == FAIL)
    { /* Error condition cleanup */

    } /* end if */

  /* Normal function cleanup */
  return ret_value;
}  /* Vattrhdfsize */
