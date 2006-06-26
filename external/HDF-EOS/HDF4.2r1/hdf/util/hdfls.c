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
static char RcsId[] = "@(#)$Revision: 1.61 $";
#endif

/* $Id: hdfls.c,v 1.61 1998/12/23 21:30:04 epourmal Exp $ */
#if defined __MWERKS__
#include <console.h>
#endif

#include "hdf.h"
#include "hfile.h"

#define MAXBUFF 8192

dd_t       *desc_buf;

intn
debug = FALSE,      /* Debugging is off by default */
    ddblocks = FALSE,   /* DD block dumping is off by default */
#ifdef DISKBLOCK_DEBUG
    diskblock_check = FALSE,   /* check disk-block boundaries */
#endif /* DISKBLOCK_DEBUG */
    sort = TRUE,        /* Sorting is on by default */
    longout = FALSE,    /* short output by default */
    labels = FALSE,     /* no label info by default */
    special = FALSE,    /* no special info by default */
    groups = FALSE;     /* no group info by default */
static intn v_init_done = FALSE;    /* whether the Vset interface has been */
/* initialized for this file */
uint16      only_tag = DFTAG_NULL;  /* by default print info about all tags */

char       *file_name;          /* name of current file being listed */

int         compare (const void *, const void *);
int         main (int, char *a[]);
void        lprint (int32, dd_t *, int);
void        print_item (int32, dd_t *, intn);
void	    printfilever (int32 file_id);
void        usage(char *argv[]);
int         dumpDD(void);

int
compare(const void * aa, const void * bb)
{
    const dd_t *a = (const dd_t *) aa;
    const dd_t *b = (const dd_t *) bb;

    if (a->tag > b->tag)
        return (1);
    if (a->tag < b->tag)
        return (-1);
    if (a->ref > b->ref)
        return (1);
    if (a->ref < b->ref)
        return (-1);
    return (0);
}

void
print_item(int32 fid, dd_t *desc_list, intn n)
{
    intn        status;
    int32       len;
    char       *name, *label_str;
    intn        i;   /* loop variable*/

    printf("\tRef no %6d\t%8d bytes\n", (int)desc_list[n].ref, (int)desc_list[n].length);

    /* print out labels and annotations if desired */
    if (labels)
      {     /* read in all of the labels */
          len = DFANgetlablen(file_name, desc_list[n].tag, desc_list[n].ref);
          if (len != FAIL)
            {
                label_str = (char *) HDmalloc((uint32) len + 1);
                status = DFANgetlabel(file_name, desc_list[n].tag, desc_list[n].ref, label_str, len + 1);
                label_str[len] = '\0';
                if (status == FAIL)
                    printf("\t  Unable to read label\n");
                else
                    printf("\t  Label: %s\n", label_str);
                HDfree(label_str);
            }

          /* read in all of the annotations */
          len = DFANgetdesclen(file_name, desc_list[n].tag, desc_list[n].ref);
          if (len != FAIL)
            {
                label_str = (char *) HDmalloc((uint32) len + 1);
                status = DFANgetdesc(file_name, desc_list[n].tag, desc_list[n].ref, label_str, len + 1);
                label_str[len] = '\0';
                if (status == FAIL)
                    printf("\t  Unable to read description\n");
                else
                    printf("\t  Description: %s\n", label_str);
                HDfree(label_str);
            }
      }

    if ((special) && (SPECIALTAG(desc_list[n].tag)))
      {     /* print out special info if desired */
          sp_info_block_t info;
          int32       aid, ret;

          aid = Hstartread(fid, desc_list[n].tag, desc_list[n].ref);
          if (aid == FAIL)
              return;

          ret = HDget_special_info(aid, &info);
          if ((ret == FAIL) || (info.key == FAIL))
              return;

          switch (info.key)
            {
            case SPECIAL_LINKED:
                printf("\tLinked Block: first %ld standard %ld per unit %ld\n",
                       (long) info.first_len, (long) info.block_len,
                       (long) info.nblocks);
                break;

            case SPECIAL_EXT:
                printf("\tExternal File: path %s  offset %ld\n", info.path,
                       (long) info.offset);
                break;

            case SPECIAL_COMP:
                printf("\tCompressed Element: compression type: %s  modeling type %s\n",
                       (info.comp_type == COMP_CODE_NONE ? "None" :
                        (info.comp_type == COMP_CODE_RLE ? "Run-Length" :
                         (info.comp_type == COMP_CODE_NBIT ? "N-Bit" : 
                          (info.comp_type == COMP_CODE_SKPHUFF ? "Skipping Huffman" : 
                           (info.comp_type == COMP_CODE_DEFLATE ? "Deflated" : 
                            "Unknown"))))),
                       (info.model_type == COMP_MODEL_STDIO ? "Standard" : "Unknown"));
                break;

             case SPECIAL_CHUNKED:
                printf("\tChunked Element: \n \tlogical size: %ld\n \tnumber of dimensions: %ld \n",
                       (long) info.chunk_size, (long) info.ndims);
                printf("\tarray of chunk lengths for each dimension:");
                    for(i=0; i < info.ndims; i++)
                               printf("\t %ld", (long) info.cdims[i]);
                printf("\n");
                HDfree(info.cdims);
                break;

            default:
                printf("\tDo not understand special element type %d\n",
                       info.key);
                break;
            }
          Hendaccess(aid);
      }

    if ((groups) && (desc_list[n].tag == DFTAG_RIG || desc_list[n].tag == DFTAG_SDG
                     || desc_list[n].tag == DFTAG_NDG || desc_list[n].tag == DFTAG_VG))
      {     /* print groups */
          if (desc_list[n].tag != DFTAG_VG)
            {   /* groups other than Vgroups */
                int32       GroupID;
                DFdi        elmt;

                printf("\tContents:\n");
                if ((GroupID = DFdiread(fid, desc_list[n].tag, desc_list[n].ref)) >= 0)
                  {
                      while (!DFdiget(GroupID, &elmt.tag, &elmt.ref))
                        {
                            name = (char *) HDgettagsname(elmt.tag);
                            if (!name)
                                printf("\t\t%-30s: (tag=%6d) ref=%d\n", "Unknown Tag",
                                       elmt.tag, elmt.ref);
                            else
                              {
                                  printf("\t\t%-30s: (tag=%6d) ref=%d\n", name,
                                         elmt.tag, elmt.ref);
                                  HDfree(name);
                              } /* end else */
                        }   /* end while */
                  }     /* end if */
                else
                    printf("\t\tNone!\n");
            }   /* end if */
          else
            {   /* dump Vgroup tag/refs */
                int32       ntagrefs;
                int32       vkey;
                int32      *tag_arr, *ref_arr;
                intn        i;

                if (v_init_done == FALSE)
                  {     /* init the V routines */
                      v_init_done = TRUE;
                      Vinitialize(fid);
                  }     /* end if */
                if ((vkey = Vattach(fid, (int32)desc_list[n].ref, "r")) != FAIL)
                  {
                      ntagrefs = Vntagrefs(vkey);
                      printf("\tContents: %d items\n", (int) ntagrefs);
                      if (ntagrefs > 0)
                        {
                            tag_arr = (int32 *) HDmalloc(sizeof(int32) * (size_t)ntagrefs);
                            ref_arr = (int32 *) HDmalloc(sizeof(int32) * (size_t)ntagrefs);
                            if (tag_arr == NULL || ref_arr == NULL)
                              {
                                  HDfree(tag_arr);
                                  HDfree(ref_arr);
                              }     /* end if */
                            else
                              {
                                  if (Vgettagrefs(vkey, tag_arr, ref_arr, ntagrefs) != FAIL)
                                    {
                                        for (i = 0; i < ntagrefs; i++)
                                          {
                                              name = (char *) HDgettagsname((uint16) tag_arr[i]);
                                              if (!name)
                                                  printf("\t\t%-30s: (tag=%6d) ref=%d\n",
                                                         "Unknown Tag", (int) tag_arr[i], (int) ref_arr[i]);
                                              else
                                                {
                                                    printf("\t\t%-30s: (tag=%6d) ref=%d\n",
                                                           name, (int) tag_arr[i], (int) ref_arr[i]);
                                                    HDfree(name);
                                                } /* end else */
                                          }     /* end for */
                                    }   /* end if */
                                  HDfree(tag_arr);
                                  HDfree(ref_arr);
                              }     /* end else */
                        }   /* end if */
                      Vdetach(vkey);
                  }     /* end if */
            }   /* end else */
      }     /* dumping groups */
}   /* print_item */

void
lprint(int32 fid, dd_t *desc_tmp, int num)
{
    intn        j = 0, empty = 0;
    uint16      prev = 0;
    char       *name;

    while (j < num)
      {
          if (desc_tmp[j].tag == DFTAG_NULL)
            {
                empty++;
                j++;
                continue;   /* don't print anything now */
            }

          /*
           * skip this tag if the user only wants to see some tags and
           *  this is not one of them
           */
          if (only_tag != DFTAG_NULL && only_tag != desc_tmp[j].tag)
            {
                j++;
                continue;
            }

          /*
          ** Find and print text description of this tag
          */
          name = (char *) HDgettagsname(desc_tmp[j].tag);
          if (!name)
              printf("\n%-30s: (tag %d)\n", "Unknown Tag", desc_tmp[j].tag);
          else
            {
                printf("\n%-30s: (tag %d)\n", name, desc_tmp[j].tag);
                HDfree(name);
            } /* end else */

          /*
          ** Print out reference number information
          */
          prev = desc_tmp[j].tag;
          if (longout)
            {
                while (desc_tmp[j].tag == prev && j < num)
                  {
                      print_item(fid, desc_tmp, j);
                      j++;
                  }
            }
          else
            {
                printf("\tRef nos:");
                while (desc_tmp[j].tag == (uint16) prev && j < num)
                  {
                      printf(" %d", desc_tmp[j].ref);
                      j++;
                  }
            }
      }

    if (empty)
        printf("\nEmpty (tag %d): %d slots\n", DFTAG_NULL, empty);
}


/* print the library version of the file */
void printfilever(int32 file_id)
{
    uint32 major, minor, release;
    char string[LIBVSTR_LEN+1];

    if (Hgetfileversion(file_id, &major, &minor, &release, string) == SUCCEED)
      {
          string[LIBVSTR_LEN] = '\0';		/* make it a null terminated string */
          printf("File library version: ");
          printf("Major= %u, Minor=%u, Release=%u\nString=%s\n",
                 (unsigned)major, (unsigned)minor, (unsigned)release, string);
      }
    else
        printf("(Does not have libraray version information)\n");
}

/* print command usage */
void usage(char *argv[])
{
    printf("%s,  version: 2.0   date: March 1, 1994\n", argv[0]);
#ifdef DISKBLOCK_DEBUG
    printf("%s [-o] [-l] [-d] [-v] [-g] [-s] [-h] [-x] [-t #] fn ....\n", argv[0]);
#else /* DISKBLOCK_DEBUG */
    printf("%s [-o] [-l] [-d] [-v] [-g] [-s] [-h] [-t #] fn ....\n", argv[0]);
#endif /* DISKBLOCK_DEBUG */
    printf("        This program displays information about the");
    printf(" data elements in\n");
    printf("        HDF file.\n");
    printf("    -d: offset & length info of each element in the file\n");
    printf("    -o: Ordered - display in reference number order\n");
    printf("    -l: Long format - display more information\n");
    printf("    -v: Verbose format - display text of annotations and labels.\n");
    printf("        (Verbose format automatically puts you in Long format).\n");
    printf("    -t #: List only information about a specific type of tag.\n");
    printf("          For example '%s -t 700 foo.hdf' \n", argv[0]);
    printf("          will list information only about Scientific Data\n");
    printf("          Groups.\n");
    printf("    -s: Give detailed descriptions of \"special elements\"\n");
    printf("    -g: List items in groups\n");
    printf("    -h: Dump DD Block information\n");
#ifdef DISKBLOCK_DEBUG
    printf("    -x: Check disk block boundaries (also dumps DD blocks)\n");
#endif /* DISKBLOCK_DEBUG */
}

/* dump the DD blocks */
int dumpDD(void)
{
    hdf_file_t file_id;    /* stdio file ID */
    int32 next_block=MAGICLEN;
    uint8 *ddbuf;         /* buffer to store the DD information */
    uint8 buf[NDDS_SZ+OFFSET_SZ]; /* buffer to hold DD block info */
    uint8 *b;
    int16 n_dds;          /* number of DDs in the current block */
    uint16 tag,ref;       /* DD tag & ref */
    int32 off,len;        /* DD offset & length */
    intn l;               /* local counting variable */

    file_id=HI_OPEN(file_name,DFACC_READ);
    if(OPENERR(file_id))
      {
          printf("Error opening file: %s\n",file_name);
          return(FAIL);
      } /* end if */
    while(next_block!=0)
      {
          if(HI_SEEK(file_id,next_block)==FAIL)
            {
                printf("Error seeking in file: %s\n",file_name);
                return(FAIL);
            } /* end if */
          if(HI_READ(file_id,buf,NDDS_SZ+OFFSET_SZ)==FAIL)
            {
                printf("Error reading in file: %s\n",file_name);
                return(FAIL);
            } /* end if */
          printf("current block: %ld,",(long)next_block);
          b=buf;
          INT16DECODE(b,n_dds);
          INT32DECODE(b,next_block);
          printf(" size of block: %ld, number of DDs:%d, next block: %ld\n",(long)(NDDS_SZ+OFFSET_SZ+(n_dds*DD_SZ)),(int)n_dds,(long)next_block);

          ddbuf=(uint8 *)HDmalloc(n_dds*DD_SZ);
          if(HI_READ(file_id,ddbuf,n_dds*DD_SZ)==FAIL)
            {
                printf("Error reading in file: %s\n",file_name);
                return(FAIL);
            } /* end if */
          b=ddbuf;
          for(l=0; l<n_dds; l++)
            {
#ifdef DISKBLOCK_DEBUG
                uint8 block_head[DISKBLOCK_HSIZE];
                uint8 block_tail[DISKBLOCK_TSIZE];
#endif /* DISKBLOCK_DEBUG */

                UINT16DECODE(b, tag);
                UINT16DECODE(b, ref);
                INT32DECODE(b, off);
                INT32DECODE(b, len);
                printf("\t[%5d] tag=%5u ref=%5u offset=%10ld length=%10ld\n",(int)l,(unsigned)tag,(unsigned)ref,(long)off,(long)len);
#ifdef DISKBLOCK_DEBUG
                if((tag!=DFTAG_NULL && tag!=DFTAG_FREE) &&
                   (len!=INVALID_LENGTH && off!=INVALID_OFFSET))
                  {
                      if(HI_SEEK(file_id,off-DISKBLOCK_HSIZE)==FAIL)
                        {
                            printf("Error seeking in file: %s\n",file_name);
                            return(FAIL);
                        } /* end if */
                      if(HI_READ(file_id,block_head,DISKBLOCK_HSIZE)==FAIL)
                        {
                            printf("Error reading in file: %s\n",file_name);
                            return(FAIL);
                        } /* end if */
                      if(HDmemcmp(block_head,diskblock_header,DISKBLOCK_HSIZE)!=0)
                        {
                            intn k;

                            printf("Header wrong for disk block!\n");
                            for(k=0; k<DISKBLOCK_HSIZE; k++)
                              {
                                  printf("\tbyte: %d, current: %x, correct: %x\n",k,(unsigned)block_head[k],(unsigned)diskblock_header[k]);
                              } /* end for */
                        } /* end if */
                      if(HI_SEEK(file_id,off+len)==FAIL)
                        {
                            printf("Error seeking in file: %s\n",file_name);
                            return(FAIL);
                        } /* end if */
                      if(HI_READ(file_id,block_tail,DISKBLOCK_TSIZE)==FAIL)
                        {
                            printf("Error reading in file: %s\n",file_name);
                            return(FAIL);
                        } /* end if */
                      if(HDmemcmp(block_tail,diskblock_tail,DISKBLOCK_TSIZE)!=0)
                        {
                            intn k;

                            printf("Tail wrong for disk block!\n");
                            for(k=0; k<DISKBLOCK_HSIZE; k++)
                              {
                                  printf("\tbyte: %d, current: %x, correct: %x\n",k,(unsigned)block_tail[k],(unsigned)diskblock_tail[k]);
                              } /* end for */
                        } /* end if */
                  } /* end if */
#endif /* DISKBLOCK_DEBUG */
            } /* end for */
          HDfree(ddbuf);
      } /* end while */
    HI_CLOSE(file_id);
    return(SUCCEED);
}

int
main(int argc, char *argv[])
{
    int32       fid, aid;
    int         i = 1, j, n, status;

#if defined __MWERKS__
    argc = ccommand(&argv);
#endif

    while ((i < argc) && (argv[i][0] == '-'))
      {
          switch (argv[i][1])
            {
            case 'o':   /* give non ordered output */
                sort = FALSE;
                break;

            case 'd':   /* go into debugging mode */
                debug = TRUE;
                break;

            case 'h':   /* dump the dd blocks */
                ddblocks = TRUE;
                break;

#ifdef DISKBLOCK_DEBUG
            case 'x':   /* check the disk-block boundaries (implies dumping dd blocks) */
                diskblock_check = TRUE;
                ddblocks = TRUE;
                break;
#endif /* DISKBLOCK_DEBUG */

            case 'v':   /* print labels for elements */
                labels = TRUE;
                longout = TRUE;
                break;

            case 'l':   /* give long output */
                longout = TRUE;
                break;

            case 's':   /* give info on special elements */
                special = TRUE;
                longout = TRUE;
                break;

            case 'g':   /* give info on groups */
                groups = TRUE;
                longout = TRUE;
                break;

            case 't':
                if (argv[i][2] != '\0')
                    only_tag = (uint16) atoi(&(argv[i][2]));
                else
                    only_tag = (uint16) atoi(&(argv[++i][0]));
                break;

            default:
                printf("Unknown option : -%c\n", argv[1][1]);
                break;
            }
          i++;
      }

    /*
     * If a file name has not been supplied print the usage message
     */
    if (i == argc)
      {
          usage(argv);
          exit(1);
      }

    desc_buf = (dd_t *) HDcalloc(MAXBUFF, sizeof(dd_t));

    while (i < argc)
      {
          file_name = argv[i];
          printf("%s:\n", file_name);

          /* Dump the DD blocks, if asked */
          if(ddblocks==TRUE)
            {
                if (dumpDD() == FAIL){
                    i++;
                    continue;
                }
            } /* end if */

          fid = Hopen(file_name, DFACC_READ, -1);
          if (fid == FAIL)
            {
                if (HEvalue(1) == DFE_NOTDFFILE)
                  {
                      printf("\tNot an HDF file.\n");
                      i++;
                      continue;
                  }
                else
                  {
                      HEprint(stderr, 0);
                  }
            }

          printfilever(fid);

          aid = Hstartread(fid, DFTAG_WILDCARD, DFREF_WILDCARD);
          if (aid == FAIL)
            {
                HEprint(stderr, 0);
                i++;
                continue;
            }

          status = SUCCEED;
          for (n = 0; (n < MAXBUFF) && (status != FAIL); n++)
            {
                Hinquire(aid, NULL, &desc_buf[n].tag, &desc_buf[n].ref, &desc_buf[n].length,
                         &desc_buf[n].offset, NULL, NULL, NULL);
                status = Hnextread(aid, DFTAG_WILDCARD, DFREF_WILDCARD, DF_CURRENT);
            }

          if (debug)
            {
                printf("\n");
                for (j = 0; j < n; j++)
                  {
                      printf("%6d) tag %6d ref %6d ", j, desc_buf[j].tag, desc_buf[j].ref);
                      printf(" offset %10d length %10d\n", (int)desc_buf[j].offset, (int)desc_buf[j].length);
                  }
            }

          if (sort)
              qsort((char *) desc_buf, (size_t)n, sizeof(dd_t), compare);

          v_init_done = FALSE;

          lprint(fid, desc_buf, n);

          if (v_init_done == TRUE)
              Vfinish(fid);

          if (Hendaccess(aid) == FAIL)
              HEprint(stderr, 0);

          if (Hclose(fid) == FAIL)
              HEprint(stderr, 0);

          i++;
          printf("\n");

          if (n >= MAXBUFF)
              fprintf(stderr,
                      "Warning:  File may have more DD's than hdfls can display\n");
      }

    HDfree(desc_buf);

    return (0);
}
