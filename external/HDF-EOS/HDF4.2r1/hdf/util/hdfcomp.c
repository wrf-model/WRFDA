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
static char RcsId[] = "$Revision: 1.16 $";
#endif

/* $Id: hdfcomp.c,v 1.16 2003/12/10 21:13:47 epourmal Exp $ */

/*
*  hdfcomp.c
*  Re-compress Raster-8 HDF file
*/
#if defined __MWERKS__
#include <console.h>
#endif

#include "hdf.h"

uint8 *space;
uint8 palette[768];
int32 xdim, ydim;
int ispal;

int main(int argc, char *argv[]) 
{
    int i;
    char *outfile;
    intn jpeg_qual=75;      /* JPEG quality factor */
    uint16 prevref, writeref, compress = (uint16) 0;
    comp_info cinfo;        /* compression structure */
    int32 out_fid;          /* file ID for the output file */
    intn copy_flabel,       /* flag to indicate to copy file labels */
        copy_fdesc,         /* flag to indicate to copy file descriptions */
        copy_ilabel,        /* flag to indicate to copy image labels */
        copy_idesc;         /* flag to indicate to copy image descriptions */
    char *annbuf=NULL;      /* buffer to store annotations in */
    int32 annbuflen=0;      /* length of the annotation buffer */

#if defined __MWERKS__
    argc = ccommand(&argv);
#endif

    if (argc < 3) { 
        printf("%s,  version: 1.3   date: October 15, 1994\n", argv[0]);
        printf("  This utility will read in raster-8 images from an\n");
        printf("  HDF file and create a new HDF containing the\n");
        printf("  images in a compressed format.  Images will be\n");
        printf("  appended to outfile, if it exists.\n\n");
        printf("Usage:\n");
        printf(" hdfcomp outfile {[-c],[-r],[-i],[-j<quality>]} imagefile ...\n");
        printf("                 {[-c],[-r],[-i],[-j<quality>]} imagefile\n");
        printf("         -r: Store without compression (default)\n");
        printf("         -c: Store using RLE compression\n");
        printf("         -i: Store using IMCOMP compression (requires a");
        printf(" palette in the HDF file)\n");
        printf("         -j<quality>: Store using JPEG compression\n");
        printf("            with a quality factor from 1-100, 75 default\n");
        exit(1);
    }

    outfile = argv[1];

    /* open the output file so that we can write Annotations into it easily */
    if((out_fid=Hopen(outfile,DFACC_ALL,0))==FAIL) {
        printf("Error opening output file: %s\n",outfile);
        exit(1);
      } /* end if */

    for (i = 2; i < argc; i++) {
        /* turn all the flags on (default settings) */
        copy_flabel=copy_fdesc=copy_ilabel=copy_idesc=TRUE;

        if (*argv[i] == '-') {
            switch (argv[i][1]) {
                case 'r':               /* raster */
                    compress = (uint16) 0;
                    break;
                case 'c':               /* RLE */
                    compress = COMP_RLE;
                    break;
                case 'i':               /* IMCOMP */
                    compress = COMP_IMCOMP;
                    break;
                case 'j':               /* JPEG */
                    compress = COMP_JPEG;
                    if((jpeg_qual=atoi(&argv[i][2]))<=0 || jpeg_qual>100) {
                        printf("Bad JPEG quality setting, should be between\n");
                        printf("1 and 100, using default value of 75\n");
                        jpeg_qual=75;
                      } /* end if */
                    cinfo.jpeg.quality=jpeg_qual;   /* set JPEG parameters */
                    cinfo.jpeg.force_baseline=TRUE;
                    break;
                default:
                    printf("Illegal option: %s, skipping....\n", argv[i]);   
                    break;
            }
        }
        else { /* file name */
            /* copy the file annotations and labels over */
            if(copy_flabel==TRUE || copy_fdesc==TRUE) {
                intn isfirst;   /* flip-flop for first image */
                int32 annlen;   /* length of the annotation to copy */
                int32 old_fid;  /* file ID for the old and new files */

                if((old_fid=Hopen(argv[i],DFACC_READ,0))==FAIL) {
                    printf("Error opening input file: %s, skipping to next file\n",argv[i]);
                    continue;
                  } /* end if */
                if(copy_flabel==TRUE) {
                    isfirst=1;
                    while((annlen=DFANgetfidlen(old_fid,isfirst))!=FAIL) {
                        if(annbuflen==0 || annlen>annbuflen) {
                            if(annbuflen!=0)
                                HDfree(annbuf);
                            if((annbuf=(char *)HDmalloc(annlen))==NULL) {
                                printf("Error allocating buffer for annotation, aborting!\n");
                                exit(1);
                              } /* end if */
                            annbuflen=annlen;
                          } /* end if */
                        if(DFANgetfid(old_fid,annbuf,annbuflen,isfirst)==FAIL) 
                            printf("Error reading file annotation from file:%s, continuing\n",argv[i]);
                        else {
                            if(DFANaddfid(out_fid,annbuf)==FAIL)
                                printf("Error wriring annotation to file:%s, continuing\n",outfile);
                          } /* end else */
                        isfirst=0; /* get the next label from the file */
                      } /* end while */
                  } /* end if */
                if(copy_fdesc==TRUE) {
                    isfirst=1; 
                    while((annlen=DFANgetfdslen(old_fid,isfirst))!=FAIL) {
                        if(annbuflen==0 || annlen>annbuflen) {
                            if(annbuflen!=0)
                                HDfree(annbuf);
                            if((annbuf=(char *)HDmalloc(annlen))==NULL) {
                                printf("Error allocating buffer for annotation, aborting!\n");
                                exit(1);
                              } /* end if */
                            annbuflen=annlen;
                          } /* end if */
                        if(DFANgetfds(old_fid,annbuf,annbuflen,isfirst)==FAIL) 
                            printf("Error reading file annotation from file:%s, continuing\n",argv[i]);
                        else {
                            if(DFANaddfds(out_fid,annbuf,annlen)==FAIL)
                                printf("Error wriring annotation to file:%s, continuing\n",outfile);
                          } /* end else */
                        isfirst=0; /* get the next label from the file */
                      } /* end while */
                  } /* end if */
              } /* end if */


            /* copy the images over */
            while (DFR8getdims(argv[i], &xdim, &ydim, &ispal) >= 0) {
                prevref = DFR8lastref();
                if ((space = (uint8 *) HDmalloc((size_t)(xdim * ydim))) == NULL) {
                    printf("Not enough memory to convert image");
                    exit(1);
                }

                if (DFR8getimage(argv[i], space, xdim, ydim, palette) < 0) {
                    printf("Error reading image from file %s\n", argv[i]);
                    exit(1);
                }
                if (ispal)
                    DFR8setpalette((uint8 *) palette);
                else if (compress == DFTAG_IMC) {
                    printf("Couldn't find palette for IMCOMP compression\n");
                    exit(1);
                }

                writeref=Hnewref(out_fid);
                DFR8writeref(outfile, writeref);

                if(compress)
                    DFR8setcompress((int32)compress,&cinfo);
                if (DFR8addimage(outfile, (VOIDP) space,
                        xdim, ydim, compress)<0) {
                    printf("Error writing image to file %s\n", outfile);
                    exit(1);
                }

                /* sequence through the annotations for this image */
                if(copy_ilabel==TRUE || copy_idesc==TRUE) {
                    uint16 image_tag=DFTAG_RIG; /* tag to look for image annotations with */
                    int32 annlen;   /* length of the annotation to copy */

                    if(copy_ilabel==TRUE) {
                        if((annlen=DFANgetlablen(argv[i],image_tag,prevref))!=FAIL) {
                            if(annbuflen==0 || annlen>annbuflen) {
                                if(annbuflen!=0)
                                    HDfree(annbuf);
                                if((annbuf=(char *)HDmalloc(annlen))==NULL) {
                                    printf("Error allocating buffer for annotation, aborting!\n");
                                    exit(1);
                                  } /* end if */
                                annbuflen=annlen;
                              } /* end if */
                            if(DFANgetlabel(argv[i],image_tag,prevref,annbuf,annbuflen)==FAIL)
                                printf("Error reading annotation from file:%s, continuing\n",argv[i]);
                            else
                                if(DFANputlabel(outfile,image_tag,writeref,annbuf)==FAIL)
                                    printf("Error writing annotation to file:%s, continuing\n",outfile);
                          } /* end if */
                      } /* end if */
                    if(copy_idesc==TRUE) {
                        if((annlen=DFANgetdesclen(argv[i],image_tag,prevref))!=FAIL) {
                            if(annbuflen==0 || annlen>annbuflen) {
                                if(annbuflen!=0)
                                    HDfree(annbuf);
                                if((annbuf=(char *)HDmalloc(annlen))==NULL) {
                                    printf("Error allocating buffer for annotation, aborting!\n");
                                    exit(1);
                                  } /* end if */
                                annbuflen=annlen;
                              } /* end if */
                            if(DFANgetdesc(argv[i],image_tag,prevref,annbuf,annbuflen)==FAIL)
                                printf("Error reading annotation from file:%s, continuing\n",argv[i]);
                            else
                                if(DFANputdesc(outfile,image_tag,writeref,annbuf,annlen)==FAIL)
                                    printf("Error writing annotation to file:%s, continuing\n",outfile);
                          } /* end if */
                      } /* end if */
                  } /* end if */

                /* sequence past this image */
                DFR8readref(argv[i], prevref);
                DFR8getdims(argv[i], &xdim, &ydim, &ispal);

                HDfree((VOIDP)space);
            }
        }
    }

    Hclose(out_fid); /* remember to close the file */
    if(annbuflen!=0)    /* and free the buffer space */
        HDfree(annbuf);

    return(0);
}
