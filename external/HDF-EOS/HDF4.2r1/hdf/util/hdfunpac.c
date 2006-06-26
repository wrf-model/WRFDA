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
static char RcsId[] = "@(#)$Revision: 1.12 $";
#endif

/* $Id: hdfunpac.c,v 1.12 1996/11/11 20:40:20 koziol Exp $ */

/*
   ** FILE
   **   hdfunpac.c
   ** USAGE
   **   hdfunpac [options] <hdffile>
   ** DESCRIPTION
   **   This program unpacks an HDF file by exporting the scientific data
   **   elements (DFTAG_SD) to external object elements.
   **      Options are:
   **           -d <datafile> Use <datafile> as the external filename.
   **              Default is "DataFile".
 */

#if defined __MWERKS__
#include <console.h>
#endif

#include "hdf.h"
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>

#define DefaultDatafile "DataFile"

/* Prototypes declaration */
int         main
            (int, char *a[]);
void        hdferror
            (void);
void        error
            (const char *);
void        usage
            (void);

/* variables */
char       *progname;           /* the name this program is invoked, i.e. argv[0] */

int
main(int argc, char *argv[])
{
    int32       infile, aid, ret;
    char       *filename;
    char        datafilename[DF_MAXFNLEN];

    uint16      tag;
    uint16      ref;
    int32       offset, fileoffset;
    int32       length;
    int16       special;

#if defined __MWERKS__
    argc = ccommand(&argv);
#endif

    /* Get invocation name of program */
    progname = *argv++;
    argc--;

    /* parse arguments */
    while (argc > 0 && **argv == '-')
      {
          switch ((*argv)[1])
            {
                case 'd':
                    argc--;
                    argv++;
                    if (argc > 0)
                      {
                          strcpy(datafilename, *argv++);
                          argc--;
                      }
                    else
                      {
                          usage();
                          exit(1);
                      }
                    break;
                default:
                    usage();
                    exit(1);
            }
      }

    if (argc == 1)
      {
          filename = *argv++;
          argc--;
      }
    else
      {
          usage();
          exit(1);
      }

    if (datafilename[0] == '\0')
        strcpy(datafilename, DefaultDatafile);

    /* Check to make sure input file is HDF */
    ret = (int) Hishdf(filename);
    if (ret == FALSE)
      {
          error("given file is not an HDF file\n");
      }

    /* check if datafile already exists.  If so, set offset to its length. */
    {
        struct stat buf;
        if (stat(datafilename, &buf) == 0)
          {
              printf("External file %s already exists.  Using append mode.\n", datafilename);
              fileoffset = (int32)buf.st_size;
          }
        else
            fileoffset = 0;
    }

    /* Open HDF file */
    infile = Hopen(filename, DFACC_RDWR, 0);
    if (infile == FAIL)
      {
          error("Can't open the HDF file\n");
      }

    /* Process the file */
    ret = aid = Hstartread(infile, DFTAG_SD, DFREF_WILDCARD);
    while (ret != FAIL)
      {
          /*
           * Get data about the current one
           */
          ret = Hinquire(aid, NULL, &tag, &ref, &length, &offset, NULL, NULL, &special);

          /* check the tag value since external element object are returned the same. */
          if (tag == DFTAG_SD)
            {
                printf("moving Scientific Data (%d,%d) to %s\n", tag, ref, datafilename);
                ret = HXcreate(infile, tag, ref, datafilename, fileoffset, length);
                fileoffset += length;
            }

          /*
           * Move to the next one
           */
          ret = Hnextread(aid, DFTAG_SD, DFREF_WILDCARD, DF_CURRENT);
      }

    /*
     * Close the access element
     */
    ret = Hendaccess(aid);
    if (ret == FAIL)
        hdferror();

    /* done; close files */
    Hclose(infile);
    return (0);
}

/*
   ** NAME
   **   hdferror -- print out HDF error number
   ** USAGE
   **   int hdferror()
   ** RETURNS
   **   none
   ** DESCRIPTION
   **   Print an HDF error number to stderr.
   ** GLOBAL VARIABLES
   ** COMMENTS, BUGS, ASSUMPTIONS
   **   This routine terminates the program with code 1.
   ** EXAMPLES
 */
void
hdferror(void)
{
    HEprint(stderr, 0);
    exit(1);
}

/*
   ** NAME
   **      error -- print error to stderr
   ** USAGE
   **      int error(string);
   **      char *string;           IN: pointer to error description string
   ** RETURNS
   **   none
   ** DESCRIPTION
   **   Print an HDF error number to stderr.
   ** GLOBAL VARIABLES
   ** COMMENTS, BUGS, ASSUMPTIONS
   **   This routine terminates the program with code 1.
   ** EXAMPLES
 */
void
error(const char *string)
{
    fprintf(stderr, "%s: %s\n", progname, string);
    exit(1);
}

void
usage(void)
{
    fprintf(stderr, "Usage: %s [-d <datafilename>] <hdffile>\n", progname);
}
