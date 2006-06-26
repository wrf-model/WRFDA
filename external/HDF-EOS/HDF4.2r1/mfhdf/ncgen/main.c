/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: main.c,v 1.9 1997/11/13 21:38:57 acheng Exp $
 *********************************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#ifdef __hpux
#include <locale.h>
#endif
    
#include "ncgen.h"
#include "genlib.h"

char *progname;			/* for error messages */
char *cdlname;

int c_flag;
int fortran_flag;
int netcdf_flag;
char *netcdf_name = NULL;	/* name of output netCDF file to write */

extern FILE *yyin, *yyout;

void usage()
{
    derror("Usage: %s [ -b ] [ -c ] [ -f ] [ -o outfile]  [ file ... ]",
	   progname);
    exit(8);
}

int
main(argc, argv)
int argc;
char *argv[];
{
    extern int optind;
    extern int opterr;
    extern char *optarg;
    int c;
    FILE *fp;

    yyin=stdin;
    yyout=stdout;
#ifdef __hpux
    setlocale(LC_CTYPE,"");
#endif
    
#ifdef MDEBUG
	malloc_debug(2) ;	/* helps find malloc/free errors on Sun */
#endif /* MDEBUG */

    opterr = 1;			/* print error message if bad option */
    progname = argv[0];
    cdlname = "-";

    c_flag = 0;
    fortran_flag = 0;
    netcdf_flag = 0;

    while ((c = getopt(argc, argv, "bcfno:")) != EOF)
      switch(c) {
	case 'c':		/* for c output */
	  c_flag = 1;
	  break;
	case 'f':		/* for fortran output */
	  fortran_flag = 1;
	  break;
	case 'b':		/* for binary netcdf output, ".nc" extension */
	  netcdf_flag = 1;
	  break;
	case 'n':		/* old version of -b, uses ".cdf" extension */
	  netcdf_flag = -1;
	  break;
	case 'o':		/* to explicitly specify output name */
	  netcdf_flag = 1;
	  netcdf_name = (char *) emalloc(strlen(optarg)+1);
	  if (! netcdf_name) {
	      derror ("%s: out of memory", progname);
	      exit(1);
	  }
	  strcpy(netcdf_name,optarg);
	  break;
	case '?':
	  usage();
	  break;
      }

    if (fortran_flag && c_flag) {
	derror("Only one of -c or -f may be specified");
	exit(8);
      }

    argc -= optind;
    argv += optind;

    if (argc > 1) {
	derror ("%s: only one input file argument permitted",progname);
	exit(6);
    }

    fp = stdin;
    if (argc > 0 && strcmp(argv[0], "-") != 0) {
	if ((fp = fopen(argv[0], "r")) == NULL) {
	    derror ("can't open file %s for reading: ", argv[0]);
	    perror("");
	    exit(7);
	}
	cdlname = (char *) emalloc(1 + strlen(argv[0]));
	strcpy(cdlname, argv[0]);
    }
    yyin = fp;
#ifdef VMS
    return (1-yyparse());
#else
    return (yyparse());
#endif
}

