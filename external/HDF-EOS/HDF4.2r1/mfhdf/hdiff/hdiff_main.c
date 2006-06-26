#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdlib.h>

#include "hdf.h"
#include "mfhdf.h"

#include "hdiff.h"


char *progname;

/*
Examples:

# Compare global attributes only
-g hdifftst1.hdf hdifftst2.hdf

# Compare SD local attributes only
-s hdifftst1.hdf hdifftst2.hdf

# Compare SD data only
-d hdifftst1.hdf hdifftst2.hdf

# Compare Vdata data only
-D hdifftst1.hdf hdifftst2.hdf

# Print statistics
-d -S hdifftst1.hdf hdifftst2.hdf

# Compare SD data on variable(s)
-d -v dset1 hdifftst1.hdf hdifftst2.hdf

# Compare vdata on variable(s) 
-D -u vdata1 hdifftst1.hdf hdifftst2.hdf

# Print difference up to count number
-d -e 2 hdifftst1.hdf hdifftst2.hdf

# Print difference when it is greater than limit
-d -t 2 hdifftst1.hdf hdifftst2.hdf

# no options
hdifftst1.hdf hdifftst2.hdf

# verbose 
-b hdifftst1.hdf hdifftst2.hdf
*/


static void
usage()
{
 (void) fprintf(stderr,
  "hdiff [-b] [-g] [-s] [-d] [-S] [-v ...] [-e counnt] [-t limit] file1 file2\n");
 fprintf(stderr,"  [-b]              Verbose mode\n");
 fprintf(stderr,"  [-g]              Compare global attributes only\n");
 fprintf(stderr,"  [-s]              Compare SD local attributes only\n");
 fprintf(stderr,"  [-d]              Compare SD data only\n");
 fprintf(stderr,"  [-D]              Compare Vdata data only\n");
 fprintf(stderr,"  [-S]              Print statistics\n");
 fprintf(stderr,"  [-v var1[,...]]   Compare SD data on variable(s) <var1>,... only\n");
 fprintf(stderr,"  [-u var1[,...]]   Compare vdata on variable(s) <var1>,... only\n");
 fprintf(stderr,"  [-e count]        Print difference up to count number for each variable\n");
 fprintf(stderr,"  [-t limit]        Print difference when it is greater than limit\n");
 fprintf(stderr,"  file1             File name of the first input HDF file\n");
 fprintf(stderr,"  file2             File name of the second input HDF file\n");
 exit(EXIT_FAILURE);
}


int
main(int argc, char *argv[])
{
 extern int optind;
 extern int opterr;
 extern char *optarg;
 static diff_opt_t opt = /* defaults, overridden on command line */
 {
  0,    /* verbose mode */
  1,    /* compare global attributes */
  1,    /* compare SD local attributes */
  1,    /* compare SD data */
  1,    /* compare GR data */
  1,    /* compare Vdata */
 -1,    /* no limit on the difference to be printed */
  0.0,  /* exact equal */
  0,    /* if -v specified, number of variables */
  0,    /* if -v specified, list of variable names */
  0,    /* if -u specified, number of variables */
  0,    /* if -u specified, list of variable names */
  0     /* if -S specified print statistics */
 };
 int   c;
 int   i;
 int   nfound;
 
 opterr = 1;
 progname = argv[0];
 
 if (argc == 1)
  usage();
 
 while ((c = getopt(argc, argv, "bgsdSDe:t:v:u:")) != EOF)
  switch(c) {
 case 'b':  /* verbose mode */
  opt.verbose =1;
  break;
 case 'g':  /* global attributes only */
  opt.ga = 1;
  opt.sa = 0;
  opt.sd = 0;
  opt.vd = 0;
  break;
 case 's':  /* SD local attributes only */
  opt.ga = 0;
  opt.sa = 1;
  opt.sd = 0;
  opt.vd = 0;
  break;
 case 'd':  /* SD data only */
  opt.ga = 0;
  opt.sa = 0;
  opt.sd = 1;
  opt.vd = 0;
  break;
 case 'D':  /* SD data only */
  opt.ga = 0;
  opt.sa = 0;
  opt.sd = 0;
  opt.vd = 1;
  break;
 case 'e':  /* max no. of difference to be printed */
  opt.max_err_cnt = atoi(optarg);
  break;
 case 't':  /* range of difference to be printed */
  opt.err_limit = (float32)atof(optarg);
  break;
 case 'v':  /* variable names */
  /* make list of names of variables specified */
  make_vars (optarg, &opt, 1);
  break;
 case 'u':  /* variable names */
  /* make list of names of variables specified */
  make_vars (optarg, &opt, 2);
  break;
 case 'S':
  opt.statistics = 1;
  break;
 case '?':
  usage();
  break;
 }
 
/*-------------------------------------------------------------------------
 * print the command line options
 *-------------------------------------------------------------------------
 */
 
 if ( opt.verbose == 1) {
 printf("$hdiff");
 for (i=1; i<argc ; i++) 
 {
  printf(" %s", argv[i] );
 }
 printf("\n");
 }
 
 argv = argv + optind;

 nfound = hdiff(argv[0],argv[1],&opt);

 if (nfound==0) 
  return 0;
 else
  return 1;
 
}
