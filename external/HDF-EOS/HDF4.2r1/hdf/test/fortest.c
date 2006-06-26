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
static char RcsId[] = "@(#)$Revision: 1.39 $";
#endif

/* $Id: fortest.c,v 1.39 2003/12/10 21:13:33 epourmal Exp $ */

#define TESTMASTER

#include "hdf.h"
#include "tutils.h"
#include "fortest.h"
#ifdef VMS
#include <processes.h>
#include <string.h>
#endif
#define NUMOFTESTS 20

static int InitTest(const char *TheName, const char *TheCall, const char *TheDescr);

struct TestStruct
  {
      int         NumErrors;
      char        Description[64];
      int         SkipFlag;
      char        Name[16];
      char        Call[20];
  }
Test[NUMOFTESTS];

static int
InitTest(const char *TheName, const char *TheCall, const char *TheDescr)
{
    static int  Index = 0;

    if (Index >= NUMOFTESTS){
	printf("*** Too many tests.  Need to increase NUMOFTESTS (%d).\n",
		NUMOFTESTS);
	printf("\tRequest (%s) ignored.\n", TheName);
    } else {
	HDstrcpy(Test[Index].Description, TheDescr);
	HDstrcpy(Test[Index].Name, TheName);
	HDstrcpy(Test[Index].Call, TheCall);
	Test[Index].NumErrors = -1;
	Test[Index].SkipFlag = 0;
	Index++;
    }
    return(Index);
}

int
main(int argc, char *argv[])
{
    int         CLLoop;         /* Command Line Loop */
    int         Loop, Loop1;
    int         Summary = 0;
    int         CleanUp = 1;
    int         num_tests=0;
    FILE	*cmdfile;
    const char	*cmdfilename="fortest.arg";


    printf(" ===========================================\n");
    printf(" HDF Library Fortran Interface Tests Setup\n");
    printf(" ===========================================\n");

    num_tests=InitTest("slab", "slabwf", "");
    num_tests=InitTest("r24", "t24f", "");
    num_tests=InitTest("an", "tanf", "");
    num_tests=InitTest("anfile", "tanfilef", "");
    num_tests=InitTest("manf", "manf", "");
    num_tests=InitTest("mgrf", "mgrf", "");
    num_tests=InitTest("p", "tpf", "");
    num_tests=InitTest("r8", "tr8f", "");
    num_tests=InitTest("sdmms", "tsdmmsf", "");
    num_tests=InitTest("sdnmms", "tsdnmmsf", "");
    num_tests=InitTest("sdnnt", "tsdnntf", "");
    num_tests=InitTest("sdnt", "tsdntf", "");
    num_tests=InitTest("sdstr", "tsdstrf", "");
    num_tests=InitTest("vsetf", "tvsetf", "");
    num_tests=InitTest("vsetblock", "tvsetblock", "");
    num_tests=InitTest("vattrf", "tvattrf", "");
#if defined DEC_ALPHA || (defined SUN && defined _LP64) || defined __ia64  ||defined AIX5L64
    printf("   Skipping stubs\n");
#else
    num_tests=InitTest("stubs", "tstubsf", "");
#endif

    if ((cmdfile = fopen(cmdfilename, "w")) == NULL){
	printf("***Can't write to cmdfile(%s)***\n", cmdfilename);
	return(-1);
    }

    /* Default setting */
    Verbosity = 3;  /* Default Verbosity is Low */
    /* setup Cleanup Command */
    fprintf(cmdfile, "%s %s\n", CLEAN_STR, CLEAN_CMD);

    for (CLLoop = 1; CLLoop < argc; CLLoop++)
      {
          if ((argc > CLLoop + 1) && ((HDstrcmp(argv[CLLoop], "-verbose") == 0) ||
                                      (HDstrcmp(argv[CLLoop], "-v") == 0)))
            {
                if (argv[CLLoop + 1][0] == 'l')
                    Verbosity = 5;
                else if (argv[CLLoop + 1][0] == 'm')
                    Verbosity = 7;
                else if (argv[CLLoop + 1][0] == 'h')
                    Verbosity = 9;
                else
                    Verbosity = atoi(argv[CLLoop + 1]);
		fprintf(cmdfile, "%s %d\n", VERBOSITY_STR, Verbosity);
            }
          if ((argc > CLLoop) && ((HDstrcmp(argv[CLLoop], "-summary") == 0) ||
                                  (HDstrcmp(argv[CLLoop], "-s") == 0)))
            {
                Summary = 1;
            }
          if ((argc > CLLoop) && (HDstrcmp(argv[CLLoop], "-help") == 0))
            {
                printf("Usage: fortest [-v[erbose] (l[ow]|m[edium]|h[igh]|0-9)] \n");
                printf("               [-[e]x[clude] name+] \n");
                printf("               [-o[nly] name+] \n");
                printf("               [-b[egin] name] \n");
                printf("               [-s[ummary]]  \n");
                printf("               [-c[leanno]]  \n");
                printf("\n\n");
                printf("verbose   controls the amount of information displayed\n");
                printf("exclude   to exclude tests by name\n");
                printf("only      to name tests which should be run\n");
                printf("begin     start at the name of the test givin\n");
                printf("summary   prints a summary of test results at the end\n");
                printf("cleanno   does not delete *.hdf files after execution of tests\n");
                printf("\n\n");
                printf("This program currently tests the following: \n\n");
                printf("%16s %s\n", "Name", "Description");
                printf("%16s %s\n", "----", "-----------");
                for (Loop = 0; Loop < num_tests; Loop++)
                  {
                      printf("%16s %s\n", Test[Loop].Name, Test[Loop].Description);
                  }
                printf("\n\n");
                exit(0);
            }
          if ((argc > CLLoop) && ((HDstrcmp(argv[CLLoop], "-cleanno") == 0) ||
                                  (HDstrcmp(argv[CLLoop], "-c") == 0)))
            {
                CleanUp = 0;
		fprintf(cmdfile, "%s %s\n", CLEAN_STR, "No");
            }
          if ((argc > CLLoop + 1) && ((HDstrcmp(argv[CLLoop], "-exclude") == 0) ||
                                      (HDstrcmp(argv[CLLoop], "-x") == 0)))
            {
                Loop = CLLoop + 1;
                while ((Loop < argc) && (argv[Loop][0] != '-'))
                  {
                      for (Loop1 = 0; Loop1 < num_tests; Loop1++)
                        {
                            if (HDstrcmp(argv[Loop], Test[Loop1].Name) == 0)
                                Test[Loop1].SkipFlag = 1;
                        }
                      Loop++;
                  }
            }
          if ((argc > CLLoop + 1) && ((HDstrcmp(argv[CLLoop], "-begin") == 0) ||
                                      (HDstrcmp(argv[CLLoop], "-b") == 0)))
            {
                Loop = CLLoop + 1;
                while ((Loop < argc) && (argv[Loop][0] != '-'))
                  {
                      for (Loop1 = 0; Loop1 < num_tests; Loop1++)
                        {
                            if (HDstrcmp(argv[Loop], Test[Loop1].Name) != 0)
                                Test[Loop1].SkipFlag = 1;
                            if (HDstrcmp(argv[Loop], Test[Loop1].Name) == 0)
                                Loop1 = num_tests;
                        }
                      Loop++;
                  }
            }
          if ((argc > CLLoop + 1) && ((HDstrcmp(argv[CLLoop], "-only") == 0) ||
                                      (HDstrcmp(argv[CLLoop], "-o") == 0)))
            {
                for (Loop = 0; Loop < num_tests; Loop++)
                  {
                      Test[Loop].SkipFlag = 1;
                  }
                Loop = CLLoop + 1;
                while ((Loop < argc) && (argv[Loop][0] != '-'))
                  {
                      for (Loop1 = 0; Loop1 < num_tests; Loop1++)
                        {
                            if (HDstrcmp(argv[Loop], Test[Loop1].Name) == 0)
                                Test[Loop1].SkipFlag = 0;
                        }
                      Loop++;
                  }
            }
      }

    for (Loop = 0; Loop < num_tests; Loop++)
      {
          if (Test[Loop].SkipFlag)
            {
		fprintf(cmdfile, "%s %s\n", SKIP_STR, Test[Loop].Name);
            }
          else
            {
		fprintf(cmdfile, "%s %s\n", TEST_STR, Test[Loop].Name);
            }
      }

    fclose(cmdfile);

    /* flush stdout to gurantee output preceed those of fortestF */
    fflush(stdout);

#ifdef VMS
    {
        char *comprocfile="fortest.com";

        if ((cmdfile = fopen(comprocfile, "w")) == NULL){
           printf("***Can't write to cmdfile(%s)***\n", comprocfile);
           return(-1);
        }
        fprintf(cmdfile, "%s %s\n",
            "DEFINE/USER_MODE SYS$INPUT", cmdfilename);
        fprintf(cmdfile, "%s\n", "run []fortestF.exe");
        fclose(cmdfile);
        return(system("@fortest.com"));
    }
#else
    return(system("./fortestF"));
#endif
}
