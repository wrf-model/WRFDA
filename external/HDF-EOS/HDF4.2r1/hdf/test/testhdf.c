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
static char RcsId[] = "@(#)$Revision: 1.48 $";
#endif

/* $Id: testhdf.c,v 1.48 2005/01/16 21:44:10 mcgrath Exp $ */

/*
   FILE
   testhdf.c
   HDF testing framework main file.

   REMARKS
   General test wrapper for HDF base library test programs

   DESIGN
   Each test function should be implemented as function having
   no parameters and returning void (i.e. no return value).  They
   should be put into the list of InitTest() calls in main()
   below.  Functions which depend on other functionality should
   be placed below the InitTest() call for the base functionality
   testing.
   Each test module should include tproto.h and define a unique
   set of names for test files they create.

   BUGS/LIMITATIONS

   EXPORTED ROUTINES/VARIABLES:
   Two variables are exported: num_errs, and Verbosity.

 */

#if defined __MWERKS__
#include <console.h>
#endif

#define MAXNUMOFTESTS 30
#define TESTMASTER

/* Internal Variables */
static int  Index = 0;

/* ANY new test needs to have a prototype in tproto.h */
#include "tproto.h"

struct TestStruct
  {
      int         NumErrors;
      char        Description[64];
      int         SkipFlag;
      char        Name[16];
      VOID        (*Call) (void);
  }
         Test[MAXNUMOFTESTS];

static void
InitTest(const char *TheName, VOID(*TheCall) (void), const char *TheDescr);
static void usage(void);

static void
InitTest(const char *TheName, VOID(*TheCall) (void), const char *TheDescr)
{
    if (Index >= MAXNUMOFTESTS)
      {
          printf("Uh-oh, too many tests added, increase MAXNUMOFTEST!\n");
          exit(0);
      }     /* end if */
    HDstrcpy(Test[Index].Description, TheDescr);
    HDstrcpy(Test[Index].Name, TheName);
    Test[Index].Call = TheCall;
    Test[Index].NumErrors = -1;
    Test[Index].SkipFlag = 0;
    Index++;
}

static void
usage(void)
{
    intn        i;

    printf("Usage: testhdf [-v[erbose] (l[ow]|m[edium]|h[igh]|0-10)] \n");
    printf("               [-[e]x[clude] name+] \n");
    printf("               [-o[nly] name+] \n");
    printf("               [-b[egin] name] \n");
    printf("               [-s[ummary]]  \n");
    printf("               [-c[leanoff]]  \n");
    printf("               [-n[ocaching]]  \n");
    printf("               [-h[elp]]  \n");
    printf("\n\n");
    printf("verbose   controls the amount of information displayed\n");
    printf("exclude   to exclude tests by name\n");
    printf("only      to name tests which should be run\n");
    printf("begin     start at the name of the test givin\n");
    printf("summary   prints a summary of test results at the end\n");
    printf("cleanoff  does not delete *.hdf files after execution of tests\n");
    printf("nocaching do not turn on low-level DD caching\n");
    printf("help      print out this information\n");
    printf("\n\n");
    printf("This program currently tests the following: \n\n");
    printf("%16s %s\n", "Name", "Description");
    printf("%16s %s\n", "----", "-----------");
    for (i = 0; i < Index; i++)
        printf("%16s %s\n", Test[i].Name, Test[i].Description);
    printf("\n\n");
}   /* end usage() */

int
main(int argc, char *argv[])
{
    int         CLLoop;         /* Command Line Loop */
    int         Loop, Loop1;
    int         Summary = 0;
    int         CleanUp = 1;
    int         Cache = 1;
    uint32      lmajor, lminor, lrelease;
    char        lstring[81];

#if defined __MWERKS__
    argc = ccommand(&argv);
#endif

#if !(defined MAC || defined __MWERKS__ || defined SYMANTEC_C)
    /* Un-buffer the stdout and stderr */
    setbuf(stderr, NULL);
    setbuf(stdout, NULL);
#endif
    /* Tests are generally arranged from least to most complexity... */
#if !(defined WIN32)
    InitTest("bitvect", test_bitvect, "Bit-Vector routines");
    InitTest("tbbt", test_tbbt, "Threaded Balanced Binary Trees");
#endif
    InitTest("vers", test_vers, "VERSION OF LIBRARY");
    InitTest("hfile", test_hfile, "HFILE");
    InitTest("hfile1", test_hfile1, "HFILE LIMITS");
    InitTest("hblocks", test_hblocks, "HBLOCKS");
    InitTest("extelt", test_hextelt, "EXTERNAL ELEMENTS");
    InitTest("comp", test_comp, "COMPRESSED ELEMENTS");
    InitTest("buffer", test_buffer, "Buffered Elements");
    InitTest("chunks", test_chunks, "Chunks");
#ifdef LATER
    InitTest("vblocks", test_hvblocks, "Variable Length Linked Blocks");
#endif /* LATER */
    InitTest("bitio", test_bitio, "BIT I/O");
    InitTest("8bit", test_r8, "8BIT RASTER IMAGE INTERFACE");
    InitTest("pal", test_pal, "PALETTE INTERFACE");
    InitTest("24bit", test_r24, "24BIT RASTER IMAGE INTERFACE");
    InitTest("macros", test_macros, "ENCODING/DECODING INTERFACE");
    InitTest("conv", test_conv, "CONVERSION INTERFACE");
    InitTest("sdmms", test_sdmms, "SDMMS");
    InitTest("sdnmms", test_sdnmms, "SDNMMS");
    InitTest("sdstr", test_tsdstr, "DATASTRINGS");
    InitTest("slabs", test_slab, "HYPERSLAB INTERFACE");
    InitTest("anot", test_an, "ANNOTATIONS");
    InitTest("anot_2", test_an_2, "UPDATE ANNOTATIONS");
    InitTest("anfile", test_anfile, "FILE ANNOTATIONS");
    InitTest("manot", test_man, "MULTI-ANNOTATIONS");
    InitTest("nbit", test_nbit, "N-Bit Dataset Interface");
    InitTest("litend", test_litend, "LITTLE-ENDIAN INTERFACE");
    InitTest("vset", test_vsets, "VSET InterfaceTest");
    InitTest("vattr", test_vset_attr, "VSET AttributeTest");
    InitTest("vsfpack", test_vspack, "Vdata fields pack Test");
    InitTest("mfgr", test_mgr, "Multi-File Generic Raster Image Interface");

    Verbosity = 4;  /* Default Verbosity is Low */
    Hgetlibversion(&lmajor, &lminor, &lrelease, lstring);

    printf("\nFor help use: testhdf -help\n");
    printf("Built with HDF Library Version: %u.%ur%u, %s\n\n", (unsigned) lmajor,
           (unsigned) lminor, (unsigned) lrelease, lstring);
    for (CLLoop = 1; CLLoop < argc; CLLoop++)
      {
          if ((argc > CLLoop + 1) && ((HDstrcmp(argv[CLLoop], "-verbose") == 0) ||
                                      (HDstrcmp(argv[CLLoop], "-v") == 0)))
            {
                if (argv[CLLoop + 1][0] == 'l')
                    Verbosity = 4;
                else if (argv[CLLoop + 1][0] == 'm')
                    Verbosity = 6;
                else if (argv[CLLoop + 1][0] == 'h')
                    Verbosity = 10;
                else
                    Verbosity = atoi(argv[CLLoop + 1]);
            }   /* end if */
          if ((argc > CLLoop) && ((HDstrcmp(argv[CLLoop], "-summary") == 0) ||
                                  (HDstrcmp(argv[CLLoop], "-s") == 0)))
              Summary = 1;

          if ((argc > CLLoop) && ((HDstrcmp(argv[CLLoop], "-help") == 0) ||
                                  (HDstrcmp(argv[CLLoop], "-h") == 0)))
            {
                usage();
                exit(0);
            }

          if ((argc > CLLoop) && ((HDstrcmp(argv[CLLoop], "-cleanoff") == 0) ||
                                  (HDstrcmp(argv[CLLoop], "-c") == 0)))
              CleanUp = 0;

          if ((argc > CLLoop) && ((HDstrcmp(argv[CLLoop], "-nocache") == 0) ||
                                  (HDstrcmp(argv[CLLoop], "-n") == 0)))
              Cache = 0;

          if ((argc > CLLoop + 1) && ((HDstrcmp(argv[CLLoop], "-exclude") == 0) ||
                                      (HDstrcmp(argv[CLLoop], "-x") == 0)))
            {
                Loop = CLLoop + 1;
                while ((Loop < argc) && (argv[Loop][0] != '-'))
                  {
                      for (Loop1 = 0; Loop1 < Index; Loop1++)
                          if (HDstrcmp(argv[Loop], Test[Loop1].Name) == 0)
                              Test[Loop1].SkipFlag = 1;
                      Loop++;
                  }     /* end while */
            }   /* end if */
          if ((argc > CLLoop + 1) && ((HDstrcmp(argv[CLLoop], "-begin") == 0) ||
                                      (HDstrcmp(argv[CLLoop], "-b") == 0)))
            {
                Loop = CLLoop + 1;
                while ((Loop < argc) && (argv[Loop][0] != '-'))
                  {
                      for (Loop1 = 0; Loop1 < Index; Loop1++)
                        {
                            if (HDstrcmp(argv[Loop], Test[Loop1].Name) != 0)
                                Test[Loop1].SkipFlag = 1;
                            if (HDstrcmp(argv[Loop], Test[Loop1].Name) == 0)
                                Loop1 = Index;
                        }   /* end for */
                      Loop++;
                  }     /* end while */
            }   /* end if */
          if ((argc > CLLoop + 1) && ((HDstrcmp(argv[CLLoop], "-only") == 0) ||
                                      (HDstrcmp(argv[CLLoop], "-o") == 0)))
            {
                for (Loop = 0; Loop < Index; Loop++)
                    Test[Loop].SkipFlag = 1;
                Loop = CLLoop + 1;
                while ((Loop < argc) && (argv[Loop][0] != '-'))
                  {
                      for (Loop1 = 0; Loop1 < Index; Loop1++)
                          if (HDstrcmp(argv[Loop], Test[Loop1].Name) == 0)
                              Test[Loop1].SkipFlag = 0;
                      Loop++;
                  }     /* end while */
            }   /* end if */
      }     /* end for */

    if(Cache) /* turn on caching, unless we were instucted not to */
	Hcache(CACHE_ALL_FILES,TRUE);

    for (Loop = 0; Loop < Index; Loop++)
      {
          if (Test[Loop].SkipFlag)
            {
                MESSAGE(2, printf("Skipping -- %s \n", Test[Loop].Description);
                    );
            }
          else
            {
                MESSAGE(2, printf("Testing  -- %s (%s) \n", Test[Loop].Description,
                                  Test[Loop].Name);
                    );
                MESSAGE(5, printf("===============================================\n");
                    );
                Test[Loop].NumErrors = num_errs;
                (*Test[Loop].Call) ();
                Test[Loop].NumErrors = num_errs - Test[Loop].NumErrors;
                MESSAGE(5, printf("===============================================\n");
                    );
                MESSAGE(5, printf("There were %d errors detected.\n\n", (int) Test[Loop].NumErrors);
                    );

#ifdef QAK
                MESSAGE(2, printf("Testing  -- %s (%s) \n", Test[Loop].Description,
                                  Test[Loop].Name);
                    );
                MESSAGE(5, printf("===============================================\n");
                    );
                Test[Loop].NumErrors = num_errs;
                (*Test[Loop].Call) ();
                Test[Loop].NumErrors = num_errs - Test[Loop].NumErrors;
                MESSAGE(5, printf("===============================================\n");
                    );
#endif

            }   /* end else */
      }     /* end for */

    MESSAGE(2, printf("\n\n");
        )
        if (num_errs)
        printf("!!! %d Error(s) were detected !!!\n\n", (int) num_errs);
    else
        printf("All tests were successful. \n\n");

    if (Summary)
      {
          printf("Summary of Test Results:\n");
          printf("Name of Test     Errors Description of Test\n");
          printf("---------------- ------ --------------------------------------\n");

          for (Loop = 0; Loop < Index; Loop++)
            {
                if (Test[Loop].NumErrors == -1)
                    printf("%16s %6s %s\n", Test[Loop].Name, "N/A", Test[Loop].Description);
                else
                    printf("%16s %6d %s\n", Test[Loop].Name, (int) Test[Loop].NumErrors,
                           Test[Loop].Description);
            }   /* end for */
          printf("\n\n");
      }     /* end if */

    if (CleanUp)
      {
          MESSAGE(2, printf("\nCleaning Up...\n\n");
              );
#ifdef VMS   
       system("delete *.tmp;*");
#else        /* VMS */
#if !(defined DOS386 | defined WIN386)
          system("rm -f *.hdf *.tmp");
#else   /* OLD_WAY */
          remove("*.hdf");
          remove("*.tmp");
#endif  /* OLD_WAY */
#endif  /* VMS */
      }     /* end if */
    exit(num_errs);
    return num_errs;
}   /* end main() */
