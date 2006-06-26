/*********************************************************************
 *   Copyright 1993, UCAR/Unidata
 *   See netcdf/COPYRIGHT file for copying and redistribution conditions.
 *   $Id: driver.c,v 1.10 2003/12/10 21:15:30 epourmal Exp $
 *********************************************************************/

#include "netcdf.h"
#include "tests.h"

/* #define MDEBUG 1 */

  /*
   * Test driver for netCDF implementation.  This program performs
   * tests against the netCDF specification for all user-level
   * functions in an implementation of the netCDF library.  Must be
   * invoked from a directory in which the invoker has write
   * permission.
   */

#if defined TEST_PC || defined TEST_WIN
#include <stdio.h>
FILE *dbg_file;
#endif

#include <stdlib.h>

#ifdef macintosh
    #include <LowMem.h>
#endif
#if defined __MWERKS__
#include <console.h>
#endif

#ifdef PROTOTYPE
int main(int argc, char *argv[])
#else
int main(argc, argv)
int argc;
char *argv[];
#endif
{
    static char testfile[] = "test.nc";
    static char unlim_testfile_name[] = "test_unlim.nc";
    char *unlim_testfile;

    int name_size;
    char *srcdir;

#ifdef macintosh
	Ptr	currStackBase, newApplLimit, currApplLimit, currHeapEnd;


	/*	Expand the stack.  hdf_write_var( ) causes the stack to collide with
		the 68K application heap when only the default stack size is used.   */
	currStackBase = LMGetCurStackBase( );
	newApplLimit = (Ptr) ( (long) currStackBase - 65536L );
	currApplLimit = GetApplLimit( );
	if ( newApplLimit > currApplLimit )	/* If we're about to shrink the stack, ...*/
		 newApplLimit = currApplLimit;	/* ... then don't. */

	currHeapEnd = LMGetHeapEnd( );
	if ( newApplLimit < currHeapEnd )	/* If we're about overlap the stack and heap,...*/
		 newApplLimit = currHeapEnd;	/* ... then don't.*/

	SetApplLimit( newApplLimit );
#endif
#if defined __MWERKS__
    argc = ccommand(&argv);
#endif

#if defined TEST_PC || defined TEST_WIN
    dbg_file=fopen("test.dbg","w+");
#endif

#ifdef MDEBUG
    malloc_debug(2);
#endif /* MDEBUG */

    ncopts &= ~NC_FATAL;	/* make errors nonfatal */
    ncopts &= ~NC_VERBOSE;	/* turn off error messages */
    ncopts |= NC_VERBOSE;	/* turn  error messages on--AKC */
    ncopts &= ~NC_VERBOSE;	/* turn off error messages */

    test_nccreate(testfile);

    test_ncopen(testfile);

    test_ncredef(testfile);

    test_ncendef(testfile);

    test_ncclose(testfile);

    test_ncinquire(testfile);

    test_ncsync(testfile);

    test_ncabort(testfile);

    test_ncdimdef(testfile);

    test_ncdimid(testfile);

    test_ncdiminq(testfile);

    test_ncdimrename(testfile);

    test_ncvardef(testfile);

    test_ncvarid(testfile);

    test_ncvarinq(testfile);

    test_ncvarput1(testfile);

    test_ncvarget1(testfile);

    test_ncvarput(testfile);

    test_ncvarget(testfile);

    name_size = strlen(unlim_testfile_name) + 1;
    srcdir = getenv("srcdir");

    if (srcdir)
        name_size += strlen(srcdir) + 1;

    unlim_testfile = calloc(name_size, 1);

    if (srcdir) {
        strcpy(unlim_testfile, srcdir);

        if (srcdir[strlen(srcdir) - 1] != '/')
            strcat(unlim_testfile, "/");
    }

    strcat(unlim_testfile, unlim_testfile_name);
    test_ncvarget_unlim(unlim_testfile);
    free(unlim_testfile);

    test_ncvarputg(testfile);

    test_ncvargetg(testfile);

    test_ncrecinq(testfile);

    test_ncrecput(testfile);

    test_ncrecget(testfile);

    test_ncvarrename(testfile);

    test_ncattput(testfile);

    test_ncattinq(testfile);

    test_ncattget(testfile);

    test_ncattcopy(testfile, "test2.nc");

    test_ncattname(testfile);

    test_ncattrename(testfile);

    test_ncattdel(testfile);

    test_nctypelen();

#if defined TEST_PC || defined TEST_WIN
    fclose(dbg_file);
#endif
#ifdef vms
#define EXIT_SUCCESS 1
#else
#define EXIT_SUCCESS 0
#endif
    return EXIT_SUCCESS;
}

