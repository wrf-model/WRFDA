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
static char RcsId[] = "$Revision: 1.4 $";
#endif

/* $Id: forsupf.c,v 1.4 2003/12/10 21:13:32 epourmal Exp $ */

#include "hdf.h"
#include "fortest.h"

/*-----------------------------------------------------------------------------
 * Name:    getverb
 * Purpose: Get the verbosity from the "HDF_VERBOSITY" environment variable
 *          and return it to the FORTRAN calling routine.
 * Inputs:  NONE
 * Returns: verbosity level on success, FAIL on failure
 * Users:   HDF Fortran programmers
 * Invokes: HDgetenv
 *---------------------------------------------------------------------------*/

FRETVAL(intf)
ngetverb(void)
{
    char *verb_str;
    intn verb_level=FAIL;
#ifdef vms
    return(4);
#else
    verb_str=HDgetenv(FOR_VERB);

    if(verb_str!=NULL)
        verb_level=(intn)HDstrtol(verb_str,NULL,0); /* convert whole string using base 10 */
    return((intf)verb_level);
#endif
}   /* end getverb() */

/*-----------------------------------------------------------------------------
 * Name:    hisystem
 * Purpose: Invoke the system call to execute cmd
 * Inputs:  cmd -- the command to execute
 * Returns: verbosity level on success, FAIL on failure
 * Users:   HDF Fortran programmers
 * Invokes: HDgetenv
 *---------------------------------------------------------------------------*/

FRETVAL(intf)
nhisystem(_fcd cmd, intf *cmdlen)
{
    char       *fn;
    intf        ret;

    fn = HDf2cstring(cmd, (intn) *cmdlen);
    if (!fn) return(FAIL);
    ret = (intf) system(fn);
    HDfree(fn);
    return (ret);
}   /* end nhisystem() */

/*-----------------------------------------------------------------------------
 * Name:    fixname
 * Purpose: Fix name for srcdir build and test
 * Inputs:  IN: name - original namea 
 *          IN: name_len - name length
 *          IN/OUT: name_out - buffer to hold modified name
 *          IN/OUT: name_out_len - length of the buffer, and length of modified
 *                  string. 
 * Returns: 0 on success and -1 on failure  
 * Users:   HDF Fortran programmers
 *---------------------------------------------------------------------------*/

FRETVAL(intf)
nfixnamec(_fcd name, intf *name_len, _fcd name_out, intf *name_len_out)
{
    char       *c_name;
    intf        ret;

    char    testfile[1024] = "";
    char   *srcdir = getenv("srcdir");

    c_name = HDf2cstring(name, (intn) *name_len);
    if (!c_name) return(FAIL);

    /* Here comes Bill's code */
    /* Generate the correct name for the test file, by prepending the source path */
    if (srcdir && ((strlen(srcdir) + strlen(c_name) + 1) < sizeof(testfile))) {
        strcpy(testfile, srcdir);
        strcat(testfile, "/");
    }
    strcat(testfile, c_name);
    *name_len_out = (intf) strlen(testfile);
    HDpackFstring(testfile, _fcdtocp(name_out), *name_len_out);
 
    ret = 0;
    HDfree(c_name);
    return (ret);
}   /* end nfixname() */
