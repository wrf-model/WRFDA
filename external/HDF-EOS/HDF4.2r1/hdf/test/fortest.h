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

/* $Id: fortest.h,v 1.4 2003/12/10 21:13:33 epourmal Exp $ */

#ifndef __FORTEST_H
#define __FORTEST_H
#include "hdf.h"

/* Verbosity Environment Variable */
#define FOR_VERB    "HDF_FOR_VERBOSITY"

#ifdef DF_CAPFNAMES
#  define ngetverb      FNAME(GETVERB)
#  define nhisystem       FNAME(HISYSTEM)
#  define nfixnamec        FNAME(FIXNAMEC) 
#else  /* !DF_CAPFNAMES */
#  define ngetverb      FNAME(getverb)
#  define nhisystem       FNAME(hisystem)
#  define nfixnamec        FNAME(fixnamec) 
#endif /* DF_CAPFNAMES */

/* FORTRAN support C-stubs for FORTRAN interface tests */

extern FRETVAL(intf) ngetverb(void);
extern FRETVAL(intf) nhisystem(_fcd cmd, intf *cmdlen);
extern FRETVAL(intf) nfixname(_fcd name, intf *name_len, _fcd name_out, intf *name_len_out);

#endif /* __FORTEST_H */

