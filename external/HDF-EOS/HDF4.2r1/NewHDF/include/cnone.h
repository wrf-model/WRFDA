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

/* $Id: cnone.h,v 1.4 1997/10/24 21:00:22 koziol Exp $ */

/*-----------------------------------------------------------------------------
 * File:    cnone.h
 * Purpose: Header file for "none" encoding information.
 * Dependencies: should only be included from hcompi.h
 * Invokes: none
 * Contents: Structures & definitions for "none" encoding.  This header
 *              should only be included in hcomp.c and cnone.c.
 * Structure definitions:
 * Constant definitions:
 *---------------------------------------------------------------------------*/

/* avoid re-inclusion */
#ifndef __CNONE_H
#define __CNONE_H

#if defined c_plusplus || defined __cplusplus
extern      "C"
{
#endif                          /* c_plusplus || __cplusplus */

/*
   ** from cnone.c
 */

    extern int32 HCPcnone_stread
                (accrec_t * rec);

    extern int32 HCPcnone_stwrite
                (accrec_t * rec);

    extern int32 HCPcnone_seek
                (accrec_t * access_rec, int32 offset, int origin);

    extern int32 HCPcnone_inquire
                (accrec_t * access_rec, int32 *pfile_id, uint16 *ptag, uint16 *pref,
               int32 *plength, int32 *poffset, int32 *pposn, int16 *paccess,
                 int16 *pspecial);

    extern int32 HCPcnone_read
                (accrec_t * access_rec, int32 length, void * data);

    extern int32 HCPcnone_write
                (accrec_t * access_rec, int32 length, const void * data);

    extern intn HCPcnone_endaccess
                (accrec_t * access_rec);

#if defined c_plusplus || defined __cplusplus
}
#endif                          /* c_plusplus || __cplusplus */

/* "none" [en|de]coding information */
typedef struct
{
    intn        space_holder;   /* merely a space holder so compilers don't barf */
}
comp_coder_none_info_t;

#ifndef CNONE_MASTER
extern funclist_t cnone_funcs;  /* functions to perform run-length encoding */
#else
funclist_t  cnone_funcs =
{                               /* functions to perform run-length encoding */
    HCPcnone_stread,
    HCPcnone_stwrite,
    HCPcnone_seek,
    HCPcnone_inquire,
    HCPcnone_read,
    HCPcnone_write,
    HCPcnone_endaccess
};
#endif

#endif /* __CNONE_H */
