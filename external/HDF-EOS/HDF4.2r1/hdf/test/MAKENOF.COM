! ****************************************************************************
! * NCSA HDF                                                                 *
! * Software Development Group                                               *
! * National Center for Supercomputing Applications                          *
! * University of Illinois at Urbana-Champaign                               *
! * 605 E. Springfield, Champaign IL 61820                                   *
! *                                                                          *
! * For conditions of distribution and use, see the accompanying             *
! * hdf/COPYING file.                                                        *
! *                                                                          *
! ***************************************************************************
!
! $Id: MAKENOF.COM,v 1.3 1993/12/01 03:44:57 sxu Exp $
!$!
!$!
!$! File to build the test files in the HDF test suite
!$!
!$!
!$! The following constants should probably be defined
!$!   only once in the main HDF MAKE.COM...
!$!
$ define/nolog hdf$lib [-.lib]df
$ define/nolog hdf$include [-.include]
$ define/nolog sys$clib sys$library:vaxcrtl
!$!
$ type sys$input
  **    Making testhdf **
$ cc/define=VMS/include=hdf$include   testhdf
$ type sys$input
  **    Making H level test programs **
$!
$ cc/define=VMS/include=hdf$include   file
$ cc/define=VMS/include=hdf$include   file1
$ cc/define=VMS/include=hdf$include   blocks
$ cc/define=VMS/include=hdf$include   extelt
$ cc/define=VMS/include=hdf$include   vers
$ type sys$input
  **  Making C interface test programs **
$!
$ cc/define=VMS/include=hdf$include   rig
$ cc/define=VMS/include=hdf$include   sdmms
$ cc/define=VMS/include=hdf$include   sdnmms
$ cc/define=VMS/include=hdf$include   an
$ cc/define=VMS/include=hdf$include   anfile
$ cc/define=VMS/include=hdf$include   slab
$ cc/define=VMS/include=hdf$include   sdstr
$ cc/define=VMS/include=hdf$include   litend
$ cc/define=VMS/include=hdf$include   tvset
$ cc/define=VMS/include=hdf$include   conv
$ cc/define=VMS/include=hdf$include   tree

$ link testhdf,rig,sdmms,sdnmms,an,anfile,sdstr,slab,litend, -
  file, file1,blocks,extelt,vers,tvset,conv, tree,  -
  hdf$lib/lib,sys$clib/lib
$ type sys$input 
  **  Making Fortran interface test programs **
$!
