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
! $Id: SETUPTEST.COM,v 1.3 1996/03/25 17:54:24 sxu Exp $
!$ ! This file is only for VAX/VMS systems.
!$ !
!$ ! set up testhdf
!$ ! change below to the hdf bin directory
!$ ! 
$ define/nolog hdf$test    mrlxp4$dka200:[folk.dev.hdf.test]
$ !
$ testhdf     :== $ hdf$test:testhdf
