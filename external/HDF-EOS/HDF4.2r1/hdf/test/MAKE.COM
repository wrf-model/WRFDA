$! ****************************************************************************
$! * NCSA HDF                                                                 *
$! * Software Development Group                                               *
$! * National Center for Supercomputing Applications                          *
$! * University of Illinois at Urbana-Champaign                               *
$! * 605 E. Springfield, Champaign IL 61820                                   *
$! *                                                                          *
$! * For conditions of distribution and use, see the accompanying             *
$! * hdf/COPYING file.                                                        *
$! *                                                                          *
$! ***************************************************************************
$!
$! Id: MAKE.COM,v 1.9 1996/01/18 05:09:44 sxu Exp $
$!
$!
$! File to build the test files in the HDF test suite
$!
$!
$! The following constants should probably be defined
$!   only once in the main HDF MAKE.COM...
$!
$ hdflib= "[-.src]df/lib,[-.jpeg]libjpeg/lib,[-.zlib]libz/lib"
$ hdfinc= "([-.src],[-.jpeg],[-.zlib])"
$! define/nolog hdf$include ([-.src],[-.jpeg],[-.zlib])
$! define/nolog sys$clib sys$library:deccrtl
$ if f$getsyi("arch_name") .eqs. "VAX"
$ then 
$ ccopt = "/DECC/STANDARD=VAXC/opt/nodebug/define=VMS/include='hdfinc"
$ define/nolog sys$clib sys$library:deccrtl
$ else
$ ccopt = "/opt/nodebug/define=VMS/include='hdfinc"
$ define/nolog sys$clib sys$library:vaxcrtl
$ endif
$! ccopt= "/debug/noopt/include=([-.src],[-.jpeg],[-.zlib])/define=VMS"
$! foropt= "/debug/noopt"
$ foropt= "/opt/nodebug"
$!
$ type sys$input
  **    Making testhdf **
$ cc 'ccopt   testhdf
$ type sys$input
  **    Making H level test programs **
$!
$ cc 'ccopt   file
$ cc 'ccopt   file1
$ cc 'ccopt   blocks
$ cc 'ccopt   extelt
$ cc 'ccopt   vers
$ cc 'ccopt   buffer
$ type sys$input
  **  Making C interface test programs **
$!
$ cc 'ccopt   rig
$ cc 'ccopt   sdmms
$ cc 'ccopt   sdnmms
$ cc 'ccopt   an
$ cc 'ccopt   anfile
$ cc 'ccopt   slab
$ cc 'ccopt   sdstr
$ cc 'ccopt   litend
$ cc 'ccopt   tvset
$ cc 'ccopt   conv
$ cc 'ccopt   tree
$ cc 'ccopt   bitio
$ cc 'ccopt   comp
$ cc 'ccopt   man
$ cc 'ccopt   mgr.c
$ cc 'ccopt   nbit
$ cc 'ccopt   tbv
$ cc 'ccopt   gentest
$ cc 'ccopt   tvsfpack
$ cc 'ccopt   tvattr
$ cc 'ccopt   chunks
$ cc 'ccopt   macros
$ link/nodebug testhdf,rig,sdmms,sdnmms,an,anfile,sdstr, buffer, -
  slab,litend, file,file1,blocks,extelt,vers,tvset,conv,tree, -
  bitio,comp,man,mgr.obj,nbit,tbv,tvsfpack,tvattr,chunks, macros, -
  'hdflib, sys$clib/lib
$!  'hdflib, sys$library:deccrtl/lib
$! link gentest, 'hdflib, sys$library:deccrtl/lib
$ link gentest, 'hdflib, sys$clib/lib
$ type sys$input 
  **  Making Fortran interface test programs **
$!
$! cc /debug/noopt/include=([-.src],[-.jpeg],[-.zlib])/define=VMS fortest.c
$ cc 'ccopt   fortest.c
$! cc/debug/noopt/include=([-.src],[-.jpeg],[-.zlib])/define=VMS forsupf.c
$ cc 'ccopt   forsupf.c
$ fortran 'foropt fortestF.f
$ fortran 'foropt tr8F.f
$ fortran 'foropt t24F.f
$ fortran 'foropt tpF.f
$ fortran 'foropt tsdmmsF.f
$ fortran 'foropt tsdnmmsF.f
$ fortran 'foropt tsdntF.f
$ fortran 'foropt tsdnntF.f
$ fortran 'foropt tanF.f
$ fortran 'foropt tanfileF.f
$ fortran 'foropt tstubsF.f
$ fortran 'foropt tsdstrF.f
$ fortran 'foropt slabwf.f
$ fortran egfhi.f
$ fortran 'foropt forsupff.f
$ fortran 'foropt manf.f
$ fortran 'foropt mgrf.f
$ fortran 'foropt tvsetf.f
$ fortran 'foropt tvattrf.f
$!
$ link fortest,forsupf, 'hdflib, sys$clib/lib
$! link tr8f, forsupff,forsupf, 'hdflib, sys$clib/lib
$! link t24f,forsupff, forsupf, 'hdflib, sys$clib/lib
$! link tpf, forsupff, forsupf, 'hdflib, sys$clib/lib
$! link tsdmmsf,forsupff,forsupf, 'hdflib, sys$clib/lib
$! link tsdnmmsf,forsupff,forsupf, 'hdflib, sys$clib/lib
$! link tsdntf,forsupff, forsupf, 'hdflib, sys$clib/lib
$! link tsdnntf,forsupff, forsupf, 'hdflib, sys$clib/lib
$! link tanf,forsupff, forsupf, 'hdflib, sys$clib/lib
$! link tanfilef,forsupff, forsupf, 'hdflib, sys$clib/lib
$! link tstubsf,forsupff, forsupf, 'hdflib, sys$clib/lib
$! link tsdstrf,forsupff, forsupf, 'hdflib, sys$clib/lib
$! link slabwf,forsupff, forsupf, 'hdflib, sys$clib/lib
$! link manf,forsupff, forsupf, 'hdflib, sys$clib/lib
$! link mgrf,forsupff, forsupf, 'hdflib, sys$clib/lib
$ link egfhi,forsupff, forsupf, 'hdflib, sys$clib/lib
$! link tvsetf,forsupff,forsupf, 'hdflib, sys$clib/lib
$! link tvattrf,forsupff,forsupf, 'hdflib, sys$clib/lib
$ link fortestF, forsupff,forsupf,tr8f,t24f,tpf,tsdmmsf, -
  tsdnmmsf,tsdntf,tsdnntf,tanf,tanfilef,tstubsf,tsdstrf, -
  slabwf,manf,mgrf,tvsetf,tvattrf, -
  'hdflib, sys$clib/lib
$! delete *.obj;*
$ exit
