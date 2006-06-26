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
$! $Id: MAKEUTIL.COM,v 1.16 1999/04/27 20:15:11 epourmal Exp $
$!
$! Make HDF utilities
$ !
$ ! This assumes that you have run top level MAKE.COM which came
$ !   with the HDF distribution.  Otherwise, you will need to 
$ !   manually define hdf$lib and sys$clib
$ !
$ type sys$input
	Making utilities
$ hdflib="[-.src]df/lib,[-.jpeg]libjpeg/lib,[-.zlib]libz/lib"
$ hdfinc="([-.src],[-.jpeg],[-.zlib])"
$ utils="hdfls,r8tohdf,hdfcomp,hdftor8,hdftopal,hdf8to24,"+-
"jpeg2hdf,fp2hdf,fptest,he_main,he_cntrl,he_disp,he_file,"  +-
"paltohdf,hdf24to8,hdfpack,ristosds,vshow,vmake,hdf2jpeg"
$! "jpeg2hdf,fp2hdf,he_main,he_cntrl,he_disp,he_file"
$! define/nolog hdf$lib MRLXP1$DKA200:[folk.dev.hdf.src]df.olb
$ if f$getsyi("arch_name") .eqs. "VAX"
$ then
$    define/nolog  sys$clib sys$library:deccrtl
$    ccopt = "/DECC/STANDARD=VAXC/define=VMS/include='hdfinc"
$ else
$    define/nolog  sys$clib sys$library:vaxcrtl
$    ccopt = "/define=VMS/include='hdfinc"
$ endif
$ cc 'ccopt  'utils
$! link/exe=hdfed he_main,he_cntrl,he_disp,he_file,'hdflib, -
 !  sys$clib/lib, helink.opt/option
$ link/exe=hdfed he_main,he_cntrl,he_disp,he_file,helink.opt/option, 'hdflib, -
   sys$clib/lib
$ link hdfls,     'hdflib, sys$clib/lib
$! link hdfrseq,   'hdflib, sys$clib/lib
$ link r8tohdf,   'hdflib, sys$clib/lib
$ link hdfcomp,   'hdflib, sys$clib/lib
$ link hdftor8,   'hdflib, sys$clib/lib
$ link hdftopal,  'hdflib, sys$clib/lib
$ link paltohdf,  'hdflib, sys$clib/lib
$ link hdf24to8,  'hdflib, sys$clib/lib
$ link hdfpack,   'hdflib, sys$clib/lib
$ link ristosds,  'hdflib, sys$clib/lib
$ link vshow,     'hdflib, sys$clib/lib
$ link vmake,     'hdflib, sys$clib/lib
$ link hdf2jpeg,  'hdflib, sys$clib/lib
$ link jpeg2hdf,  'hdflib, sys$clib/lib
$ link hdf24to8,  'hdflib, sys$clib/lib
$ link fp2hdf,    'hdflib, sys$clib/lib
$ link fptest,    'hdflib, sys$clib/lib
$ type sys$input
	Made all utils
****	Please edit setuputils.com and run the script to set up executables
$ delete *.obj;*
$
