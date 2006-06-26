$! --------------------------------------------------------------------------
$! For making FTEST.EXE on VMS if you don't have MMS.
$! --------------------------------------------------------------------------
$!
$! $Id: make.com,v 1.5 1996/11/07 00:31:10 sxu Exp $
$
$ if f$getsyi("arch_name") .eqs. "VAX"
$ then 
$ ccopt = "/DECC/STANDARD=VAXC"
$ define/nolog sys$clib sys$library:deccrtl
$ else
$ ccopt = ""
$ define/nolog sys$clib sys$library:vaxcrtl
$ endif
$ ccc := cc 'ccopt /opt/nodebug/nolist -
          /define=(HDF, VMS, NO_SYS_XDR_INC)    -
         /include=([--.libsrc],[---.include],[---.hdf.src], -
          [---.hdf.jpeg], [---.hdf.zlib],[--.xdr])
$
$ ccc JACKETS.C
$ ccc MFSDF.C
$ fort/opt/nodebug MFSDFF.FOR
$ fort/opt/nodebug  HDFTEST.FOR
$ fort/opt/nodebug  FTEST.FOR
$
$ link/nodebug/notraceback/exec=HDFTEST.exe -
    hdftest.obj, mfsdf.obj, mfsdff.obj, -
    [---.lib]mfhdf.olb/lib, [---.hdf.src]df.olb/lib, -
    [---.hdf.jpeg]libjpeg/lib,  [---.hdf.zlib]libz.olb/lib, -
    sys$input/opt
        sys$clib/lib

$ link/nodebug/notraceback/exec=FTEST.exe -
    ftest.obj, -
    jackets.obj, -
    [---.lib]mfhdf.olb/lib, [---.hdf.src]df.olb/lib,  -
    [---.hdf.jpeg]libjpeg/lib,  [---.hdf.zlib]libz.olb/lib, -
    sys$input/opt
	sys$clib/lib
$
$ library/replace [---.lib]mfhDF.OLB JACKETS
$ library/replace [---.lib]mfhDF.OLB mfsdf
$ library/replace [---.lib]mfhDF.OLB mfsdff
$
$ copy netcdf.inc [---.include]
$ type sys$input
     Run ftest
$ run ftest
$ type sys$input
     Run hdftest
$ create/dir [.testdir]
$ run hdftest
$
$ type sys$input
     Clean up ...
$ delete *.obj;*
$ delete *.exe;*

