$! --------------------------------------------------------------------------
$! For making NCDUMP.EXE on VMS.
$! --------------------------------------------------------------------------
$!
$! $Id: make.com,v 1.3 1996/11/07 00:29:37 sxu Exp $
$!
$ if f$getsyi("arch_name") .eqs. "VAX"
$ then 
$ ccopt = "/DECC/STANDARD=VAXC"
$ define/nolog sys$clib sys$library:deccrtl
$ getopt = ""
$ else
$ ccopt = ""
$ define/nolog sys$clib sys$library:vaxcrtl
$ getopt = "getopt.obj, "
$ endif
$ ccc := cc 'ccopt /opt/nodebug -
           /define=(HDF,VMS,NO_SYS_XDR_INC)/nolist -
            /include=([-.libsrc],[-.xdr],[--.hdf.src], -
            [--.hdf.jpeg], [--.hdf.zlib]) 
$
$ ccc [-.UTIL]GETOPT.C
$ ccc DUMPLIB.C
$ ccc NCDUMP.C
$ ccc VARDATA.C
$
$ link/nodebug/exe=NCDUMP.exe -
    'getopt   dumplib.obj, -
    ncdump.obj, -
    vardata.obj, -
    [--.lib]mfhdf/lib,[--.hdf.src]df/lib, -
    [--.hdf.jpeg]libjpeg.olb/lib,  -
    [--.hdf.zlib]libz.olb/lib, -
    sys$input/opt
        sys$clib/lib
