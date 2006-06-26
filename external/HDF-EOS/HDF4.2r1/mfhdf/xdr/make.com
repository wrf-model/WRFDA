$! --------------------------------------------------------------------------
$! For making XDRTEST.EXE on VMS if you don't have MMS.
$! --------------------------------------------------------------------------
$!
$! $Id: make.com,v 1.4 1996/11/07 00:27:30 sxu Exp $
$
$ macro :== macro/nolist/migration
$ if f$getsyi("arch_name") .eqs. "VAX"
$ then 
$ ccopt = "/DECC/STANDARD=VAXC"
$ define/nolog sys$clib sys$library:deccrtl
$ else
$ ccopt = ""
$ define/nolog sys$clib sys$library:vaxcrtl
$ endif
$ ccc := cc 'ccopt /opt/nodebug/nolist -
            /define=(HDF,VMS, NO_SYS_XDR_INC)  -
            /include=([],[--.hdf.src], [--.hdf.jpeg],[--.hdf.zlib])
$!
$ ccc XDR.C
$ ccc XDRTEST.C
$ ccc XDRARRAY.C
$ ccc XDRFLOAT.C
$ ccc XDRSTDIO.C
$! macro HTONL.MAR
$! macro NTOHL.MAR
$!
$ library/create [-.-.LIB]MFHDF.OLB
$! library/replace [-.-.LIB]MFHDF.OLB XDR, XDRFLOAT, XDRSTDIO, HTONL, NTOHL
$ library/replace [-.-.LIB]MFHDF.OLB XDR, XDRFLOAT, XDRSTDIO
$!
$ link/nodebug/exec=XDRTEST.exe -
    xdrtest.obj, -
    xdrarray.obj, -
    [-.-.lib]mfhdf.olb/lib, -
    sys$input/opt
       sys$clib/lib
$ delete *.obj;*
