$! --------------------------------------------------------------------------
$! For making NCGEN.EXE on VMS.
$! --------------------------------------------------------------------------
$!
$! $Id: make.com,v 1.6 2001/10/22 20:31:09 epourmal Exp $
$!
$ if f$getsyi("arch_name") .eqs. "VAX"
$ then 
$ ccopt = "/DECC/STANDARD=VAXC"
$ define/nolog sys$clib sys$library:deccrtl
$ else
$ ccopt = ""
$ define/nolog sys$clib sys$library:vaxcrtl
$ endif
$ ccc := cc 'ccopt  -
       /opt/nodebug/include=([--.include],[--.hdf.src], [--.mfhdf.libsrc], -
     [--.hdf.jpeg],[--.hdf.zlib], sys$library)/nolist    -
     /define=(HDF,VMS,NO_SYS_XDR_INC)
$
$ copy vmstab.c ncgentab.c
$ copy vmstab.h ncgentab.h
$ copy vms_yy.c ncgenyy.c
$
$ ccc MAIN.C
$ ccc GENERATE.C
$ ccc LOAD.C
$ ccc NCGENTAB.C
$ ccc ESCAPES.C
$ ccc GETFILL.C
$ ccc INIT.C
$ ccc CLOSE.C
$ ccc GENLIB.C
$ ccc [-.UTIL]GETOPT.C
$
$ link/nodebug/notraceback/exe=NCGEN.exe -
    getfill.obj, -
    close.obj, -
    genlib.obj, -
    escapes.obj, -
    generate.obj, -
    getopt.obj, -
    init.obj, -
    load.obj, -
    main.obj, -
    ncgentab.obj, -
    [--.lib]mfhdf/lib, [--.hdf.src]df/lib,  -
    [--.hdf.jpeg]libjpeg.olb/lib, -
    [--.hdf.zlib]libz.olb/lib, -
    sys$input/opt
	sys$clib/lib
