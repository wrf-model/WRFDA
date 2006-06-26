$! --------------------------------------------------------------------------
$! For making HDP.EXE on VMS.
$! --------------------------------------------------------------------------
$!
$!
$ if f$getsyi("arch_name") .eqs. "VAX"
$ then 
$ ccopt = "/DECC/STANDARD=VAXC"
$ define/nolog sys$clib sys$library:deccrtl
$ else
$ ccopt = ""
$ define/nolog sys$clib sys$library:vaxcrtl
$ endif
$ ccc := cc 'ccopt /opt/nodebug/define=(HDF,NO_SYS_XDR_INC)/nolist -
            /include=([-.libsrc],[-.xdr],[--.hdf.src], -
            [--.hdf.jpeg], [--.hdf.zlib]) 
$
$ ccc hdp.C
$ ccc hdp_dump.C
$ ccc hdp_list.C
$ ccc hdp_rig.C
$ ccc hdp_sds.C
$ ccc hdp_util.C
$ ccc hdp_vd.C
$ ccc hdp_vg.C
$ ccc hdp_gr.C
$ ccc show.C
$ link/nodebug/notraceback/exe=hdp.exe -
    hdp.obj, -
    hdp_dump.obj, -
    hdp_list.obj, -
    hdp_rig.obj, -
    hdp_sds.obj, -
    hdp_util.obj, -
    hdp_vd.obj, -
    hdp_vg.obj, -
    hdp_gr.obj, -
    show.obj, -
    [--.lib]mfhdf/lib,[--.hdf.src]df/lib,[--.hdf.jpeg]libjpeg.olb/lib,  -
    [--.hdf.zlib]libz.olb/lib, -
    sys$input/opt
        sys$clib/lib

