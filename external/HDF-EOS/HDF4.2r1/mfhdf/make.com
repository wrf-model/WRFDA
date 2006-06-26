$! --------------------------------------------------------------------------
$! For making CDFTEST.EXE on VMS if you don't have MMS.
$! --------------------------------------------------------------------------
$!
$! $Id: make.com,v 1.2 1993/09/03 12:16:43 koziol Exp $
$!
$! You must create the netcdf library, NETCDF.OLB, from the XDR directory,
$! [-.xdr], before executing this procedure.
$
$
$ macro :== macro/nolist
$ ccc := cc /opt/nodebug/include=([-.xdr],[-.-.hdf.include])/nolist  -
            /define=stdc_includes/define=swap/define=VMS/define=HDF

$ librep := library/replace [-.-.-.LIB]NETCDF.OLB
$
$ define rpc sys$disk:[-.xdr]
$ define sys sys$library
$
$ ccc ARRAY.C
$ ccc ATTR.C
$ ccc CDF.C
$ ccc CDFTEST.C
$ ccc DIM.C
$ ccc ERROR.C
$ ccc FILE.C
$ ccc IARRAY.C
$ ccc PUTGET.C
$ ccc PUTGETG.C
$ ccc SHARRAY.C
$ ccc STRING.C
$ ccc VAR.C
$ ccc XDRPOSIX.C
! $ ccc XDRSTDIO.C
$ ccc HDFSDS.C
$ ccc MFSD.C
$ ccc hdftest.c
$ macro HTONS.MAR
$ macro NTOHS.MAR
$
$ librep ARRAY, ATTR, CDF, DIM, FILE, IARRAY, ERROR, -
    PUTGET, PUTGETG, SHARRAY, STRING, VAR, HTONS, NTOHS, -
    HDFSDS, MFSD, XDRPOSIX
$ library/list=netcdf.list/name [-.-.-.lib]netcdf.olb
$ link/nodebug/exec=CDFTEST.exe -
    cdftest.obj, -
    [-.-.-.lib]netcdf/library, -
    [-.-.hdf.lib]df/library, -
    sys$input/opt
	sys$library:vaxcrtl.exe/share
$
$ create/dir [-.-.-.include]
$
$ copy netcdf.h [-.-.-.include]
$
$ link/nodebug/exec=hdftest.exe -
    hdftest.obj, -
    [-.-.-.lib]netcdf/library, -
    [-.-.hdf.lib]df/library, -
        sys$input/opt
        sys$library:vaxcrtl.exe/share
$
