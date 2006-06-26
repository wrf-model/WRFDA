$! Tests NCGEN on VMS.
$
$! Assumptions:
$!   You are in the NCGEN source directory.
$!   You have already made the NCGEN and NCDUMP executables from their
$!   respective sources. 
$
$ ncgen == "$ sys$disk:[]ncgen.exe"
$ ncdump == "$ sys$disk:[-.ncdump]ncdump.exe"
$
$! Test "-n" option of ncgen
$
$! Create test0.cdf from test0.cdl
$ ncgen -n test0.cdl
$
$! Create test1.cdl from test0.cdf
$ define/user sys$output test1.cdl
$ ncdump -n test1 test0.cdf
$
$! Create test1.cdf from test1.cdl
$ ncgen -n test1.cdl
$
$! Create test2.cdl from test1.cdf
$ define/user sys$output test2.cdl
$ ncdump -n test2 test1.cdf
$
$! Compare test1.cdl and test2.cdl.  They should be identical.
$ difference test1.cdl test2.cdl	
$
$! Create test1.cdl
$! Create test0.nc from test0.cdl
$
$ define/usrs sys$output test0.nc
$ ncgen -b test0.cdl
$ Create test1.cdf from test0.nc
$ ncdump -n test1 test0.nc
$
$! Test "-c" option of ncgen
$
$! Create C program TEST0.C from test0.cdl
$ define/user sys$output test0.c
$ ncgen -c -o ctest0.cdf test0.cdl
$
$! Compile generated C program
$ cc/include_dir=[--.include]/nodebug test0
$ link /exe=test0 test0,[--.lib]mfhdf/lib,sys$input/opt
	[--.hdf.src]df/lib,[--.hdf.jpeg]libjpeg.olb/lib, -
        [--.hdf.zlib]libz.olb/lib, sys$library:vaxcrtl/lib
$
$! Run generated C program to create netCDF file ctest0.cdf
$ run test0
$
$! Dump ctest0.cdf into CDL file ctest1.cdl
$ define/user sys$output ctest1.cdl
$ ncdump -n ctest1 ctest0.cdf
$
$! Compare test1.cdl and ctest1.cdl.  They should be identical.
$ difference test1.cdl ctest1.cdl	
$
$! Test "-f" option of ncgen
$
$! Create FORTRAN program TEST0.FOR from test0.cdl
$ define/user sys$output ftest0.for
$ ncgen -f -o ftest0.cdf test0.cdl
$
$! Compile generated FORTRAN program
$ copy [--.include]netcdf.inc netcdf.inc
$ fortran/nodebug ftest0
$ link /exe=ftest0 ftest0.obj,[--.lib]mfhdf/lib,sys$input/opt
	[--.hdf.src]df/lib,[--.hdf.jpeg]libjpeg.olb/lib, -
        [--.hdf.zlib]libz.olb/lib, sys$library:vaxcrtl/lib
$
$! Run generated FORTRAN program to create netCDF file ftest0.cdf
$ run ftest0
$
$! Dump ftest0.cdf into CDL file ftest1.cdl
$ define/user sys$output ftest1.cdl
$ ncdump -n ftest1 ftest0.cdf
$
$! Compare test1.cdl and ftest1.cdl.  They should be identical.
$ difference test1.cdl ftest1.cdl	
$
