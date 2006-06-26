$ set def [.HDF4r13]
$ !
$ write sys$output "Start building HDF"
$ !
$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
$ ! Create directories [.lib], [.bin], [.include]
$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ !
$ hdftop = F$DIRECTORY()
$ len = F$LENGTH(hdftop)
$ hdftop_dir = F$EXTRACT(0,len-1,hdftop)
$ subdir = hdftop + "LIB.DIR"
$ lib_directory = hdftop_dir + ".LIB]"
$ if F$SEARCH(subdir) .NES. ""
$ then
$    write sys$output "''subdir' directory exists, will not create"
$ else
$    write sys$output "Creating ''subdir'..."
$    create/dir 'lib_directory'
$    write sys$output "Done"
$ endif 
$ subdir = hdftop + "BIN.DIR"
$ bin_directory = hdftop_dir + ".BIN]"
$ if F$SEARCH(subdir) .NES. ""
$ then
$    write sys$output "''subdir' directory exists, will not create"
$ else
$    write sys$output "Creating ''subdir'..."
$    create/dir 'bin_directory'
$    write sys$output "Done"
$ endif 
$ subdir = hdftop + "INCLUDE.DIR"
$ include_directory = hdftop_dir + ".INCLUDE]"
$ if F$SEARCH(subdir) .NES. "" 
$ then
$    write sys$output "''subdir' directory exists, will not create"
$ else
$    write sys$output "Creating ''subdir'..."
$    create/dir 'include_directory'
$    write sys$output "Done"
$ endif
$ ! 
$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ ! Build JPEG Library and copy library and include files to [.lib] and
$ ! [.include] subdirectories.
$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ !
$ write sys$output " Building JPEG"
$ jpeg_library = hdftop_dir +".HDF.JPEG]"
$ set def 'jpeg_library'
$ copy jconfig.vms jconfig.h
$ copy makefile.vms make.com
$ @make
$ copy libjpeg.olb 'lib_directory'
$ copy jconfig.h 'include_directory'
$ copy jerror.h  'include_directory'
$ copy jmorecfg.h 'include_directory'
$ copy jpeglib.h  'include_directory'
$ delete *.obj;*
$ delete *.exe;*
$ write sys$output "Done"
$ !
$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ ! Build GZIP Library and copy library and include files to [.lib] and
$ ! [.include] subdirectories.
$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ !
$ write sys$output " Building ZLIB"
$ zlib_library = hdftop_dir +".HDF.ZLIB]"
$ set def 'zlib_library'
$ copy make_vms.com make.com
$ @make
$ run example
$ copy libz.olb 'lib_directory'
$ copy zconf.h 'include_directory'
$ copy zlib.h  'include_directory'
$ delete *.obj;*
$ delete *.exe;*
$ write sys$output "Done"
$ !
$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ ! Make full DF library.
$ ! Fortran stubs will be also built.
$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ !
$ write sys$output " Building DF Library"
$ hdfsrc_directory = hdftop_dir + ".HDF.SRC]"
$ set def 'hdfsrc_directory'
$ @makenof
$ !
$ ! If you do not want to include the HDF Fortran interfaces, comment
$ !   out the following command.
$ !
$ @makefs
$ !
$ ! Move the files to their correct destinations
$ !
$ copy df.olb 'lib_directory'
$ copy *.h    'include_directory'
$ copy dffunc.* 'include_directory'
$ delete *.obj;*
$ !
$ ! Build the HDF utility programs
$ !
$ hdfutil_directory = hdftop_dir + ".HDF.UTIL]"
$ set default 'hdfutil_directory'
$ @makeutil
$ !
$ ! Move the HDF utilities to where they belong and define symbols
$ !
$ copy *.exe 'bin_directory'
$ define hdf$bin 'bin_directory'
$ !
$ @setuputils
$ !
$ ! Build the test programs
$ !
$ hdftest_directory = hdftop_dir + ".HDF.TEST]"
$ set default 'hdftest_directory'
$ @make
$ !
$ delete *.obj;*
$ write sys$output "Testing..."
$ create/dir [.testdir]
$ !
$ ! Run testhdf
$ !
$ run testhdf
$ !
$ ! Cleanup
$ !
$ copy tvattr.hdf tvattr.dat
$ delete [...]*.hdf;*
$ !
$ ! Run fortest 
$ !
$ run fortest
$ !
$ write sys$output " Done with DF library"
$ !
$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ ! Make full MFHDF library.
$ !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
$ !
$ ! Build XDR
$ !
$ xdr_directory = hdftop_dir + ".MFHDF.XDR]"
$ set def 'xdr_directory'
$ write sys$output "Building XDR..."
$ @make  
$ write sys$output "Testing XDR..."
$ run xdrtest
$ write sys$output "Done"
$ !
$ ! Build MFHDF
$ !
$ mfhdflibsrc_directory = hdftop_dir + ".MFHDF.LIBSRC]"
$ set def 'mfhdflibsrc_directory'
$ write sys$output "Building and testing MFHDF Library..."
$ @make
$ copy *.h    'include_directory'
$ write sys$output "Done"
$ !
$ ! Testing netCDF library
$ ! 
$ nctest_directory = hdftop_dir + ".MFHDF.NCTEST]"
$ set def 'nctest_directory'
$ write sys$output "Testing netCDF..."
$ @make
$ write sys$output "Done"
$ !
$ ! Adding Fortran interfaces to the MFHDF Library
$ !
$ fortran_directory = hdftop_dir + ".MFHDF.FORTRAN.VMS]"
$ set def 'fortran_directory'
$ write sys$output "Adding Fortran routines to MFHDF Library"
$ write sys$output " and running the tests..."
$ @make
$ copy netcdf.inc 'include_directory'
$ write sys$output "Done"
$ !
$ ! Build NCDUMP utility
$ !
$ ncdump_directory = hdftop_dir + ".MFHDF.NCDUMP]"
$ set def 'ncdump_directory'
$ @make
$ write sys$output "NCDUMP has been built"
$ copy ncdump.exe 'bin_directory'
$ !
$ ! Build NCGEN
$ !
$ ncgen_directory = hdftop_dir + ".MFHDF.NCGEN]"
$ set def 'ncgen_directory'
$ @make
$ write sys$output "NCGEN has been built"
$ copy ncgen.exe 'bin_directory'
$ write sys$output "Testing..."
$ @test
$ write sys$output "Done"
$ !
$ ! Build and test HDP
$ !
$ hdp_directory = hdftop_dir + ".MFHDF.DUMPER]"
$ set def 'hdp_directory'
$ write sys$output "Building HDP..."
$ @make
$ copy hdp.exe 'bin_directory'
$ write sys$output "Testing HDP..."
$ @test
$ set def 'hdftop'
$ write sys$output "........That's All, Folks!........." 
$ exit
