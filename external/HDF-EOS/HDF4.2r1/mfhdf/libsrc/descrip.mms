! $Id: descrip.mms,v 1.2 1993/05/03 21:25:02 chouck Exp $
!
!          MMS file for netcdf
!
!==============================================================================
! ASSUMPTIONS:

!==============================================================================
! CUSTOMIZATIONS (change as necessary):

!Root of the installation:
INSTROOT	= LDMROOT:

!Operating system version:
OS		= vms_5_4

!Compilation options:
COPTS		= /opt/nodebug

!==============================================================================
INCDIR		= $(INSTROOT)[include]
LIBDIR		= $(INSTROOT)[lib]
LIBRARY		= [-]netcdf.olb
LINKFLAGS	= /nodebug/exec=$(mms$target_name).exe
!LINKFLAGS	= /debug/exec=$(mms$target_name).exe
CFLAGS		= $(COPTS)/include=[-.xdr]
LIBOBJS       = array=array.obj, attr=attr.obj, cdf=cdf.obj, dim=dim.obj,-
                  file=file.obj, iarray=iarray.obj, error=error.obj,-
                  putget=putget.obj, putgetg=putgetg.obj,-
                  sharray=sharray.obj, string=string.obj,-
                  var=var.obj,-
                  htons.obj, ntohs.obj
CLIB 		=  sys$library:vaxcrtl.olb/lib
LINKLIBS 	=  $(LIBRARY)/lib, $(CLIB)

all :		library, test
	@ continue

library :	$(LIBRARY)($(LIBOBJS))
	@ continue

test :		cdftest.exe
	run sys$disk:[]cdftest.exe

cdftest.exe :	cdftest.obj, $(LIBRARY)
	- $(LINK)$(LINKFLAGS) cdftest.obj, $(LINKLIBS)

install :	all
	copy netcdf.h $(INCDIR)

clean :
	purge
	- delete *.obj.*,*.lis.*,*.map.*,*.exe.*

cdftest.obj :	cdftest.c local_nc.h netcdf.h alloc.h

array.obj :	array.c local_nc.h netcdf.h alloc.h
attr.obj :	attr.c local_nc.h netcdf.h alloc.h
cdf.obj :	cdf.c local_nc.h netcdf.h alloc.h
dim.obj :	dim.c local_nc.h netcdf.h alloc.h
file.obj :	file.c local_nc.h netcdf.h alloc.h
iarray.obj :	iarray.c local_nc.h netcdf.h alloc.h
error.obj :	error.c alloc.h
putget.obj :	putget.c local_nc.h netcdf.h
putgetg.obj:    putgetg.c local_nc.h netcdf.h
sharray.obj :	sharray.c local_nc.h netcdf.h alloc.h
string.obj :	string.c local_nc.h netcdf.h alloc.h
var.obj :	var.c local_nc.h netcdf.h alloc.h
htons.obj :	htons.mar
ntohs.obj :	ntohs.mar
