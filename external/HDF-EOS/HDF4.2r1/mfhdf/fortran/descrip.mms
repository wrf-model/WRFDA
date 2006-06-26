! $Id: descrip.mms,v 1.2 1993/12/01 04:13:50 sxu Exp $
!
!          MMS file for netcdf Fortran API
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
COPTS		= /opt/nodebug/define=HDF

!==============================================================================
INCDIR		= $(INSTROOT)[include]
LIBDIR		= $(INSTROOT)[lib]
LIBRARY		= [---.lib]netcdf.olb
HDFLIB          = [--.hdf.lib]df.olb
LINKFLAGS	= /nodebug/exec=$(mms$target_name).exe
!LINKFLAGS	= /debug/exec=$(mms$target_name).exe
SRCINCDIR	= [-.libsrc]
INCLUDES	= /include=([-.xdr],$(SRCINCDIR),[--.hdf.include])
CFLAGS		= $(COPTS)$(INCLUDES)/obj=$(mms$target_name).obj
LIBOBJS 	= array=array.obj, attr=attr.obj, cdf=cdf.obj, dim=dim.obj,-
		    file=file.obj, iarray=iarray.obj, error=error.obj,-
		    putget=putget.obj, sharray=sharray.obj, string=string.obj,-
		    var=var.obj,-
		    htons.obj, ntohs.obj
CLIB 		=  sys$library:vaxcrtl/lib
LINKLIBS 	=  $(LIBRARY)/lib, $(HDFLIB)/lib, $(CLIB)

all :		netcdf.inc, library, test
	@ continue

netcdf.inc :	[.vms]netcdf.inc
	copy [.vms]$@ $@

library :	$(LIBRARY)(jackets.obj)
	@ continue

jackets.obj :	[.vms]jackets.c

mfsdf.obj :	[.vms]mfsdf.c

mfsdff.obj :     [.vms]mfsdff.for

test :		ftest.exe  hdftest.exe
	run hdftest
	run ftest
	@ if $severity .eq. 0 then exit 1

hdftest.exe :	hdftest.obj, jackets.obj, mfsdf.obj, mfsdff.obj 
	$(LINK)$(LINKFLAGS) hdftest.obj, mfsdf.obj, mfsdff.obj, -
               jackets.obj, $(LINKLIBS)

hdftest.obj :	netcdf.inc, [.vms]hdftest.for

ftest.exe :	ftest.obj, jackets.obj
	$(LINK)$(LINKFLAGS) ftest.obj,jackets.obj,$(LINKLIBS)

install :	netcdf.inc
	copy netcdf.inc $(INCDIR)

clean :
	purge
	- delete netcdf.inc.*,*.obj.*,*.lis.*,*.map.*,*.exe.*
ftest.obj :	netcdf.inc, [.vms]ftest.for
jackets.obj :	$(SRCINCDIR)netcdf.h
