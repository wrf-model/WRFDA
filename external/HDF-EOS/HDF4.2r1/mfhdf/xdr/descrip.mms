! $Id: descrip.mms,v 1.2 1993/12/01 04:05:19 sxu Exp $
!
!          Build/test an xdr library on vms
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
COPTS		= 

!==============================================================================
INCDIR		= $(INSTROOT)[include]
LIBDIR		= $(INSTROOT)[lib]
LIBOBJS		= xdr.obj, xdrfloat.obj, xdrstdio.obj, htonl.obj, ntohl.obj
LIBRARY		= [---.lib]netcdf.olb
LINKFLAGS	= /nodebug/exec=$(mms$target_name).exe
!LINKFLAGS	= /debug/exec=$(mms$target_name).exe
CFLAGS		= $(COPTS)/include=sys$disk:[]
SYSLIBS		= sys$library:vaxcrtl/lib

all :		library, xdrtest.exe
	continue

install :	all
	continue

library :	$(LIBRARY)($(LIBOBJS))
	continue

xdrtest.exe :	xdrtest.obj, xdrarray.obj, $(LIBRARY)
	$(LINK) $(LINKFLAGS) xdrtest.obj,xdrarray.obj,$(LIBRARY)/lib,$(SYSLIBS)

rtest.exe :  rtest.obj, xdrarray.obj, $(LIBRARY)
	$(LINK) $(LINKFLAGS) rtest.obj,xdrarray.obj,$(LIBRARY)/lib,$(SYSLIBS)

rtest.obj : xdrtest.c xdr.h types.h
	$(CC) $(CFLAGS) /undefine="CREATE" xdrtest.c /OBJECT= $@

clean :
	purge
	- delete *.obj.*,*.lis.*,*.map.*,*.exe.*

xdr.obj : xdr.c xdr.h types.h
xdrfloat.obj : xdrfloat.c xdr.h types.h
xdrstdio.obj : xdrstdio.c xdr.h types.h
xdrarray.obj : xdrarray.c xdr.h types.h
htonl.obj : htonl.mar
ntohl.obj : ntohl.mar
xdrtest.obj : xdrtest.c xdr.h types.h
