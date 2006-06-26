#   Copyright 1993, University Corporation for Atmospheric Research
#
#  DOS and OS/2 Makefile for netCDF library
#
#  NOTE:  Set the definition of the OS2 macro to match OS as follows:
#         OS2 = 0     -> DOS
#         OS2 = 1     -> OS/2

!INCLUDE ..\macros.mk

AR        = LIB
ARFLAGS   =

LINK      = link
#LFLAGS    = /nod /st:3000
#LFLAGS    = /cod /nod /st:8000
LFLAGS=/INFO /COD /ST:8192 /MAP:FULL /SEG:256

LIBDIR    = $(DESTDIR)\lib

HDFDIR    = \hdf\hdf

INCDIR    = ..\xdr
INCLUDES  = /I$(INCDIR) /I$(HDFDIR)\include

DEFINES  = /DSWAP /DNO_SYS_XDR_INC /DDOS_FS

NETCDFLIB = netcdf.lib
CLIBS     = llibc7.lib oldnames.lib
!IF $(OS2)
OS2LIB    = os2.lib
!ELSE
OS2LIB    =
!ENDIF
XDRLIB    = ..\xdr\xdr.lib
HDFLIB    = \hdf\hdf\lib\df.lib
LIBS      = $(NETCDFLIB) $(XDRLIB) $(OS2LIB) $(CLIBS) $(HDFLIB)

MANIFEST = Makefile alloc.h array.c attr.c cdf.c \
	cdftest.c descrip.mms dim.c error.c error.h file.c \
	htons.mar iarray.c local_nc.h netcdf.h ntohs.mar \
    putget.c sharray.c string.c test.cdf.sav var.c hdfsds.c \
    mfsd.c globdef.c

CSRCS = array.c attr.c cdf.c dim.c error.c file.c globdef.c iarray.c \
    putget.c putgetg.c sharray.c string.c var.c hdfsds.c mfsd.c xdrposix.c

COBJS = array.obj attr.obj cdf.obj dim.obj error.obj file.obj globdef.obj \
	iarray.obj putget.obj putgetg.obj sharray.obj string.obj var.obj \
    hdfsds.obj mfsd.obj xdrposix.obj

LOBJS1 = -+array -+attr -+cdf -+dim -+error -+file -+globdef -+iarray
LOBJS2 = -+putget -+putgetg -+sharray -+string -+var -+xdrposix
LOBJS3 = -+hdfsds -+mfsd

.c.obj:
        $(CC) $(CFLAGS) $(INCLUDES) $(DEFINES) $<

all:		$(NETCDFLIB)

$(NETCDFLIB): netcdf.h $(COBJS)
        $(AR) $@ $(ARFLAGS) $(LOBJS1),LIB.LST;
        $(AR) $@ $(ARFLAGS) $(LOBJS2),LIB.LST;
        $(AR) $@ $(ARFLAGS) $(LOBJS3),LIB.LST;

test:       cdftest.exe hdftest.exe FORCE
	cdftest
    hdftest

FORCE:

cdftest.exe: cdftest.obj $(NETCDFLIB)
	$(LINK) $(LFLAGS) @cdftest.lnk
#	$(LINK) $(LFLAGS) cdftest.obj,,,$(LIBS),cdftest;

hdftest.exe: hdftest.obj $(NETCDFLIB)
    $(LINK) $(LFLAGS) @hdftest.lnk
#    $(LINK) $(LFLAGS) hdftest.obj,,,$(LIBS),hdftest;

clean  :
	del *.obj *.map *.lst *.bak netcdf.lib *.exe test.cdf

install : $(NETCDFLIB)
	copy $(NETCDFLIB) $(LIBDIR)

array.obj: array.c
array.obj: ./local_nc.h
array.obj: ./netcdf.h
array.obj: ./alloc.h
attr.obj: attr.c
attr.obj: ./local_nc.h
attr.obj: ./netcdf.h
attr.obj: ./alloc.h
cdf.obj: cdf.c
cdf.obj: ./local_nc.h
cdf.obj: ./netcdf.h
cdf.obj: ./alloc.h
cdftest.obj: cdftest.c
cdftest.obj: ./netcdf.h
dim.obj: dim.c
dim.obj: ./local_nc.h
dim.obj: ./netcdf.h
dim.obj: ./alloc.h
error.obj: error.c
error.obj: ./local_nc.h
error.obj: ./netcdf.h
file.obj: file.c
file.obj: ./local_nc.h
file.obj: ./netcdf.h
file.obj: ./alloc.h
globdef.obj: globdef.c
globdef.obj: ./netcdf.h
iarray.obj: iarray.c
iarray.obj: ./local_nc.h
iarray.obj: ./netcdf.h
iarray.obj: ./alloc.h
mfsd.obj: mfsd.c
mfsd.obj: mfhdf.h
mfsd.obj: local_nc.h
putget.obj: putget.c
putget.obj: ./local_nc.h
putget.obj: ./netcdf.h
putget.obj: ./alloc.h
putgetg.obj: putgetg.c
putgetg.obj: ./local_nc.h
putgetg.obj: ./netcdf.h
sharray.obj: sharray.c
sharray.obj: ./local_nc.h
sharray.obj: ./netcdf.h
sharray.obj: ./alloc.h
string.obj: string.c
string.obj: ./local_nc.h
string.obj: ./netcdf.h
string.obj: ./alloc.h
var.obj: var.c
var.obj: ./local_nc.h
var.obj: ./netcdf.h
var.obj: ./alloc.h
xdrposix.obj: xdrposix.c
xdrposix.obj: ./netcdf.h
xdrposix.obj: ./local_nc.h
xdrposix.obj: ./netcdf.h

