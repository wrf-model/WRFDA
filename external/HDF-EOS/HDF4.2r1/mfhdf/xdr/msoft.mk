#   Copyright 1989, University Corporation for Atmospheric Research
#
#	PC Makefile for eXternal Data Representation library routines
#		used by the netCDF.
#
#  NOTE:  Set the definition of the OS2 macro to match OS as follows:
#         OS2 = 0     -> DOS
#         OS2 = 1     -> OS/2

!INCLUDE ..\macros.mk

AR       = LIB
ARFLAGS  =

LINK     = link
LFLAGS   = /nod

INCDIR   = .
INCLUDES = /I$(INCDIR) /I\hdf\hdf\include

LIBDIR   = $(DESTDIR)\lib

XDRLIB   = xdr.lib
!IF $(OS2)
OS2LIB    = os2.lib
!ELSE
OS2LIB    =
!ENDIF
LIBS     = $(XDRLIB) llibe7.lib $(OS2LIB)

XDROBJS  = xdr.obj xdrarray.obj xdrfloat.obj xdrstdio.obj byteordr.obj
XDRLOBJS = -+xdr -+xdrarray -+xdrfloat -+xdrstdio -+byteordr

all:		$(XDRLIB)

$(XDRLIB): $(XDROBJS)
	$(AR) $@ $(ARFLAGS) $(XDRLOBJS),LIB.LST;

test:		xdrtest.exe FORCE
	xdrtest

FORCE:

xdrtest.exe: xdrtest.obj $(XDROBJS)
	$(LINK) $(LFLAGS) xdrtest.obj,,,$(LIBS);

install:
	copy $(XDRLIB) $(LIBDIR)

clean:
	rm -f *.obj *.map *.lst *.bak xdr.lib xdrtest.exe test.xdr

xdr.obj: xdr.c types.h xdr.h
	$(CC) $(CFLAGS) $(INCLUDES) xdr.c
xdrfloat.obj: xdrfloat.c types.h xdr.h
	$(CC) $(CFLAGS) $(INCLUDES) xdrfloat.c
xdrstdio.obj: xdrstdio.c types.h xdr.h
	$(CC) $(CFLAGS) $(INCLUDES) xdrstdio.c
xdrarray.obj: xdrarray.c types.h xdr.h
	$(CC) $(CFLAGS) $(INCLUDES) xdrarray.c
xdrtest.obj: xdrtest.c types.h xdr.h
	$(CC) $(CFLAGS) $(INCLUDES) xdrtest.c
byteordr.obj: byteordr.c
	$(CC) $(CFLAGS) byteordr.c
