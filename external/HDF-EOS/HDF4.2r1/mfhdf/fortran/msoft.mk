#  Copyright 1993, UCAR/Unidata
#
#  Makefile for netCDF exhaustive test on PS/2
#
#  NOTE:  Set the definition of the OS2 macro to match OS as follows:
#         OS2 = 0     -> DOS
#         OS2 = 1     -> OS/2

!INCLUDE ..\macros.mk

LINK	  = link
LFLAGS	  = /st:10000 /nod /noe

INCDIR    = ..\libsrc
INCLUDES  = /I$(INCDIR)

BINDIR    = $(DESTDIR)\bin
LIBDIR    = $(DESTDIR)\lib
CLIB      = llibce.lib
FORTLIB   = llibfor7.lib
NETCDFLIB = ..\libsrc\netcdf.lib
!IF $(OS2)
OS2LIB    = os2.lib
!ELSE
OS2LIB    =
!ENDIF
XDRLIB    = ..\xdr\xdr.lib
LIBS      = $(XDRLIB) $(NETCDFLIB) $(OS2LIB) $(CLIB) $(FORTLIB)

.c.obj:
	$(CC) $(CFLAGS) $(CPPFLAGS) $(INCLUDES) $<

.for.obj:
	$(F77) $(FFLAGS) $(FPPFLAGS) $<

LOADLIBS =

GOAL =  ftest.exe

SRCS =  ftest.for jackets.c
OBJS =  ftest.obj jackets.obj fslen.obj

all:	$(GOAL)

test:	$(GOAL) FORCE
	$(GOAL)

$(GOAL): $(OBJS)
	$(LINK) $(LFLAGS) $(OBJS) , $(GOAL) ,, @ftest.lnk;

ftest.obj: ftest.for msoft.int netcdf.inc
	$(F77) $(FFLAGS) $(FPPFLAGS) $?

fslen.obj: fslen.asm

ftest.for: msoft\ftest.for
	copy msoft\ftest.for ftest.for

jackets.c: msoft\jackets.c
	copy msoft\jackets.c jackets.c

netcdf.inc: msoft\netcdf.inc
	copy msoft\netcdf.inc netcdf.inc

msoft.int: msoft\msoft.int
	copy msoft\msoft.int msoft.int

fslen.asm: msoft\fslen.asm
	copy msoft\fslen.asm fslen.asm

install: $(GOAL)
	copy $(GOAL) $(BINDIR)

clean:
	rm -f ftest.obj jackets.obj fslen.obj ftest.map $(GOAL) *.cdf
	rm -f ftest.for jackets.c netcdf.inc msoft.int fslen.asm

FORCE:

# DO NOT DELETE THIS LINE -- make depend depends on it.

jackets.obj: jackets.c $(INCDIR)\netcdf.h
	$(CC) $(CFLAGS) $(CPPFLAGS) $(INCLUDES) jackets.c
