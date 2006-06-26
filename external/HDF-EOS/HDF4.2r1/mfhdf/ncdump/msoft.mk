#   Copyright 1989, University Corporation for Atmospheric Research
#
#  DOS and OS/2 Makefile for ncdump
#
#  NOTE:  Set the definition of the OS2 macro to match OS as follows:
#         OS2 = 0     -> DOS
#         OS2 = 1     -> OS/2

!INCLUDE ..\macros.mk

AR        = lib
ARFLAGS   =

LINK      = link
LFLAGS    = /st:30000 /nod

INCDIR1   = ..\libsrc
INCDIR2   = ..\xdr
INCLUDES  = /I$(INCDIR1) /I$(INCDIR2)

BINDIR    = $(DESTDIR)\bin
INCDIR    = $(DESTDIR)\include
LIBDIR    = $(DESTDIR)\lib
NCDUMPLIB = ncdump.lib
NETCDFLIB = ..\libsrc\netcdf.lib
CLIB      = llibc7.lib oldnames.lib
!IF $(OS2)
OS2LIB    = os2.lib
!ELSE
OS2LIB    = 
!ENDIF
XDRLIB    = ..\xdr\xdr.lib
HDFLIB    = \hdf\hdf\lib\df.lib
LIBS      = $(NCDUMPLIB) $(NETCDFLIB) $(XDRLIB) $(OS2LIB) $(HDFLIB) $(CLIB)

.c.obj:
	$(CC) $(CFLAGS) $(CPPFLAGS) $(INCLUDES) $<

LOADLIBS =

GOAL =  ncdump.exe

SRCS =  ncdump.c vardata.c dumplib.c

MAIN = ncdump.obj

OBJS =  vardata.obj dumplib.obj getopt.obj
LOBJS = -+vardata.obj -+dumplib.obj -+getopt.obj

all:	$(GOAL)

$(GOAL): $(MAIN) $(NCDUMPLIB) $(NETCDFLIB) $(XDRLIB)
    $(LINK) $(LFLAGS) $(MAIN),$(GOAL),,@ncdump.lnk;

$(NCDUMPLIB): $(OBJS)
	$(AR) $@ $(ARFLAGS) $(LOBJS),LIB.LST;

getopt.obj: ..\util\getopt.c
	$(CC) $(CFLAGS) $(CPPFLAGS) $(INCLUDES) $?

install: $(GOAL)
	copy $(GOAL) $(BINDIR)
	del $(GOAL)

test:	ncdump.exe test0.cdl FORCE
	..\ncgen\ncgen -o test0.cdf -n test0.cdl
	ncdump test0.cdf > test1.cdl
	..\ncgen\ncgen -o test1.cdf -n test1.cdl
	ncdump -n test0 test1.cdf > test2.cdl
	diff test1.cdl test2.cdl 
	@echo "Test successful."

ncdump: ncdump.exe

clean:
	rm -f *.obj *.map *.lst *.bak ncdump.lib $(GOAL) \
		test0.cdf test1.cdf test1.cdl test2.cdl

FORCE:

# DO NOT DELETE THIS LINE -- make depend depends on it.

ncdump.obj:  $(INCDIR1)\netcdf.h
vardata.obj: $(INCDIR1)\netcdf.h
