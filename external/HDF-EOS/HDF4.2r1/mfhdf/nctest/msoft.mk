#  Copyright 1993, UCAR/Unidata
#
#  DOS and OS/2 Makefile for netcdf exhaustive test on PS/2
#
#  NOTE:  Set the definition of the OS2 macro to match OS as follows:
#         OS2 = 0     -> DOS
#         OS2 = 1     -> OS/2

!INCLUDE ..\macros.mk

AR       = LIB
ARFLAGS  =

LINK      = link
LFLAGS    = /st:10000 /SEG:256 /nod

INCDIR    =  ..\libsrc
HDFINCDIR =  \hdf\hdf\include
INCLUDES  = /I$(INCDIR) /I$(HDFINCDIR)

BINDIR    = $(DESTDIR)\bin
LIBDIR    = $(DESTDIR)\lib
NCTESTLIB = nctest.lib
NETCDFLIB = ..\libsrc\netcdf.lib
CLIB      = llibc7.lib oldnames.lib
!IF $(OS2)
OS2LIB    = os2.lib
!ELSE
OS2LIB    =
!ENDIF
XDRLIB    = ..\xdr\xdr.lib
HDFLIB    = \hdf\hdf\lib\df.lib
LIBS      = $(NCTESTLIB) $(NETCDFLIB) $(XDRLIB) $(OS2LIB) $(HDFLIB) $(CLIB)

.c.obj:
	$(CC) $(CFLAGS) $(CPPFLAGS) $(INCLUDES) $<

GOAL = nctest.exe

SRCS =  varget.c vargetg.c varput.c varputg.c vardef.c vartests.c \
	vputget.c vputgetg.c driver.c cdftests.c dimtests.c rec.c \
	atttests.c misctest.c add.c error.c emalloc.c val.c slabs.c

MAIN =  driver.obj

OBJS =  varget.obj vargetg.obj varput.obj varputg.obj vardef.obj vartests.obj \
	vputget.obj vputgetg.obj cdftests.obj dimtests.obj rec.obj \
	atttests.obj misctest.obj add.obj error.obj emalloc.obj val.obj \
	slabs.obj

LOBJS1 =  -+varget.obj -+vargetg.obj -+varput.obj -+varputg.obj -+vardef.obj \
          -+vartests.obj -+vputget.obj
LOBJS2 =  -+vputgetg.obj -+cdftests.obj -+dimtests.obj -+rec.obj \
	  -+atttests.obj -+misctest.obj
LOBJS3 =  -+add.obj -+error.obj -+emalloc.obj -+val.obj -+slabs.obj

all:	$(GOAL)

test:	$(GOAL) FORCE
	$(GOAL)

FORCE:

$(GOAL): $(MAIN) $(NCTESTLIB) $(NETCDFLIB) $(XDRLIB)
    $(LINK) $(LFLAGS) @nctest.lnk
#    $(LINK) $(LFLAGS) $(MAIN)+(vartests+cdftests+dimtests+atttests+misctest),$(GOAL),,@nctest.lnk;
#    $(LINK) $(LFLAGS) $(MAIN),$(GOAL),,@nctest.lnk;

$(NCTESTLIB):	$(OBJS)
	$(AR) $@ $(ARFLAGS) $(LOBJS1),LIB.LST;
	$(AR) $@ $(ARFLAGS) $(LOBJS2),LIB.LST;
	$(AR) $@ $(ARFLAGS) $(LOBJS3),LIB.LST;

install:

clean:
	rm -f *.obj *.lst *.map *.bak nctest.lib nctest.exe *.cdf

# DO NOT DELETE THIS LINE -- make depend depends on it.

add.obj: add.c
add.obj: ../libsrc/netcdf.h
add.obj: ./testcdf.h
add.obj: ./add.h
add.obj: ./emalloc.h
atttests.obj: atttests.c
atttests.obj: ../libsrc/netcdf.h
atttests.obj: ./testcdf.h
atttests.obj: ./add.h
atttests.obj: ./error.h
atttests.obj: ./emalloc.h
atttests.obj: ./tests.h
atttests.obj: ./val.h
bug.obj: bug.c
bug.obj: ../libsrc/netcdf.h
cdftests.obj: cdftests.c
cdftests.obj: ../libsrc/netcdf.h
cdftests.obj: ./testcdf.h
cdftests.obj: ./add.h
cdftests.obj: ./error.h
cdftests.obj: ./tests.h
dimtests.obj: dimtests.c
dimtests.obj: ../libsrc/netcdf.h
dimtests.obj: ./testcdf.h
dimtests.obj: ./add.h
dimtests.obj: ./error.h
dimtests.obj: ./tests.h
driver.obj: driver.c
driver.obj: ../libsrc/netcdf.h
driver.obj: ./tests.h
emalloc.obj: emalloc.c
emalloc.obj: ./error.h
emalloc.obj: ./emalloc.h
error.obj: error.c
error.obj: ../libsrc/netcdf.h
error.obj: ./error.h
fixed1.obj: fixed1.c
fixed1.obj: ../libsrc/netcdf.h
fixed2.obj: fixed2.c
fixed2.obj: ../libsrc/netcdf.h
misctest.obj: misctest.c
misctest.obj: ../libsrc/netcdf.h
misctest.obj: ./testcdf.h
misctest.obj: ./add.h
misctest.obj: ./error.h
nctime.obj: nctime.c
nctime.obj: ../libsrc/netcdf.h
nctime0.obj: nctime0.c
nctime0.obj: ../libsrc/netcdf.h
rec.obj: rec.c
rec.obj: ../libsrc/netcdf.h
rec.obj: ./testcdf.h
rec.obj: ./val.h
rec.obj: ./error.h
rec.obj: ./tests.h
slabs.obj: slabs.c
slabs.obj: ../libsrc/netcdf.h
slabs.obj: ./testcdf.h
slabs.obj: ./add.h
slabs.obj: ./error.h
slabs.obj: ./tests.h
val.obj: val.c
val.obj: ../libsrc/netcdf.h
val.obj: ./testcdf.h
val.obj: ./val.h
val.obj: ./error.h
vardef.obj: vardef.c
vardef.obj: ../libsrc/netcdf.h
vardef.obj: ./testcdf.h
vardef.obj: ./add.h
vardef.obj: ./error.h
vardef.obj: ./tests.h
varget.obj: varget.c
varget.obj: ../libsrc/netcdf.h
varget.obj: ./testcdf.h
varget.obj: ./error.h
varget.obj: ./tests.h
vargetg.obj: vargetg.c
vargetg.obj: ../libsrc/netcdf.h
vargetg.obj: ./testcdf.h
vargetg.obj: ./error.h
vargetg.obj: ./tests.h
varput.obj: varput.c
varput.obj: ../libsrc/netcdf.h
varput.obj: ./testcdf.h
varput.obj: ./val.h
varput.obj: ./error.h
varput.obj: ./tests.h
varputg.obj: varputg.c
varputg.obj: ../libsrc/netcdf.h
varputg.obj: ./testcdf.h
varputg.obj: ./val.h
varputg.obj: ./error.h
varputg.obj: ./tests.h
vartests.obj: vartests.c
vartests.obj: ../libsrc/netcdf.h
vartests.obj: ./testcdf.h
vartests.obj: ./add.h
vartests.obj: ./error.h
vartests.obj: ./tests.h
vputget.obj: vputget.c
vputget.obj: ../libsrc/netcdf.h
vputget.obj: ./testcdf.h
vputget.obj: ./add.h
vputget.obj: ./val.h
vputget.obj: ./error.h
vputget.obj: ./tests.h
vputget.obj: ./emalloc.h
vputgetg.obj: vputgetg.c
vputgetg.obj: ../libsrc/netcdf.h
vputgetg.obj: ./testcdf.h
vputgetg.obj: ./add.h
vputgetg.obj: ./val.h
vputgetg.obj: ./error.h
vputgetg.obj: ./tests.h
vputgetg.obj: ./emalloc.h
