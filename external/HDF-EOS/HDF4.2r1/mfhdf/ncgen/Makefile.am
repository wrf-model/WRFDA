#############################################################################
##                      Setup for building programs                        ##
#############################################################################

## Setup the different includes and preprocessor #defines we need.
INCLUDES=-I$(top_srcdir)/hdf/src        \
         -I$(top_srcdir)/mfhdf/libsrc   \
         -I$(top_srcdir)/mfhdf/port     \
         -I../libsrc
DEFINES=-DNDEBUG -DHDF
AM_CPPFLAGS=$(INCLUDES) $(DEFINES)

#############################################################################
##                          Programs to build                              ##
#############################################################################

bin_PROGRAMS = ncgen

if HDF_BUILD_FORTRAN
noinst_PROGRAMS = ctest0 ftest0
else 
noinst_PROGRAMS = ctest0
endif

## Information for building the "ncgen" program
ncgen_SOURCES = close.c escapes.c generate.c genlib.c getfill.c init.c      \
                load.c main.c ncgentab.c
ncgen_LDADD = ../libsrc/libmfhdf.a $(top_builddir)/hdf/src/libdf.a
ncgen_DEPENDENCIES = $(top_builddir)/hdf/src/libdf.a ../libsrc/libmfhdf.a

## Recipe for building the ncgentab.c file
ncgentab.c ncgentab.h: ncgen.h ncgen.y ncgenyy.c
	$(YACC) -d $(srcdir)/ncgen.y
	mv y.tab.c ncgentab.c
	mv y.tab.h ncgentab.h

ncgenyy.c: ncgen.l
	$(LEX) $(srcdir)/ncgen.l
	mv lex.yy.c ncgenyy.c

#############################################################################
##                            Documentation                                ##
#############################################################################

man1_MANS = ncgen.1

#############################################################################
##                   Testing -- Here there be dragons.                     ##
#############################################################################

NCDUMP=../ncdump/ncdump

if HDF_BUILD_FORTRAN
check: ncgen $(srcdir)/test0.cdl b-check c-check f-check
else
check: ncgen $(srcdir)/test0.cdl b-check c-check
endif

## Test the "-b" option of ncgen
b-check:	ncgen $(srcdir)/test0.cdl test1.cdl
	./ncgen -b test1.cdl
	$(NCDUMP) test1.nc > test2.cdl
	@if $(DIFF) test1.cdl test2.cdl; then                               \
	  echo "*** ncgen -b test successful ***";                          \
	else                                                                \
	  echo "*** ncgen -b test failed ***";                              \
	fi

## Test the "-c" option of ncgen
c-check:	test2.cdl ctest0
	./ctest0        # tests `-c' option, creates ctest0.nc
	$(NCDUMP) -n test1 ctest0.nc > ctest1.cdl
	@if $(DIFF) test1.cdl ctest1.cdl; then                              \
	  echo "*** ncgen -c test successful ***";                          \
	else                                                                \
	  echo "*** ncgen -c test failed  ***";                             \
	fi

if HDF_BUILD_FORTRAN
## Test the "-f" option of ncgen
f-check:	test1.cdl ftest0
	./ftest0
	$(NCDUMP) -n test1 ftest0.nc > ftest1.cdl
	@if $(DIFF) test1.cdl ftest1.cdl; then                              \
	  echo "*** ncgen -f test successful ***";                          \
	else                                                                \
	  echo "*** ncgen -f test failed (but roundoff differences are OK) ***"; \
	fi

ftest0:		ncgen test0.cdl netcdf.inc
	./ncgen -f -o ftest0.nc $(srcdir)/test0.cdl > test0.f
	$(F77) $(FFLAGS) -o $@ test0.f $(LDFLAGS) $(ncgen_LDADD) $(LIBS)
endif

netcdf.inc:
	ln -s ../fortran/$@ .

test1.cdl:	test0.nc
	$(NCDUMP) -n test1 test0.nc > $@

test0.nc:	ncgen $(srcdir)/test0.cdl
	./ncgen -b -o test0.nc $(srcdir)/test0.cdl

ctest0:		ncgen $(srcdir)/test0.cdl
	./ncgen -c -o ctest0.nc $(srcdir)/test0.cdl > test0.c
	$(COMPILE) -o $@ test0.c $(LDFLAGS) $(ncgen_LDADD) $(LIBS)

#############################################################################
##                            Miscellaneous                                ##
#############################################################################

## This was in the original Makefile.
vms-stuff:	ncgentab.h ncgentab.c ncgenyy.c
	cp $(srcdir)/ncgentab.h vmstab.h
	cp $(srcdir)/ncgentab.c vmstab.c
	cp $(srcdir)/ncgenyy.c vms_yy.c

#############################################################################
##                          And the cleanup                                ##
#############################################################################

DISTCLEANFILES = ctest0 ctest0.nc ctest1.cdl ftest0 ftest0.nc ftest1.cdl    \
                 ncgentab.c ncgentab.h ncgenyy.c netcdf.inc test0.c test0.f \
                 test0.nc test1.cdl test1.nc test2.cdl
