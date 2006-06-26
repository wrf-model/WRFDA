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

bin_PROGRAMS = ncdump
man1_MANS = ncdump.1

## Information for building the "ncdump" program
ncdump_SOURCES = dumplib.c ncdump.c vardata.c
ncdump_LDADD = ../libsrc/libmfhdf.a $(top_builddir)/hdf/src/libdf.a
ncdump_DEPENDENCIES = $(top_builddir)/hdf/src/libdf.a ../libsrc/libmfhdf.a

check: ncdump $(srcdir)/test0.cdl
	../ncgen/ncgen -o test0.nc -n $(srcdir)/test0.cdl
	./ncdump test0.nc > test1.cdl
	../ncgen/ncgen -o test1.nc -n test1.cdl
	./ncdump -n test0 test1.nc > test2.cdl
	@cmp test1.cdl test2.cdl && echo "*** ncdump test successful ***" ;     \
	    rm -f test1.nc test1.cdl test2.cdl

#############################################################################
##                          And the cleanup                                ##
#############################################################################

DISTCLEANFILES=test0.nc
