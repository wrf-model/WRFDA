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

bin_PROGRAMS = hdfimport

hdfimport_SOURCES = hdfimport.c
hdfimport_LDADD = ../libsrc/libmfhdf.a $(top_builddir)/hdf/src/libdf.a @LIBS@
hdfimport_DEPENDENCIES = $(top_builddir)/hdf/src/libdf.a ../libsrc/libmfhdf.a

#############################################################################
##                   Testing -- Here there be dragons.                     ##
#############################################################################

check_PROGRAMS = hdfimporttest
check_SCRIPTS = testutil.sh

hdfimporttest_SOURCES = hdfimporttest.c
hdfimporttest_LDADD = ../libsrc/libmfhdf.a $(top_builddir)/hdf/src/libdf.a @LIBS@
hdfimporttest_DEPENDENCIES = $(top_builddir)/hdf/src/libdf.a ../libsrc/libmfhdf.a

check: hdfimporttest
	@$(SHELL) ./testutil.sh
