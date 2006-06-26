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

TESTS = nctest
check_PROGRAMS = nctest

# Information for building the "ncgen" program
nctest_SOURCES = add.c atttests.c cdftests.c dimtests.c driver.c emalloc.c  \
                 error.c misctest.c rec.c slabs.c val.c varget_unlim.c      \
                 varget.c vargetg.c varput.c varputg.c vardef.c vartests.c  \
                 vputget.c vputgetg.c
nctest_LDADD = ../libsrc/libmfhdf.a $(top_builddir)/hdf/src/libdf.a
nctest_DEPENDENCIES = $(top_builddir)/hdf/src/libdf.a ../libsrc/libmfhdf.a

#############################################################################
##                          And the cleanup                                ##
#############################################################################

DISTCLEANFILES=test2.nc test.nc
