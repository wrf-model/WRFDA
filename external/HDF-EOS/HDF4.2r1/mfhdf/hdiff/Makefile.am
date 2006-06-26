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

bin_PROGRAMS = hdiff

## Information for building the "hdiff" program
hdiff_SOURCES = hdiff.c hdiff_array.c hdiff_gr.c hdiff_list.c hdiff_main.c  \
                hdiff_mattbl.c hdiff_gattr.c hdiff_misc.c hdiff_sds.c       \
                hdiff_table.c hdiff_vs.c
hdiff_LDADD = ../libsrc/libmfhdf.a $(top_builddir)/hdf/src/libdf.a @LIBS@
hdiff_DEPENDENCIES = $(top_builddir)/hdf/src/libdf.a ../libsrc/libmfhdf.a

#############################################################################
##                              Testing                                    ##
#############################################################################

check_SCRIPTS = testhdiff.sh

noinst_PROGRAMS = hdifftst

hdifftst_SOURCES = hdifftst.c
hdifftst_LDADD = ../libsrc/libmfhdf.a $(top_builddir)/hdf/src/libdf.a @LIBS@
hdifftst_DEPENDENCIES = $(top_builddir)/hdf/src/libdf.a ../libsrc/libmfhdf.a

check: hdifftst
	@srcdir="$(srcdir)" $(SHELL) $(srcdir)/testhdiff.sh
 
