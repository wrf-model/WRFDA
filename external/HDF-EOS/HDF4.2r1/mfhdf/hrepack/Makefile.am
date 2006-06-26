#############################################################################
##                      Setup for building programs                        ##
#############################################################################

## Setup the different includes and preprocessor #defines we need.
INCLUDES=-I$(top_srcdir)/hdf/src        \
         -I$(top_srcdir)/mfhdf/hdiff    \
         -I$(top_srcdir)/mfhdf/libsrc   \
         -I$(top_srcdir)/mfhdf/port     \
         -I../libsrc
DEFINES=-DNDEBUG -DHDF
AM_CPPFLAGS=$(INCLUDES) $(DEFINES)

#############################################################################
##                          Programs to build                              ##
#############################################################################

bin_PROGRAMS = hrepack

hrepack_SOURCES = hrepack.c hrepack_an.c hrepack_cmp.c hrepack_gr.c         \
                  hrepack_list.c hrepack_lsttable.c hrepack_main.c          \
                  hrepack_opttable.c hrepack_pal.c hrepack_parse.c          \
                  hrepack_sds.c hrepack_sdutil.c hrepack_utils.c            \
                  hrepack_vg.c hrepack_vs.c
hrepack_LDADD = ../libsrc/libmfhdf.a $(top_builddir)/hdf/src/libdf.a
hrepack_DEPENDENCIES = $(top_builddir)/hdf/src/libdf.a ../libsrc/libmfhdf.a

#############################################################################
##                   Testing -- Here there be dragons.                     ##
#############################################################################

check_PROGRAMS = hrepack_check test_hrepack
check_SCRIPTS = test_hrepack.sh

test_hrepack_SOURCES = hrepack.c hrepack_an.c hrepack_cmp.c hrepack_gr.c    \
                       hrepack_list.c hrepack_lsttable.c hrepack_opttable.c \
                       hrepack_pal.c hrepack_parse.c hrepack_sds.c          \
                       hrepack_sdutil.c hrepack_utils.c hrepack_vg.c        \
                       hrepack_vs.c test_hrepack_add.c test_hrepack_main.c  \
                       test_hrepack_verify.c ../hdiff/hdiff.c               \
                       ../hdiff/hdiff_array.c ../hdiff/hdiff_gattr.c        \
                       ../hdiff/hdiff_gr.c ../hdiff/hdiff_list.c            \
                       ../hdiff/hdiff_mattbl.c ../hdiff/hdiff_misc.c        \
                       ../hdiff/hdiff_sds.c ../hdiff/hdiff_table.c          \
                       ../hdiff/hdiff_vs.c
test_hrepack_LDADD = ../libsrc/libmfhdf.a $(top_builddir)/hdf/src/libdf.a
test_hrepack_DEPENDENCIES = $(top_builddir)/hdf/src/libdf.a ../libsrc/libmfhdf.a

hrepack_check_SOURCES = hrepack_check.c
hrepack_check_LDADD = ../libsrc/libmfhdf.a $(top_builddir)/hdf/src/libdf.a
hrepack_check_DEPENDENCIES = $(top_builddir)/hdf/src/libdf.a ../libsrc/libmfhdf.a

check: test_hrepack
	@srcdir="$(srcdir)" $(SHELL) $(srcdir)/test_hrepack.sh

#############################################################################
##                          And the cleanup                                ##
#############################################################################

DISTCLEANFILES = *.hdf
