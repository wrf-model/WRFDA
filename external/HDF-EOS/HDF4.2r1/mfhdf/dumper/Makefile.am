#############################################################################
##                      Setup for building programs                        ##
#############################################################################

INCLUDES=-I$(top_srcdir)/hdf/src        \
         -I$(top_srcdir)/mfhdf/libsrc   \
         -I$(top_srcdir)/mfhdf/port     \
         -I../libsrc
DEFINES=-DNDEBUG -DHDF
AM_CPPFLAGS=$(INCLUDES) $(DEFINES)

#############################################################################
##                          Programs to build                              ##
#############################################################################

bin_PROGRAMS = hdp

## Information for building the "hdp" program
hdp_SOURCES = hdp.c hdp_dump.c hdp_gr.c hdp_list.c hdp_rig.c hdp_sds.c      \
              hdp_util.c hdp_vd.c hdp_vg.c show.c
hdp_LDADD = ../libsrc/libmfhdf.a $(top_builddir)/hdf/src/libdf.a @LIBS@
hdp_DEPENDENCIES = $(top_builddir)/hdf/src/libdf.a ../libsrc/libmfhdf.a

#############################################################################
##                   Testing -- Here there be dragons.                     ##
#############################################################################

check_SCRIPTS = testhdp.sh

check:
	@$(SHELL) ./testhdp.sh
