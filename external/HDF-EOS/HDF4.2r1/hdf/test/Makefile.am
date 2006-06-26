#############################################################################
##                      Setup for building programs                        ##
#############################################################################

AM_CPPFLAGS=-I$(top_srcdir)/hdf/src

#############################################################################
##                              Testing                                    ##
#############################################################################

if HDF_BUILD_FORTRAN
TESTS = testhdf fortest
check_PROGRAMS = testhdf fortest fortestF
else
TESTS = testhdf
check_PROGRAMS = testhdf
endif

testhdf_SOURCES = an.c anfile.c bitio.c blocks.c buffer.c chunks.c comp.c   \
                  conv.c extelt.c file.c file1.c litend.c macros.c man.c    \
                  mgr.c nbit.c rig.c sdmms.c sdnmms.c sdstr.c slab.c tbv.c  \
                  tdupimgs.c testhdf.c tree.c tszip.c tvattr.c tvset.c      \
		  tvsfpack.c vers.c
testhdf_LDADD = ../src/libdf.a
testhdf_DEPENDENCIES = testdir ../src/libdf.a

if HDF_BUILD_FORTRAN
fortest_SOURCES = fortest.c
fortest_LDADD = ../src/libdf.a
fortest_DEPENDENCIES = testdir fortestF ../src/libdf.a

fortestF_SOURCES = forsupff.f forsupf.c fortestF.f manf.f mgrf.f slabwf.f   \
                   t24f.f tanf.f tanfilef.f tpf.f tr8f.f tsdmmsf.f          \
                   tsdnmmsf.f tsdnntf.f tsdntf.f tsdstrf.f tstubsf.f        \
                   tvattrf.f tvsetf.f
fortestF_LDADD = ../src/libdf.a
fortestF_DEPENDENCIES = testdir ../src/libdf.a
endif

testdir:
	-mkdir testdir
#############################################################################
##                          And the cleanup                                ##
#############################################################################

DISTCLEANFILES = fortest.arg Fortran_err.dat testdir/t5.hdf
