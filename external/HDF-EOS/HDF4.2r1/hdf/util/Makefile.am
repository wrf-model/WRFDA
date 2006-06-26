#############################################################################
##                      Setup for building programs                        ##
#############################################################################

## Setup the different includes and preprocessor #defines we need.
AM_CPPFLAGS=-I$(top_srcdir)/hdf/src

#############################################################################
##                          Programs to build                              ##
#############################################################################

bin_PROGRAMS = gif2hdf hdf2gif hdf2jpeg hdf24to8 hdf8to24 hdfcomp hdfed     \
               hdfls hdfpack hdftopal hdftor8 hdfunpac jpeg2hdf paltohdf    \
               r8tohdf ristosds vmake vshow 

if HDF_BUILD_FORTRAN
bin_SCRIPTS = h4redeploy h4cc h4fc
else
bin_SCRIPTS = h4redeploy h4cc
endif

gif2hdf_SOURCES = gif2hdf.c gif2mem.c gifread.c decompress.c writehdf.c
gif2hdf_LDADD = ../src/libdf.a
gif2hdf_DEPENDENCIES = ../src/libdf.a

hdf2gif_SOURCES = hdf2gif.c hdfgifwr.c
hdf2gif_LDADD = ../src/libdf.a
hdf2gif_DEPENDENCIES = ../src/libdf.a

hdf2jpeg_SOURCES = hdf2jpeg.c
hdf2jpeg_LDADD = ../src/libdf.a
hdf2jpeg_DEPENDENCIES = ../src/libdf.a

hdf24to8_SOURCES = hdf24to8.c
hdf24to8_LDADD = ../src/libdf.a
hdf24to8_DEPENDENCIES = ../src/libdf.a

hdf8to24_SOURCES = hdf8to24.c
hdf8to24_LDADD = ../src/libdf.a
hdf8to24_DEPENDENCIES = ../src/libdf.a

hdfcomp_SOURCES = hdfcomp.c
hdfcomp_LDADD = ../src/libdf.a
hdfcomp_DEPENDENCIES = ../src/libdf.a

hdfed_SOURCES = he_cntrl.c he_disp.c he_file.c he_main.c
hdfed_LDADD = ../src/libdf.a
hdfed_DEPENDENCIES = ../src/libdf.a

hdfls_SOURCES = hdfls.c
hdfls_LDADD = ../src/libdf.a
hdfls_DEPENDENCIES = ../src/libdf.a

hdfpack_SOURCES = hdfpack.c
hdfpack_LDADD = ../src/libdf.a
hdfpack_DEPENDENCIES = ../src/libdf.a

hdftopal_SOURCES = hdftopal.c
hdftopal_LDADD = ../src/libdf.a
hdftopal_DEPENDENCIES = ../src/libdf.a

hdftor8_SOURCES = hdftor8.c
hdftor8_LDADD = ../src/libdf.a
hdftor8_DEPENDENCIES = ../src/libdf.a

hdfunpac_SOURCES = hdfunpac.c
hdfunpac_LDADD = ../src/libdf.a
hdfunpac_DEPENDENCIES = ../src/libdf.a

jpeg2hdf_SOURCES = jpeg2hdf.c
jpeg2hdf_LDADD = ../src/libdf.a
jpeg2hdf_DEPENDENCIES = ../src/libdf.a

paltohdf_SOURCES = paltohdf.c
paltohdf_LDADD = ../src/libdf.a
paltohdf_DEPENDENCIES = ../src/libdf.a

r8tohdf_SOURCES = r8tohdf.c
r8tohdf_LDADD = ../src/libdf.a
r8tohdf_DEPENDENCIES = ../src/libdf.a

ristosds_SOURCES = ristosds.c
ristosds_LDADD = ../src/libdf.a
ristosds_DEPENDENCIES = ../src/libdf.a

vmake_SOURCES = vmake.c
vmake_LDADD = ../src/libdf.a
vmake_DEPENDENCIES = ../src/libdf.a

vshow_SOURCES = vshow.c
vshow_LDADD = ../src/libdf.a
vshow_DEPENDENCIES = ../src/libdf.a

#############################################################################
##                   Testing -- Here there be dragons.                     ##
#############################################################################

check:  $(bin_PROGRAMS)
	sh ./testutil.sh $(target_canonical)

#############################################################################
##                          And the cleanup                                ##
#############################################################################

DISTCLEANFILES = gif_hdf_temp/bttrfly.hdf gif_hdf_temp/skull.gif    \
                 gif_hdf_temp/SunWheel.hdf
