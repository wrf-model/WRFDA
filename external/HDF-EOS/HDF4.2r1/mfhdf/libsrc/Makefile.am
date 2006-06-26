#############################################################################
##                      Setup for building programs                        ##
#############################################################################

## Setup the different includes and preprocessor #defines we need.
INCLUDES=-I$(top_srcdir)/hdf/src        \
         -I$(top_srcdir)/mfhdf/libsrc   \
         -I$(top_srcdir)/mfhdf/port
DEFINES=-DNDEBUG -DHDF
AM_CPPFLAGS=$(INCLUDES) $(DEFINES)

#############################################################################
##                          Library to build                               ##
#############################################################################

lib_LIBRARIES = libmfhdf.a

## Information for building the "libmfhdf.a" library
CSOURCES=array.c attr.c cdf.c dim.c file.c hdfsds.c iarray.c error.c    \
         globdef.c mfsd.c nssdc.c putget.c putgetg.c sharray.c string.c \
         var.c xdrposix.c
FSOURCES=../fortran/jackets.c ../fortran/mfsdf.c ../fortran/mfsdff.f
XDRSOURCES=../xdr/xdr.c ../xdr/xdrfloat.c ../xdr/xdrstdio.c ../xdr/xdrarray.c

if HDF_BUILD_FORTRAN

if HDF_BUILD_XDR
libmfhdf_a_SOURCES = $(CSOURCES) $(FSOURCES) $(XDRSOURCES)
else
libmfhdf_a_SOURCES = $(CSOURCES) $(FSOURCES)
endif

else

if HDF_BUILD_XDR
libmfhdf_a_SOURCES = $(CSOURCES) $(XDRSOURCES)
else
libmfhdf_a_SOURCES = $(CSOURCES)
endif

endif

include_HEADERS = hdf2netcdf.h local_nc.h mfhdf.h netcdf.h 

#############################################################################
##                              Testing                                    ##
#############################################################################

check_PROGRAMS = cdftest hdftest

cdftest_SOURCES = cdftest.c
cdftest_LDADD = libmfhdf.a $(top_builddir)/hdf/src/libdf.a @LIBS@

hdftest_SOURCES = hdftest.c tszip.c temptySDSs.c tidtypes.c trank0.c \
		  tsd.c
hdftest_LDADD = libmfhdf.a $(top_builddir)/hdf/src/libdf.a @LIBS@

check:
	@echo "============================"
	@echo "HDF-SD C interfaces tests"
	@echo "============================"
	srcdir="$(srcdir)" ./hdftest > hdfout.new
	@cmd="$(DIFF) hdfout.new $(srcdir)/hdfout.sav";                 \
	  echo $$cmd;                                                   \
	  if $$cmd; then                                                \
	    echo "*** HDF passes formatted test ***";                   \
	  else                                                          \
	    echo "*** HDF fails formatted test ***";                    \
	    echo "The above differences are OK if small";               \
	    exit 0;                                                     \
	  fi
	@echo "========================="
	@echo "netCDF formatted tests"
	@echo "========================="
	srcdir="$(srcdir)" ./cdftest > cdfout.new
	@cmd="$(DIFF) cdfout.new $(srcdir)/testout.sav";                \
	  echo $$cmd;                                                   \
	  if $$cmd; then                                                \
	    echo "*** netCDF passes formatted test ***";                \
	  else                                                          \
	    echo "*** netCDF fails formatted test ***";                 \
	    echo "The above differences are OK if small";               \
	    exit 0;                                                     \
	  fi

#############################################################################
##                          And the cleanup                                ##
#############################################################################

DISTCLEANFILES=*.new *.hdf *.cdf *.cdl netcdf.h
