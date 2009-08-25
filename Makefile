#  Makefile to build wrf4dvar driver - set path for ESMF_INSTALL to the 
#  directory where ESMF has been installed on your system.
#-----------------------------------------------------------------------------
ESMF_INSTALL = /blhome/stark/esmf_bin/MPI

WRFDA_HOME   = /ptmp/stark
WRFVAR_HOME  = $(WRFDA_HOME)/WRFDA/var/da
WRFVAR_LIB   = -L$(WRFVAR_HOME) -lwrfvar
WRFNL_HOME   = $(WRFDA_HOME)/WRFNL/main
WRFNL_LIB    = -L$(WRFNL_HOME) -lwrfnl

WRF_LIB      = -L$(WRF_HOME)/external/fftpack/fftpack5  -lfftpack \
               -L$(WRF_HOME)/external/io_grib1 -lio_grib1  \
               -L$(WRF_HOME)/external/io_grib_share  -lio_grib_share  \
               -L$(WRF_HOME)/external/io_int -lwrfio_int   \
               -L$(WRF_HOME)/external/RSL_LITE  -lrsl_lite   \
               -L$(WRF_HOME)/external/io_esmf  -lwrfio_esmf   \
               $(WRF_HOME)/frame/module_internal_header_util.o  \
               $(WRF_HOME)/frame/pack_utils.o  \
               -L$(WRF_HOME)/external/io_netcdf  -lwrfio_nf

EXTERNAL_LIB = -L$(BUFR) -lbufr -L$(LAPACK) -llapack -L$(BLAS) -lblas  \
               -L/mmm/users/wrfhelp/external/netcdf/netcdf-3.6.1/ibm_powerpc/lib  \
               -lnetcdf

#-----------------------------------------------------------------------------
include  $(ESMF_INSTALL)/lib/esmf.mk

OBJS = esmf_wrfnl_component.o esmf_wrfplus_component.o esmf_wrfvar_component.o \
       esmf_solver_component.o  esmf_outerloop_coupler.o  \
       esmf_wrf4Dvar_driver.o

# -----------------------------------------------------------------------------
#  $(ESMF_F90LINKER) -o wrf4dvar  $(OBJS) -bnoquiet $(ESMF_F90LINKOPTS)      \

wrf4dvar: $(OBJS)
	$(ESMF_F90LINKER) -o wrf4dvar  $(OBJS) $(ESMF_F90LINKOPTS)      \
	$(ESMF_F90LINKPATHS) $(ESMF_F90ESMFLINKLIBS)

# -----------------------------------------------------------------------------

esmf_wrfvar_component.o: esmf_wrfvar_component.F90 
	$(ESMF_F90COMPILER) -c esmf_wrfvar_component.F90 $(ESMF_F90COMPILEOPTS) \
	$(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILECPPFLAGS)

esmf_wrfplus_component.o: esmf_wrfplus_component.F90 
	$(ESMF_F90COMPILER) -c esmf_wrfplus_component.F90 $(ESMF_F90COMPILEOPTS) \
	$(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILECPPFLAGS) 

esmf_wrfnl_component.o: esmf_wrfnl_component.F90 
	$(ESMF_F90COMPILER) -c esmf_wrfnl_component.F90 $(ESMF_F90COMPILEOPTS) \
	$(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILECPPFLAGS)

esmf_outerloop_coupler.o: esmf_outerloop_coupler.F90 
	$(ESMF_F90COMPILER) -c esmf_outerloop_coupler.F90 $(ESMF_F90COMPILEOPTS) \
	$(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILECPPFLAGS) 

esmf_solver_component.o: esmf_solver_component.F90 
	$(ESMF_F90COMPILER) -c esmf_solver_component.F90 $(ESMF_F90COMPILEOPTS) \
	$(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILECPPFLAGS)

esmf_wrf4Dvar_driver.o: esmf_wrf4Dvar_driver.F90 
	$(ESMF_F90COMPILER) -c esmf_wrf4Dvar_driver.F90 $(ESMF_F90COMPILEOPTS) \
	$(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILECPPFLAGS) 

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------


.PHONY: clobber cleanall clean
clean:
	rm -f PET*.ESMF_LogFile *.err *.out

cleanall: clean
	rm -f $(OBJS) *.mod

clobber: cleanall clean
	rm -f wrf4dvar
