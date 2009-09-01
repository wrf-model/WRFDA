#  Makefile to build wrf4dvar driver - set path for ESMF_INSTALL to the 
#  directory where ESMF has been installed on your system.
#-----------------------------------------------------------------------------
ESMF_INSTALL = /blhome/stark/esmf_bin/MPI

WRFDA_HOME   = /ptmp/stark/WRF

WRFVAR_HOME  = $(WRFDA_HOME)/WRFDA
WRFVAR_MOD   = -I$(WRFVAR_HOME)/var/build -I$(WRFVAR_HOME)/inc
WRFVAR_LIB   = -L$(WRFVAR_HOME)/var/build -lwrfvar
WRFVAR_EXLIB = -L$(WRFVAR_HOME)/external/fftpack/fftpack5  -lfftpack \
               -L$(WRFVAR_HOME)/external/io_grib1 -lio_grib1  \
               -L$(WRFVAR_HOME)/external/io_grib_share  -lio_grib_share  \
               -L$(WRFVAR_HOME)/external/io_int -lwrfio_int   \
               -L$(WRFVAR_HOME)/external/io_esmf  -lwrfio_esmf   \
               -L$(WRFVAR_HOME)/external/RSL_LITE  -lrsl_lite   \
               $(WRFVAR_HOME)/frame/module_internal_header_util.o  \
               $(WRFVAR_HOME)/frame/pack_utils.o  \
               -L$(WRFVAR_HOME)/external/io_netcdf  -lwrfio_nf

WRFNL_HOME   = $(WRFDA_HOME)/WRFNL
WRFNL_MOD    = -I$(WRFNL_HOME)/main -I$(WRFNL_HOME)/dyn_em -I$(WRFNL_HOME)/phys  \
               -I$(WRFNL_HOME)/frame -I$(WRFNL_HOME)/share -I$(WRFNL_HOME)/inc
WRFNL_LIB    = $(WRFNL_HOME)/main/module_wrf_top.o -L$(WRFNL_HOME)/main -lwrfnl
WRFNL_EXLIB  = -lmass -lmassv

# For standalone NL build we need all of these objects, which are duplicated
# for the VAR build.
# WRFNL_MOD    = -I$(WRFNL_HOME)/main -I$(WRFNL_HOME)/external/io_esmf    \
#               -I$(WRFNL_HOME)/external/io_netcdf -I$(WRFNL_HOME)/external/io_int \
#               -I$(WRFNL_HOME)/dyn_em -I$(WRFNL_HOME)/frame -I$(WRFNL_HOME)/share  \
#               -I$(WRFNL_HOME)/phys -I$(WRFNL_HOME)/inc
# WRFNL_EXLIB  = -lmass -lmassv -L$(WRFNL_HOME)/external/fftpack/fftpack5  -lfftpack \
#               -L$(WRFNL_HOME)/external/io_grib1 -lio_grib1  \
#               -L$(WRFNL_HOME)/external/io_grib_share  -lio_grib_share  \
#               -L$(WRFNL_HOME)/external/io_int -lwrfio_int   \
#               -L$(WRFNL_HOME)/external/io_esmf  -lwrfio_esmf  \
#               -L$(WRFNL_HOME)/external/RSL_LITE  -lrsl_lite   \
#               $(WRFNL_HOME)/frame/module_internal_header_util.o  \
#               $(WRFNL_HOME)/frame/pack_utils.o  \
#               -L$(WRFNL_HOME)/external/io_netcdf  -lwrfio_nf

WRFPLUS_HOME = $(WRFDA_HOME)/WRFPLUSesmf
WRFPLUS_LIB  = -L$(WRFNL_HOME)/main -lwrflib

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
	$(ESMF_F90LINKER) -o wrf4dvar.exe  $(OBJS) $(ESMF_F90LINKOPTS)       \
	$(ESMF_F90LINKPATHS) $(ESMF_F90ESMFLINKLIBS) $(WRFVAR_LIB)            \
	$(WRFNL_LIB) $(WRFVAR_EXLIB) $(WRFNL_EXLIB) $(EXTERNAL_LIB)

# -----------------------------------------------------------------------------

esmf_wrfvar_component.o: esmf_wrfvar_component.F90 
	$(ESMF_F90COMPILER) -c esmf_wrfvar_component.F90 $(ESMF_F90COMPILEOPTS) \
	$(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILECPPFLAGS) $(WRFVAR_MOD)

esmf_wrfplus_component.o: esmf_wrfplus_component.F90 
	$(ESMF_F90COMPILER) -c esmf_wrfplus_component.F90 $(ESMF_F90COMPILEOPTS) \
	$(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILECPPFLAGS) 

esmf_wrfnl_component.o: esmf_wrfnl_component.F90 
	$(ESMF_F90COMPILER) -c esmf_wrfnl_component.F90 $(ESMF_F90COMPILEOPTS) \
	$(ESMF_F90COMPILEPATHS) $(ESMF_F90COMPILECPPFLAGS) $(WRFNL_MOD)

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
	rm -f wrf4dvar.exe
