include ../configure.wrf

MODULES = 

OBJS    =

LIBPATHS = 

wrfvar : wrfvar.o
	$(RANLIB) libwrflib.a
	$(LD) -o wrfvar.exe $(LDFLAGS) wrfvar.o libwrflib.a $(VAR_LIB)

kma2netcdf : kma2netcdf.o
	$(RANLIB) libwrflib.a
	$(LD) -o kma2netcdf.exe $(LDFLAGS) kma2netcdf.o libwrflib.a $(VAR_LIB) -L../convertor

netcdf2kma : netcdf2kma.o
	$(RANLIB) libwrflib.a
	$(LD) -o netcdf2kma.exe $(LDFLAGS) netcdf2kma.o libwrflib.a $(VAR_LIB) -L../convertor 

$(SOLVER) : $(SOLVER).o
	$(RANLIB) libwrflib.a
	$(LD) -o $(SOLVER).exe $(LDFLAGS) $(SOLVER).o libwrflib.a $(WRF_LIB)

$(SOLVER)_wrf : wrf.o
	$(RANLIB) libwrflib.a
	$(LD) -o wrf.exe $(LDFLAGS) wrf.o libwrflib.a $(WRF_LIB)

$(SOLVER)_wrfplus : wrf.o
	$(RANLIB) libwrflib.a
	$(LD) -o wrfplus.exe $(LDFLAGS) wrf.o module_wrf_top.o libwrflib.a $(WRF_LIB)

$(SOLVER)_wrf_ESMFApp : wrf_ESMFMod.o wrf_ESMFApp.o wrf_SST_ESMF.o
	$(RANLIB) libwrflib.a
	$(LD) -o wrf_ESMFApp.exe $(LDFLAGS) wrf_ESMFApp.o wrf_ESMFMod.o libwrflib.a $(WRF_LIB)
	$(LD) -o wrf_SST_ESMF.exe $(LDFLAGS) wrf_SST_ESMF.o wrf_ESMFMod.o libwrflib.a $(WRF_LIB)

$(SOLVER)_ideal : module_initialize ideal.o
	$(RANLIB) libwrflib.a
	$(LD) -o ideal.exe $(LDFLAGS) ideal.o $(DYNAMICS)/module_initialize_$(IDEAL_CASE).o libwrflib.a $(WRF_LIB)

$(SOLVER)_real : module_initialize real_$(SOLVER).o ndown_$(SOLVER).o nup_$(SOLVER).o
	$(RANWRF_LIB) libwrflib.a
	$(LD) -o real.exe $(LDFLAGS) real_$(SOLVER).o $(DYNAMICS)/module_initialize_$(IDEAL_CASE).o libwrflib.a $(WRF_LIB)
	$(LD) -o ndown.exe $(LDFLAGS) ndown_$(SOLVER).o  $(DYNAMICS)/module_initialize_$(IDEAL_CASE).o libwrflib.a $(WRF_LIB)
	$(LD) -o nup.exe $(LDFLAGS) nup_$(SOLVER).o  $(DYNAMICS)/module_initialize_$(IDEAL_CASE).o libwrflib.a $(WRF_LIB)

convert_em : convert_em.o
	$(RANWRF_LIB) libwrflib.a
	$(LD) -o convert_em.exe $(LDFLAGS) convert_em.o libwrflib.a $(WRF_LIB)

convert_nmm : convert_nmm.o
	$(RANWRF_LIB) libwrflib.a
	$(FC) -o convert_nmm.exe $(LDFLAGS) convert_nmm.o libwrflib.a $(WRF_LIB)

real_nmm : real_nmm.o
	( cd ../dyn_nmm ;  $(MAKE) module_initialize_real.o )
	$(RANWRF_LIB) libwrflib.a
	$(FC) -o real_nmm.exe $(LDFLAGS) real_nmm.o \
          $(DYN_NMM)/module_initialize_real.o \
          $(SHARE)/module_optional_si_input.o \
          $(SHARE)/input_wrf.o \
          $(SHARE)/module_io_domain.o libwrflib.a $(WRF_LIB)

module_initialize :
	( cd $(DYNAMICS) ;  $(MAKE) module_initialize_$(IDEAL_CASE).o )

## prevent real being compiled for OMP -- only for regtesting
#$(SOLVER)_real : module_initialize real_$(SOLVER).o
#	$(RANWRF_LIB) libwrflib.a
#	if [ -z "$(OMP)" ] ; then $(FC) -o real.exe $(LDFLAGS) real_$(SOLVER).o $(DYNAMICS)/module_initialize_$(IDEAL_CASE).o libwrflib.a $(WRF_LIB) ; fi
#
## prevent module_initialize being compiled for OMP --remove after IBM debugging
#module_initialize :
#	if [ -z "$(OMP)" ] ; then ( cd $(DYNAMICS) ;  $(MAKE) module_initialize_$(IDEAL_CASE).o ) ; fi
# end of regtest changes

clean:
	@ $(RM) *.o *.f

superclean : clean
	@ $(RM) mpif.h *.stb

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

convert_nmm.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_dm.o \
	../share/module_wrf_esmf_super.o \
	../share/module_bc.o \
	../share/module_io_domain.o \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)

convert_em.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_dm.o \
	../share/module_bc.o \
	../share/module_io_domain.o \
	$(ESMF_MOD_DEPENDENCE)

ideal.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_dm.o \
	../share/module_wrf_esmf_super.o \
	../share/module_io_domain.o \
	$(DYNAMICS)/$(CASE_MODULE) \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)

ndown_em.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_dm.o \
	../share/module_wrf_esmf_super.o \
	../frame/module_wrf_error.o \
	../frame/module_integrate.o \
	../share/module_bc.o \
	../share/module_io_domain.o \
	../share/module_get_file_names.o \
	../share/module_soil_pre.o \
	../dyn_em/module_initialize_real.o \
	../dyn_em/module_big_step_utilities_em.o \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)

nup_em.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_dm.o \
	../frame/module_wrf_error.o \
	../frame/module_integrate.o \
	../share/module_bc.o \
	../share/module_io_domain.o \
	../share/module_get_file_names.o \
	../share/module_soil_pre.o \
	../dyn_em/module_initialize_real.o \
	../dyn_em/module_big_step_utilities_em.o \
	$(ESMF_MOD_DEPENDENCE)

# this already built above :../dyn_em/module_initialize.real.o \
real_em.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_dm.o \
	../share/module_wrf_esmf_super.o \
	../dyn_em/module_si_io_em.o \
	../dyn_em/module_big_step_utilities_em.o \
	../share/module_io_domain.o \
	../share/module_date_time.o \
	../share/module_optional_si_input.o \
	../share/module_bc_time_utilities.o \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)
#	../chem/module_input_chem_data.o \
#	../chem/module_input_chem_bioemiss.o \

wrf.o: \
	../frame/module_machine.o \
	../frame/module_domain.o \
	../frame/module_integrate.o \
	../frame/module_driver_constants.o \
	../frame/module_configure.o \
	../frame/module_timing.o \
	../frame/module_wrf_error.o \
	../frame/module_dm.o \
	../share/module_wrf_esmf_super.o \
        ../share/module_wrf_top.o \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)

wrf_ESMFMod.o:  ../share/module_wrf_top.o

wrf_ESMFApp.o:  wrf_ESMFMod.o

wrf_SST_ESMF.o:  wrf_ESMFMod.o

# DO NOT DELETE
