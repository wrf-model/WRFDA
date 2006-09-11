# MAIN

var : wrfvar

wrfvar  : advance_cymdh.exe da_update_bc.exe $(WRFVAR_OBJS) $(WRFVAR_LIBS) wrfvar.o
	$(LD) -o wrfvar.exe $(LDFLAGS) wrfvar.o $(WRFVAR_LIB)
	cp wrfvar.exe ../main

wrfvar_esmf  : advance_cymdh.exe da_update_bc.exe $(WRFVAR_OBJS) $(WRFVAR_LIBS) wrfvar_esmf.o \
          da_wrfvar_esmf_super.o
	$(LD) -o wrfvar_esmf.exe $(LDFLAGS) wrfvar_esmf.o $(WRFVAR_LIB) \
          da_wrfvar_esmf_super.o
	cp wrfvar_esmf.exe ../main

wrf     : $(WRF_LIBS) wrf.o
	$(LD) -o wrf.exe $(LDFLAGS) wrf.o $(WRF_LIB)

wrfplus : $(WRFPLUS_LIBS) wrf.o
	$(LD) -o wrfplus.exe $(LDFLAGS) wrf.o $(WRFPLUS_LIB)

k2n : kma2netcdf

n2k : netcdf2kma

kma2netcdf :  $(WRFVAR_LIBS) $(CONVERTOR_MODULES) $(CONVERTOR_OBJS) kma2netcdf.o
	$(LD) -o kma2netcdf.exe $(LDFLAGS) kma2netcdf.o \
          $(CONVERTOR_MODULES) $(CONVERTOR_OBJS) $(WRFVAR_LIB)
	cp kma2netcdf.exe ../main

netcdf2kma : $(WRFVAR_LIBS) $(CONVERTOR_MODULES) $(CONVERTOR_OBJS) netcdf2kma.o
	$(LD) -o netcdf2kma.exe $(LDFLAGS) netcdf2kma.o \
           $(CONVERTOR_MODULES) $(CONVERTOR_OBJS) $(WRFVAR_LIB)
	cp netcdf2kma.exe ../main

$(SOLVER) : $(WRF_LIBS) $(SOLVER).o
	$(LD) -o $(SOLVER).exe $(LDFLAGS) $(SOLVER).o libwrf.a $(WRF_LIB)
	cp $(SOLVER).exe ../main

$(SOLVER)_wrf : $(WRF_LIBS) wrf.o
	$(LD) -o wrf.exe $(LDFLAGS) wrf.o $(WRF_LIB)
	cp wrf.exe ../main

$(SOLVER)_wrfplus : $(WRFPLUS_LIBS) wrfplus.o
	$(LD) -o wrfplus.exe $(LDFLAGS) wrf.o $(WRFPLUS_LIB)
	cp wrfplus.exe ../main

$(SOLVER)_wrf_ESMFApp : $(WRF_LIBS) wrf_ESMFMod.o wrf_ESMFApp.o wrf_SST_ESMF.o
	$(LD) -o wrf_ESMFApp.exe $(LDFLAGS) wrf_ESMFApp.o wrf_ESMFMod.o $(WRF_LIB)
	$(LD) -o wrf_SST_ESMF.exe $(LDFLAGS) wrf_SST_ESMF.o wrf_ESMFMod.o $(WRF_LIB)
	cp wrf_ESMFApp.exe wrf_SST_ESMF.exe ../main

$(SOLVER)_ideal : $(WRF_LIBS) module_initialize ideal.o
	$(LD) -o ideal.exe $(LDFLAGS) ideal.o module_initialize_$(IDEAL_CASE).o $(WRF_LIB)
	cp ideal.exe ../main

$(SOLVER)_real : $(WRF_LIBS) module_initialize real_$(SOLVER).o ndown_$(SOLVER).o
	$(LD) -o real.exe $(LDFLAGS) real_$(SOLVER).o module_initialize_$(IDEAL_CASE).o $(WRF_LIB)
	$(LD) -o ndown.exe $(LDFLAGS) ndown_$(SOLVER).o  module_initialize_$(IDEAL_CASE).o $(WRF_LIB)
	cp real.exe ndown.exe ../main

convert_bioemiss : $(WRF_LIBS) convert_bioemiss.o
	$(FC) -o convert_bioemiss.exe $(LDFLAGS) convert_bioemiss.o libwrf.a $(WRF_LIB)
	cp convert_bioemiss.exe ../main

convert_emiss : $(WRF_LIBS) convert_emiss.o
	$(FC) -o convert_emiss.exe $(LDFLAGS) convert_emiss.o libwrf.a $(WRF_LIB)
	cp convert_emiss.exe ../main

real_em : $(WRF_LIBS) real_em.o
	$(FC) -o real_em.exe $(LDFLAGS) real_em.o module_bc.o \
            module_big_step_utilities_em.o \
            module_date_time.o module_optional_si_input.o module_bc_time_utilities.o \
            module_dm.o module_utility.o module_timing.o module_configure.o \
            module_driver_constants.o module_io_domain.o module_initialize_real.o \
            module_domain.o module_machine.o $(WRF_LIB)
	cp real_em.exe ../main

real_em.o : real_em.F version_decl module_bc.o module_big_step_utilities_em.o \
              module_date_time.o module_optional_si_input.o module_bc_time_utilities.o \
              module_dm.o module_utility.o module_timing.o module_configure.o \
              module_driver_constants.o module_io_domain.o module_initialize_real.o \
              module_domain.o module_machine.o 

real_nmm : $(WRF_LIBS) real_nmm.o module_initialize_real.o \
          module_optional_si_input.o input_wrf.o module_io_domain.o
	$(FC) -o real_nmm.exe $(LDFLAGS) real_nmm.o module_initialize_real.o \
          module_optional_si_input.o input_wrf.o module_io_domain.o $(WRF_LIB)
	cp real_nmm.exe ../main

convert_nmm : $(WRF_LIBS) convert_nmm.o
	$(FC) -o convert_nmm.exe $(LDFLAGS) convert_nmm.o $(WRF_LIB)
	cp convert_nmm.exe ../main

module_initialize : module_initialize_$(IDEAL_CASE).o


diffwrf : diffwrf_netcdf.exe diffwrf_int.exe

diffwrf_netcdf.exe : $(WRF_LIBS) diffwrf_netcdf.o
	$(FC) $(LDFLAGS) -o diffwrf_netcdf.exe diffwrf_netcdf.o $(WRF_LIB)
	cp diffwrf_netcdf.exe ../main

diffwrf_int.exe : $(WRF_LIBS) diffwrf_int.o
	$(FC) $(LDFLAGS) -o diffwrf_int.exe diffwrf_int.o $(WRF_LIB)
	cp diffwrf_int.exe ../main

#############################################################################################################################

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

convert_nmm.o: \
	module_machine.o \
	module_domain.o \
	module_driver_constants.o \
	module_configure.o \
	module_timing.o \
	module_dm.o \
	module_wrf_esmf_super.o \
	module_bc.o \
	module_io_domain.o \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)

ideal.o: \
	module_machine.o \
	module_domain.o \
	module_driver_constants.o \
	module_configure.o \
	module_timing.o \
	module_dm.o \
	module_wrf_esmf_super.o \
	module_io_domain.o \
	$(CASE_MODULE) \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)

ndown_em.o: \
	module_machine.o \
	module_domain.o \
	module_driver_constants.o \
	module_configure.o \
	module_timing.o \
	module_dm.o \
	module_wrf_esmf_super.o \
	module_wrf_error.o \
	wrf_debug.o \
	module_integrate.o \
	module_bc.o \
	module_io_domain.o \
	module_get_file_names.o \
	module_soil_pre.o \
	module_initialize_real.o \
	module_big_step_utilities_em.o \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)

# this already built above :module_initialize.real.o \
real_em.o: \
	module_machine.o \
	module_domain.o \
	module_driver_constants.o \
	module_configure.o \
	module_timing.o \
	module_dm.o \
	module_wrf_esmf_super.o \
	module_si_io_em.o \
	module_big_step_utilities_em.o \
	module_io_domain.o \
	module_date_time.o \
	module_optional_si_input.o \
	module_bc_time_utilities.o \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)
#	module_input_chem_data.o \
#	module_input_chem_bioemiss.o \

wrf.o: \
	module_machine.o \
	module_domain.o \
	module_integrate.o \
	module_driver_constants.o \
	module_configure.o \
	module_timing.o \
	module_wrf_error.o \
	wrf_debug.o \
	module_dm.o \
	module_wrf_esmf_super.o \
        module_wrf_top.o \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)

# DO NOT DELETE
