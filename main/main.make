# MAIN

var : wrfvar

wrfvar : arch links $(VAR_LIBS) wrfvar.o libwrfvar.a
	$(LD) -o wrfvar.exe $(LDFLAGS) wrfvar.o -lwrfvar $(LIB)

wrf : arch links wrf.o $(WRF_LIBS)
	$(LD) -o wrf.exe $(LDFLAGS) wrf.o $(LIB)

wrfplus : arch links wrf.o $(WRF_LIBS)
	$(LD) -o wrfplus.exe $(LDFLAGS) wrf.o $(LIB)

kma2netcdf :  arch $(LIBS) $(CONVERTOR_MODULES) $(CONVERTOR_OBJS) kma2netcdf.o
	$(LD) -o kma2netcdf.exe $(LDFLAGS) kma2netcdf.o libwrflib.a \
          $(CONVERTOR_MODULES) $(CONVERTOR_OBJS) $(LIB)

netcdf2kma : arch $(LIBS) $(CONVERTOR_MODULES) $(CONVERTOR_OBJS) netcdf2kma.o
	$(LD) -o netcdf2kma.exe $(LDFLAGS) netcdf2kma.o libwrflib.a \
           $(CONVERTOR_MODULES) $(CONVERTOR_OBJS) $(LIB)

$(SOLVER) : arch $(SOLVER).o
	$(LD) -o $(SOLVER).exe $(LDFLAGS) $(SOLVER).o libwrflib.a $(LIB)

$(SOLVER)_wrf : arch wrf.o
	$(LD) -o wrf.exe $(LDFLAGS) wrf.o libwrflib.a $(LIB)

$(SOLVER)_wrfplus : arch wrfplus.o
	$(LD) -o wrfplus.exe $(LDFLAGS) wrf.o libwrflib.a $(LIB)

$(SOLVER)_wrf_ESMFApp : wrf_ESMFMod.o wrf_ESMFApp.o wrf_SST_ESMF.o
	$(RANLIB) libwrflib.a
	$(LD) -o wrf_ESMFApp.exe $(LDFLAGS) wrf_ESMFApp.o wrf_ESMFMod.o libwrflib.a $(LIB)
	$(LD) -o wrf_SST_ESMF.exe $(LDFLAGS) wrf_SST_ESMF.o wrf_ESMFMod.o libwrflib.a $(LIB)

$(SOLVER)_ideal : arch module_initialize ideal.o
	$(LD) -o ideal.exe $(LDFLAGS) ideal.o module_initialize_$(IDEAL_CASE).o libwrflib.a $(LIB)

$(SOLVER)_real : arch module_initialize real_$(SOLVER).o ndown_$(SOLVER).o
	$(LD) -o real.exe $(LDFLAGS) real_$(SOLVER).o module_initialize_$(IDEAL_CASE).o libwrflib.a $(LIB)
	$(LD) -o ndown.exe $(LDFLAGS) ndown_$(SOLVER).o  module_initialize_$(IDEAL_CASE).o libwrflib.a $(LIB)

convert_bioemiss : arch convert_bioemiss.o
	$(FC) -o convert_bioemiss.exe $(LDFLAGS) convert_bioemiss.o libwrflib.a $(LIB)

convert_emiss : arch convert_emiss.o
	$(FC) -o convert_emiss.exe $(LDFLAGS) convert_emiss.o libwrflib.a $(LIB)

real_nmm : arch real_nmm.o module_initialize_real.o \
          module_optional_si_input.o input_wrf.o module_io_domain.o
	$(FC) -o real_nmm.exe $(LDFLAGS) real_nmm.o module_initialize_real.o \
          module_optional_si_input.o input_wrf.o module_io_domain.o $(LIB)

convert_nmm : arch convert_nmm.o
	$(FC) -o convert_nmm.exe $(LDFLAGS) convert_nmm.o $(LIB)

module_initialize : module_initialize_$(IDEAL_CASE).o


diffwrf : diffwrf_netcdf.exe diffwrf_int.exe

diffwrf_netcdf.exe : diffwrf_netcdf.o $( LIBS)
	$(FC) $(LDFLAGS) -o diffwrf_netcdf.exe diffwrf_netcdf.o $(LIB)

diffwrf_int.exe : diffwrf_int.o $( LIBS)
	$(FC) $(LDFLAGS) -o diffwrf_int.exe diffwrf_int.o $(LIB)

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
	module_dm.o \
	module_wrf_esmf_super.o \
        module_wrf_top.o \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)

# DO NOT DELETE
