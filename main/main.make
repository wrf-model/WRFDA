# MAIN

setup : links arch depend

var : wrfvar

wrfvar  : setup $(WRFVAR_LIBS) wrfvar.o pack_utils.o
	$(LD) -o wrfvar.exe $(LDFLAGS) wrfvar.o $(WRFVAR_LIB) pack_utils.o

wrf     : setup $(WRF_LIBS) pack_utils.o wrf.o
	$(LD) -o wrf.exe $(LDFLAGS) wrf.o $(WRF_LIB) pack_utils.o

wrfplus : arch links $(WRFPLUS_LIBS) pack_utils.o wrf.o
	$(LD) -o wrfplus.exe $(LDFLAGS) wrf.o $(WRFPLUS_LIB) pack_utils.o

kma2netcdf :  setup $(WRFVAR_LIBS) $(CONVERTOR_MODULES) $(CONVERTOR_OBJS) kma2netcdf.o
	$(LD) -o kma2netcdf.exe $(LDFLAGS) kma2netcdf.o \
          $(CONVERTOR_MODULES) $(CONVERTOR_OBJS) $(WRFVAR_LIB)

netcdf2kma : setup $(WRFVAR_LIBS) $(CONVERTOR_MODULES) $(CONVERTOR_OBJS) netcdf2kma.o
	$(LD) -o netcdf2kma.exe $(LDFLAGS) netcdf2kma.o \
           $(CONVERTOR_MODULES) $(CONVERTOR_OBJS) $(WRFVAR_LIB)

$(SOLVER) : setup $(WRF_LIBS) $(SOLVER).o
	$(LD) -o $(SOLVER).exe $(LDFLAGS) $(SOLVER).o libwrflib.a $(WRF_LIB)

$(SOLVER)_wrf : setup $(WRF_LIBS) wrf.o
	$(LD) -o wrf.exe $(LDFLAGS) wrf.o $(WRF_LIB)

$(SOLVER)_wrfplus : setup $(WRFPLUS_LIBS) wrfplus.o
	$(LD) -o wrfplus.exe $(LDFLAGS) wrf.o $(WRFPLUS_LIB)

$(SOLVER)_wrf_ESMFApp : $(WRF_LIBS) wrf_ESMFMod.o wrf_ESMFApp.o wrf_SST_ESMF.o
	$(LD) -o wrf_ESMFApp.exe $(LDFLAGS) wrf_ESMFApp.o wrf_ESMFMod.o $(WRF_LIB)
	$(LD) -o wrf_SST_ESMF.exe $(LDFLAGS) wrf_SST_ESMF.o wrf_ESMFMod.o $(WRF_LIB)

$(SOLVER)_ideal : setup $(WRF_LIBS) module_initialize ideal.o
	$(LD) -o ideal.exe $(LDFLAGS) ideal.o module_initialize_$(IDEAL_CASE).o $(WRF_LIB)

$(SOLVER)_real : setup $(WRF_LIBS) module_initialize real_$(SOLVER).o ndown_$(SOLVER).o
	$(LD) -o real.exe $(LDFLAGS) real_$(SOLVER).o module_initialize_$(IDEAL_CASE).o $(WRF_LIB)
	$(LD) -o ndown.exe $(LDFLAGS) ndown_$(SOLVER).o  module_initialize_$(IDEAL_CASE).o $(WRF_LIB)

convert_bioemiss : setup $(WRF_LIBS) convert_bioemiss.o
	$(FC) -o convert_bioemiss.exe $(LDFLAGS) convert_bioemiss.o libwrflib.a $(WRF_LIB)

convert_emiss : setup $(WRF_LIBS) convert_emiss.o
	$(FC) -o convert_emiss.exe $(LDFLAGS) convert_emiss.o libwrflib.a $(WRF_LIB)

real_nmm : setup $(WRF_LIBS) real_nmm.o module_initialize_real.o \
          module_optional_si_input.o input_wrf.o module_io_domain.o
	$(FC) -o real_nmm.exe $(LDFLAGS) real_nmm.o module_initialize_real.o \
          module_optional_si_input.o input_wrf.o module_io_domain.o $(WRF_LIB)

convert_nmm : setup $(WRF_LIBS) convert_nmm.o
	$(FC) -o convert_nmm.exe $(LDFLAGS) convert_nmm.o $(WRF_LIB)

module_initialize : module_initialize_$(IDEAL_CASE).o


diffwrf : diffwrf_netcdf.exe diffwrf_int.exe

diffwrf_netcdf.exe : $(WRF_LIBS) diffwrf_netcdf.o
	$(FC) $(LDFLAGS) -o diffwrf_netcdf.exe diffwrf_netcdf.o $(WRF_LIB)

diffwrf_int.exe : $(WRF_LIBS) diffwrf_int.o
	$(FC) $(LDFLAGS) -o diffwrf_int.exe diffwrf_int.o $(WRF_LIB)

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
