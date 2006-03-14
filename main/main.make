# MAIN

var : arch wrfvar.o $(LIBS)
	$(LD) -o wrfvar.exe $(LDFLAGS) wrfvar.o $(LIB)

wrf : arch wrf.o $(LIBS)
	$(LD) -o wrf.exe $(LDFLAGS) wrf.o $(LIB)

kma2netcdf : kma2netcdf.o
	$(LD) -o kma2netcdf.exe $(LDFLAGS) kma2netcdf.o libwrflib.a $(LIB)

netcdf2kma : netcdf2kma.o
	$(LD) -o netcdf2kma.exe $(LDFLAGS) netcdf2kma.o libwrflib.a $(LIB)

$(SOLVER) : $(SOLVER).o
	$(LD) -o $(SOLVER).exe $(LDFLAGS) $(SOLVER).o libwrflib.a $(LIB)

$(SOLVER)_wrf : wrf.o
	$(LD) -o wrf.exe $(LDFLAGS) wrf.o libwrflib.a $(LIB)

$(SOLVER)_ideal : module_initialize ideal.o
	$(LD) -o ideal.exe $(LDFLAGS) ideal.o ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o libwrflib.a $(LIB)

$(SOLVER)_real : module_initialize real_$(SOLVER).o ndown_$(SOLVER).o
	$(LD) -o real.exe $(LDFLAGS) real_$(SOLVER).o ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o libwrflib.a $(LIB)
	$(LD) -o ndown.exe $(LDFLAGS) ndown_$(SOLVER).o  ../dyn_$(SOLVER)/module_initialize_$(IDEAL_CASE).o libwrflib.a $(LIB)

convert_bioemiss : convert_bioemiss.o
	$(FC) -o convert_bioemiss.exe $(LDFLAGS) convert_bioemiss.o libwrflib.a $(LIB)

convert_emiss : convert_emiss.o
	$(FC) -o convert_emiss.exe $(LDFLAGS) convert_emiss.o libwrflib.a $(LIB)

real_nmm : real_nmm.o
	( cd ../dyn_nmm ;  $(MAKE) module_initialize_real.o )
	$(FC) -o real_nmm.exe $(LDFLAGS) real_nmm.o module_initialize_real.o module_optional_si_input.o input_wrf.o module_io_domain.o libwrflib.a $(LIB)

convert_nmm : convert_nmm.o
	$(FC) -o convert_nmm.exe $(LDFLAGS) convert_nmm.o libwrflib.a $(LIB)

module_initialize :
	( cd ../dyn_$(SOLVER) ;  $(MAKE) module_initialize_$(IDEAL_CASE).o )

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
	../dyn_$(SOLVER)/$(CASE_MODULE) \
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

# Needs explicit rule because of clashing wrfvar.c from tools/CodeBase

wrfvar.o: wrfvar_interface wrfvar_io wrfvar_obj $(LIBS)
	@ $(RM) $@
	@ $(SED_FTN) $*.F > $*.b
	  $(CPP) $(CPPFLAGS) $*.b  > $*.f
	@ $(RM) $*.b
	  $(FC) -c $(FCFLAGS) $*.f

# DO NOT DELETE
