# main

all_wrf : all_$(SOLVER) diffwrf

chem : convert_bioemiss convert_emiss

all_em : wrf_em em_quarter_ss em_squall2d_x em_squall2d_y em_b_wave em_hill2d_x \
   em_grav2d_x real_em

all_nmm : wrf_nmm

#wrf     : setup $(WRF_LIBS) wrf.o
#	$(AR) libtemp.a -o wrf.exe $(LDFLAGS) wrf.o $(WRF_LIB)
#	$(LD) -o wrf.exe $(LDFLAGS) wrf.o $(WRF_LIB)
#	(cd ../main; $(LN) ../build/wrf.exe .)
#	(cd ../inc; $(LN) ../build/inc/namelist_script.inc .)

wrf_em : setup $(WRF_LIBS) libwrf.a libem.a wrf.o
	$(LD) -o wrf.exe $(LDFLAGS) wrf.o -lem $(WRF_LIB)
	(cd ../main; $(LN) ../build/wrf.exe .)

wrf_nmm : setup $(WRF_LIBS) libwrf.a libnmm.a wrf.o
	$(LD) -o wrf.exe $(LDFLAGS) wrf.o -lnmm $(WRF_LIB)
	(cd ../main; $(LN) ../build/wrf.exe .)

$(SOLVER)_wrf : setup $(WRF_LIBS) wrf.o
	$(LD) -o wrf.exe $(LDFLAGS) wrf.o $(WRF_LIB)
	(cd ../main; $(LN) ../build/wrf.exe .)

$(SOLVER)_wrf_ESMFApp : setup $(WRF_LIBS) wrf_ESMFMod.o wrf_ESMFApp.o wrf_SST_ESMF.o
	$(LD) -o wrf_ESMFApp.exe $(LDFLAGS) wrf_ESMFApp.o wrf_ESMFMod.o $(WRF_LIB)
	$(LD) -o wrf_SST_ESMF.exe $(LDFLAGS) wrf_SST_ESMF.o wrf_ESMFMod.o $(WRF_LIB)

$(SOLVER)_real : setup $(WRF_LIBS) module_initialize_real.o real_$(SOLVER).o ndown_$(SOLVER).o
	$(LD) -o real.exe $(LDFLAGS) real_$(SOLVER).o module_initialize_real.o -l$(SOLVER) $(WRF_LIB)
	$(LD) -o ndown.exe $(LDFLAGS) ndown_$(SOLVER).o  module_initialize_real.o $(WRF_LIB)
	(cd ../main; $(LN) ../build/real.exe ../build/ndown.exe .)

em_quarter_ss : setup $(WRF_LIBS) module_initialize_quarter_ss.o ideal.o
	$(LD) -o em_quarter_ss.exe $(LDFLAGS) ideal.o module_initialize_quarter_ss.o -lem $(WRF_LIB)
	cp em_quarter_ss.exe ideal.exe
	(cd ../main; $(LN) ../build/em_quarter_ss.exe .)

em_squall2d_x : setup $(WRF_LIBS) module_initialize_squall2d_x.o ideal.o
	$(LD) -o em_squall2d_x.exe $(LDFLAGS) ideal.o module_initialize_squall2d_x.o -lem $(WRF_LIB)
	cp em_squall2d_x.exe ideal.exe
	(cd ../main; $(LN) ../build/em_squall2d_x.exe .)

em_squall2d_y : setup $(WRF_LIBS) module_initialize_squall2d_y.o ideal.o
	$(LD) -o em_squall2d_y.exe $(LDFLAGS) ideal.o module_initialize_squall2d_y.o -lem $(WRF_LIB)
	cp em_squall2d_y.exe ideal.exe
	(cd ../main; $(LN) ../build/em_squall2d_y.exe .)

em_b_wave : setup $(WRF_LIBS) module_initialize_b_wave.o ideal.o
	$(LD) -o em_b_wave.exe $(LDFLAGS) ideal.o module_initialize_b_wave.o -lem $(WRF_LIB)
	cp em_b_wave.exe ideal.exe
	(cd ../main; $(LN) ../build/em_b_wave.exe .)

em_hill2d_x : setup $(WRF_LIBS) module_initialize_hill2d_x.o ideal.o
	$(LD) -o em_hill2d_x.exe $(LDFLAGS) ideal.o module_initialize_hill2d_x.o -lem $(WRF_LIB)
	cp em_hill2d_x.exe ideal.exe
	(cd ../main; $(LN) ../build/em_hill2d_x.exe .)

em_grav2d_x : setup $(WRF_LIBS) module_initialize_grav2d_x.o ideal.o
	$(LD) -o em_grav2d_x.exe $(LDFLAGS) ideal.o module_initialize_grav2d_x.o -lem $(WRF_LIB)
	cp em_grav2d_x.exe ideal.exe
	(cd ../main; $(LN) ../build/em_grav2d_x.exe .)

convert_bioemiss : setup $(WRF_LIBS) convert_bioemiss.o module_input_chem_bioemiss.o
	$(FC) -o convert_bioemiss.exe $(LDFLAGS) convert_bioemiss.o \
        module_input_chem_bioemiss.o libwrf.a $(WRF_LIB)
	(cd ../main; $(LN) ../build/convert_bioemiss.exe .)

convert_emiss : setup $(WRF_LIBS) convert_emiss.o
	$(FC) -o convert_emiss.exe $(LDFLAGS) convert_emiss.o libwrf.a $(WRF_LIB)
	(cd ../main; $(LN) ../build/convert_emiss.exe .)

real_em : setup $(WRF_LIBS) real_em.o
	$(FC) -o real.exe $(LDFLAGS) real_em.o module_bc.o \
            module_big_step_utilities_em.o \
            module_date_time.o module_optional_si_input.o module_bc_time_utilities.o \
            module_dm.o module_utility.o module_timing.o module_configure.o \
            module_driver_constants.o module_io_domain.o module_initialize_real.o \
            module_domain.o module_machine.o -lem $(WRF_LIB)
	(cd ../main; $(LN) ../build/real.exe .)

real_em.o : real_em.F version_decl module_bc.o module_big_step_utilities_em.o \
              module_date_time.o module_optional_si_input.o module_bc_time_utilities.o \
              module_dm.o module_utility.o module_timing.o module_configure.o \
              module_driver_constants.o module_io_domain.o module_initialize_real.o \
              module_domain.o module_machine.o 

real_nmm : $(WRF_LIBS) real_nmm.o module_initialize_real.o \
          module_optional_si_input.o input_wrf.o module_io_domain.o
	$(FC) -o real_nmm.exe $(LDFLAGS) real_nmm.o module_initialize_real.o \
          module_optional_si_input.o input_wrf.o module_io_domain.o -lnmm $(WRF_LIB)
	(cd ../main; $(LN) ../build/real_nmm.exe .)

convert_nmm : $(WRF_LIBS) convert_nmm.o
	$(FC) -o convert_nmm.exe $(LDFLAGS) convert_nmm.o -lnmm $(WRF_LIB)
	(cd ../main; $(LN) ../build/convert_nmm.exe .)

diffwrf : diffwrf_netcdf.exe diffwrf_int.exe

#############################################################################################################################

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

convert_nmm.o: \
	module_machine.o \
	module_domain.o \
	module_driver_constants.o \
	module_configure.o \
	module_timing.o \
	module_dm.o \
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
	module_io_domain.o \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)

ndown_em.o: \
	module_machine.o \
	module_domain.o \
	module_driver_constants.o \
	module_configure.o \
	module_timing.o \
	module_dm.o \
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
        module_wrf_top.o \
	$(ESMF_MOD_DEPENDENCE) $(EXTRAMODULES)

# DO NOT DELETE
