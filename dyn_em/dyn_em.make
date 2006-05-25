# DYN_EM

DYN_MODULES =                 		\
        module_advect_em.o   		\
        module_advect_em_ad.o  		\
        module_advect_em_tl.o  		\
	module_diffusion_em.o  		\
	module_diffusion_em_ad.o	\
	module_diffusion_em_tl.o	\
	module_small_step_em.o 		\
	module_small_step_em_ad.o 	\
	module_small_step_em_tl.o 	\
        module_big_step_utilities_em.o  \
        module_big_step_utilities_em_ad.o  \
        module_big_step_utilities_em_tl.o  \
        module_em.o         		\
        module_em_ad.o         		\
        module_em_tl.o         		\
        module_solvedebug_em.o    	\
        module_bc_em.o                  \
        module_bc_em_ad.o               \
        module_bc_em_tl.o               \
        module_init_utilities.o         \
        module_check.o                  \
	$(CASE_MODULE)

# possible CASE_MODULE settings
#	module_initialize_b_wave.o      \
#	module_initialize_grav2d_x.o   \
#	module_initialize_hill2d_x.o    \
#	module_initialize_quarter_ss.o  \
#	module_initialize_real.o        \
#	module_initialize_lsm_x.o  \
#	module_initialize_squall2d_x.o  \
#	module_initialize_squall2d_y.o 

DYN_OBJS    = 		        \
	init_modules_em.o       \
	solve_em.o              \
	solve_em_ad.o           \
	solve_em_tl.o           \
        start_em.o              \
        shift_domain_em.o       \
        nest_init_utils.o	\
        interp_domain_em.o
 

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

init_modules_em.o: module_big_step_utilities_em.o

interp_domain_em.o: module_domain.o \
		module_configure.o 

module_check.o: module_check.F \
                module_em_tl.o \
                module_em_ad.o \
                module_small_step_em_ad.o \
                module_small_step_em_tl.o

module_advect_em.o: module_advect_em.F \
                module_bc.o \
		module_model_constants.o \
		module_wrf_error.o

module_advect_em_ad.o: module_advect_em_ad.F \
                module_bc.o \
		module_model_constants.o \
		module_wrf_error.o

module_advect_em_tl.o: module_advect_em_tl.F \
                module_bc.o \
		module_model_constants.o \
		module_wrf_error.o

module_bc_em.o: module_bc.o module_configure.o \
		module_wrf_error.o

module_bc_em_ad.o: module_bc.o module_configure.o \
		module_wrf_error.o \
                module_bc_ad.o

module_bc_em_tl.o: module_bc.o module_configure.o \
		module_wrf_error.o \
                module_bc_tl.o

module_big_step_utilities_em.o: \
		module_domain.o \
		module_configure.o  \
		module_state_description.o \
		module_model_constants.o 

module_big_step_utilities_em_ad.o: \
                module_big_step_utilities_em.o \
		module_domain.o \
		module_configure.o  \
		module_state_description.o \
		module_model_constants.o 

module_big_step_utilities_em_tl.o: \
                module_big_step_utilities_em.o \
		module_domain.o \
		module_configure.o  \
		module_state_description.o \
		module_model_constants.o 

module_diffusion_em.o:  module_big_step_utilities_em.o \
		module_configure.o  \
		module_state_description.o \
		module_model_constants.o \
		module_bc.o \
		module_wrf_error.o

module_diffusion_em_ad.o:  module_big_step_utilities_em_ad.o \
		module_configure.o  \
		module_state_description.o \
		module_model_constants.o \
		module_bc_ad.o \
		module_wrf_error.o

module_diffusion_em_tl.o:  module_big_step_utilities_em_tl.o \
		module_configure.o  \
		module_state_description.o \
		module_model_constants.o \
		module_bc_tl.o \
		module_wrf_error.o

module_em.o:    module_big_step_utilities_em.o \
                module_advect_em.o \
		module_state_description.o \
		module_model_constants.o 

module_em_ad.o: module_em_ad.F \
                module_big_step_utilities_em_ad.o \
                module_advect_em_ad.o \
		module_state_description.o \
		module_model_constants.o 

module_em_tl.o: module_em_tl.F \
                module_big_step_utilities_em_tl.o \
                module_advect_em_tl.o \
		module_state_description.o \
		module_model_constants.o 

module_small_step_em.o: \
		module_configure.o  \
		module_model_constants.o 

module_small_step_em_ad.o: \
                module_small_step_em.o \
		module_configure.o  \
		module_model_constants.o 

module_small_step_em_tl.o: \
                module_small_step_em.o \
		module_configure.o  \
		module_model_constants.o 

module_initialize_b_wave.o : \
		module_domain.o \
		module_configure.o  \
		module_state_description.o \
		module_timing.o \
		module_dm.o \
		module_io_domain.o \
		module_model_constants.o \
		module_bc.o \
		module_init_utilities.o

module_initialize_grav2d_x.o: \
		module_domain.o \
		module_configure.o  \
		module_state_description.o \
		module_timing.o \
		module_dm.o \
		module_io_domain.o \
		module_model_constants.o \
		module_bc.o \
		module_init_utilities.o

module_initialize_hill2d_x.o: \
		module_domain.o \
		module_configure.o  \
		module_state_description.o \
		module_timing.o \
		module_dm.o \
		module_io_domain.o \
		module_model_constants.o \
		module_bc.o \
		module_init_utilities.o

module_initialize_quarter_ss.o : \
		module_domain.o \
		module_configure.o  \
		module_state_description.o \
		module_timing.o \
		module_dm.o \
		module_io_domain.o \
		module_model_constants.o \
		module_bc.o \
		module_init_utilities.o

module_initialize_real.o :  module_domain.o \
		module_configure.o  \
		module_state_description.o \
		module_timing.o \
		module_dm.o \
		module_io_domain.o \
		module_model_constants.o \
		module_bc.o \
		module_soil_pre.o \
		module_optional_si_input.o

module_initialize_squall2d_x.o : \
		module_domain.o \
		module_configure.o  \
		module_state_description.o \
		module_timing.o \
		module_dm.o \
		module_io_domain.o \
		module_model_constants.o \
		module_bc.o \
		module_init_utilities.o

module_initialize_squall2d_y.o : \
		module_domain.o \
		module_configure.o  \
		module_state_description.o \
		module_timing.o \
		module_dm.o \
		module_io_domain.o \
		module_model_constants.o \
		module_bc.o \
		module_init_utilities.o

nest_init_utils.o: \
		module_domain.o \
		module_configure.o

start_em.o: module_bc_em.o \
		module_domain.o \
		module_configure.o  \
		module_state_description.o \
		module_timing.o \
		module_dm.o \
		module_io_domain.o \
		module_model_constants.o \
		module_bc.o \
		module_date_time.o \
		module_physics_init.o 

solve_em.o:     module_small_step_em.o \
		module_em.o            \
		module_solvedebug_em.o \
                module_bc_em.o         \
		module_diffusion_em.o  \
		module_big_step_utilities_em.o \
		module_domain.o \
		module_configure.o  \
		module_driver_constants.o \
		module_state_description.o \
		module_machine.o \
		module_tiles.o \
		module_dm.o \
		module_model_constants.o \
		module_bc.o \
		module_radiation_driver.o \
		module_surface_driver.o \
		module_cumulus_driver.o \
		module_microphysics_driver.o \
		module_microphysics_zero_out.o \
		module_pbl_driver.o \
		module_physics_addtendc.o \
                module_check.o

solve_em_tl.o:  module_small_step_em_tl.o \
		module_em_tl.o            \
		module_solvedebug_em.o \
                module_bc_em_tl.o         \
		module_diffusion_em_tl.o  \
		module_big_step_utilities_em.o \
		module_domain.o \
		module_configure.o  \
		module_driver_constants.o \
		module_state_description.o \
		module_machine.o \
		module_tiles.o \
		module_dm.o \
		module_model_constants.o \
		module_bc.o \
		module_radiation_driver.o \
		module_surface_driver.o \
		module_cumulus_driver.o \
		module_microphysics_driver.o \
		module_microphysics_zero_out.o \
		module_pbl_driver.o \
		module_physics_addtendc.o

solve_em_ad.o:  module_small_step_em_ad.o \
		module_em_ad.o          \
		module_solvedebug_em.o \
                module_bc_em_ad.o         \
		module_diffusion_em_ad.o  \
		module_big_step_utilities_em.o \
		module_domain.o \
		module_configure.o  \
		module_driver_constants.o \
		module_state_description.o \
		module_machine.o \
		module_tiles.o \
		module_dm.o \
		module_model_constants.o \
		module_bc.o \
		module_radiation_driver.o \
		module_surface_driver.o \
		module_cumulus_driver.o \
		module_microphysics_driver.o \
		module_microphysics_zero_out.o \
		module_pbl_driver.o \
		module_physics_addtendc.o

#		module_chem_utilities.o \
#		module_input_chem_data.o

# DO NOT DELETE
