#

LN      =       ln -sf
MAKE    =       make -i -r
RM      =       rm -f


MODULES =                 		\
        module_advect_em.o   		\
        module_advect_em_ad.o   	\
        module_advect_em_tl.o   	\
	module_diffusion_em.o  		\
	module_diffusion_em_ad.o 	\
	module_diffusion_em_tl.o 	\
	module_small_step_em.o 		\
	module_small_step_em_ad.o 		\
	module_small_step_em_tl.o 		\
        module_big_step_utilities_em.o  \
        module_big_step_utilities_em_ad.o \
        module_big_step_utilities_em_tl.o \
        module_em.o         		\
        module_em_ad.o         		\
        module_em_tl.o         		\
        module_solvedebug_em.o    	\
        module_bc_em.o                  \
        module_bc_em_ad.o               \
        module_bc_em_tl.o               \
        module_bc_ad.o                  \
        module_bc_tl.o                  \
        module_check.o                  \
        module_init_utilities.o         \
        module_linked_lisk.o            \
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
#	test_solve_em.o          \

OBJS    = 		        \
	init_modules_em.o       \
	solve_em.o              \
	solve_em_ad.o           \
	solve_em_tl.o           \
 	solve_em_tst.o          \
        start_em.o              \
        couple_or_uncouple_em.o \
        nest_init_utils.o	\
        addroutines.o   \
        interp_domain_em.o
 

LIBTARGET    =  dyn_em
TARGETDIR    =  ./
$(LIBTARGET) :  $(MODULES) $(OBJS)
		$(AR) ../main/libwrflib.a $(MODULES) $(OBJS)

include ../configure.wrf

clean:
	@ echo 'use the clean script'

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

couple_or_uncouple_em.o: ../frame/module_domain.o \
		../frame/module_configure.o \
		../frame/module_driver_constants.o \
		../frame/module_machine.o \
		../frame/module_tiles.o \
		../frame/module_dm.o \
		../frame/module_state_description.o

init_modules_em.o: module_big_step_utilities_em.o

interp_domain_em.o: ../frame/module_domain.o \
		../frame/module_configure.o 

module_advect_em.o: ../share/module_bc.o \
		../share/module_model_constants.o \
		../frame/module_wrf_error.o

module_advect_em_ad.o: module_bc_ad.o \
		../share/module_bc.o \
		../share/module_model_constants.o \
		../frame/module_wrf_error.o

module_advect_em_tl.o: module_bc_tl.o \
		../share/module_bc.o \
		../share/module_model_constants.o \
		../frame/module_wrf_error.o

module_bc_em.o: ../share/module_bc.o ../frame/module_configure.o \
		../frame/module_wrf_error.o

module_bc_em_ad.o: ../share/module_bc.o ../frame/module_configure.o \
		module_bc_ad.o \
		../frame/module_wrf_error.o

module_bc_em_tl.o: ../share/module_bc.o ../frame/module_configure.o \
		module_bc_tl.o \
		../frame/module_wrf_error.o

module_bc_ad.o: ../share/module_bc.o ../frame/module_configure.o \
		../frame/module_wrf_error.o

module_bc_tl.o: ../share/module_bc.o ../frame/module_configure.o \
		../frame/module_wrf_error.o

module_check.o: module_em_tl.o module_em_ad.o \
		module_bc_em_tl.o \
		module_bc_em_ad.o \
		module_small_step_em_tl.o \
		module_small_step_em_ad.o \
		module_diffusion_em_tl.o \
		module_big_step_utilities_em_tl.o \
		module_big_step_utilities_em_ad.o \
		module_diffusion_em_ad.o

module_big_step_utilities_em.o: \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../share/module_model_constants.o 

module_big_step_utilities_em_ad.o: \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../share/module_model_constants.o \
		module_big_step_utilities_em.o

module_big_step_utilities_em_tl.o: \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../share/module_model_constants.o \
		module_big_step_utilities_em.o

module_diffusion_em.o:  module_big_step_utilities_em.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		../frame/module_wrf_error.o

module_diffusion_em_ad.o:  module_big_step_utilities_em.o \
		module_big_step_utilities_em_ad.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		../frame/module_wrf_error.o

module_diffusion_em_tl.o:  module_big_step_utilities_em.o \
		module_big_step_utilities_em_tl.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		../frame/module_wrf_error.o

module_em.o:    module_big_step_utilities_em.o module_advect_em.o \
		../frame/module_state_description.o \
		../share/module_model_constants.o

module_em_tl.o: module_big_step_utilities_em.o module_advect_em.o \
		module_big_step_utilities_em_tl.o module_advect_em_tl.o \
		../frame/module_state_description.o \
		../share/module_model_constants.o module_em.o

module_em_ad.o: module_big_step_utilities_em.o module_advect_em.o \
		module_big_step_utilities_em_ad.o module_advect_em_ad.o \
		../frame/module_state_description.o \
		../share/module_model_constants.o module_em.o

module_small_step_em.o: \
		../frame/module_configure.o  \
		../share/module_model_constants.o 

module_small_step_em_ad.o: \
		../frame/module_configure.o  \
		../share/module_model_constants.o 

module_small_step_em_tl.o: \
		../frame/module_configure.o  \
		../share/module_model_constants.o 

module_initialize_b_wave.o : \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../frame/module_timing.o \
		../frame/module_dm.o \
		../share/module_io_domain.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		module_init_utilities.o

module_initialize_grav2d_x.o: \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../frame/module_timing.o \
		../frame/module_dm.o \
		../share/module_io_domain.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		module_init_utilities.o

module_initialize_hill2d_x.o: \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../frame/module_timing.o \
		../frame/module_dm.o \
		../share/module_io_domain.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		module_init_utilities.o

module_initialize_quarter_ss.o : \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../frame/module_timing.o \
		../frame/module_dm.o \
		../share/module_io_domain.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		module_init_utilities.o

module_initialize_real.o :  ../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../frame/module_timing.o \
		../frame/module_dm.o \
		../share/module_io_domain.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		../share/module_soil_pre.o \
		../share/module_optional_si_input.o

module_initialize_squall2d_x.o : \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../frame/module_timing.o \
		../frame/module_dm.o \
		../share/module_io_domain.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		module_init_utilities.o

module_initialize_squall2d_y.o : \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../frame/module_timing.o \
		../frame/module_dm.o \
		../share/module_io_domain.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		module_init_utilities.o

nest_init_utils.o: \
		../frame/module_domain.o \
		../frame/module_configure.o

start_em.o: module_bc_em.o \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_state_description.o \
		../frame/module_timing.o \
		../frame/module_dm.o \
		../share/module_io_domain.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		../share/module_date_time.o \
		../phys/module_physics_init.o 

solve_em.o:     module_small_step_em.o \
		module_em.o            \
		module_solvedebug_em.o \
                module_bc_em.o         \
		module_diffusion_em.o  \
		module_big_step_utilities_em.o \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_driver_constants.o \
		../frame/module_state_description.o \
		../frame/module_machine.o \
		../frame/module_tiles.o \
		../frame/module_trace.o \
		../frame/module_dm.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		../phys/module_radiation_driver.o \
		../phys/module_surface_driver.o \
		../phys/module_cumulus_driver.o \
		../phys/module_microphysics_driver.o \
		../phys/module_pbl_driver.o \
		../phys/module_physics_addtendc.o \
		module_check.o

solve_em_tl.o:  module_small_step_em.o \
		module_em.o            \
		module_em_tl.o         \
		module_solvedebug_em.o \
                module_bc_em.o         \
		module_diffusion_em.o  \
		module_big_step_utilities_em.o \
		module_big_step_utilities_em_tl.o \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_driver_constants.o \
		../frame/module_state_description.o \
		../frame/module_machine.o \
		../frame/module_tiles.o \
		../frame/module_trace.o \
		../frame/module_dm.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		../phys/module_radiation_driver.o \
		../phys/module_surface_driver.o \
		../phys/module_cumulus_driver.o \
		../phys/module_microphysics_driver.o \
		../phys/module_pbl_driver.o \
		../phys/module_physics_addtendc.o

solve_em_ad.o:  module_small_step_em.o \
		module_em.o            \
		module_em_ad.o         \
		module_solvedebug_em.o \
                module_bc_ad.o         \
                module_bc_em.o         \
                module_bc_em_ad.o      \
		module_diffusion_em.o  \
		module_diffusion_em_ad.o  \
		module_big_step_utilities_em.o \
		module_big_step_utilities_em_ad.o \
		module_linked_lisk.o \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_driver_constants.o \
		../frame/module_state_description.o \
		../frame/module_machine.o \
		../frame/module_tiles.o \
		../frame/module_trace.o \
		../frame/module_dm.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		../phys/module_radiation_driver.o \
		../phys/module_surface_driver.o \
		../phys/module_cumulus_driver.o \
		../phys/module_microphysics_driver.o \
		../phys/module_pbl_driver.o \
		../phys/module_physics_addtendc.o

solve_em_tst.o: solve_em_ad.o \
		solve_em_tl.o \
		module_small_step_em.o \
		module_em.o            \
		module_em_ad.o         \
		module_em_tl.o         \
		module_solvedebug_em.o \
                module_bc_em.o         \
		module_diffusion_em.o  \
		module_big_step_utilities_em.o \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_driver_constants.o \
		../frame/module_state_description.o \
		../frame/module_machine.o \
		../frame/module_tiles.o \
		../frame/module_trace.o \
		../frame/module_dm.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		../phys/module_radiation_driver.o \
		../phys/module_surface_driver.o \
		../phys/module_cumulus_driver.o \
		../phys/module_microphysics_driver.o \
		../phys/module_pbl_driver.o \
		../phys/module_physics_addtendc.o

test_solve_em.o: solve_em_ad.o \
		solve_em_tl.o \
		module_small_step_em.o \
		module_em.o            \
		module_em_ad.o         \
		module_em_tl.o         \
		module_solvedebug_em.o \
                module_bc_em.o         \
		module_diffusion_em.o  \
		module_big_step_utilities_em.o \
		../frame/module_domain.o \
		../frame/module_configure.o  \
		../frame/module_driver_constants.o \
		../frame/module_state_description.o \
		../frame/module_machine.o \
		../frame/module_tiles.o \
		../frame/module_trace.o \
		../frame/module_dm.o \
		../share/module_model_constants.o \
		../share/module_bc.o \
		../phys/module_radiation_driver.o \
		../phys/module_surface_driver.o \
		../phys/module_cumulus_driver.o \
		../phys/module_microphysics_driver.o \
		../phys/module_pbl_driver.o \
		../phys/module_physics_addtendc.o

# DO NOT DELETE
