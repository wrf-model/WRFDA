# DYN_EM

DYN_MODULES_WRFPLUS =         		\
        module_advect_em_ad.o  		\
        module_advect_em_tl.o  		\
	module_diffusion_em_ad.o	\
	module_diffusion_em_tl.o	\
	module_small_step_em_ad.o 	\
	module_small_step_em_tl.o 	\
        module_big_step_utilities_em_ad.o  \
        module_big_step_utilities_em_tl.o  \
        module_em_ad.o         		\
        module_em_tl.o         		\
        module_bc_em_ad.o               \
        module_bc_em_tl.o               \
        module_check.o

DYN_MODULES =                 		\
        module_advect_em.o   		\
	module_diffusion_em.o  		\
	module_small_step_em.o 		\
        module_big_step_utilities_em.o  \
        module_em.o         		\
        module_solvedebug_em.o    	\
        module_bc_em.o                  \
        module_init_utilities.o         \
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

DYN_OBJS_WRFPLUS = 	        \
	solve_em_ad.o           \
	solve_em_tl.o

DYN_OBJS    = 		        \
	init_modules_em.o       \
	solve_em.o              \
        start_em.o              \
        shift_domain_em.o       \
        nest_init_utils.o	\
        interp_domain_em.o

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

# DO NOT DELETE
