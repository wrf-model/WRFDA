# DYN_EM

DYN_MODULES =                 		\
        module_advect_em.o   		\
	module_diffusion_em.o  		\
	module_small_step_em.o 		\
        module_big_step_utilities_em.o  \
        module_em.o         		\
        module_solvedebug_em.o    	\
        module_bc_em.o                  \
        module_init_utilities.o

DYN_OBJS    = 		        \
	init_modules_em.o       \
	solve_em.o              \
        start_em.o              \
        shift_domain_em.o       \
        nest_init_utils.o	\
        interp_domain_em.o

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

# DO NOT DELETE
