# dyn_em

DYN_OBJS = module_init_utilities.o

DYN_EM_OBJS = \
	module_advect_em.o \
	module_diffusion_em.o \
	module_small_step_em.o \
	module_big_step_utilities_em.o \
	module_em.o \
	module_solvedebug_em.o \
	module_bc_em.o \
        nest_init_utils.o \
	init_modules_em.o \
	solve_em.o \
	start_em.o \
	shift_domain_em.o \
	interp_domain_em.o
