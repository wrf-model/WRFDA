SHARE_MODULES =                       \
	module_bc.o	        \
        module_bc_ad.o                  \
        module_bc_tl.o                  \
	module_bc_time_utilities.o	\
	module_io_wrf.o	        \
	module_date_time.o	\
	module_get_file_names.o	\
	module_io_domain.o	\
	module_model_constants.o \
	module_optional_si_input.o	\
	module_compute_geop.o           \
	module_soil_pre.o

SHARE_OBJS    = 		\
	mediation_integrate.o		\
	mediation_interp_domain.o	\
	mediation_force_domain.o	\
	mediation_feedback_domain.o	\
        mediation_nest_move.o           \
	mediation_wrfmain.o		\
	solve_interface.o               \
        start_domain.o                  \
        init_modules.o                  \
        set_timekeeping.o               \
        interp_fcn.o sint.o             \
        input_wrf.o                     \
        output_wrf.o                    \
        landread.o                      \
        wrf_ext_write_field.o           \
        wrf_ext_read_field.o            \
        wrf_inputout.o        		\
        wrf_auxinput1out.o		\
        wrf_auxinput2out.o		\
        wrf_auxinput3out.o		\
        wrf_auxinput4out.o		\
        wrf_auxinput5out.o		\
        wrf_histout.o			\
        wrf_auxhist1out.o		\
        wrf_auxhist2out.o		\
        wrf_auxhist3out.o		\
        wrf_auxhist4out.o		\
        wrf_auxhist5out.o		\
        wrf_restartout.o		\
        wrf_bdyout.o			\
        wrf_inputin.o         	  	\
        wrf_auxhist1in.o                \
        wrf_auxhist2in.o                \
        wrf_auxhist3in.o                \
        wrf_auxhist4in.o                \
        wrf_auxhist5in.o                \
        wrf_auxinput1in.o               \
        wrf_auxinput2in.o               \
        wrf_auxinput3in.o               \
        wrf_auxinput4in.o               \
        wrf_auxinput5in.o               \
        wrf_bdyin.o             	\
        wrf_histin.o            	\
        wrf_restartin.o

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

#module_wrf_top.o :

module_wrf_esmf_super.o : module_integrate_ad.o \
                          module_integrate_tl.o \
                          module_integrate_tst.o

solve_interface.o: solve_em.int module_domain.o module_configure.o \
		module_timing.o module_driver_constants.o \
		module_wrf_error.o

start_domain: start_domain_em.int module_domain.o module_configure.o

module_bc.o: module_configure.o module_state_description.o \
		module_wrf_error.o

module_bc_ad.o: module_configure.o module_state_description.o \
		module_wrf_error.o

module_bc_tl.o: module_configure.o module_state_description.o \
		module_wrf_error.o

module_bc_time_utilities.o: $(ESMF_MOD_DEPENDENCE)

module_get_file_names.o:  module_dm.o

module_io_domain.o: module_io_wrf.o module_date_time.o module_io.o  \
		module_domain.o module_configure.o \
		module_state_description.o

module_io_wrf.o: module_date_time.o module_bc_time_utilities.o \
		module_wrf_error.o module_domain.o \
		module_state_description.o module_configure.o \
		module_io.o module_timing.o \
		$(ESMF_MOD_DEPENDENCE)

output_wrf.o:   module_io.o module_wrf_error.o \
                module_domain.o module_state_description.o \
                module_configure.o module_io_wrf.o  \
		$(ESMF_MOD_DEPENDENCE)

input_wrf.o:    module_io.o module_wrf_error.o \
                module_domain.o module_state_description.o \
                module_configure.o module_io_wrf.o  \
		$(ESMF_MOD_DEPENDENCE)

wrf_ext_write_field.o : module_io.o module_wrf_error.o \
                module_domain.o module_timing.o

wrf_ext_read_field.o : module_io.o module_wrf_error.o \
                module_domain.o module_timing.o

module_date_time.o: module_wrf_error.o module_configure.o

module_soil_pre.o: module_date_time.o module_state_description.o

module_optional_si_input.o: module_io_wrf.o module_io_domain.o \
		module_domain.o module_configure.o

mediation_wrfmain.o: module_domain.o module_configure.o \
		module_timing.o $(ESMF_MOD_DEPENDENCE) \
		module_bc_time_utilities.o module_io_domain.o

init_modules.o: module_configure.o module_driver_constants.o \
		module_domain.o module_machine.o \
		module_nesting.o module_timing.o \
		module_tiles.o module_io.o \
		module_io_quilt.o module_dm.o \
		io_int.o \
		 module_io_wrf.o module_bc.o module_model_constants.o 

interp_fcn.o: module_timing.o module_state_description.o module_configure.o \
		module_wrf_error.o

mediation_feedback_domain.o: module_domain.o module_configure.o

mediation_force_domain.o: module_domain.o module_configure.o

mediation_integrate.o: module_domain.o module_configure.o \
			module_timing.o \
			$(ESMF_MOD_DEPENDENCE) \
			module_date_time.o module_bc_time_utilities.o \
			module_compute_geop.o                         \
			module_io_domain.o


mediation_interp_domain.o: module_domain.o module_configure.o \
			module_timing.o

#mediation_conv_emissions.o: module_domain.o module_configure.o \
#			ESMF_Mod.o \
#			module_date_time.o module_bc_time_utilities.o \
#			module_io_domain.o

set_timekeeping.o: module_domain.o module_configure.o \
                   $(ESMF_MOD_DEPENDENCE)

wrf_inputout.o                  : module_domain.o \
				module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o 
wrf_auxinput1out.o              : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput2out.o              : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput3out.o              : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput4out.o              : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput5out.o              : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_histout.o                   : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist1out.o               : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist2out.o               : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist3out.o               : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist4out.o               : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist5out.o               : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_restartout.o                : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_bdyout.o                    : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_inputin.o                   : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist1in.o                : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist2in.o                : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist3in.o                : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist4in.o                : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxhist5in.o                : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput1in.o               : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput2in.o               : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput3in.o               : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput4in.o               : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_auxinput5in.o               : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_bdyin.o                     : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_histin.o                    : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o
wrf_restartin.o                 : module_domain.o \
                                module_configure.o module_io.o module_io_wrf.o module_bc_time_utilities.o


# DO NOT DELETE
