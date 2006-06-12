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
        wrf_restartin.o                 \
        couple_or_uncouple_em.o

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

# DO NOT DELETE
