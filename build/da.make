DA_MODULES        =	da_par_util.o           \
			da_par_util1.o          \
			da_setup_structures.o	\
			da_minimisation.o	\
			da_vtox_transforms.o	\
			da_obs.o		\
			da_metar.o		\
			da_geoamv.o	        \
			da_polaramv.o	        \
			da_ships.o		\
			da_synop.o		\
			da_sound.o		\
			da_bogus.o		\
			da_airep.o		\
			da_pilot.o		\
			da_radar.o		\
			da_gpspw.o		\
			da_gpsref.o		\
			da_ssmi.o		\
			da_satem.o		\
			da_qscat.o		\
			da_pseudo.o		\
			da_profiler.o		\
			da_buoy.o   		\
			da_dynamics.o		\
			da_physics.o		\
			da_ffts.o		\
			da_test.o		\
			da_tools.o		\
			da_recursive_filter.o	\
			da_interpolation.o	\
			da_grid_definitions.o	\
			da_statistics.o		\
			da_define_structures.o	\
			da_constants.o		\
			da_spectral.o           \
			da_radiance.o		\
                        da_tracing.o            \
			rttov_const.o		\
			rttov_global.o		\
			rttov_types.o		\
			parkind1.o	        \
			gsi_kinds.o		\
			gsi_constants.o		\
                        da_wrfvar_io.o      \
                        da_airsr.o          \
	   		da_wrfvar_top.o     \
                        da_reporting.o

DA_OBJS        =	da_memory.o \
                        da_solve.o

inc/da_generic_boilerplate.inc: da_generic_boilerplate.m4
	@ $(RM) inc/da_generic_boilerplate.inc
	  $(M4) da_generic_boilerplate.m4 > inc/da_generic_boilerplate.inc

da_utils : da_diagnostics \
           da_generate_difference \
           da_ominusb \
           da_just_be \
           da_to_be_file \
           da_tune \
           da_update_bc \
           da_write_sl_2_be

da_plots : da_scale_length \
           da_plot_eigen \
           da_plot_eigen_in_be \
           da_plot_eigen_gen_be
           

da_be4_scale_length: da_be4_scale_length.o
	$(LD) -o $@.exe $@.o

da_scale_length: da_scale_length.o
	$(LD) -o $@.exe $@.o da_constants.o

da_diagnostics: da_diagnostics.o
	$(LD) -o $@.exe $@.o

da_generate_difference: da_generate_difference.o
	$(LD) -o $@.exe $@.o da_module_io.o da_module_define.o

da_ominusb: da_ominusb.o
	$(LD) -o $@.exe $@.o

da_just_be: da_just_be.o
	$(LD) -o $@.exe $@.o  da_module_io.o  da_module_trans.o \
           da_module_define.o

da_plot_eigen: da_plot_eigen.o
	$(LD) -o $@.exe $@.o

da_plot_eigen_in_be: da_plot_eigen_in_be.o
	$(LD) -o $@.exe $@.o

da_plot_eigen_gen_be: da_plot_eigen_gen_be.o
	$(LD) -o $@.exe $@.o

da_to_be_file: da_to_be_file.o
	$(LD) -o $@.exe $@.o da_module_io.o  da_module_trans.o \
           da_module_define.o

da_tune: da_tune.o
	$(LD) -o $@.exe $@.o

da_update_bc: da_update_bc.o
	$(LD) -L${NETCDF_PATH}/lib -o $@.exe $@.o da_netcdf_interface.o \
           da_module_couple_uv.o ${NETCDF_LIB}

da_write_sl_2_be: da_write_sl_2_be.o
	$(LD) -o $@.exe $@.o da_module_io.o  da_module_trans.o \
           da_module_define.o

grabbufr: grabbufr.o
	$(LD) -o $@.exe $@.o

