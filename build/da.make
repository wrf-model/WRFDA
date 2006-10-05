WRFVAR_OBJS        =	da_par_util.o           \
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
                        da_reporting.o \
                        module_wrf_error.o \
                        module_configure.o \
                        module_state_description.o \
                        module_timing.o \
                        module_driver_constants.o \
                        module_domain.o \
                        module_machine.o \
                        module_utility.o \
                        module_symbols_util.o \
                        module_date_time.o \
                        module_io_wrf.o \
                        module_io.o \
                        module_io_domain.o \
                        module_io_quilt.o \
                        module_dm.o \
                        module_bc.o \
                        module_model_constants.o \
                        module_internal_header_util.o \
                        module_integrate.o \
                        module_nesting.o \
                        module_tiles.o \
                        module_quilt_outbuf_ops.o \
                        module_get_file_names.o \
                        module_bc_time_utilities.o \
                        solve_interface.o \
                        mediation_feedback_domain.o \
                        mediation_force_domain.o \
                        mediation_interp_domain.o \
                        nest_init_utils.o \
                        landread.o \
                        wrf_io.o \
                        io_int.o \
                        da_memory.o \
                        da_solve.o \
                        wrf_debug.o \
                        set_timekeeping.o \
                        wrf_shutdown.o \
                        init_modules.o \
                        mediation_wrfmain.o \
                        mediation_integrate.o \
                        pack_utils.o \
                        Meat.o \
                        wrf_num_bytes_between.o \
                        input_wrf.o \
                        wrf_auxhist1in.o \
                        wrf_auxhist2out.o \
                        wrf_auxhist4in.o \
                        wrf_auxhist5out.o \
                        wrf_auxinput2in.o \
                        wrf_auxinput3out.o \
                        wrf_auxinput5in.o \
                        wrf_auxhist1out.o \
                        wrf_auxhist3in.o \
                        wrf_auxhist4out.o \
                        wrf_auxinput1in.o \
                        wrf_auxinput2out.o \
                        wrf_auxinput4in.o \
                        wrf_auxinput5out.o \
                        wrf_auxhist2in.o \
                        wrf_auxhist3out.o \
                        wrf_auxhist5in.o \
                        wrf_auxinput1out.o \
                        wrf_auxinput3in.o \
                        wrf_auxinput4out.o \
                        wrf_bdyin.o \
                        wrf_bdyout.o \
                        wrf_restartin.o \
                        wrf_restartout.o \
                        output_wrf.o \
                        wrf_restartin.o \
                        wrf_histin.o \
                        wrf_histout.o \
                        wrf_inputout.o \
                        wrf_inputin.o \
                        wrf_ext_read_field.o \
                        wrf_ext_write_field.o \
                        collect_on_comm.o \
                        field_routines.o \
                        start_domain.o \
                        interp_fcn.o \
                        couple_or_uncouple_em.o

inc/da_generic_boilerplate.inc: da_generic_boilerplate.m4
	@ $(RM) inc/da_generic_boilerplate.inc
	  $(M4) da_generic_boilerplate.m4 > inc/da_generic_boilerplate.inc

da_utils : da_diagnostics \
           da_ominusb \
           da_tune \
           da_update_bc

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

update_bc : da_update_bc

da_update_bc : da_update_bc.exe

da_update_bc.exe : da_update_bc.o
	$(LD) $(LDFLAGS) -L$(NETCDF_PATH)/lib -o da_update_bc.exe da_update_bc.o \
           da_netcdf_interface.o \
           da_module_couple_uv.o $(NETCDF_LIB) $(EXTRA_LIBS)
	@ $(RM) ../main/da_update_bc.exe
	cp da_update_bc.exe ../main

da_write_sl_2_be: da_write_sl_2_be.o
	$(LD) -o $@.exe $@.o da_module_io.o  da_module_trans.o \
           da_module_define.o

grabbufr: grabbufr.o
	$(LD) -o $@.exe $@.o

