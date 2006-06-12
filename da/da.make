DA_OBJS        =	da_solve_v3d.o		\
			da_par_util.o           \
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
                        da_memory.o             \
			rttov_const.o		\
			rttov_global.o		\
			rttov_types.o		\
			parkind1.o	        \
			gsi_kinds.o		\
			gsi_constants.o		\
                        da_wrfvar_io.o      \
	   		da_wrfvar_top.o

libwrfvar.a : $(DA_OBJS)
	$(AR) libwrfvar.a $(DA_OBJS)
	$(RANLIB) libwrfvar.a

inc/da_generic_boilerplate.inc: da_generic_boilerplate.m4
	@ $(RM) inc/da_generic_boilerplate.inc
	  $(M4) da_generic_boilerplate.m4 > inc/da_generic_boilerplate.inc

