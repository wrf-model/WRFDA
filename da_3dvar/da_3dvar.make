DA_OBJS        =	da_solve_v3d.o		\
			par_util.o              \
			par_util1.o             \
			DA_Setup_Structures.o	\
			DA_Minimisation.o	\
			DA_VToX_Transforms.o	\
			DA_Obs.o		\
			DA_Metar.o		\
			DA_GeoAMV.o	        \
			DA_PolarAMV.o	        \
			DA_Ships.o		\
			DA_Synop.o		\
			DA_Sound.o		\
			DA_Bogus.o		\
			DA_Airep.o		\
			DA_Pilot.o		\
			DA_Radar.o		\
			DA_Gpspw.o		\
			DA_Gpsref.o		\
			DA_SSMI.o		\
			DA_Satem.o		\
			da_qscat.o		\
			da_pseudo.o		\
			DA_Profiler.o		\
			DA_Buoy.o   		\
			DA_ReadWrite_MM5.o	\
			DA_Dynamics.o		\
			DA_Physics.o		\
			DA_FFTs.o		\
			DA_Test.o		\
			DA_Tools.o		\
			DA_Recursive_Filter.o	\
			DA_Interpolation.o	\
			DA_Grid_Definitions.o	\
			DA_Statistics.o		\
			DA_Define_Structures.o	\
	                da_bufrlib.o 		\
	                bort_exit.o 		\
	                restd.o 		\
	                wrdesc.o 		\
			da_h_ops.o		\
			da_c_mat.o		\
			DA_Constants.o		\
			LAPACK.o		\
			da_spectral.o           \
			da_fftpack5.o           \
			da_radiance.o		\
                        da_tracing.o            \
                        da_memory.o             \
			rttov_const.o		\
			rttov_global.o		\
			rttov_types.o		\
			parkind1.o	        \
			gsi_kinds.o		\
			gsi_constants.o		\
			BLAS.o                  \
                        module_wrf_3dvar_io.o \
                        module_wrf_3dvar_interface.o	\
	   		module_wrfvar_top.o

libwrfvar.a : $(DA_OBJS)
	$(AR) ru libwrfvar.a $(DA_OBJS)
	$(RANLIB) libwrfvar.a

##########################################################################

wrfvar.o : module_wrfvar_top.o

module_wrfvar_top.o : module_wrf_3dvar_interface.o \
                      module_integrate.o \
                      module_wrf_3dvar_io.o

module_wrf_3dvar_io.o : module_io_domain.o  \
                        da_tracing.o

generic_boilerplate.inc: generic_boilerplate.m4
			$(RM) generic_boilerplate.inc
			$(M4) generic_boilerplate.m4 > generic_boilerplate.inc

par_util.o:		par_util.F                     \
			alloc_and_copy_be_arrays.inc   \
			be_local_copy.inc              \
			copy_dims.inc                  \
			copy_tile_dims.inc             \
			cv_to_vv.inc                   \
			local_to_global.inc            \
			mm5_struct_bcast.inc           \
			pack_count_obs.inc             \
			proc_maxmin_combine.inc        \
			proc_stats_combine.inc         \
			proc_sum_count_obs.inc         \
			transpose.inc                  \
			unpack_count_obs.inc           \
			vv_to_cv.inc                   \
			wrf_dm_interface.inc           \
			cv_to_global.inc               \
			generic_typedefs.inc           \
			generic_methods.inc            \
			specific_methods.inc           \
			generic_boilerplate.inc        \
			y_facade_to_global.inc         \
                        DA_Define_Structures.o	       \
			DA_Constants.o

par_util1.o:		par_util1.F                     \
			proc_sum_int.inc               \
			proc_sum_real.inc \
                        module_wrf_error.o

module_wrf_3dvar_interface.o : module_wrf_3dvar_interface.F \
                        module_domain.o                     \
                        module_tiles.o                      \
                        module_dm.o                         \
                        module_model_constants.o            \
                        da_solve_v3d.o

da_solve_v3d.o:		DA_Constants.o			\
			DA_Define_Structures.o		\
			DA_Setup_Structures.o		\
			DA_Test.o			\
			DA_Tools.o			\
			DA_Minimisation.o		\
			par_util.o			\
			da_init_wrfvar.inc              \
			da_solve_v3d.F

DA_Minimisation.o:	DA_Minimisation.F             \
                        module_wrf_3dvar_io.o         \
                        module_get_file_names.o       \
			DA_Constants.o                \
			DA_Define_Structures.o        \
			DA_VToX_Transforms.o          \
			DA_Obs.o                      \
			DA_Metar.o                    \
			DA_GeoAMV.o                   \
			DA_PolarAMV.o                 \
			DA_Ships.o                    \
			DA_Synop.o                    \
			DA_Sound.o                    \
			DA_Bogus.o                    \
			DA_Airep.o                    \
			DA_Pilot.o                    \
			DA_Radar.o                    \
			DA_Gpspw.o                    \
			DA_Gpsref.o                   \
			DA_SSMI.o                     \
			DA_Satem.o                    \
			da_pseudo.o                   \
			da_qscat.o                    \
			DA_Profiler.o                 \
			DA_Buoy.o                     \
			DA_Setup_Structures.o	      \
			da_radiance.o                 \
			da_calculate_j.inc            \
			da_calculate_jo_and_grady.inc \
			da_calculate_residual.inc     \
			da_get_var_diagnostics.inc    \
			da_get_innov_vector.inc       \
			da_dot.inc                    \
			da_dot_cv.inc                 \
			da_minimisation_warning.inc   \
			da_sum_reals.inc              \
			da_vd05bd.inc                 \
			da_write_diagnostics.inc      \
			DA_Calculate_GradY.inc        \
			DA_Minimise_CG.inc

DA_Setup_Structures.o:	DA_Setup_Structures.F             \
                        module_wrf_3dvar_io.o             \
			DA_Define_Structures.o            \
			DA_Constants.o                    \
			DA_Grid_Definitions.o             \
			DA_Obs.o                          \
			DA_SSMI.o                         \
			DA_VToX_Transforms.o              \
			DA_Physics.o                      \
			da_h_ops.o                        \
	                da_bufrlib.o                      \
	                bort_exit.o                       \
	                restd.o                           \
	                wrdesc.o                          \
	                DA_ReadWrite_MM5.o                \
	                da_spectral.o                     \
			da_radiance.o                     \
			da_add_increments.inc             \
			DA_Add_PBL_And_SFC_Info.inc       \
			da_chgvres.inc                    \
			da_get_vertical_truncation.inc    \
			da_interpolate_regcoeff.inc       \
			da_interpolate_stats.inc          \
			da_write_interpolated_be.inc      \
			da_rescale_background_errors.inc  \
			da_setup_background_errors.inc    \
			DA_Get_Bins_Info.inc              \
			da_setup_be_global.inc            \
			da_setup_be_regional.inc          \
			da_setup_firstguess.inc           \
			da_setup_firstguess_mm5.inc       \
			da_setup_firstguess_wrf.inc       \
			da_setup_firstguess_kma.inc       \
			da_setup_obs_structures.inc       \
			da_setup_obs_structures_ascii.inc \
			da_setup_obs_structures_bufr.inc  \
			da_setup_obs_interp_wts.inc       \
			da_setup_runconstants.inc         \
			da_transfer_mm5toxb.inc           \
			da_transfer_wrftoxb.inc           \
			da_transfer_kmatoxb.inc           \
			da_transfer_xatowrf.inc           \
			da_transfer_xatokma.inc           \
			DA_Write_KMA_Increments.inc       \
			da_transfer_xatowrftl.inc         \
			da_transfer_xatowrftl_adj.inc     \
			da_transfer_wrftltoxa.inc         \
			da_transfer_wrftltoxa_adj.inc     \
			DA_Transfer_XatoAnalysis.inc      \
			da_write_increments.inc           \
                        da_setup_cv.inc

bufrlib.prm:		bufrlib.PRM
			$(RM) $@
			$(CPP) $(FPPFLAGS_BUFR) bufrlib.PRM > bufrlib.prm

da_bufrlib.o:		da_bufrlib.F bufrlib.prm
			$(RM) $@
			$(CPP) $(FPPFLAGS) da_bufrlib.F > da_bufrlib.f
			$(FFC) -c $(FIXEDFLAGS_BUFR) da_bufrlib.f

bort_exit.o:		bort_exit.c
			$(RM) $@
			$(CC) -c $(CCFLAGS_BUFR) bort_exit.c

restd.o:		restd.c
			$(RM) $@
			$(CC) -c $(CCFLAGS_BUFR) restd.c

wrdesc.o:		wrdesc.c
			$(RM) $@
			$(CC) -c $(CCFLAGS_BUFR) wrdesc.c

DA_VToX_Transforms.o:	DA_VToX_Transforms.F              \
                        module_tiles.o                    \
			par_util.o                        \
			DA_Define_Structures.o            \
			DA_Tools.o                        \
			DA_Recursive_Filter.o             \
			DA_Constants.o                    \
			DA_Dynamics.o                     \
			DA_Physics.o                      \
			da_fftpack5.o                     \
			da_spectral.o                     \
			DA_SSMI.o                         \
                        da_tracing.o                      \
			DA_Transform_VToVv.inc            \
			DA_Transform_VToVv_Adj.inc        \
			DA_Transform_VToVv_Global.inc     \
			DA_Transform_VToVv_Global_Adj.inc \
			da_transform_vtox.inc             \
			da_transform_vtox_adj.inc         \
			DA_Transform_VpToVv.inc           \
			DA_Transform_VpToX.inc            \
			DA_Transform_VpToX_Adj.inc        \
			DA_Transform_VvToVp.inc           \
			DA_Transform_VvToVp_Adj.inc       \
			DA_Get_VPoles.inc                 \
			DA_Get_SPoles.inc                 \
			DA_Get_AVPoles.inc                \
			DA_Get_ASPoles.inc                \
			DA_Vertical_Transform.inc         \
                        DA_Check_EOF_Decomposition.inc

DA_Obs.o:		DA_Obs.F                   \
			DA_Constants.o             \
			DA_Define_Structures.o     \
			DA_Airep.o                 \
			DA_Gpspw.o                 \
			DA_Gpsref.o                \
			DA_Metar.o                 \
			DA_Pilot.o                 \
			DA_Radar.o                 \
			DA_SSMI.o                  \
			DA_GeoAMV.o                \
			DA_PolarAMV.o              \
			DA_Satem.o                 \
			DA_Ships.o                 \
			DA_Synop.o                 \
			DA_Sound.o                 \
			DA_Bogus.o                 \
			da_pseudo.o                \
			da_qscat.o                 \
			da_radiance.o              \
			DA_Profiler.o              \
			DA_Buoy.o                  \
			par_util.o                 \
	                DA_Obs_Proc_Station.inc    \
			DA_Read_Obs.inc            \
			DA_Scan_Obs.inc            \
			DA_Read_BUFR_Obs.inc       \
			DA_Scan_BUFR_Obs.inc       \
			DA_Read_Radar.inc          \
			DA_Scan_Radar.inc          \
			DA_Transform_XToY.inc      \
			DA_Transform_XToY_Adj.inc  \
			da_add_noise_to_ob.inc     \
			da_check_missing.inc       \
			da_fill_obs_structures.inc \
			da_random_omb_all.inc      \
			da_read_errfac.inc         \
			da_setup_pseudo_obs.inc    \
			da_store_obs_grid_info.inc \
			da_use_obs_errfac.inc      \
			da_write_obs.inc           \
			da_write_filtered_obs.inc  \
			da_write_y.inc             \
                        da_deallocate_obs.inc

da_pseudo.o:		da_pseudo.F                      \
			DA_Constants.o                   \
			par_util.o                       \
			DA_Define_Structures.o           \
			DA_Interpolation.o               \
			DA_Statistics.o                  \
			DA_Tools.o                       \
			da_cal_jo_and_grady_pseudo.inc   \
			da_calculate_residual_pseudo.inc \
			da_get_innov_vector_pseudo.inc   \
			da_oa_stats_pseudo.inc           \
			da_ob_stats_pseudo.inc           \
			da_print_stats_pseudo.inc        \
			da_transform_xtoy_pseudo.inc     \
			da_transform_xtoy_pseudo_adj.inc

DA_Metar.o:		DA_Metar.F                          \
			DA_Constants.o                      \
			par_util.o                          \
			DA_Define_Structures.o              \
			DA_Interpolation.o                  \
			DA_Statistics.o                     \
			DA_Tools.o                          \
			DA_Physics.o                        \
			DA_AO_Stats_Metar.inc               \
			DA_Calculate_Jo_and_GradY_Metar.inc \
			DA_Calculate_Residual_Metar.inc     \
			DA_OI_Stats_Metar.inc               \
			DA_Print_Stats_Metar.inc            \
			DA_Transform_XToY_Metar.inc         \
			DA_Transform_XToY_Metar_Adj.inc     \
			da_check_max_iv_metar.inc           \
			da_get_innov_vector_metar.inc

DA_GeoAMV.o:		DA_GeoAMV.F                      \
			DA_Constants.o                   \
			par_util.o                       \
			DA_Define_Structures.o           \
			DA_Interpolation.o               \
			DA_Statistics.o                  \
			DA_Tools.o                       \
			DA_AO_Stats_GeoAMV.inc           \
			DA_Get_Jo_and_GradY_GeoAMV.inc   \
			DA_Calculate_Residual_GeoAMV.inc \
			DA_OI_Stats_GeoAMV.inc           \
			DA_Print_Stats_GeoAMV.inc        \
			DA_Transform_XToY_GeoAMV.inc     \
			DA_Transform_XToY_GeoAMV.inc     \
			da_check_max_iv_geoamv.inc       \
			da_get_innov_vector_geoamv.inc

DA_PolarAMV.o:		\
			DA_Constants.o \
			par_util.o \
			DA_Define_Structures.o \
			DA_Interpolation.o \
			DA_Statistics.o \
			DA_Tools.o \
			DA_PolarAMV.F                      \
			DA_AO_Stats_PolarAMV.inc           \
			DA_Get_Jo_and_GradY_PolarAMV.inc   \
			DA_Calculate_Residual_PolarAMV.inc \
			DA_OI_Stats_PolarAMV.inc           \
			DA_Print_Stats_PolarAMV.inc        \
			DA_Transform_XToY_PolarAMV.inc     \
			DA_Transform_XToY_PolarAMV.inc     \
			da_check_max_iv_polaramv.inc       \
			da_get_innov_vector_polaramv.inc

DA_Satem.o:		DA_Satem.F                          \
			DA_Physics.o                        \
			DA_Constants.o                      \
			par_util.o                          \
			DA_Define_Structures.o              \
			DA_Interpolation.o                  \
			DA_Statistics.o                     \
			DA_Tools.o                          \
			DA_AO_Stats_Satem.inc               \
			DA_Calculate_Jo_and_GradY_Satem.inc \
			DA_Calculate_Residual_Satem.inc     \
			DA_OI_Stats_Satem.inc               \
			DA_Print_Stats_Satem.inc            \
			DA_Transform_XToY_Satem.inc         \
			DA_Transform_XToY_Satem_Adj.inc     \
			da_check_max_iv_satem.inc           \
			da_get_innov_vector_satem.inc

DA_Ships.o:		DA_Ships.F                          \
			DA_Constants.o                      \
			par_util.o                          \
			DA_Define_Structures.o              \
			DA_Interpolation.o                  \
			DA_Statistics.o                     \
			DA_Tools.o                          \
			DA_Physics.o                        \
			DA_AO_Stats_Ships.inc               \
			DA_Calculate_Jo_and_GradY_Ships.inc \
			DA_Calculate_Residual_Ships.inc     \
			DA_OI_Stats_Ships.inc               \
			DA_Print_Stats_Ships.inc            \
			DA_Transform_XToY_Ships.inc         \
			DA_Transform_XToY_Ships_Adj.inc     \
			da_check_max_iv_ships.inc           \
			da_get_innov_vector_ships.inc

DA_Synop.o:		DA_Synop.F                          \
			DA_Constants.o                      \
			par_util.o                          \
			DA_Define_Structures.o              \
			DA_Interpolation.o                  \
			DA_Statistics.o                     \
			DA_Tools.o                          \
			DA_Physics.o                        \
			da_check_max_iv_synop.inc           \
			da_get_innov_vector_synop.inc       \
			DA_AO_Stats_Synop.inc               \
			DA_Calculate_Jo_and_GradY_Synop.inc \
			compute_jo_synop_uvtq.inc           \
			DA_Calculate_Residual_Synop.inc     \
			DA_OI_Stats_Synop.inc               \
			DA_Print_Stats_Synop.inc            \
			DA_Transform_XToY_Synop.inc         \
			DA_Transform_XToY_Synop_Adj.inc

DA_Sound.o:		DA_Sound.F                          \
			DA_Physics.o                        \
			DA_Constants.o                      \
			par_util.o                          \
			DA_Define_Structures.o              \
			DA_Interpolation.o                  \
			DA_Statistics.o                     \
			DA_Tools.o                          \
			DA_AO_Stats_Sound.inc               \
			DA_Calculate_Jo_and_GradY_Sound.inc \
			compute_jo_sound_uvtq.inc           \
			DA_Calculate_Residual_Sound.inc     \
			DA_OI_Stats_Sound.inc               \
			DA_Print_Stats_Sound.inc            \
			DA_Transform_XToY_Sound.inc         \
			DA_Transform_XToY_Sound_Adj.inc     \
			da_check_max_iv_sound.inc           \
			da_get_innov_vector_sound.inc       \
			da_obs_diagnostics.inc              \
			DA_AO_Stats_Sonde_sfc.inc           \
			DA_Get_Jo_and_GradY_Sonde_sfc.inc   \
			compute_jo_sonde_sfc_uvtq.inc       \
			DA_Calculate_Residual_Sonde_sfc.inc \
			DA_OI_Stats_Sonde_sfc.inc           \
			DA_Print_Stats_Sonde_sfc.inc        \
			DA_Transform_XToY_Sonde_sfc.inc     \
			DA_Transform_XToY_Sonde_sfc_Adj.inc \
			da_get_innov_vector_sonde_sfc.inc   \
			da_check_max_iv_sonde_sfc.inc       \
			DA_Calculate_GradY_Sonde_sfc.inc

DA_Airep.o:		DA_Airep.F                          \
			DA_Constants.o                      \
			par_util.o                          \
			DA_Define_Structures.o              \
			DA_Interpolation.o                  \
			DA_Statistics.o                     \
			DA_Tools.o                          \
			DA_AO_Stats_Airep.inc               \
			DA_Calculate_Jo_and_GradY_Airep.inc \
			DA_Calculate_Residual_Airep.inc     \
			DA_OI_Stats_Airep.inc               \
			DA_Print_Stats_Airep.inc            \
			DA_Transform_XToY_Airep.inc         \
			DA_Transform_XToY_Airep_Adj.inc     \
			da_check_max_iv_airep.inc           \
			da_get_innov_vector_airep.inc

DA_Pilot.o:		DA_Pilot.F                          \
			DA_Constants.o                      \
			par_util.o                          \
			DA_Define_Structures.o              \
			DA_Interpolation.o                  \
			DA_Statistics.o                     \
			DA_Tools.o                          \
			DA_AO_Stats_Pilot.inc               \
			DA_Calculate_Jo_and_GradY_Pilot.inc \
			DA_Calculate_Residual_Pilot.inc     \
			DA_OI_Stats_Pilot.inc               \
			DA_Print_Stats_Pilot.inc            \
			DA_Transform_XToY_Pilot.inc         \
			DA_Transform_XToY_Pilot_Adj.inc     \
			da_check_max_iv_pilot.inc           \
			da_get_innov_vector_pilot.inc

DA_Bogus.o:		DA_Bogus.F                          \
			DA_Constants.o                      \
			par_util.o                          \
			DA_Define_Structures.o              \
			DA_Interpolation.o                  \
			DA_Statistics.o                     \
			DA_Tools.o                          \
			DA_Physics.o                        \
			DA_AO_Stats_Bogus.inc               \
			DA_Calculate_Jo_and_GradY_Bogus.inc \
			DA_Calculate_Residual_Bogus.inc     \
			DA_OI_Stats_Bogus.inc               \
			DA_Print_Stats_Bogus.inc            \
			DA_Transform_XToY_Bogus.inc         \
			DA_Transform_XToY_Bogus_Adj.inc     \
			da_check_max_iv_bogus.inc           \
			da_get_innov_vector_bogus.inc

DA_Radar.o:		DA_Radar.F                          \
			DA_Constants.o                      \
			par_util.o                          \
			DA_Define_Structures.o              \
			DA_Interpolation.o                  \
			DA_Statistics.o                     \
			DA_Tools.o                          \
			DA_Radar.F                          \
			DA_AO_Stats_Radar.inc               \
			DA_Calculate_GradY_Radar.inc        \
			DA_Calculate_Jo_and_GradY_Radar.inc \
			DA_Calculate_Residual_Radar.inc     \
			DA_OI_Stats_Radar.inc               \
			DA_Print_Stats_Radar.inc            \
			DA_Transform_XToY_Radar.inc         \
			DA_Transform_XToY_Radar_Adj.inc     \
			da_check_max_iv_Radar.inc           \
			da_get_innov_vector_Radar.inc       \
			da_reflectivity.inc                 \
			da_reflectivity_Adj.inc             \
			da_reflectivity_Lin.inc             \
			da_radial_velocity.inc              \
			da_radial_velocity_Lin.inc          \
			da_radial_velocity_Adj.inc          \
			da_max_error_qc_Radar.inc

DA_Gpspw.o:		DA_Gpspw.F                          \
			DA_Constants.o                      \
			par_util.o                          \
			DA_Define_Structures.o              \
			DA_Interpolation.o                  \
			DA_Statistics.o                     \
			DA_Physics.o                        \
			DA_Tools.o                          \
			DA_AO_Stats_Gpspw.inc               \
			DA_Calculate_Jo_and_GradY_Gpspw.inc \
			DA_Calculate_Residual_Gpspw.inc     \
			DA_OI_Stats_Gpspw.inc               \
			DA_Print_Stats_Gpspw.inc            \
			da_check_max_iv_gpspw.inc           \
			da_get_innov_vector_gpspw.inc       \
			DA_Transform_XToY_Gpspw.inc         \
			DA_Transform_XToY_Gpspw_Adj.inc

DA_Gpsref.o:		DA_Gpsref.F                         \
			DA_Constants.o                      \
			par_util.o                          \
			DA_Define_Structures.o              \
			DA_Interpolation.o                  \
			DA_Statistics.o                     \
			DA_Tools.o                          \
			DA_AO_Stats_Gpsref.inc              \
			DA_Calculate_GradY_Gpsref.inc       \
			DA_Get_Jo_and_GradY_Gpsref.inc      \
			DA_Calculate_Residual_Gpsref.inc    \
			DA_OI_Stats_Gpsref.inc              \
			DA_Print_Stats_Gpsref.inc           \
			DA_Transform_XToY_Gpsref.inc        \
			DA_Transform_XToY_Gpsref_Adj.inc    \
			da_check_max_iv_Gpsref.inc          \
			da_get_innov_vector_Gpsref.inc

DA_SSMI.o:		DA_SSMI.F                           \
			DA_Constants.o                      \
			par_util.o                          \
			DA_Define_Structures.o              \
			DA_Interpolation.o                  \
			DA_Grid_Definitions.o               \
			DA_Physics.o                        \
			DA_Statistics.o                     \
			DA_Tools.o                          \
			DA_AO_Stats_SSMI.inc                \
			DA_AO_Stats_SSMI_Rv.inc             \
			DA_AO_Stats_SSMI_Tb.inc             \
			DA_Read_SSMI.inc                    \
			DA_Scan_SSMI.inc                    \
			DA_Cal_Jo_and_GradY_SSMI.inc        \
			DA_Cal_Jo_and_GradY_SSMI_Rv.inc     \
			DA_Cal_Jo_and_GradY_SSMI_Tb.inc     \
			DA_Cal_Residual_SSMI.inc            \
			DA_Cal_Residual_SSMI_Rv.inc         \
			DA_Cal_Residual_SSMI_Tb.inc         \
			DA_OI_Stats_SSMI.inc                \
			DA_OI_Stats_SSMI_Rv.inc             \
			DA_OI_Stats_SSMI_Tb.inc             \
			DA_Transform_XToSpeed.inc           \
			DA_Transform_XToSpeed_Lin.inc       \
			DA_Transform_XToSpeed_Adj.inc       \
			DA_Transform_XToSeaSfcWind.inc      \
			DA_Transform_XToSeaSfcWind_Lin.inc  \
			DA_Transform_XToSeaSfcWind_Adj.inc  \
			DA_Transform_XToTb.inc              \
			DA_Transform_XToTb_Lin.inc          \
			DA_Transform_XToTb_Adj.inc          \
			DA_Transform_XToY_SSMI.inc          \
			DA_Transform_XToY_SSMI_Adj.inc      \
			DA_Transform_XToY_SSMI_Rv.inc       \
			DA_Transform_XToY_SSMI_Rv_Adj.inc   \
			DA_Transform_XToY_SSMI_Tb.inc       \
			DA_Transform_XToY_SSMI_Tb_Adj.inc   \
			DA_Transform_XToZRhoQ.inc           \
			DA_Transform_XToZRhoQ_Lin.inc       \
			DA_Transform_XToZRhoQ_Adj.inc       \
			cal_sigma_v.inc                     \
			da_cal_jo_and_grady_ssmt1.inc       \
			da_cal_jo_and_grady_ssmt2.inc       \
			da_cal_residual_ssmt1.inc           \
			da_cal_residual_ssmt2.inc           \
			da_check_max_iv_ssmi_rv.inc         \
			da_check_max_iv_ssmi_tb.inc         \
			da_check_max_iv_ssmt1.inc           \
			da_check_max_iv_ssmt2.inc           \
			da_get_innov_vector_ssmi.inc        \
			da_get_innov_vector_ssmi_rv.inc     \
			da_get_innov_vector_ssmi_tb.inc     \
			da_get_innov_vector_ssmt1.inc       \
			da_get_innov_vector_ssmt2.inc       \
			da_oa_stats_ssmt1.inc               \
			da_oa_stats_ssmt2.inc               \
			da_ob_stats_ssmt1.inc               \
			da_ob_stats_ssmt2.inc               \
			da_print_stats_ssmt1.inc            \
			da_print_stats_ssmt2.inc            \
			da_transform_xtoy_ssmt1.inc         \
			da_transform_xtoy_ssmt1_adj.inc     \
			da_transform_xtoy_ssmt2.inc         \
			da_transform_xtoy_ssmt2_adj.inc

da_qscat.o:		da_qscat.F                          \
			DA_Constants.o                      \
			DA_Define_Structures.o              \
			DA_Interpolation.o                  \
			DA_Statistics.o                     \
			DA_Tools.o                          \
			par_util.o                          \
			da_calculate_jo_and_grady_qscat.inc \
			da_calculate_residual_qscat.inc     \
			da_check_max_iv_qscat.inc           \
			da_get_innov_vector_qscat.inc       \
			da_oa_stats_qscat.inc               \
			da_ob_stats_qscat.inc               \
			da_print_stats_qscat.inc            \
			da_transform_xtoy_qscat.inc         \
			da_transform_xtoy_qscat_adj.inc

DA_Profiler.o:		DA_Profiler.F                       \
			DA_Constants.o                      \
			par_util.o                          \
			DA_Define_Structures.o              \
			DA_Interpolation.o                  \
			DA_Statistics.o                     \
			DA_Tools.o                          \
			DA_AO_Stats_Profiler.inc            \
			DA_Get_Jo_and_GradY_Profiler.inc    \
			DA_Calculate_Residual_Profiler.inc  \
			DA_OI_Stats_Profiler.inc            \
			DA_Print_Stats_Profiler.inc         \
			DA_Transform_XToY_Profiler.inc      \
			DA_Transform_XToY_Profiler_Adj.inc  \
			da_check_max_iv_profiler.inc        \
			da_get_innov_vector_profiler.inc


DA_Buoy.o:		DA_Buoy.F                           \
			DA_Constants.o                      \
			par_util.o                          \
			DA_Define_Structures.o              \
			DA_Interpolation.o                  \
			DA_Statistics.o                     \
			DA_Tools.o                          \
			DA_Physics.o                        \
			DA_AO_Stats_Buoy.inc                \
			DA_Calculate_Jo_and_GradY_Buoy.inc  \
			DA_Calculate_Residual_Buoy.inc      \
			DA_OI_Stats_Buoy.inc                \
			DA_Print_Stats_Buoy.inc             \
			DA_Transform_XToY_Buoy.inc          \
			DA_Transform_XToY_Buoy_Adj.inc      \
			da_check_max_iv_buoy.inc            \
			da_get_innov_vector_buoy.inc

DA_ReadWrite_MM5.o:	DA_ReadWrite_MM5.F                  \
	                DA_Constants.o                      \
			DA_Define_Structures.o              \
			DA_Grid_Definitions.o               \
			par_util.o                          \
			da_h_ops.o                          \
			DA_crs_to_dot.inc                   \
			DA_cleanrows.inc                    \
			DA_print_big_header.inc             \
			DA_print_sub_header.inc             \
			DA_Read_MM5.inc                     \
			DA_Write_Analysis_MM5.inc

da_h_ops.o:		da_h_ops.F                          \
			da_c_mat.o                          \
			a2b.inc                             \
			a2c.inc                             \
			b2a.inc                             \
			c2a.inc                             \
			graph.inc                           \
			hops2.inc


da_c_mat.o:		da_c_mat.F                          \
			cpt_constd.inc                      \
			cpt_consts.inc                      \
			pmat1.inc                           \
			pmat2.inc                           \
			powmat.inc                          \
			set_vops.inc                        \
                        DA_Constants.o

DA_Dynamics.o:		DA_Dynamics.F                       \
			DA_Constants.o                      \
			DA_Define_Structures.o              \
			DA_Statistics.o                     \
			DA_FFTs.o                           \
			DA_Interpolation.o                  \
			DA_Balance_CycloTerm.inc            \
			DA_Balance_CycloTerm_Adj.inc        \
			DA_Balance_CycloTerm_Lin.inc        \
			DA_Balance_Equation_Adj.inc         \
			DA_Balance_Equation_Lin.inc         \
			DA_Balance_GeoTerm.inc              \
			DA_Balance_GeoTerm_Adj.inc          \
			DA_Balance_GeoTerm_Lin.inc          \
			DA_HydrostaticP_To_Rho_Adj.inc      \
			DA_HydrostaticP_To_Rho_Lin.inc      \
			DA_PsiChi_To_UV.inc                 \
			DA_PsiChi_To_UV_Adj.inc             \
			DA_UV_To_Divergence.inc             \
			DA_UV_To_Divergence_Adj.inc         \
			DA_W_Adjustment_Lin.inc             \
			DA_W_Adjustment_Adj.inc             \
			DA_WZ_BASE.inc                      \
			DA_UV_To_Vorticity.inc

DA_Physics.o:		DA_Physics.F                        \
			DA_Constants.o                      \
			DA_Define_Structures.o              \
			DA_Grid_Definitions.o               \
			DA_Interpolation.o                  \
			DA_Dynamics.o                       \
			DA_UVPRho_To_W_Adj.inc              \
			DA_UVPRho_To_W_Lin.inc              \
			DA_PRho_To_T_Adj.inc                \
			DA_PRho_To_T_Lin.inc                \
			DA_PT_To_Rho_Adj.inc                \
			DA_PT_To_Rho_Lin.inc                \
			DA_TPQ_To_RH.inc                    \
			DA_TPQ_To_RH_Lin.inc                \
			DA_TPQ_To_SLP.inc                   \
			DA_TPQ_To_SLP_Lin.inc               \
			DA_TPQ_To_SLP_Adj.inc               \
			DA_TPQ_To_Thickness.inc             \
			DA_TPRH_To_Q_Adj.inc                \
			DA_TPRH_To_Q_Lin.inc                \
			DA_TP_To_Qs.inc                     \
			DA_TP_To_Qs_Adj.inc                 \
			DA_TP_To_Qs_Lin.inc                 \
	                DA_TRH_To_TD.inc                    \
			DA_Transform_XToGPSRef.inc          \
			DA_Transform_XToGPSRef_Adj.inc      \
			DA_Transform_XToPsfc.inc            \
			DA_Transform_XToPsfc_Adj.inc        \
			DA_Transform_XToTPW.inc             \
			DA_Transform_XToTPW_Adj.inc         \
			DA_Transform_XToGPSRef.inc          \
			DA_Transform_XToGPSRef_Adj.inc      \
			DA_Transform_XToGPSRef_Lin.inc      \
	                da_check_rh.inc                     \
	                da_check_rh_simple.inc              \
	                da_e_qv_from_rh.inc                 \
	                da_get_q_error.inc                  \
			roughness_from_lanu.inc             \
			da_sfc_wtq.inc                      \
			da_sfc_wtq_Lin.inc                  \
			da_sfc_wtq_Adj.inc                  \
			DA_Transform_XToWTQ.inc             \
			DA_Transform_XToWTQ_Adj.inc         \
			da_sfc_pre.inc                      \
			da_sfc_pre_Lin.inc                  \
			da_sfc_pre_Adj.inc                  \
			da_filter.inc                       \
			da_filter_adj.inc                   \
			da_wdt.inc                          \
			DA_Moist_Phys_Adj.inc               \
			DA_Moist_Phys_Lin.inc

DA_FFTs.o:		DA_FFTs.F                           \
			DA_Define_Structures.o              \
			par_util.o                          \
			DA_Fast_Cosine_Transform.inc        \
			DA_Fast_Sine_Transform.inc          \
			DA_QPASSM.inc                       \
			DA_Solve_PoissonEqn_FCT.inc         \
			DA_Solve_PoissonEqn_FCT_Adj.inc     \
			DA_Solve_PoissonEqn_FST.inc         \
			DA_Solve_PoissonEqn_FST_Adj.inc

DA_Tools.o:		DA_Tools.F                     \
                        module_bc.o                    \
			DA_Constants.o                 \
			LAPACK.o                       \
			DA_Define_Structures.o         \
                        map_utils_defines.inc          \
                        map_utils.inc                  \
			DA_1D_EigenDecomposition.inc   \
			DA_Diff_Seconds.inc            \
			DA_Obs_Sfc_correction.inc      \
			da_global_ll_to_xy.inc         \
			DA_ll_to_xy.inc                \
			Residual.inc                   \
			da_add_noise.inc               \
			da_eof_decomposition.inc       \
			da_eof_decomposition_test.inc  \
			da_max_error_qc.inc            \
			da_random_omb.inc              \
			da_random_seed.inc             \
			gaus_noise.inc                 \
			llxy.inc                       \
			open_afile.inc                 \
			smooth_anl.inc                 \
			toGrid.inc                     \
			unifva.inc                     \
			DA_Set_Boundary_Xa.inc         \
			DA_Set_Boundary_Xb.inc         \
			get_2d_sum.inc                 \
			get_3d_sum.inc                 \
			xyll.inc  

DA_Recursive_Filter.o:	DA_Recursive_Filter.F          \
			DA_Constants.o                 \
			DA_Define_Structures.o         \
                        da_perform_2drf.inc            \
			DA_Calculate_RF_Factors.inc    \
			DA_RF_Turning_Conditions.inc   \
			DA_Recursive_Filter_1d.inc     \
			DA_Recursive_Filter_1d_Adj.inc \
			DA_Transform_Through_RF.inc    \
			DA_Transform_Through_RF_Adj.inc

DA_Interpolation.o:	DA_Interpolation.F        \
			DA_Constants.o            \
			DA_Define_Structures.o    \
			DA_Grid_Definitions.o     \
			DA_Tools.o                \
			to_zk.inc                 \
			Interp_Obs_lin_2D.inc     \
			Interp_Obs_lin_2D_adj.inc \
			Interp_lin_2D.inc         \
			Interp_lin_2D_adj.inc     \
			Interp_lin_3D.inc         \
			Interp_lin_3D_adj.inc

DA_Grid_Definitions.o:	DA_Grid_Definitions.F  \
			DA_Constants.o         \
                        DA_Define_Structures.o \
			da_ref_height.inc      \
			da_ref_pres.inc        \
			earth_2_model_wind.inc \
			ffdduv.inc             \
			set_map_para.inc

DA_Statistics.o:	DA_Statistics.F            \
			DA_Define_Structures.o     \
	                par_util.o                 \
			DA_Analysis_Stats.inc      \
			DA_Correlation_Coeff1d.inc \
			DA_Correlation_Coeff2d.inc \
			DA_Data_Distribution.inc   \
			DA_Print_Stats.inc         \
			Stats_Calculate.inc

DA_Define_Structures.o:	DA_Define_Structures.F              \
			DA_Constants.o                      \
                        da_tracing.o                        \
			DA_Define_Structures.F              \
			DA_Allocate_Background_Errors.inc   \
			DA_Allocate_MM5_Model.inc           \
			DA_Allocate_Observations.inc        \
			DA_Allocate_Y.inc                   \
			DA_Deallocate_Background_Errors.inc \
			DA_Deallocate_MM5_Model.inc         \
			DA_Deallocate_Observations.inc      \
			DA_Deallocate_Y.inc                 \
			DA_Zero_X.inc                       \
			DA_Zero_vp_type.inc                 \
			da_allocate_cv.inc                  \
			da_deallocate_cv.inc                \
			da_gauss_noise.inc                  \
			DA_Zero_Y.inc                       \
                        module_domain.o                     \
                        module_dm.o                 

DA_Constants.o:		DA_Constants.F             \
			DA_Array_Print.inc         \
			da_advance_cymdh.inc       \
			da_change_date.inc         \
			DA_Find_FFT_Factors.inc    \
			DA_Find_FFT_Trig_Funcs.inc \
                        module_driver_constants.o 

LAPACK.o:		LAPACK.F   \
			BLAS.o     \
			dlae2.inc  \
			dlaev2.inc \
			dlamc1.inc \
			dlamc2.inc \
			dlamc3.inc \
			dlamc4.inc \
			dlamc5.inc \
			dlamch.inc \
			dlanst.inc \
			dlansy.inc \
			dlapy2.inc \
			dsyev.inc
			$(CPP) $(FPPFLAGS) LAPACK.F > LAPACK.f
			$(FFC) -c $(FIXEDFLAGS) LAPACK.f

BLAS.o:	               BLAS.F       \
		       daxpy.inc    \
		       dcopy.inc    \
		       ddot.inc     \
		       dgemm.inc    \
		       dgemv.inc    \
		       dger.inc     \
		       dnrm2.inc    \
		       dscal.inc    \
		       dswap.inc    \
		       dsymv.inc    \
		       dsyr2.inc    \
		       dsyr2k.inc   \
		       dtrmm.inc    \
		       dtrmv.inc    \
		       lsame.inc    \
		       xerbla.inc   \
		       module_wrf_error.o
			$(CPP) $(FPPFLAGS) BLAS.F > BLAS.f
			$(FFC) -c $(FIXEDFLAGS) BLAS.f

DA_Test.o:	       DA_Test.F                            \
			DA_Constants.o                      \
			DA_Minimisation.o                   \
			DA_Define_Structures.o              \
			DA_Physics.o                        \
			DA_VToX_Transforms.o                \
			DA_Obs.o                            \
			DA_Airep.o                          \
			DA_Gpspw.o                          \
			DA_Metar.o                          \
			DA_Pilot.o                          \
			DA_Radar.o                          \
			DA_SSMI.o                           \
			DA_Satem.o                          \
			DA_GeoAMV.o                         \
			DA_PolarAMV.o                       \
			DA_Ships.o                          \
			DA_Sound.o                          \
			DA_Bogus.o                          \
			DA_Synop.o                          \
			da_pseudo.o                         \
			DA_Profiler.o                       \
			DA_Buoy.o                           \
			DA_Setup_Structures.o	            \
			DA_Tools.o                          \
			da_qscat.o                          \
			DA_Test.F                           \
			DA_Check_Balance.inc                \
			DA_Check_CvToVv_Adjoint.inc         \
			DA_Check_VToX_Adjoint.inc           \
			DA_Check_VpToX_Adjoint.inc          \
			DA_Check_Vp_Errors.inc              \
			DA_Check_VvToVp_Adjoint.inc         \
			DA_Check_XToVpToX_Errors.inc        \
			DA_Check_XToY_Adjoint.inc           \
			DA_Check_XToY_Adjoint_Airep.inc     \
			DA_Check_XToY_Adjoint_Gpspw.inc     \
			DA_Check_XToY_Adjoint_Gpsref.inc    \
			DA_Check_XToY_Adjoint_Metar.inc     \
			DA_Check_XToY_Adjoint_Pilot.inc     \
			DA_Check_XToY_Adjoint_SSMI.inc      \
			DA_Check_XToY_Adjoint_SSMI_Rv.inc   \
			DA_Check_XToY_Adjoint_SSMI_Tb.inc   \
			DA_Check_XToY_Adjoint_GeoAMV.inc    \
			DA_Check_XToY_Adjoint_PolarAMV.inc  \
			DA_Check_XToY_Adjoint_Ships.inc     \
			DA_Check_XToY_Adjoint_Sound.inc     \
			DA_Check_XToY_Adjoint_Sonde_sfc.inc \
			DA_Check_XToY_Adjoint_Bogus.inc     \
			DA_Check_XToY_Adjoint_Synop.inc     \
			DA_Check_XToY_Adjoint_Radar.inc     \
			DA_Check_XToY_Adjoint_Profiler.inc  \
			DA_Check_XToY_Adjoint_Buoy.inc      \
			DA_Test_VXTransform.inc             \
			DA_Transform_XToVp.inc              \
			da_check.inc                        \
			da_check_xtoy_adjoint_pseudo.inc    \
			da_check_xtoy_adjoint_qscat.inc     \
			da_check_xtoy_adjoint_ssmt1.inc     \
			da_check_xtoy_adjoint_ssmt2.inc     \
			da_setup_testfield.inc              \
			set_tst_trnsf_fld.inc

da_gen_be.o:		da_gen_be.F                      \
                        module_wrf_error.o               \
                        DA_Constants.o			 \
			LAPACK.o			 \
			BLAS.o				 \
                        da_tracing.o                     \
			DA_Transform_VpToVv.inc	         \
			da_eof_decomposition.inc         \
			da_eof_decomposition_test.inc    \
                        da_perform_2drf.inc              \
                        DA_Recursive_Filter_1d.inc       \
			da_create_bins.inc               \
                        da_filter_regcoeffs.inc          \
                        da_print_be_stats_h_global.inc   \
                        da_print_be_stats_h_regional.inc \
                        da_print_be_stats_p.inc          \
                        da_print_be_stats_v.inc          \
			da_readwrite_be_stage2.inc       \
			da_readwrite_be_stage3.inc       \
			da_readwrite_be_stage4.inc

da_fftpack5.o:	        da_fftpack5.F  \
			DA_Constants.o \
			r1f2kb.inc     \
			r1f2kf.inc     \
			r1f3kb.inc     \
			r1f3kf.inc     \
			r1f4kb.inc     \
			r1f4kf.inc     \
			r1f5kb.inc     \
			r1f5kf.inc     \
			r1fgkb.inc     \
			r1fgkf.inc     \
			rfft1b.inc     \
			rfft1f.inc     \
			rfft1i.inc     \
			rfftb1.inc     \
			rfftf1.inc     \
			rffti1.inc     \
			xerfft.inc
			$(CPP) $(FPPFLAGS) da_fftpack5.F > da_fftpack5.f
			$(FFC) -c $(FIXEDFLAGS) da_fftpack5.f

da_spectral.o:		da_spectral.F               \
			DA_Constants.o		    \
			DA_Define_Structures.o	    \
			da_fftpack5.o		    \
			da_asslegpol.inc            \
			da_calc_power.inc           \
			da_get_gausslats.inc        \
			da_get_reglats.inc          \
			da_initialize_h.inc         \
			da_legtra.inc               \
			da_legtra_inv.inc           \
			da_setlegpol.inc            \
			da_setlegpol_test.inc       \
			da_test_spectral.inc        \
			da_v_to_vv_spectral.inc     \
			da_vv_to_v_spectral.inc     \
			da_v_to_vv_spectral_adj.inc \
			da_legtra_inv_adj.inc       \
			da_apply_power.inc

be_spectral.o:		be_spectral.F               \
			DA_Constants.o	            \
			da_fftpack5.o		    \
			da_asslegpol.inc            \
			da_calc_power.inc           \
			da_get_gausslats.inc        \
			da_get_reglats.inc          \
			da_initialize_h.inc         \
			da_legtra.inc               \
			da_legtra_inv.inc           \
			da_setlegpol.inc            \
			da_setlegpol_test.inc       \
			da_vv_to_v_spectral.inc     \
			da_legtra_inv_adj.inc       \
			da_apply_power.inc

da_radiance.o:		da_radiance.F                     \
                        DA_Constants.o                    \
			DA_Define_Structures.o            \
			DA_Interpolation.o                \
			DA_Statistics.o                   \
			DA_Tools.o	                  \
			par_util.o		          \
			rttov_const.o	                  \
			rttov_types.o                     \
			gsi_kinds.o	                  \
			gsi_constants.o	                  \
			da_rtm_init.inc                   \
			da_rtm_direct.inc                 \
			da_rtm_tl.inc                     \
			da_rtm_ad.inc                     \
			da_calculate_grady_rad.inc        \
			da_calculate_jo_and_grady_rad.inc \
			da_calculate_residual_rad.inc     \
			da_transform_xtoy_rad.inc         \
			da_transform_xtoy_rad_adj.inc     \
			da_get_innov_vector_rad.inc       \
			da_biascorr_rad.inc               \
			da_qc_rad.inc                     \
			da_qc_amsua.inc                   \
			da_qc_amsub.inc                   \
			da_read_bufrtovs.inc              \
			da_read_bufrairs.inc              \
			da_read_kma1dvar.inc              \
			da_setup_bufrtovs_structures.inc  \
			da_oma_stats_rad.inc              \
			da_omb_stats_rad.inc              \
			da_print_stats_rad.inc            \
			da_detsurtyp.inc                  \
			gsi_emiss.inc                     \
			emiss_ssmi.inc                    \
			iceem_amsu.inc                    \
			landem.inc                        \
			snwem_amsu.inc                    \
			seaem.inc

da_tracing.o:           da_tracing.F                      \
                        da_trace_init.inc                 \
                        da_trace_entry.inc                \
                        da_trace.inc                      \
                        da_trace_exit.inc                 \
                        da_trace_int_sort.inc             \
                        da_trace_real_sort.inc            \
                        da_trace_report.inc               \
                        DA_Constants.o                    \
                        da_memory.o                       \
                        module_wrf_error.o                \
                        par_util1.o

rttov_const.o:		parkind1.o	\
			rttov_const.F

rttov_global.o:		parkind1.o	\
			rttov_const.o	\
			rttov_global.F

rttov_types.o:		parkind1.o	\
			rttov_const.o	\
			rttov_types.F

gsi_constants.o:	gsi_kinds.o      \
			gsi_constants.F

##############################################################################

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)

