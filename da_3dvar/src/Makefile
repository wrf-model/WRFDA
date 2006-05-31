# Makefile for WRF-VAR Program and its applications
# Lower level Makefile

OBJS_FOR_ALL =		da_solve_v3d.o		\
                        da_memory.o             \
                        da_tracing.o            \
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
			rttov_const.o		\
			rttov_global.o		\
			rttov_types.o		\
			parkind1.o	        \
			gsi_kinds.o		\
			gsi_constants.o		\
			BLAS.o                  \
                        module_wrfvar_top.o     \
                        module_wrf_3dvar_interface.o \
                        module_wrf_3dvar_io.o

include ../../configure.wrf

TARGETDIR    =  ../..
wrfvar :		$(OBJS_FOR_ALL)
		$(AR) ../../main/libwrflib.a $(OBJS_FOR_ALL)

##########################################################################

clean:
#	@ echo 'use the clean script
	$(RM) *.o *.mod *.f *.stb *.out core *~ mpif.h par_util/generic_boilerplate.inc bufrlib.prm

par_util/generic_boilerplate.inc:	par_util/generic_boilerplate.m4
			$(RM) par_util/generic_boilerplate.inc
			$(M4) par_util/generic_boilerplate.m4 > par_util/generic_boilerplate.inc

par_util.o:		DA_Define_Structures.o		\
			DA_Constants.o			\
                        par_util1.o                      \
			par_util/alloc_and_copy_be_arrays.inc	\
			par_util/be_local_copy.inc		\
			par_util/copy_dims.inc			\
			par_util/copy_tile_dims.inc		\
			par_util/cv_to_vv.inc			\
			par_util/local_to_global.inc		\
			par_util/mm5_struct_bcast.inc		\
			par_util/pack_count_obs.inc		\
			par_util/proc_maxmin_combine.inc	\
			par_util/proc_stats_combine.inc		\
			par_util/proc_sum_count_obs.inc		\
			par_util/proc_sum_int.inc		\
			par_util/proc_sum_real.inc		\
			par_util/transpose.inc			\
			par_util/unpack_count_obs.inc		\
			par_util/vv_to_cv.inc			\
			par_util/wrf_dm_interface.inc		\
			par_util/cv_to_global.inc		\
			par_util/generic_typedefs.inc		\
			par_util/generic_methods.inc		\
			par_util/specific_methods.inc		\
			par_util/generic_boilerplate.inc		\
			par_util/y_facade_to_global.inc		\
			par_util/par_util.F
			$(RM) $@
			$(CPP) -I./par_util $(CPPFLAGS) par_util/par_util.F > par_util.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) par_util.f

par_util1.o:		par_util/proc_sum_int.inc		\
			par_util/proc_sum_real.inc \
			par_util/par_util1.F
			$(RM) $@
			$(CPP) -I./par_util $(CPPFLAGS) par_util/par_util1.F > par_util1.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) par_util1.f

da_solve_v3d.o:		DA_Constants.o			\
			DA_Define_Structures.o		\
			DA_Setup_Structures.o		\
			DA_Test.o			\
			DA_Tools.o			\
			DA_Minimisation.o		\
			par_util.o			\
			da_solve_v3d/da_init_wrfvar.inc	\
			da_solve_v3d/da_solve_v3d.F
			$(RM) $@
			$(CPP) -I./da_solve_v3d $(CPPFLAGS) da_solve_v3d/da_solve_v3d.F > da_solve_v3d.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) da_solve_v3d.f

DA_Minimisation.o:	\
			module_wrf_3dvar_io.o \
			DA_Constants.o \
			DA_Define_Structures.o \
			DA_VToX_Transforms.o \
			DA_Obs.o \
			DA_Metar.o \
			DA_GeoAMV.o \
			DA_PolarAMV.o \
			DA_Ships.o \
			DA_Synop.o \
			DA_Sound.o \
			DA_Bogus.o \
			DA_Airep.o \
			DA_Pilot.o \
			DA_Radar.o \
			DA_Gpspw.o \
			DA_Gpsref.o \
			DA_SSMI.o \
			DA_Satem.o \
			da_pseudo.o \
			da_qscat.o \
			DA_Profiler.o \
			DA_Buoy.o  \
			DA_Setup_Structures.o		\
			da_radiance.o \
			DA_Minimisation/DA_Minimisation.F \
			DA_Minimisation/da_calculate_j.inc \
			DA_Minimisation/da_calculate_jo_and_grady.inc \
			DA_Minimisation/da_calculate_residual.inc \
			DA_Minimisation/da_get_var_diagnostics.inc \
			DA_Minimisation/da_get_innov_vector.inc \
			DA_Minimisation/da_dot.inc \
			DA_Minimisation/da_dot_cv.inc \
			DA_Minimisation/da_minimisation_warning.inc \
			DA_Minimisation/da_sum_reals.inc \
			DA_Minimisation/da_write_diagnostics.inc \
			DA_Minimisation/DA_Calculate_GradY.inc   \
			DA_Minimisation/DA_Minimise_CG.inc
			$(RM) $@
			$(CPP) -I./DA_Minimisation $(CPPFLAGS) DA_Minimisation/DA_Minimisation.F > DA_Minimisation.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Minimisation.f

DA_Setup_Structures.o:	\
			DA_Define_Structures.o \
			DA_Constants.o \
			DA_Grid_Definitions.o \
			DA_Obs.o \
			DA_SSMI.o \
			DA_VToX_Transforms.o \
			DA_Physics.o \
			da_h_ops.o \
	                da_bufrlib.o \
	                bort_exit.o \
	                restd.o \
	                wrdesc.o \
	                DA_ReadWrite_MM5.o \
	                da_spectral.o \
			da_radiance.o \
                        module_wrf_3dvar_io.o \
			DA_Setup_Structures/DA_Setup_Structures.F \
			DA_Setup_Structures/da_add_increments.inc \
			DA_Setup_Structures/DA_Add_PBL_And_SFC_Info.inc \
			DA_Setup_Structures/da_chgvres.inc \
			DA_Setup_Structures/da_get_vertical_truncation.inc \
			DA_Setup_Structures/da_interpolate_regcoeff.inc \
			DA_Setup_Structures/da_interpolate_stats.inc \
			DA_Setup_Structures/da_write_interpolated_be.inc \
			DA_Setup_Structures/da_rescale_background_errors.inc \
			DA_Setup_Structures/da_setup_background_errors.inc \
			DA_Setup_Structures/da_setup_be_global.inc \
			DA_Setup_Structures/da_setup_be_regional.inc \
			DA_Setup_Structures/DA_Get_Bins_Info.inc            \
			DA_Setup_Structures/da_setup_firstguess.inc \
			DA_Setup_Structures/da_setup_firstguess_mm5.inc \
			DA_Setup_Structures/da_setup_firstguess_wrf.inc \
			DA_Setup_Structures/da_setup_firstguess_kma.inc \
                        DA_Setup_Structures/da_setup_flow_predictors.inc \
			DA_Setup_Structures/da_setup_obs_structures.inc \
			DA_Setup_Structures/da_setup_obs_structures_ascii.inc \
			DA_Setup_Structures/da_setup_obs_structures_bufr.inc \
			DA_Setup_Structures/da_setup_obs_interp_wts.inc \
			DA_Setup_Structures/da_setup_runconstants.inc \
			DA_Setup_Structures/da_transfer_mm5toxb.inc \
			DA_Setup_Structures/da_transfer_wrftoxb.inc \
			DA_Setup_Structures/da_transfer_kmatoxb.inc \
			DA_Setup_Structures/da_transfer_xatowrf.inc \
			DA_Setup_Structures/da_transfer_xatokma.inc \
			DA_Setup_Structures/DA_Write_KMA_Increments.inc   \
			DA_Setup_Structures/da_transfer_xatowrftl.inc \
			DA_Setup_Structures/da_transfer_xatowrftl_adj.inc \
			DA_Setup_Structures/da_transfer_wrftltoxa.inc \
			DA_Setup_Structures/da_transfer_wrftltoxa_adj.inc \
			DA_Setup_Structures/DA_Transfer_XatoAnalysis.inc \
			DA_Setup_Structures/da_write_increments.inc
			$(RM) $@
			$(CPP) -I./DA_Setup_Structures $(CPPFLAGS) DA_Setup_Structures/DA_Setup_Structures.F > DA_Setup_Structures.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Setup_Structures.f

bufrlib.prm:		da_bufrlib/bufrlib.PRM
			$(RM) $@
			$(CPP) -I./da_bufrlib $(CPPFLAGS_BUFR) da_bufrlib/bufrlib.PRM > bufrlib.prm

da_bufrlib.o:		da_bufrlib/da_bufrlib.F bufrlib.prm
			$(RM) $@
			$(CPP) $(CPPFLAGS) da_bufrlib/da_bufrlib.F > da_bufrlib.f
			$(FFC) -c $(FIXEDFLAGS_BUFR) $(MODULE_DIRS) da_bufrlib.f

bort_exit.o:		da_bufrlib/bort_exit.c
			$(RM) $@
			$(CC) -c $(CCFLAGS_BUFR) da_bufrlib/bort_exit.c

restd.o:		da_bufrlib/restd.c
			$(RM) $@
			$(CC) -c $(CCFLAGS_BUFR) da_bufrlib/restd.c

wrdesc.o:		da_bufrlib/wrdesc.c
			$(RM) $@
			$(CC) -c $(CCFLAGS_BUFR) da_bufrlib/wrdesc.c

da_memory.o:		da_tracing/da_memory.c
			$(RM) $@
			$(CC) -c $(CFLAGS) da_tracing/da_memory.c

DA_VToX_Transforms.o:	\
			par_util.o \
			DA_Define_Structures.o \
			DA_Tools.o \
			DA_Recursive_Filter.o \
			DA_Constants.o \
			DA_Dynamics.o \
			DA_Physics.o \
			da_fftpack5.o \
			da_spectral.o \
			DA_SSMI.o \
			DA_VToX_Transforms/DA_VToX_Transforms.F \
			DA_VToX_Transforms/DA_Transform_VToVv.inc \
			DA_VToX_Transforms/DA_Transform_VToVv_Adj.inc \
			DA_VToX_Transforms/DA_Transform_VToVv_Global.inc \
			DA_VToX_Transforms/DA_Transform_VToVv_Global_Adj.inc \
			DA_VToX_Transforms/da_transform_vtox.inc \
			DA_VToX_Transforms/da_transform_vtox_adj.inc \
			DA_VToX_Transforms/DA_Transform_VpToVv.inc \
			DA_VToX_Transforms/DA_Transform_VpToX.inc \
			DA_VToX_Transforms/DA_Transform_VpToX_Adj.inc \
			DA_VToX_Transforms/DA_Transform_VvToVp.inc \
			DA_VToX_Transforms/DA_Transform_VvToVp_Adj.inc \
			DA_VToX_Transforms/DA_Get_VPoles.inc   \
			DA_VToX_Transforms/DA_Get_SPoles.inc   \
			DA_VToX_Transforms/DA_Get_AVPoles.inc   \
			DA_VToX_Transforms/DA_Get_ASPoles.inc   \
			DA_VToX_Transforms/DA_Vertical_Transform.inc
			$(RM) $@
			$(CPP) -I./DA_VToX_Transforms $(CPPFLAGS) DA_VToX_Transforms/DA_VToX_Transforms.F > DA_VToX_Transforms.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_VToX_Transforms.f

DA_Obs.o:		\
			DA_Constants.o \
			DA_Define_Structures.o \
			DA_Airep.o \
			DA_Gpspw.o \
			DA_Gpsref.o \
			DA_Metar.o \
			DA_Pilot.o \
			DA_Radar.o \
			DA_SSMI.o \
			DA_GeoAMV.o \
			DA_PolarAMV.o \
			DA_Satem.o \
			DA_Ships.o \
			DA_Synop.o \
			DA_Sound.o \
			DA_Bogus.o \
			da_pseudo.o \
			da_qscat.o \
			da_radiance.o \
			DA_Profiler.o \
			DA_Buoy.o  \
                        da_tracing.o \
			par_util.o \
			DA_Obs/DA_Obs.F \
	                DA_Obs/DA_Obs_Proc_Station.inc \
			DA_Obs/DA_Read_Obs.inc \
			DA_Obs/DA_Scan_Obs.inc \
                        DA_Obs/DA_Count_Filtered_Obs.inc \
			DA_Obs/DA_Read_BUFR_Obs.inc \
			DA_Obs/DA_Scan_BUFR_Obs.inc \
			DA_Obs/DA_Read_Radar.inc \
			DA_Obs/DA_Scan_Radar.inc \
			DA_Obs/DA_Transform_XToY.inc \
			DA_Obs/DA_Transform_XToY_Adj.inc \
			DA_Obs/da_add_noise_to_ob.inc \
			DA_Obs/da_check_missing.inc \
			DA_Obs/da_fill_obs_structures.inc \
			DA_Obs/da_random_omb_all.inc \
			DA_Obs/da_read_errfac.inc \
			DA_Obs/da_setup_pseudo_obs.inc \
			DA_Obs/da_store_obs_grid_info.inc \
			DA_Obs/da_use_obs_errfac.inc \
			DA_Obs/da_write_obs.inc \
			DA_Obs/da_write_filtered_obs.inc \
			DA_Obs/da_write_y.inc \
                        DA_Obs/da_deallocate_obs.inc
			$(RM) $@
			$(CPP) -I./DA_Obs $(CPPFLAGS) DA_Obs/DA_Obs.F > DA_Obs.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Obs.f

da_pseudo.o:		\
			DA_Constants.o \
			par_util.o \
			DA_Define_Structures.o \
			DA_Interpolation.o \
			DA_Statistics.o \
			DA_Tools.o \
			da_pseudo/da_pseudo.F \
			da_pseudo/da_cal_jo_and_grady_pseudo.inc \
			da_pseudo/da_calculate_residual_pseudo.inc \
			da_pseudo/da_get_innov_vector_pseudo.inc \
			da_pseudo/da_oa_stats_pseudo.inc \
			da_pseudo/da_ob_stats_pseudo.inc \
			da_pseudo/da_print_stats_pseudo.inc \
			da_pseudo/da_transform_xtoy_pseudo.inc \
			da_pseudo/da_transform_xtoy_pseudo_adj.inc
			$(RM) $@
			$(CPP) -I./da_pseudo $(CPPFLAGS) da_pseudo/da_pseudo.F > da_pseudo.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) da_pseudo.f

DA_Metar.o:		\
			DA_Constants.o \
			par_util.o \
			DA_Define_Structures.o \
			DA_Interpolation.o \
			DA_Statistics.o \
			DA_Tools.o \
			DA_Physics.o \
			DA_Metar/DA_Metar.F \
			DA_Metar/DA_AO_Stats_Metar.inc \
			DA_Metar/DA_Calculate_Jo_and_GradY_Metar.inc \
			DA_Metar/DA_Calculate_Residual_Metar.inc \
			DA_Metar/DA_OI_Stats_Metar.inc \
			DA_Metar/DA_Print_Stats_Metar.inc \
			DA_Metar/DA_Transform_XToY_Metar.inc \
			DA_Metar/DA_Transform_XToY_Metar_Adj.inc \
			DA_Metar/da_check_max_iv_metar.inc \
			DA_Metar/da_get_innov_vector_metar.inc
			$(RM) $@
			$(CPP) -I./DA_Metar $(CPPFLAGS) DA_Metar/DA_Metar.F > DA_Metar.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Metar.f

DA_GeoAMV.o:		\
			DA_Constants.o \
			par_util.o \
			DA_Define_Structures.o \
			DA_Interpolation.o \
			DA_Statistics.o \
			DA_Tools.o \
			DA_GeoAMV/DA_GeoAMV.F \
			DA_GeoAMV/DA_AO_Stats_GeoAMV.inc \
			DA_GeoAMV/DA_Get_Jo_and_GradY_GeoAMV.inc \
			DA_GeoAMV/DA_Calculate_Residual_GeoAMV.inc \
			DA_GeoAMV/DA_OI_Stats_GeoAMV.inc \
			DA_GeoAMV/DA_Print_Stats_GeoAMV.inc \
			DA_GeoAMV/DA_Transform_XToY_GeoAMV.inc \
			DA_GeoAMV/DA_Transform_XToY_GeoAMV.inc \
			DA_GeoAMV/da_check_max_iv_geoamv.inc \
			DA_GeoAMV/da_get_innov_vector_geoamv.inc
			$(RM) $@
			$(CPP) -I./DA_GeoAMV $(CPPFLAGS) DA_GeoAMV/DA_GeoAMV.F > DA_GeoAMV.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_GeoAMV.f

DA_PolarAMV.o:		\
			DA_Constants.o \
			par_util.o \
			DA_Define_Structures.o \
			DA_Interpolation.o \
			DA_Statistics.o \
			DA_Tools.o \
			DA_PolarAMV/DA_PolarAMV.F \
			DA_PolarAMV/DA_AO_Stats_PolarAMV.inc \
			DA_PolarAMV/DA_Get_Jo_and_GradY_PolarAMV.inc \
			DA_PolarAMV/DA_Calculate_Residual_PolarAMV.inc \
			DA_PolarAMV/DA_OI_Stats_PolarAMV.inc \
			DA_PolarAMV/DA_Print_Stats_PolarAMV.inc \
			DA_PolarAMV/DA_Transform_XToY_PolarAMV.inc \
			DA_PolarAMV/DA_Transform_XToY_PolarAMV.inc \
			DA_PolarAMV/da_check_max_iv_polaramv.inc \
			DA_PolarAMV/da_get_innov_vector_polaramv.inc
			$(RM) $@
			$(CPP) -I./DA_PolarAMV $(CPPFLAGS) DA_PolarAMV/DA_PolarAMV.F > DA_PolarAMV.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_PolarAMV.f

DA_Satem.o:		\
			DA_Constants.o \
			par_util.o \
			DA_Define_Structures.o \
			DA_Interpolation.o \
			DA_Statistics.o \
			DA_Tools.o \
			DA_Satem/DA_Satem.F \
			DA_Satem/DA_AO_Stats_Satem.inc \
			DA_Satem/DA_Calculate_Jo_and_GradY_Satem.inc \
			DA_Satem/DA_Calculate_Residual_Satem.inc \
			DA_Satem/DA_OI_Stats_Satem.inc \
			DA_Satem/DA_Print_Stats_Satem.inc \
			DA_Satem/DA_Transform_XToY_Satem.inc \
			DA_Satem/DA_Transform_XToY_Satem_Adj.inc \
			DA_Satem/da_check_max_iv_satem.inc \
			DA_Satem/da_get_innov_vector_satem.inc
			$(RM) $@
			$(CPP) -I./DA_Satem $(CPPFLAGS) DA_Satem/DA_Satem.F > DA_Satem.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Satem.f

DA_Ships.o:		\
			DA_Constants.o \
			par_util.o \
			DA_Define_Structures.o \
			DA_Interpolation.o \
			DA_Statistics.o \
			DA_Tools.o \
			DA_Physics.o \
			DA_Ships/DA_Ships.F \
			DA_Ships/DA_AO_Stats_Ships.inc \
			DA_Ships/DA_Calculate_Jo_and_GradY_Ships.inc \
			DA_Ships/DA_Calculate_Residual_Ships.inc \
			DA_Ships/DA_OI_Stats_Ships.inc \
			DA_Ships/DA_Print_Stats_Ships.inc \
			DA_Ships/DA_Transform_XToY_Ships.inc \
			DA_Ships/DA_Transform_XToY_Ships_Adj.inc \
			DA_Ships/da_check_max_iv_ships.inc \
			DA_Ships/da_get_innov_vector_ships.inc
			$(RM) $@
			$(CPP) -I./DA_Ships $(CPPFLAGS) DA_Ships/DA_Ships.F > DA_Ships.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Ships.f

DA_Synop.o:		\
			DA_Constants.o \
			par_util.o \
			DA_Define_Structures.o \
			DA_Interpolation.o \
			DA_Statistics.o \
			DA_Tools.o \
			DA_Physics.o \
			DA_Synop/DA_Synop.F \
			DA_Synop/da_check_max_iv_synop.inc \
			DA_Synop/da_get_innov_vector_synop.inc \
			DA_Synop/DA_AO_Stats_Synop.inc \
			DA_Synop/DA_Calculate_Jo_and_GradY_Synop.inc \
			DA_Synop/compute_jo_synop_uvtq.inc \
			DA_Synop/DA_Calculate_Residual_Synop.inc \
			DA_Synop/DA_OI_Stats_Synop.inc \
			DA_Synop/DA_Print_Stats_Synop.inc \
			DA_Synop/DA_Transform_XToY_Synop.inc \
			DA_Synop/DA_Transform_XToY_Synop_Adj.inc
			$(RM) $@
			$(CPP) -I./DA_Synop $(CPPFLAGS) DA_Synop/DA_Synop.F > DA_Synop.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Synop.f

DA_Sound.o:		\
			DA_Constants.o \
			par_util.o \
			DA_Define_Structures.o \
			DA_Interpolation.o \
			DA_Statistics.o \
			DA_Tools.o \
			DA_Sound/DA_AO_Stats_Sound.inc \
			DA_Sound/DA_Calculate_Jo_and_GradY_Sound.inc \
			DA_Sound/compute_jo_sound_uvtq.inc \
			DA_Sound/DA_Calculate_Residual_Sound.inc \
			DA_Sound/DA_OI_Stats_Sound.inc \
			DA_Sound/DA_Print_Stats_Sound.inc \
			DA_Sound/DA_Transform_XToY_Sound.inc \
			DA_Sound/DA_Transform_XToY_Sound_Adj.inc \
			DA_Sound/da_check_max_iv_sound.inc \
			DA_Sound/da_get_innov_vector_sound.inc \
			DA_Sound/da_obs_diagnostics.inc	\
			DA_Sound/DA_AO_Stats_Sonde_sfc.inc \
			DA_Sound/DA_Get_Jo_and_GradY_Sonde_sfc.inc \
			DA_Sound/compute_jo_sonde_sfc_uvtq.inc \
			DA_Sound/DA_Calculate_Residual_Sonde_sfc.inc \
			DA_Sound/DA_OI_Stats_Sonde_sfc.inc \
			DA_Sound/DA_Print_Stats_Sonde_sfc.inc \
			DA_Sound/DA_Transform_XToY_Sonde_sfc.inc \
			DA_Sound/DA_Transform_XToY_Sonde_sfc_Adj.inc \
			DA_Sound/da_get_innov_vector_sonde_sfc.inc \
			DA_Sound/da_check_max_iv_sonde_sfc.inc \
			DA_Sound/DA_Calculate_GradY_Sonde_sfc.inc \
			DA_Sound/DA_Sound.F
			$(RM) $@
			$(CPP) -I./DA_Sound $(CPPFLAGS) DA_Sound/DA_Sound.F > DA_Sound.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Sound.f

DA_Airep.o:		\
			DA_Constants.o \
			par_util.o \
			DA_Define_Structures.o \
			DA_Interpolation.o \
			DA_Statistics.o \
			DA_Tools.o \
			DA_Airep/DA_Airep.F \
			DA_Airep/DA_AO_Stats_Airep.inc \
			DA_Airep/DA_Calculate_Jo_and_GradY_Airep.inc \
			DA_Airep/DA_Calculate_Residual_Airep.inc \
			DA_Airep/DA_OI_Stats_Airep.inc \
			DA_Airep/DA_Print_Stats_Airep.inc \
			DA_Airep/DA_Transform_XToY_Airep.inc \
			DA_Airep/DA_Transform_XToY_Airep_Adj.inc \
			DA_Airep/da_check_max_iv_airep.inc \
			DA_Airep/da_get_innov_vector_airep.inc
			$(RM) $@
			$(CPP) -I./DA_Airep $(CPPFLAGS) DA_Airep/DA_Airep.F > DA_Airep.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Airep.f

DA_Pilot.o:		\
			DA_Constants.o \
			par_util.o \
			DA_Define_Structures.o \
			DA_Interpolation.o \
			DA_Statistics.o \
			DA_Tools.o \
			DA_Pilot/DA_Pilot.F \
			DA_Pilot/DA_AO_Stats_Pilot.inc \
			DA_Pilot/DA_Calculate_Jo_and_GradY_Pilot.inc \
			DA_Pilot/DA_Calculate_Residual_Pilot.inc \
			DA_Pilot/DA_OI_Stats_Pilot.inc \
			DA_Pilot/DA_Print_Stats_Pilot.inc \
			DA_Pilot/DA_Transform_XToY_Pilot.inc \
			DA_Pilot/DA_Transform_XToY_Pilot_Adj.inc \
			DA_Pilot/da_check_max_iv_pilot.inc \
			DA_Pilot/da_get_innov_vector_pilot.inc
			$(RM) $@
			$(CPP) -I./DA_Pilot $(CPPFLAGS) DA_Pilot/DA_Pilot.F > DA_Pilot.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Pilot.f

DA_Bogus.o:		\
			DA_Constants.o \
			par_util.o \
			DA_Define_Structures.o \
			DA_Interpolation.o \
			DA_Statistics.o \
			DA_Tools.o \
			DA_Physics.o \
			DA_Bogus/DA_Bogus.F \
			DA_Bogus/DA_AO_Stats_Bogus.inc \
			DA_Bogus/DA_Calculate_Jo_and_GradY_Bogus.inc \
			DA_Bogus/DA_Calculate_Residual_Bogus.inc \
			DA_Bogus/DA_OI_Stats_Bogus.inc \
			DA_Bogus/DA_Print_Stats_Bogus.inc \
			DA_Bogus/DA_Transform_XToY_Bogus.inc \
			DA_Bogus/DA_Transform_XToY_Bogus_Adj.inc \
			DA_Bogus/da_check_max_iv_bogus.inc \
			DA_Bogus/da_get_innov_vector_bogus.inc
			$(RM) $@
			$(CPP) -I./DA_Bogus $(CPPFLAGS) DA_Bogus/DA_Bogus.F > DA_Bogus.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Bogus.f

DA_Radar.o:		\
			DA_Constants.o \
			par_util.o \
			DA_Define_Structures.o \
			DA_Interpolation.o \
			DA_Statistics.o \
			DA_Tools.o \
			DA_Radar/DA_Radar.F \
			DA_Radar/DA_AO_Stats_Radar.inc \
			DA_Radar/DA_Calculate_GradY_Radar.inc \
			DA_Radar/DA_Calculate_Jo_and_GradY_Radar.inc \
			DA_Radar/DA_Calculate_Residual_Radar.inc \
			DA_Radar/DA_OI_Stats_Radar.inc \
			DA_Radar/DA_Print_Stats_Radar.inc \
			DA_Radar/DA_Transform_XToY_Radar.inc \
			DA_Radar/DA_Transform_XToY_Radar_Adj.inc \
			DA_Radar/da_check_max_iv_Radar.inc \
			DA_Radar/da_get_innov_vector_Radar.inc \
			DA_Radar/da_reflectivity.inc \
			DA_Radar/da_reflectivity_Adj.inc \
			DA_Radar/da_reflectivity_Lin.inc \
			DA_Radar/da_radial_velocity.inc \
			DA_Radar/da_radial_velocity_Lin.inc \
			DA_Radar/da_radial_velocity_Adj.inc \
			DA_Radar/da_max_error_qc_Radar.inc
			$(RM) $@
			$(CPP) -I./DA_Radar $(CPPFLAGS) DA_Radar/DA_Radar.F > DA_Radar.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Radar.f

DA_Gpspw.o:		\
			DA_Constants.o \
			par_util.o \
			DA_Define_Structures.o \
			DA_Interpolation.o \
			DA_Statistics.o \
			DA_Physics.o \
			DA_Tools.o \
			DA_Gpspw/DA_Gpspw.F \
			DA_Gpspw/DA_AO_Stats_Gpspw.inc \
			DA_Gpspw/DA_Calculate_Jo_and_GradY_Gpspw.inc \
			DA_Gpspw/DA_Calculate_Residual_Gpspw.inc \
			DA_Gpspw/DA_OI_Stats_Gpspw.inc \
			DA_Gpspw/DA_Print_Stats_Gpspw.inc \
			DA_Gpspw/da_check_max_iv_gpspw.inc \
			DA_Gpspw/da_get_innov_vector_gpspw.inc \
			DA_Gpspw/DA_Transform_XToY_Gpspw.inc \
			DA_Gpspw/DA_Transform_XToY_Gpspw_Adj.inc
			$(RM) $@
			$(CPP) -I./DA_Gpspw $(CPPFLAGS) DA_Gpspw/DA_Gpspw.F > DA_Gpspw.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Gpspw.f

DA_Gpsref.o:		\
			DA_Constants.o \
			par_util.o \
			DA_Define_Structures.o \
			DA_Interpolation.o \
			DA_Statistics.o \
			DA_Tools.o \
			DA_Gpsref/DA_Gpsref.F \
			DA_Gpsref/DA_AO_Stats_Gpsref.inc \
			DA_Gpsref/DA_Calculate_GradY_Gpsref.inc \
			DA_Gpsref/DA_Get_Jo_and_GradY_Gpsref.inc \
			DA_Gpsref/DA_Calculate_Residual_Gpsref.inc \
			DA_Gpsref/DA_OI_Stats_Gpsref.inc \
			DA_Gpsref/DA_Print_Stats_Gpsref.inc \
			DA_Gpsref/DA_Transform_XToY_Gpsref.inc \
			DA_Gpsref/DA_Transform_XToY_Gpsref_Adj.inc \
			DA_Gpsref/da_check_max_iv_Gpsref.inc \
			DA_Gpsref/da_get_innov_vector_Gpsref.inc
			$(RM) $@
			$(CPP) -I./DA_Gpsref $(CPPFLAGS) DA_Gpsref/DA_Gpsref.F > DA_Gpsref.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Gpsref.f

DA_SSMI.o:		\
			DA_Constants.o \
			par_util.o \
			DA_Define_Structures.o \
			DA_Interpolation.o \
			DA_Grid_Definitions.o \
			DA_Physics.o \
			DA_Statistics.o \
			DA_Tools.o \
			DA_SSMI/DA_SSMI.F \
			DA_SSMI/DA_AO_Stats_SSMI.inc \
			DA_SSMI/DA_AO_Stats_SSMI_Rv.inc \
			DA_SSMI/DA_AO_Stats_SSMI_Tb.inc \
			DA_SSMI/DA_Read_SSMI.inc \
			DA_SSMI/DA_Scan_SSMI.inc \
			DA_SSMI/DA_Cal_Jo_and_GradY_SSMI.inc \
			DA_SSMI/DA_Cal_Jo_and_GradY_SSMI_Rv.inc \
			DA_SSMI/DA_Cal_Jo_and_GradY_SSMI_Tb.inc \
			DA_SSMI/DA_Cal_Residual_SSMI.inc \
			DA_SSMI/DA_Cal_Residual_SSMI_Rv.inc \
			DA_SSMI/DA_Cal_Residual_SSMI_Tb.inc \
			DA_SSMI/DA_OI_Stats_SSMI.inc \
			DA_SSMI/DA_OI_Stats_SSMI_Rv.inc \
			DA_SSMI/DA_OI_Stats_SSMI_Tb.inc \
			DA_SSMI/DA_Transform_XToSpeed.inc \
			DA_SSMI/DA_Transform_XToSpeed_Lin.inc \
			DA_SSMI/DA_Transform_XToSpeed_Adj.inc \
			DA_SSMI/DA_Transform_XToSeaSfcWind.inc \
			DA_SSMI/DA_Transform_XToSeaSfcWind_Lin.inc \
			DA_SSMI/DA_Transform_XToSeaSfcWind_Adj.inc \
			DA_SSMI/DA_Transform_XToTb.inc \
			DA_SSMI/DA_Transform_XToTb_Lin.inc \
			DA_SSMI/DA_Transform_XToTb_Adj.inc \
			DA_SSMI/DA_Transform_XToY_SSMI.inc \
			DA_SSMI/DA_Transform_XToY_SSMI_Adj.inc \
			DA_SSMI/DA_Transform_XToY_SSMI_Rv.inc \
			DA_SSMI/DA_Transform_XToY_SSMI_Rv_Adj.inc \
			DA_SSMI/DA_Transform_XToY_SSMI_Tb.inc \
			DA_SSMI/DA_Transform_XToY_SSMI_Tb_Adj.inc \
			DA_SSMI/DA_Transform_XToZRhoQ.inc \
			DA_SSMI/DA_Transform_XToZRhoQ_Lin.inc \
			DA_SSMI/DA_Transform_XToZRhoQ_Adj.inc \
			DA_SSMI/cal_sigma_v.inc \
			DA_SSMI/da_cal_jo_and_grady_ssmt1.inc \
			DA_SSMI/da_cal_jo_and_grady_ssmt2.inc \
			DA_SSMI/da_cal_residual_ssmt1.inc \
			DA_SSMI/da_cal_residual_ssmt2.inc \
			DA_SSMI/da_check_max_iv_ssmi_rv.inc \
			DA_SSMI/da_check_max_iv_ssmi_tb.inc \
			DA_SSMI/da_check_max_iv_ssmt1.inc \
			DA_SSMI/da_check_max_iv_ssmt2.inc \
			DA_SSMI/da_get_innov_vector_ssmi.inc \
			DA_SSMI/da_get_innov_vector_ssmi_rv.inc \
			DA_SSMI/da_get_innov_vector_ssmi_tb.inc \
			DA_SSMI/da_get_innov_vector_ssmt1.inc \
			DA_SSMI/da_get_innov_vector_ssmt2.inc \
			DA_SSMI/da_oa_stats_ssmt1.inc \
			DA_SSMI/da_oa_stats_ssmt2.inc \
			DA_SSMI/da_ob_stats_ssmt1.inc \
			DA_SSMI/da_ob_stats_ssmt2.inc \
			DA_SSMI/da_print_stats_ssmt1.inc \
			DA_SSMI/da_print_stats_ssmt2.inc \
			DA_SSMI/da_transform_xtoy_ssmt1.inc \
			DA_SSMI/da_transform_xtoy_ssmt1_adj.inc \
			DA_SSMI/da_transform_xtoy_ssmt2.inc \
			DA_SSMI/da_transform_xtoy_ssmt2_adj.inc
			$(RM) $@
			$(CPP) -I./DA_SSMI $(CPPFLAGS) DA_SSMI/DA_SSMI.F > DA_SSMI.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_SSMI.f

da_qscat.o:		\
			DA_Constants.o \
			DA_Define_Structures.o \
			DA_Interpolation.o \
			DA_Statistics.o \
			DA_Tools.o \
			par_util.o \
			da_qscat/da_qscat.F \
			da_qscat/da_calculate_jo_and_grady_qscat.inc \
			da_qscat/da_calculate_residual_qscat.inc \
			da_qscat/da_check_max_iv_qscat.inc \
			da_qscat/da_get_innov_vector_qscat.inc \
			da_qscat/da_oa_stats_qscat.inc \
			da_qscat/da_ob_stats_qscat.inc \
			da_qscat/da_print_stats_qscat.inc \
			da_qscat/da_transform_xtoy_qscat.inc \
			da_qscat/da_transform_xtoy_qscat_adj.inc
			$(RM) $@
			$(CPP) -I./da_qscat $(CPPFLAGS) da_qscat/da_qscat.F > da_qscat.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) da_qscat.f
DA_Profiler.o:		\
			DA_Constants.o \
			par_util.o \
			DA_Define_Structures.o \
			DA_Interpolation.o \
			DA_Statistics.o \
			DA_Tools.o \
			DA_Profiler/DA_Profiler.F \
			DA_Profiler/DA_AO_Stats_Profiler.inc \
			DA_Profiler/DA_Get_Jo_and_GradY_Profiler.inc \
			DA_Profiler/DA_Calculate_Residual_Profiler.inc \
			DA_Profiler/DA_OI_Stats_Profiler.inc \
			DA_Profiler/DA_Print_Stats_Profiler.inc \
			DA_Profiler/DA_Transform_XToY_Profiler.inc \
			DA_Profiler/DA_Transform_XToY_Profiler_Adj.inc \
			DA_Profiler/da_check_max_iv_profiler.inc \
			DA_Profiler/da_get_innov_vector_profiler.inc
			$(RM) $@
			$(CPP) -I./DA_Profiler $(CPPFLAGS) DA_Profiler/DA_Profiler.F > DA_Profiler.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Profiler.f


DA_Buoy.o:		\
			DA_Constants.o \
			par_util.o \
			DA_Define_Structures.o \
			DA_Interpolation.o \
			DA_Statistics.o \
			DA_Tools.o \
			DA_Physics.o \
			DA_Buoy/DA_Buoy.F \
			DA_Buoy/DA_AO_Stats_Buoy.inc \
			DA_Buoy/DA_Calculate_Jo_and_GradY_Buoy.inc \
			DA_Buoy/DA_Calculate_Residual_Buoy.inc \
			DA_Buoy/DA_OI_Stats_Buoy.inc \
			DA_Buoy/DA_Print_Stats_Buoy.inc \
			DA_Buoy/DA_Transform_XToY_Buoy.inc \
			DA_Buoy/DA_Transform_XToY_Buoy_Adj.inc \
			DA_Buoy/da_check_max_iv_buoy.inc \
			DA_Buoy/da_get_innov_vector_buoy.inc
			$(RM) $@
			$(CPP) -I./DA_Buoy $(CPPFLAGS) DA_Buoy/DA_Buoy.F > DA_Buoy.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Buoy.f

DA_ReadWrite_MM5.o:	\
	                DA_Constants.o \
			DA_Define_Structures.o \
			DA_Grid_Definitions.o \
			par_util.o \
			da_h_ops.o \
			DA_ReadWrite_MM5/DA_ReadWrite_MM5.F \
			DA_ReadWrite_MM5/DA_crs_to_dot.inc \
			DA_ReadWrite_MM5/DA_cleanrows.inc \
			DA_ReadWrite_MM5/DA_print_big_header.inc \
			DA_ReadWrite_MM5/DA_print_sub_header.inc \
			DA_ReadWrite_MM5/DA_Read_MM5.inc \
			DA_ReadWrite_MM5/DA_Write_Analysis_MM5.inc
			$(RM) $@
			$(CPP) -I./DA_ReadWrite_MM5 $(CPPFLAGS) DA_ReadWrite_MM5/DA_ReadWrite_MM5.F > DA_ReadWrite_MM5.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_ReadWrite_MM5.f

da_h_ops.o:		\
			da_c_mat.o \
			da_h_ops/a2b.inc \
			da_h_ops/a2c.inc \
			da_h_ops/b2a.inc \
			da_h_ops/c2a.inc \
			da_h_ops/graph.inc \
			da_h_ops/hops2.inc \
			da_h_ops/da_h_ops.F
			$(RM) $@
			$(CPP) -I./da_h_ops $(CPPFLAGS) da_h_ops/da_h_ops.F > da_h_ops.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) da_h_ops.f


da_c_mat.o:		\
			da_c_mat/da_c_mat.F \
			da_c_mat/cpt_constd.inc \
			da_c_mat/cpt_consts.inc \
			da_c_mat/pmat1.inc \
			da_c_mat/pmat2.inc \
			da_c_mat/powmat.inc \
			da_c_mat/set_vops.inc
			$(RM) $@
			$(CPP) -I./da_c_mat $(CPPFLAGS) da_c_mat/da_c_mat.F > da_c_mat.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) da_c_mat.f

DA_Dynamics.o:		\
			DA_Constants.o \
			DA_Define_Structures.o \
			DA_Statistics.o \
			DA_FFTs.o \
			DA_Interpolation.o \
			DA_Dynamics/DA_Dynamics.F \
			DA_Dynamics/DA_Balance_CycloTerm.inc \
			DA_Dynamics/DA_Balance_CycloTerm_Adj.inc \
			DA_Dynamics/DA_Balance_CycloTerm_Lin.inc \
			DA_Dynamics/DA_Balance_Equation_Adj.inc \
			DA_Dynamics/DA_Balance_Equation_Lin.inc \
			DA_Dynamics/DA_Balance_GeoTerm.inc \
			DA_Dynamics/DA_Balance_GeoTerm_Adj.inc \
			DA_Dynamics/DA_Balance_GeoTerm_Lin.inc \
			DA_Dynamics/DA_HydrostaticP_To_Rho_Adj.inc \
			DA_Dynamics/DA_HydrostaticP_To_Rho_Lin.inc \
			DA_Dynamics/DA_PsiChi_To_UV.inc \
			DA_Dynamics/DA_PsiChi_To_UV_Adj.inc \
			DA_Dynamics/DA_UV_To_Divergence.inc \
			DA_Dynamics/DA_UV_To_Divergence_Adj.inc \
			DA_Dynamics/DA_W_Adjustment_Lin.inc \
			DA_Dynamics/DA_W_Adjustment_Adj.inc \
			DA_Dynamics/DA_WZ_BASE.inc          \
			DA_Dynamics/DA_UV_To_Vorticity.inc
			$(RM) $@
			$(CPP) -I./DA_Dynamics $(CPPFLAGS) DA_Dynamics/DA_Dynamics.F > DA_Dynamics.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Dynamics.f

DA_Physics.o:		\
			DA_Constants.o \
			DA_Define_Structures.o \
			DA_Grid_Definitions.o \
			DA_Interpolation.o \
			DA_Dynamics.o      \
			DA_Physics/DA_Physics.F \
			DA_Physics/DA_UVPRho_To_W_Adj.inc \
			DA_Physics/DA_UVPRho_To_W_Lin.inc \
			DA_Physics/DA_PRho_To_T_Adj.inc \
			DA_Physics/DA_PRho_To_T_Lin.inc \
			DA_Physics/DA_PT_To_Rho_Adj.inc \
			DA_Physics/DA_PT_To_Rho_Lin.inc \
			DA_Physics/DA_TPQ_To_RH.inc \
			DA_Physics/DA_TPQ_To_RH_Lin.inc \
			DA_Physics/DA_TPQ_To_SLP.inc \
			DA_Physics/DA_TPQ_To_SLP_Lin.inc \
			DA_Physics/DA_TPQ_To_SLP_Adj.inc \
			DA_Physics/DA_TPQ_To_Thickness.inc \
			DA_Physics/DA_TPRH_To_Q_Adj.inc \
			DA_Physics/DA_TPRH_To_Q_Lin.inc \
			DA_Physics/DA_TP_To_Qs.inc \
			DA_Physics/DA_TP_To_Qs_Adj.inc \
			DA_Physics/DA_TP_To_Qs_Lin.inc \
	                DA_Physics/DA_TRH_To_TD.inc  \
			DA_Physics/DA_Transform_XToGPSRef.inc \
			DA_Physics/DA_Transform_XToGPSRef_Adj.inc \
			DA_Physics/DA_Transform_XToPsfc.inc \
			DA_Physics/DA_Transform_XToPsfc_Adj.inc \
			DA_Physics/DA_Transform_XToTPW.inc \
			DA_Physics/DA_Transform_XToTPW_Adj.inc \
			DA_Physics/DA_Transform_XToGPSRef.inc \
			DA_Physics/DA_Transform_XToGPSRef_Adj.inc \
			DA_Physics/DA_Transform_XToGPSRef_Lin.inc \
	                DA_Physics/da_check_rh.inc \
	                DA_Physics/da_check_rh_simple.inc \
	                DA_Physics/da_e_qv_from_rh.inc \
	                DA_Physics/da_get_q_error.inc \
			DA_Physics/roughness_from_lanu.inc \
			DA_Physics/da_sfc_wtq.inc \
			DA_Physics/da_sfc_wtq_Lin.inc \
			DA_Physics/da_sfc_wtq_Adj.inc \
			DA_Physics/DA_Transform_XToWTQ.inc \
			DA_Physics/DA_Transform_XToWTQ_Adj.inc \
			DA_Physics/da_sfc_pre.inc \
			DA_Physics/da_sfc_pre_Lin.inc \
			DA_Physics/da_sfc_pre_Adj.inc \
			DA_Physics/da_filter.inc \
			DA_Physics/da_filter_adj.inc \
			DA_Physics/da_wdt.inc        \
			DA_Physics/DA_Moist_Phys_Adj.inc \
			DA_Physics/DA_Moist_Phys_Lin.inc
			$(RM) $@
			$(CPP) -I./DA_Physics $(CPPFLAGS) DA_Physics/DA_Physics.F > DA_Physics.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Physics.f

DA_FFTs.o:		\
			DA_Define_Structures.o \
			par_util.o \
			DA_FFTs/DA_FFTs.F \
			DA_FFTs/DA_Fast_Cosine_Transform.inc \
			DA_FFTs/DA_Fast_Sine_Transform.inc \
			DA_FFTs/DA_QPASSM.inc \
			DA_FFTs/DA_Solve_PoissonEqn_FCT.inc \
			DA_FFTs/DA_Solve_PoissonEqn_FCT_Adj.inc \
			DA_FFTs/DA_Solve_PoissonEqn_FST.inc \
			DA_FFTs/DA_Solve_PoissonEqn_FST_Adj.inc
			$(RM) $@
			$(CPP) -I./DA_FFTs $(CPPFLAGS) DA_FFTs/DA_FFTs.F > DA_FFTs.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_FFTs.f

DA_Tools.o:		\
			DA_Constants.o \
			LAPACK.o \
			DA_Define_Structures.o \
			DA_Tools/DA_Tools.F \
			DA_Tools/map_utils_defines.inc \
			DA_Tools/map_utils.inc \
			DA_Tools/DA_1D_EigenDecomposition.inc \
			DA_Tools/DA_Diff_Seconds.inc \
			DA_Tools/DA_Obs_Sfc_correction.inc \
			DA_Tools/da_global_ll_to_xy.inc \
			DA_Tools/DA_ll_to_xy.inc \
			DA_Tools/Residual.inc \
			DA_Tools/da_add_noise.inc \
			DA_Tools/da_eof_decomposition.inc	\
			DA_Tools/da_eof_decomposition_test.inc	\
			DA_Tools/da_max_error_qc.inc \
			DA_Tools/da_random_omb.inc \
			DA_Tools/da_random_seed.inc \
			DA_Tools/gaus_noise.inc \
			DA_Tools/llxy.inc \
			DA_Tools/open_afile.inc \
			DA_Tools/smooth_anl.inc \
			DA_Tools/toGrid.inc \
			DA_Tools/unifva.inc \
			DA_Tools/DA_Set_Boundary_Xa.inc \
			DA_Tools/DA_Set_Boundary_Xb.inc \
			DA_Tools/get_2d_sum.inc \
			DA_Tools/get_3d_sum.inc \
			DA_Tools/xyll.inc    
			$(RM) $@
			$(CPP) -I./DA_Tools $(CPPFLAGS) DA_Tools/DA_Tools.F > DA_Tools.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Tools.f

DA_Recursive_Filter.o:	\
			DA_Constants.o \
			DA_Recursive_Filter/DA_Recursive_Filter.F \
                        DA_Recursive_Filter/da_perform_2drf.inc \
			DA_Recursive_Filter/DA_Calculate_RF_Factors.inc \
			DA_Recursive_Filter/DA_RF_Turning_Conditions.inc \
			DA_Recursive_Filter/DA_Recursive_Filter_1d.inc \
			DA_Recursive_Filter/DA_Recursive_Filter_1d_Adj.inc \
			DA_Recursive_Filter/DA_Transform_Through_RF.inc \
			DA_Recursive_Filter/DA_Transform_Through_RF_Adj.inc
			$(RM) $@
			$(CPP) -I./DA_Recursive_Filter $(CPPFLAGS) DA_Recursive_Filter/DA_Recursive_Filter.F > DA_Recursive_Filter.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Recursive_Filter.f

DA_Interpolation.o:	\
			DA_Constants.o \
			DA_Define_Structures.o \
			DA_Grid_Definitions.o \
			DA_Tools.o \
			DA_Interpolation/DA_Interpolation.F \
			DA_Interpolation/to_zk.inc \
			DA_Interpolation/Interp_Obs_lin_2D.inc \
			DA_Interpolation/Interp_Obs_lin_2D_adj.inc \
			DA_Interpolation/Interp_lin_2D.inc \
			DA_Interpolation/Interp_lin_2D_adj.inc \
			DA_Interpolation/Interp_lin_3D.inc \
			DA_Interpolation/Interp_lin_3D_adj.inc
			$(RM) $@
			$(CPP) -I./DA_Interpolation $(CPPFLAGS) DA_Interpolation/DA_Interpolation.F > DA_Interpolation.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Interpolation.f

DA_Grid_Definitions.o:	\
			DA_Constants.o \
			DA_Grid_Definitions/DA_Grid_Definitions.F \
			DA_Grid_Definitions/da_ref_height.inc \
			DA_Grid_Definitions/da_ref_pres.inc \
			DA_Grid_Definitions/earth_2_model_wind.inc \
			DA_Grid_Definitions/ffdduv.inc \
			DA_Grid_Definitions/set_map_para.inc
			$(RM) $@
			$(CPP) -I./DA_Grid_Definitions $(CPPFLAGS) DA_Grid_Definitions/DA_Grid_Definitions.F > DA_Grid_Definitions.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Grid_Definitions.f

DA_Statistics.o:	\
			DA_Define_Structures.o \
	                par_util.o \
			DA_Statistics/DA_Statistics.F \
			DA_Statistics/DA_Analysis_Stats.inc \
			DA_Statistics/DA_Correlation_Coeff1d.inc \
			DA_Statistics/DA_Correlation_Coeff2d.inc \
			DA_Statistics/DA_Data_Distribution.inc \
			DA_Statistics/DA_Print_Stats.inc \
			DA_Statistics/Stats_Calculate.inc
			$(RM) $@
			$(CPP) -I./DA_Statistics $(CPPFLAGS) DA_Statistics/DA_Statistics.F > DA_Statistics.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Statistics.f

DA_Define_Structures.o:	\
			DA_Constants.o \
                        da_tracing.o \
			DA_Define_Structures/DA_Define_Structures.F \
			DA_Define_Structures/DA_Allocate_Background_Errors.inc \
			DA_Define_Structures/DA_Allocate_MM5_Model.inc \
			DA_Define_Structures/DA_Allocate_Observations.inc \
			DA_Define_Structures/DA_Allocate_Y.inc \
			DA_Define_Structures/DA_Deallocate_Background_Errors.inc \
			DA_Define_Structures/DA_Deallocate_MM5_Model.inc \
			DA_Define_Structures/DA_Deallocate_Observations.inc \
			DA_Define_Structures/DA_Deallocate_Y.inc \
			DA_Define_Structures/DA_Zero_X.inc \
			DA_Define_Structures/DA_Zero_vp_type.inc \
			DA_Define_Structures/da_initialize_cv.inc \
			DA_Define_Structures/da_gauss_noise.inc  \
			DA_Define_Structures/DA_Zero_Y.inc    
			$(RM) $@
			$(CPP) -I./DA_Define_Structures $(CPPFLAGS) DA_Define_Structures/DA_Define_Structures.F > DA_Define_Structures.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Define_Structures.f

DA_Constants.o:		DA_Constants/DA_Constants.F \
			DA_Constants/DA_Array_Print.inc \
			DA_Constants/da_advance_cymdh.inc \
			DA_Constants/da_change_date.inc \
			DA_Constants/DA_Find_FFT_Factors.inc \
			DA_Constants/DA_Find_FFT_Trig_Funcs.inc \
                        ../../frame/module_driver_constants.o 
			$(RM) $@
			$(CPP) -I./DA_Constants -I../../inc $(CPPFLAGS) DA_Constants/DA_Constants.F > DA_Constants.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Constants.f

LAPACK.o:		\
			BLAS.o \
			LAPACK/LAPACK.F \
			LAPACK/dlae2.inc \
			LAPACK/dlaev2.inc \
			LAPACK/dlamc1.inc \
			LAPACK/dlamc2.inc \
			LAPACK/dlamc3.inc \
			LAPACK/dlamc4.inc \
			LAPACK/dlamc5.inc \
			LAPACK/dlamch.inc \
			LAPACK/dlanst.inc \
			LAPACK/dlansy.inc \
			LAPACK/dlapy2.inc \
			LAPACK/dsyev.inc
			$(CPP) -I./LAPACK $(CPPFLAGS) LAPACK/LAPACK.F > LAPACK.f
			$(FFC) -c -I./LAPACK $(FIXEDFLAGS_WRFVAR) $(MODULE_DIRS) LAPACK.f

BLAS.o:	         \
			BLAS/daxpy.inc	\
			BLAS/dcopy.inc	\
			BLAS/ddot.inc	\
			BLAS/dgemm.inc	\
			BLAS/dgemv.inc	\
			BLAS/dger.inc	\
			BLAS/dnrm2.inc	\
			BLAS/dscal.inc	\
			BLAS/dswap.inc	\
			BLAS/dsymv.inc	\
			BLAS/dsyr2.inc	\
			BLAS/dsyr2k.inc	\
			BLAS/dtrmm.inc	\
			BLAS/dtrmv.inc	\
			BLAS/lsame.inc	\
			BLAS/xerbla.inc \
			BLAS/BLAS.F
			$(CPP) -I./BLAS $(CPPFLAGS) BLAS/BLAS.F > BLAS.f
			$(FFC) -c -I./BLAS $(FIXEDFLAGS_WRFVAR) $(MODULE_DIRS) BLAS.f

DA_Test.o:	\
			DA_Constants.o \
			DA_Define_Structures.o \
			DA_Physics.o \
			DA_VToX_Transforms.o \
			DA_Obs.o \
			DA_Airep.o \
			DA_Gpspw.o \
			DA_Metar.o \
			DA_Pilot.o \
			DA_Radar.o \
			DA_SSMI.o \
			DA_Satem.o \
			DA_GeoAMV.o \
			DA_PolarAMV.o \
			DA_Ships.o \
			DA_Sound.o \
			DA_Bogus.o \
			DA_Synop.o \
			da_pseudo.o \
			DA_Profiler.o \
			DA_Buoy.o  \
			DA_Minimisation.o	\
			DA_Setup_Structures.o	\
			DA_Tools.o \
			da_qscat.o \
			DA_Test/DA_Test.F \
			DA_Test/DA_Check_Balance.inc \
			DA_Test/DA_Check_CvToVv_Adjoint.inc \
			DA_Test/DA_Check_VToX_Adjoint.inc \
			DA_Test/DA_Check_VpToX_Adjoint.inc \
			DA_Test/DA_Check_Vp_Errors.inc \
			DA_Test/DA_Check_VvToVp_Adjoint.inc \
			DA_Test/DA_Check_XToVpToX_Errors.inc \
			DA_Test/DA_Check_XToY_Adjoint.inc \
			DA_Test/DA_Check_XToY_Adjoint_Airep.inc \
			DA_Test/DA_Check_XToY_Adjoint_Gpspw.inc \
			DA_Test/DA_Check_XToY_Adjoint_Gpsref.inc \
			DA_Test/DA_Check_XToY_Adjoint_Metar.inc \
			DA_Test/DA_Check_XToY_Adjoint_Pilot.inc \
			DA_Test/DA_Check_XToY_Adjoint_SSMI.inc \
			DA_Test/DA_Check_XToY_Adjoint_SSMI_Rv.inc \
			DA_Test/DA_Check_XToY_Adjoint_SSMI_Tb.inc \
			DA_Test/DA_Check_XToY_Adjoint_GeoAMV.inc \
			DA_Test/DA_Check_XToY_Adjoint_PolarAMV.inc \
			DA_Test/DA_Check_XToY_Adjoint_Ships.inc \
			DA_Test/DA_Check_XToY_Adjoint_Sound.inc \
			DA_Test/DA_Check_XToY_Adjoint_Sonde_sfc.inc \
			DA_Test/DA_Check_XToY_Adjoint_Bogus.inc \
			DA_Test/DA_Check_XToY_Adjoint_Synop.inc \
			DA_Test/DA_Check_XToY_Adjoint_Radar.inc \
			DA_Test/DA_Check_XToY_Adjoint_Profiler.inc \
			DA_Test/DA_Check_XToY_Adjoint_Buoy.inc  \
			DA_Test/DA_Transform_XToVp.inc \
			DA_Test/da_check.inc \
			DA_Test/da_check_xtoy_adjoint_pseudo.inc \
			DA_Test/da_check_xtoy_adjoint_qscat.inc \
			DA_Test/da_check_xtoy_adjoint_ssmt1.inc \
			DA_Test/da_check_xtoy_adjoint_ssmt2.inc \
			DA_Test/da_setup_testfield.inc \
			DA_Test/set_tst_trnsf_fld.inc
			$(RM) $@
			$(CPP) -I./DA_Test $(CPPFLAGS) DA_Test/DA_Test.F > DA_Test.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) DA_Test.f

da_gen_be.o:	         \
			DA_Constants.o			\
			LAPACK.o			\
			BLAS.o				\
                        da_tracing.o                    \
			DA_VToX_Transforms/DA_Transform_VpToVv.inc	\
			DA_Tools/da_eof_decomposition.inc	\
			DA_Tools/da_eof_decomposition_test.inc	\
                        DA_Recursive_Filter/da_perform_2drf.inc \
                        DA_Recursive_Filter/DA_Recursive_Filter_1d.inc  \
			da_gen_be/da_create_bins.inc	\
                        da_gen_be/da_filter_regcoeffs.inc       \
                        da_gen_be/da_get_field.inc       \
                        da_gen_be/da_get_height.inc       \
                        da_gen_be/da_get_trh.inc       \
                        da_gen_be/da_print_be_stats_h_global.inc       \
                        da_gen_be/da_print_be_stats_h_regional.inc       \
                        da_gen_be/da_print_be_stats_p.inc       \
                        da_gen_be/da_print_be_stats_v.inc       \
			da_gen_be/da_readwrite_be_stage2.inc	\
			da_gen_be/da_readwrite_be_stage3.inc	\
			da_gen_be/da_readwrite_be_stage4.inc	\
			da_gen_be/da_stage0_initialize.inc	\
			da_gen_be/da_gen_be.F
			$(CPP) -I./da_gen_be -I./DA_VToX_Transforms -I./DA_Tools \
			-I./DA_Recursive_Filter \
			$(CPPFLAGS) da_gen_be/da_gen_be.F > da_gen_be.f
			$(FC) -c $(FCFLAGS) -I${NETCDF}/include -I../../frame da_gen_be.f

da_fftpack5.o:	         \
			DA_Constants.o		\
			da_fftpack5/r1f2kb.inc	\
			da_fftpack5/r1f2kf.inc	\
			da_fftpack5/r1f3kb.inc	\
			da_fftpack5/r1f3kf.inc	\
			da_fftpack5/r1f4kb.inc	\
			da_fftpack5/r1f4kf.inc	\
			da_fftpack5/r1f5kb.inc	\
			da_fftpack5/r1f5kf.inc	\
			da_fftpack5/r1fgkb.inc	\
			da_fftpack5/r1fgkf.inc	\
			da_fftpack5/rfft1b.inc	\
			da_fftpack5/rfft1f.inc	\
			da_fftpack5/rfft1i.inc	\
			da_fftpack5/rfftb1.inc	\
			da_fftpack5/rfftf1.inc	\
			da_fftpack5/rffti1.inc	\
			da_fftpack5/xerfft.inc	\
			da_fftpack5/da_fftpack5.F
			$(CPP) -I./da_fftpack5 $(CPPFLAGS) da_fftpack5/da_fftpack5.F > da_fftpack5.f
			$(FFC) -c $(FIXEDFLAGS_WRFVAR) $(MODULE_DIRS) da_fftpack5.f

da_spectral.o:		\
			DA_Constants.o				\
			DA_Define_Structures.o			\
			da_fftpack5.o			        \
			da_spectral/da_asslegpol.inc		\
			da_spectral/da_calc_power.inc		\
			da_spectral/da_calc_power_spectrum.inc	\
			da_spectral/da_get_gausslats.inc	\
			da_spectral/da_get_reglats.inc		\
			da_spectral/da_initialize_h.inc		\
			da_spectral/da_legtra.inc		\
			da_spectral/da_legtra_inv.inc		\
			da_spectral/da_setlegpol.inc		\
			da_spectral/da_setlegpol_test.inc	\
			da_spectral/da_test_spectral.inc	\
			da_spectral/da_v_to_vv_spectral.inc	\
			da_spectral/da_vv_to_v_spectral.inc	\
			da_spectral/da_v_to_vv_spectral_adj.inc	\
			da_spectral/da_legtra_inv_adj.inc	\
			da_spectral/da_apply_power.inc		\
			da_spectral/da_spectral.F
			$(CPP) -I./da_spectral $(CPPFLAGS) da_spectral/da_spectral.F > da_spectral.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) da_spectral.f


module_wrf_3dvar_interface.o : da_modules/module_wrf_3dvar_interface.F \
                               da_tracing.o
			$(CPP) -I./da_modules $(CPPFLAGS) da_modules/module_wrf_3dvar_interface.F > module_wrf_3dvar_interface.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) module_wrf_3dvar_interface.f

module_wrf_3dvar_io.o : da_modules/module_wrf_3dvar_io.F \
                        da_tracing.o
			$(CPP) -I./da_modules $(CPPFLAGS) da_modules/module_wrf_3dvar_io.F > module_wrf_3dvar_io.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) module_wrf_3dvar_io.f

module_wrfvar_top.o : da_modules/module_wrfvar_top.F \
                      module_wrf_3dvar_interface.o \
                      da_tracing.o
			$(CPP) -I./da_modules $(CPPFLAGS) da_modules/module_wrfvar_top.F > module_wrfvar_top.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) module_wrfvar_top.f

module_wrfvar_esmf_super.o : da_modules/module_wrfvar_esmf_super.F \
                             module_wrf_3dvar_io.o \
                             module_wrf_3dvar_interface.o
			$(CPP) -I./da_modules $(CPPFLAGS) da_modules/module_wrfvar_esmf_super.F > module_wrfvar_esmf_super.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) module_wrfvar_esmf_super.f

da_tracing.o:           da_tracing/da_tracing.F           \
                        da_tracing/da_trace_init.inc      \
                        da_tracing/da_trace_entry.inc     \
                        da_tracing/da_trace.inc           \
                        da_tracing/da_trace_exit.inc      \
                        da_tracing/da_trace_int_sort.inc  \
                        da_tracing/da_trace_real_sort.inc \
                        da_tracing/da_trace_report.inc    \
                        DA_Constants.o                    \
                        da_memory.o                       \
                        par_util1.o
			$(CPP) -I./da_tracing $(CPPFLAGS) da_tracing/da_tracing.F > da_tracing.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) da_tracing.f

be_spectral.o:		\
			DA_Constants.o				\
			da_fftpack5.o			        \
			da_spectral/da_asslegpol.inc		\
			da_spectral/da_calc_power.inc		\
			da_spectral/da_get_gausslats.inc	\
			da_spectral/da_get_reglats.inc		\
			da_spectral/da_initialize_h.inc		\
			da_spectral/da_legtra.inc		\
			da_spectral/da_legtra_inv.inc		\
			da_spectral/da_setlegpol.inc		\
			da_spectral/da_setlegpol_test.inc	\
			da_spectral/da_vv_to_v_spectral.inc	\
			da_spectral/da_legtra_inv_adj.inc	\
			da_spectral/da_apply_power.inc		\
			da_spectral/be_spectral.F
			$(CPP) -I./da_spectral $(CPPFLAGS) da_spectral/be_spectral.F > be_spectral.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) be_spectral.f

da_radiance.o:		DA_Constants.o          \
			DA_Define_Structures.o  \
			DA_Interpolation.o \
			DA_Statistics.o \
			DA_Tools.o		\
			par_util.o		\
			rttov_const.o	\
			rttov_types.o  \
			gsi_kinds.o		\
			gsi_constants.o	\
			da_radiance/da_rtm_init.inc   \
			da_radiance/da_rtm_direct.inc \
			da_radiance/da_rtm_tl.inc \
			da_radiance/da_rtm_ad.inc \
			da_radiance/da_calculate_grady_rad.inc \
			da_radiance/da_calculate_jo_and_grady_rad.inc \
			da_radiance/da_calculate_residual_rad.inc \
			da_radiance/da_transform_xtoy_rad.inc \
			da_radiance/da_transform_xtoy_rad_adj.inc \
			da_radiance/da_get_innov_vector_rad.inc \
			da_radiance/da_biascorr_rad.inc \
			da_radiance/da_qc_rad.inc \
			da_radiance/da_qc_amsua.inc \
			da_radiance/da_qc_amsub.inc \
			da_radiance/da_read_bufrtovs.inc \
			da_radiance/da_read_bufrairs.inc \
			da_radiance/da_read_kma1dvar.inc \
			da_radiance/da_setup_bufrtovs_structures.inc \
			da_radiance/da_oma_stats_rad.inc  \
			da_radiance/da_omb_stats_rad.inc  \
			da_radiance/da_print_stats_rad.inc \
			da_radiance/da_detsurtyp.inc \
			da_radiance/gsi_emiss.inc \
			da_radiance/emiss_ssmi.inc \
			da_radiance/iceem_amsu.inc \
			da_radiance/landem.inc \
			da_radiance/snwem_amsu.inc \
			da_radiance/seaem.inc \
			da_radiance/da_radiance.F
			$(RM) $@
			$(CPP) -I./da_radiance -I$(RTTOV)/src $(CPPFLAGS) da_radiance/da_radiance.F > da_radiance.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) da_radiance.f

rttov_const.o:		parkind1.o	\
			da_radiance/rttov_const.F
			$(RM) $@
			$(CPP) -I./da_radiance -I$(RTTOV)/src $(CPPFLAGS) da_radiance/rttov_const.F > rttov_const.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) rttov_const.f

rttov_global.o:		parkind1.o	\
			rttov_const.o	\
			da_radiance/rttov_global.F
			$(RM) $@
			$(CPP) -I./da_radiance -I$(RTTOV)/src $(CPPFLAGS) da_radiance/rttov_global.F > rttov_global.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) rttov_global.f

rttov_types.o:		parkind1.o	\
			rttov_const.o	\
			da_radiance/rttov_types.F
			$(CPP) -I./da_radiance -I$(RTTOV)/src $(CPPFLAGS) da_radiance/rttov_types.F > rttov_types.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) rttov_types.f

parkind1.o:		\
			da_radiance/parkind1.F
			$(CPP) -I./da_radiance -I$(RTTOV)/src $(CPPFLAGS) da_radiance/parkind1.F > parkind1.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) parkind1.f

gsi_kinds.o:		\
			da_radiance/gsi_kinds.F
			$(CPP) -I./da_radiance -I$(RTTOV)/src $(CPPFLAGS) da_radiance/gsi_kinds.F > gsi_kinds.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) gsi_kinds.f

gsi_constants.o:	gsi_kinds.o      \
			da_radiance/gsi_constants.F
			$(CPP) -I./da_radiance -I$(RTTOV)/src $(CPPFLAGS) da_radiance/gsi_constants.F > gsi_constants.f
			$(FC) -c $(FCFLAGS) $(MODULE_DIRS) gsi_constants.f

##############################################################################

# DEPENDENCIES : only dependencies after this line (don't remove the word DEPENDENCIES)
