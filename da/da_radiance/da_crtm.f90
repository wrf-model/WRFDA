module da_crtm

   !---------------------------------------------------------------------------
   ! Purpose: module for CRTM radiance data assimilation. 
   !---------------------------------------------------------------------------

   use module_domain, only : xpose_type, x_type, xb_type
#ifdef CRTM
   use module_radiance, only : CRTM_RTSolution_type,CRTM_ChannelInfo_type, &
      CRTM_Atmosphere_type, CRTM_Surface_type,CRTM_GeometryInfo_type, &
      CRTM_Adjoint,CRTM_Forward,CRTM_K_Matrix,CRTM_Tangent_Linear, &
      CRTM_Allocate_Atmosphere,H2O_ID,GRAUPEL_CLOUD,ICE_CLOUD,HAIL_CLOUD, &
      INVALID_WMO_SENSOR_ID,NEW_SNOW,rain_cloud,snow_cloud,O3_ID, GRASS_SOIL, &
      WMO_AMSRE, WATER_CLOUD, WMO_AMSUB, WMO_AMSUA,WMO_SSMI, Sensor_Descriptor, &
      crtm_destroy_atmosphere,crtm_set_channelinfo,crtm_sensor_name, &
      crtm_allocate_surface,crtm_destroy_surface,crtm_assign_atmosphere, &
      crtm_assign_surface,crtm_zero_surface,CRTM_Zero_Atmosphere, satinfo, &
      time_slots,crtm_platform_name, crtm_init,inst_name,platform_name
#endif

   use da_control, only : trace_use, crtm_cloud, gravity,stdout, biascorr, &
      biasprep, qc_rad,missing_r,rtminit_sensor,rtminit_nsensor, filename_len, &
      use_error_factor_rad,read_biascoef, analysis_date,time_window_max, &
      time_window_min,print_detail_obs,num_fgat_time,rtminit_platform, &
      rtminit_satid
   use da_define_structures, only : y_type, ob_type
   use da_interpolation, only : da_interp_lin_2d,da_interp_lin_2d_adj
   use da_radiance1, only : da_biasprep,da_detsurtyp,da_biascorr_rad, &
      da_qc_rad, da_qc_crtm, da_get_time_slots

   use da_reporting, only : da_error,message
   use da_tools1, only : da_free_unit, da_get_unit
   use da_tracing, only : da_trace_entry, da_trace_exit

contains

#include "da_transform_xtoy_crtmk.inc"
#include "da_transform_xtoy_crtmk_f.inc"
#include "da_transform_xtoy_crtmk_f_adj.inc"
#include "da_transform_xtoy_crtmk_adj.inc"
#include "da_transform_xtoy_crtm.inc"
#include "da_transform_xtoy_crtm_adj.inc"
#include "da_get_innov_vector_crtmk.inc"
#include "da_get_innov_vector_crtm.inc"
#include "da_crtm_tl.inc"
#include "da_crtm_k.inc"
#include "da_crtm_direct.inc"
#include "da_crtm_ad.inc"
#include "da_crtm_init.inc"
#include "da_crtm_sensor_descriptor.inc"

end module da_crtm
