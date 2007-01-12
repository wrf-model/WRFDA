module da_crtm

   !---------------------------------------------------------------------------
   ! Purpose: module for CRTM radiance data assimilation. 
   !---------------------------------------------------------------------------

   use module_radiance
   use da_radiance

   use da_control
   use da_define_structures
   use da_interpolation
   use da_statistics
   use da_tools
   use da_par_util
   use da_reporting


contains

#include "da_transform_xtoy_rad_crtmk.inc"
#include "da_transform_xtoy_crtmk_f.inc"
#include "da_transform_xtoy_crtmk_f_adj.inc"
#include "da_transform_xtoy_rad_crtmk_adj.inc"
#include "da_transform_xtoy_rad_crtm.inc"
#include "da_transform_xtoy_rad_crtm_adj.inc"
#include "da_get_innov_vector_rad_crtmk.inc"
#include "da_get_innov_vector_rad_crtm.inc"
#include "da_crtm_tl.inc"
#include "da_crtm_k.inc"
#include "da_crtm_direct.inc"
#include "da_crtm_ad.inc"

! da_crtm_init.inc and da_crtm_sensor_descriptor
! in da_radiance to avoid a circular dependancy between 
! modules

end module da_crtm

