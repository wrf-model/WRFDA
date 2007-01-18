module da_radiance

   !---------------------------------------------------------------------------
   ! Purpose: module for radiance data assimilation. 
   !---------------------------------------------------------------------------

   use da_radiance1

   implicit none
   
contains

#include "da_qc_rad.inc"
#include "da_get_innov_vector_rad.inc"
#include "da_transform_xtoy_rad.inc"
#include "da_transform_xtoy_rad_adj.inc"
#include "da_calculate_grady_rad.inc"

#include "da_read_filtered_rad.inc"
#include "da_write_filtered_rad.inc"
#include "da_get_julian_time.inc"
#include "da_get_time_slots.inc"

#include "da_rttov_init.inc"
#include "da_rttov_direct.inc"
#include "da_rttov_tl.inc"
#include "da_rttov_ad.inc"
#include "da_read_bufrtovs.inc"
#include "da_read_bufrairs.inc"
#include "da_read_kma1dvar.inc"
#include "da_sort_rad.inc"
#include "da_setup_bufrtovs_structures.inc"
#include "da_status_rad.inc"

! This CRTM file has to be in da_radiance to avoid a circular dependancy

#include "da_crtm_init.inc"
#include "da_crtm_sensor_descriptor.inc"

end module da_radiance

