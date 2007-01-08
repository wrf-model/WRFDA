module da_obs_io

   use da_obs

   implicit none

contains

#include "da_read_obs.inc"
#include "da_scan_obs.inc"
#include "da_read_radar.inc"
#include "da_scan_radar.inc"
#include "da_read_errfac.inc"
#include "da_use_obs_errfac.inc"
#include "da_write_obs.inc"
#include "da_write_filtered_obs.inc"
#include "da_write_y.inc"
#include "da_read_bufr_obs.inc"
#include "da_scan_bufr_obs.inc"
#include "da_final_write_obs.inc"
#include "da_final_write_y.inc"
#include "da_read_y_unit.inc"
#include "da_read_rand_unit.inc"
#include "da_read_omb_tmp.inc"
#include "da_write_noise_to_ob.inc"
#include "da_final_write_filtered_obs.inc"

end module da_obs_io
