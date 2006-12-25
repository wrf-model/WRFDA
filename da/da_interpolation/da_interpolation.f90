module da_interpolation

   use da_tools
   use da_control
   use module_domain, only : xpose_type
   use da_tracing
   use da_define_structures, only : da_gauss_noise

   implicit none

contains

#include "da_to_zk.inc"
#include "da_to_zk_new.inc"

#include "da_interp_obs_lin_2d.inc"
#include "da_interp_obs_lin_2d_adj.inc"
#include "da_interp_lin_2d.inc"
#include "da_interp_lin_2d_new.inc"
#include "da_interp_lin_2d_adj.inc"
#include "da_interp_lin_2d_adj_new.inc"
#include "da_interp_lin_3d.inc"
#include "da_interp_lin_3d_new.inc"
#include "da_interp_lin_3d_adj.inc"
#include "da_interp_lin_3d_adj_new.inc"

end module da_interpolation

