MODULE da_interpolation

   USE da_control
   USE da_define_structures
   USE da_tools

   IMPLICIT NONE

CONTAINS

#include "da_to_zk.inc"

#include "da_interp_obs_lin_2d.inc"
#include "da_interp_obs_lin_2d_adj.inc"
#include "da_interp_lin_2d.inc"
#include "da_interp_lin_2d_adj.inc"
#include "da_interp_lin_3d.inc"
#include "da_interp_lin_3d_new.inc"
#include "da_interp_lin_3d_adj.inc"

END MODULE da_interpolation

