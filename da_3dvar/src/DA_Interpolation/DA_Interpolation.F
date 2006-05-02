MODULE da_interpolation

   USE da_constants
   USE da_define_structures
   USE da_tools

   IMPLICIT NONE

CONTAINS

#include "to_zk.inc"

#include "Interp_Obs_lin_2D.inc"
#include "Interp_Obs_lin_2D_adj.inc"
#include "Interp_lin_2D.inc"
#include "Interp_lin_2D_adj.inc"
#include "Interp_lin_3D.inc"
#include "Interp_lin_3D_adj.inc"

END MODULE da_interpolation

