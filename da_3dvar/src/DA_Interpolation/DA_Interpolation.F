MODULE DA_Interpolation

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Tools

   IMPLICIT NONE

CONTAINS

#include "to_zk.inc"

#include "Interp_Obs_lin_2D.inc"
#include "Interp_Obs_lin_2D_adj.inc"
#include "Interp_lin_2D.inc"
#include "Interp_lin_2D_adj.inc"
#include "Interp_lin_3D.inc"
#include "Interp_lin_3D_adj.inc"

END MODULE DA_Interpolation

