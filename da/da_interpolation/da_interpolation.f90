module da_interpolation

   use da_control, only : trace_use, trace_use_frequent, missing_r, &
      anal_type_verify, v_interp_h, v_interp_p,ims,ime,jms,jme,kms,kme, &
      kts,kte, trace_use_dull
   use da_define_structures, only : model_loc_type, infa_type
   use da_tools, only : da_togrid
   use da_tracing, only : da_trace_entry, da_trace_exit

   implicit none

contains

#include "da_to_zk.inc"
#include "da_to_zk_new.inc"

#include "da_interp_lin_2d_newer.inc"
#include "da_interp_lin_2d_newest.inc"
#include "da_interp_lin_2d_adj_newer.inc"
#include "da_interp_lin_2d_adj_newest.inc"
#include "da_interp_lin_3d_newest.inc"
#include "da_interp_lin_3d_adj_newest.inc"

end module da_interpolation

