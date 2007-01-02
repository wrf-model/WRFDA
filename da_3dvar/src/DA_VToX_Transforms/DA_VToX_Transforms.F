MODULE DA_VToX_Transforms

!------------------------------------------------------------------------------
!  PURPOSE: Contains routines used to transform control variable V to model
!           variables X.
!
!  METHOD:  See individual routines.
!
!  HISTORY: 01/07/2000 - Creation of F90 version.           Dale Barker
!
! Additions: 
!           10/26/2004 - Poles Treatment for Global 3DVAR   Syed RH Rizvi
!------------------------------------------------------------------------------

! Driver layer modules
   USE module_domain
   USE module_configure
   USE module_driver_constants
   USE module_machine
   USE module_tiles
   USE module_dm

! Registry generated module
   USE module_state_description

   USE DA_Define_Structures
   USE DA_Tools
   USE DA_Recursive_Filter
   USE DA_Dynamics
   USE DA_Physics
   USE DA_Constants
   USE PAR_UTIL
   USE DA_SSMI
   USE da_spectral
   USE da_fftpack5

   IMPLICIT NONE

#ifdef DM_PARALLEL
   include 'mpif.h'
#endif

   CONTAINS

#include "DA_Check_EOF_Decomposition.inc"
#include "DA_Transform_VToVv.inc"
#include "DA_Transform_VToVv_Adj.inc"
#include "da_transform_vtox.inc"
#include "da_transform_vtox_adj.inc"
#include "DA_Transform_VpToX.inc"
#include "DA_Transform_VpToX_Adj.inc"

#include "da_apply_be.inc"
#include "da_apply_be_adj.inc"
#include "da_apply_rf_1v.inc"
#include "da_apply_rf_1v_adj.inc"
#include "da_apply_rf.inc"
#include "da_apply_rf_adj.inc"
#include "da_calc_covariance.inc"
#include "da_transform_bal.inc"
#include "da_transform_bal_adj.inc"

#include "DA_Transform_VvToVp.inc"
#include "DA_Transform_VvToVp_Adj.inc"

#include "DA_Transform_VpToVv.inc"
#include "DA_Vertical_Transform.inc"
#include "DA_Get_VPoles.inc"
#include "DA_Get_SPoles.inc"
#include "DA_Get_AVPoles.inc"
#include "DA_Get_ASPoles.inc"
#include "DA_Transform_VToVv_Global.inc"
#include "DA_Transform_VToVv_Global_Adj.inc"

END MODULE DA_VToX_Transforms
