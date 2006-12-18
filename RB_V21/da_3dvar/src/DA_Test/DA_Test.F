module DA_Test

!------------------------------------------------------------------------------
!     PURPOSE: Collection of routines associated with minimisation.
!
!     METHOD:  Straightforward definitions.
!
!     HISTORY: 01/07/2000 - Creation.              Dale Barker
!------------------------------------------------------------------------------

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Physics
   USE DA_VToX_Transforms
   USE DA_Obs
   USE DA_Airep
   USE DA_Gpspw
   USE DA_Gpsref
   USE DA_Metar
   USE DA_Pilot
   USE DA_Radar
   USE DA_SSMI
   USE DA_Satem
   USE DA_GeoAMV
   USE DA_PolarAMV
   USE DA_Ships
   USE DA_Sound
   USE DA_Synop
   USE da_pseudo
   USE DA_Profiler
   USE DA_Buoy 
   USE DA_Bogus 
   USE DA_Setup_Structures
   USE DA_Tools
   use da_qscat

   implicit none
   
#ifdef DM_PARALLEL
   LOGICAL, EXTERNAL :: wrf_dm_on_monitor
#endif

#if ( DWORDSIZE != RWORDSIZE )
#define TRUE_MPI_REAL     MPI_REAL
#else
#define TRUE_MPI_REAL     MPI_REAL8
#endif

contains

#include "DA_Check_Balance.inc"
#include "DA_Check_CvToVv_Adjoint.inc"
#include "DA_Check_VToX_Adjoint.inc"
#include "DA_Check_VpToX_Adjoint.inc"
#include "DA_Check_Vp_Errors.inc"
#include "DA_Check_VvToVp_Adjoint.inc"
#include "DA_Check_XToVpToX_Errors.inc"
#include "DA_Check_XToY_Adjoint.inc"
#include "DA_Check_XToY_Adjoint_Airep.inc"
#include "DA_Check_XToY_Adjoint_Gpspw.inc"
#include "DA_Check_XToY_Adjoint_Gpsref.inc"
#include "DA_Check_XToY_Adjoint_Metar.inc"
#include "DA_Check_XToY_Adjoint_Pilot.inc"
#include "DA_Check_XToY_Adjoint_SSMI.inc"
#include "DA_Check_XToY_Adjoint_SSMI_Rv.inc"
#include "DA_Check_XToY_Adjoint_SSMI_Tb.inc"
#include "DA_Check_XToY_Adjoint_Satem.inc"
#include "DA_Check_XToY_Adjoint_GeoAMV.inc"
#include "DA_Check_XToY_Adjoint_PolarAMV.inc"
#include "DA_Check_XToY_Adjoint_Ships.inc"
#include "DA_Check_XToY_Adjoint_Radar.inc"
#include "DA_Check_XToY_Adjoint_Bogus.inc"
#include "DA_Check_XToY_Adjoint_Sound.inc"
#include "DA_Check_XToY_Adjoint_Sonde_sfc.inc"
#include "DA_Check_XToY_Adjoint_Synop.inc"
#include "DA_Test_VXTransform.inc"
#include "DA_Transform_XToVp.inc"
#include "da_check.inc"
#include "da_check_xtoy_adjoint_pseudo.inc"
#include "da_check_xtoy_adjoint_qscat.inc"
#include "da_check_xtoy_adjoint_ssmt1.inc"
#include "da_check_xtoy_adjoint_ssmt2.inc"
#include "DA_Check_XToY_Adjoint_Profiler.inc"
#include "DA_Check_XToY_Adjoint_Buoy.inc"
#include "da_setup_testfield.inc"
#include "check_sfc_assi.inc"
#include "set_tst_trnsf_fld.inc"

end module DA_Test
