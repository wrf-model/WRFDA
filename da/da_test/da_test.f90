module da_test

!------------------------------------------------------------------------------
!     PURPOSE: Collection of routines associated with minimisation.
!
!     METHOD:  Straightforward definitions.
!
!     HISTORY: 01/07/2000 - Creation.              Dale Barker
!------------------------------------------------------------------------------

   use da_control
   use da_define_structures
   use da_physics
   use da_vtox_transforms
   use da_obs
   use da_airep
   use da_gpspw
   use da_gpsref
   use da_metar
   use da_pilot
   use da_radar
   use da_ssmi
   use da_satem
   use da_geoamv
   use da_polaramv
   use da_ships
   use da_sound
   use da_synop
   use da_pseudo
   use da_profiler
   use da_buoy 
   use da_bogus 
   use da_setup_structures
   use da_qscat
   use da_wrfvar_io
   use da_minimisation
   use da_reporting

   implicit none

#ifdef DM_PARALLEL
   INCLUDE 'mpif.h'
#if ( DWORDSIZE != RWORDSIZE )
#define TRUE_MPI_REAL     MPI_REAL
#else
#define TRUE_MPI_REAL     MPI_REAL8
#endif
#endif

contains

#include "da_check_balance.inc"
#include "da_check_cvtovv_adjoint.inc"
#include "da_check_vtox_adjoint.inc"
#include "da_check_vptox_adjoint.inc"
#include "da_check_vp_errors.inc"
#include "da_check_vvtovp_adjoint.inc"
#include "da_check_xtovptox_errors.inc"
#include "da_check_xtoy_adjoint.inc"
#include "da_check_xtoy_adjoint_airep.inc"
#include "da_check_xtoy_adjoint_gpspw.inc"
#include "da_check_xtoy_adjoint_gpsref.inc"
#include "da_check_xtoy_adjoint_metar.inc"
#include "da_check_xtoy_adjoint_pilot.inc"
#include "da_check_xtoy_adjoint_ssmi.inc"
#include "da_check_xtoy_adjoint_ssmi_rv.inc"
#include "da_check_xtoy_adjoint_ssmi_tb.inc"
#include "da_check_xtoy_adjoint_satem.inc"
#include "da_check_xtoy_adjoint_geoamv.inc"
#include "da_check_xtoy_adjoint_polaramv.inc"
#include "da_check_xtoy_adjoint_ships.inc"
#include "da_check_xtoy_adjoint_radar.inc"
#include "da_check_xtoy_adjoint_bogus.inc"
#include "da_check_xtoy_adjoint_sound.inc"
#include "da_check_xtoy_adjoint_sonde_sfc.inc"
#include "da_check_xtoy_adjoint_synop.inc"
!#include "da_test_vxtransform.inc"
#include "da_transform_xtovp.inc"
#include "da_check.inc"
#include "da_check_xtoy_adjoint_pseudo.inc"
#include "da_check_xtoy_adjoint_qscat.inc"
#include "da_check_xtoy_adjoint_ssmt1.inc"
#include "da_check_xtoy_adjoint_ssmt2.inc"
#include "da_check_xtoy_adjoint_profiler.inc"
#include "da_check_xtoy_adjoint_buoy.inc"
#include "da_setup_testfield.inc"
#include "da_check_sfc_assi.inc"
#include "da_check_psfc.inc"
#include "da_set_tst_trnsf_fld.inc"
#include "da_check_vtoy_adjoint.inc"
#include "da_get_y_lhs_value.inc"

end module da_test
