      module da_minimisation

!------------------------------------------------------------------------------
!     PURPOSE: Collection of routines associated with minimisation.
!
!     METHOD:  Straightforward definitions.
!
!     HISTORY: 01/07/2000 - Creation.                           Dale Barker
!   Additions: 07/15/2003 - Added Buo and Profiler               S.R.H. Rizvi
!   Additions: 07/20/2003 - Added DA_Add_PBL_And_SFC_Info        S.R.H. Rizvi
!------------------------------------------------------------------------------

      USE DA_Constants
      USE DA_Define_Structures
      USE DA_VToX_Transforms
      USE DA_Obs
      USE DA_Metar
      USE DA_Satob
      USE DA_Ships
      USE DA_Synop
      USE DA_Sound
      USE DA_Airep
      USE DA_Pilot
      USE DA_Gpspw
      USE DA_Gpsref
      USE DA_SSMI
      USE DA_Satem
      USE da_pseudo
      USE DA_Profiler
      USE DA_Buoy 
      USE DA_Setup_Structures
      use da_qscat

      USE module_wrf_3dvar_io

      implicit none

      integer :: infoc
      logical :: brackt,stage1
      real :: dg,dgm,dginit,dgtest,dgx,dgxm,dgy,dgym, &
              finit,ftest1,fm,fx,fxm,fy,fym,p5,p66,stx,sty, &
              stmin,stmax,width,width1,xtrapf,zero

#ifdef DM_PARALLEL
!     INCLUDE 'mpif.h'

      LOGICAL, EXTERNAL :: wrf_dm_on_monitor

#if ( DWORDSIZE != RWORDSIZE )
#define TRUE_MPI_REAL     MPI_REAL
#else
#define TRUE_MPI_REAL     MPI_REAL8
#endif
#endif

      contains
      
#include <da_calculate_j.inc>
#include <da_calculate_jo_and_grady.inc>
#include <da_calculate_residual.inc>
#include <da_get_var_diagnostics.inc>
#include <da_get_innov_vector.inc>
#include <da_lmdir.inc>
#include <da_minimisation_warning.inc>
#include <da_minimise.inc>
#include <da_sum_reals.inc>
#include <da_va15bd.inc>
#include <da_vd05ad.inc>
#include <da_vd05bd.inc>
#include <da_dot.inc>
#include <da_write_diagnostics.inc>
#include <DA_Minimise_CG.inc>
#include <DA_Minimise_QN.inc>
#include "DA_Calculate_GradY.inc"

      end module da_minimisation

