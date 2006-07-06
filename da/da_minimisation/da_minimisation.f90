      module da_minimisation

!------------------------------------------------------------------------------
!     PURPOSE: Collection of routines associated with minimisation.
!
!     METHOD:  Straightforward definitions.
!------------------------------------------------------------------------------

      use da_constants
      use da_define_structures
      use da_vtox_transforms
      use da_obs
      use da_metar
      use da_geoamv
      use da_polaramv
      use da_ships
      use da_synop
      use da_sound
      use da_airep
      use da_pilot
      use da_gpspw
      use da_gpsref
      use da_ssmi
      use da_satem
      use da_pseudo
      use da_bogus
      use da_profiler
      use da_buoy 
      use da_setup_structures
      use da_qscat
      use da_radiance
      use da_airsr     
      use module_get_file_names ! for system call on cray

      use da_wrfvar_io

      implicit none

      integer :: infoc
      logical :: brackt,stage1
      real :: dg,dgm,dginit,dgtest,dgx,dgxm,dgy,dgym, &
              finit,ftest1,fm,fx,fxm,fy,fym,p5,p66,stx,sty, &
              stmin,stmax,width,width1,xtrapf

! These are used if -DDEREF_KLUDGE is compiled
!  see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
      INTEGER     :: sm31  , em31  , sm32  , em32  , sm33  , em33
      INTEGER     :: sm31x , em31x , sm32x , em32x , sm33x , em33x
      INTEGER     :: sm31y , em31y , sm32y , em32y , sm33y , em33y

      LOGICAL, EXTERNAL :: wrf_dm_on_monitor

      private :: da_dot, da_dot_cv
      private :: infoc, brackt, stage1
      private :: dg,dgm,dginit,dgtest,dgx,dgxm,dgy,dgym, &
                 finit,ftest1,fm,fx,fxm,fy,fym,p5,p66,stx,sty, &
                 stmin,stmax,width,width1,xtrapf,zero

      contains
      
#include "da_calculate_j.inc"
#include "da_calculate_jo_and_grady.inc"
#include "da_calculate_residual.inc"
#include "da_get_var_diagnostics.inc"
#include "da_get_innov_vector.inc"
#include "da_minimisation_warning.inc"
#include "da_sum_reals.inc"
#include "da_dot.inc"
#include "da_dot_cv.inc"
#include "da_write_diagnostics.inc"
#include "da_minimise_cg.inc"
#include "da_calculate_grady.inc"
#include "da_transform_vtoy.inc"
#include "da_transform_vtoy_adj.inc"


      end module da_minimisation

