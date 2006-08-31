module da_minimisation

   !---------------------------------------------------------------------------
   ! PURPOSE: Collection of routines associated with minimisation. 
   !---------------------------------------------------------------------------

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
   ! see http://www.mmm.ucar.edu/wrf/WG2/topics/deref_kludge.htm
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

! Not sure if this should go here, but here it goes for now.
! This is a wrapper around the "system" call.  Will compile
! to just system for single processor. On DM_PARALLEL compiles,
! it will call system only on the 0th processor.

      SUBROUTINE system_4dv( cmd_line_in )
      IMPLICIT NONE
      CHARACTER*(*) cmd_line_in
      CHARACTER*256 cmd_line
#ifdef DM_PARALLEL
      INTEGER comm, ierr
      LOGICAL, EXTERNAL :: wrf_dm_on_monitor
      IF ( wrf_dm_on_monitor() ) THEN
         cmd_line = TRIM(cmd_line_in) // " monitor"
      ELSE
         cmd_line = TRIM(cmd_line_in) // " other"
      ENDIF
      call wrf_get_dm_communicator ( comm )
      call mpi_barrier ( comm, ierr )
#else
      cmd_line = cmd_line_in // " other"
#endif
      CALL system( cmd_line )
#ifdef DM_PARALLEL
      call mpi_barrier ( comm, ierr )
#endif

      RETURN
      END
