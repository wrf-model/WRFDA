!MEDIATION_LAYER:SOLVE_V3D
!

SUBROUTINE solve_v3d ( grid , config_flags , &
#include <em_dummy_args.inc>
                 )


! Driver layer modules
   USE module_domain
   USE module_configure
   USE module_driver_constants
   USE module_machine
   USE module_tiles
   USE module_dm
! Mediation layer modules
! Model layer modules
   USE module_model_constants

   USE DA_Constants
   USE DA_Define_Structures
   USE DA_Setup_Structures
   USE DA_Test
   USE DA_Tools
   USE DA_Minimisation
   USE par_util
   USE da_tracing

! Registry generated module
   USE module_state_description

   IMPLICIT NONE

#ifdef DM_PARALLEL 
   INCLUDE 'mpif.h'
#endif

   TYPE(domain),                intent(inout) :: grid
   TYPE (grid_config_rec_type), intent(inout) :: config_flags

   !  Definitions of dummy arguments to solve
#include <em_dummy_decl.inc>

   TYPE (xbx_type)              :: xbx         ! For header & non-grid arrays.
   TYPE (be_type)               :: be          ! Background error structure.
   real, allocatable            :: cvt(:)      ! Control variable structure.
   real, allocatable            :: xhat(:)     ! Control variable structure.
   TYPE (y_type)                :: ob          ! Observation structure.
   TYPE (ob_type)               :: iv          ! Obs. increment structure.
   TYPE (y_type)                :: re          ! Residual (o-a) structure.
   TYPE (y_type)                :: y           ! y = H(x_inc) structure.
   INTEGER                      :: it          ! External loop counter.
   type (j_type)                :: j           ! Cost function.

   INTEGER                      :: ids , ide , jds , jde , kds , kde , &
                                   ims , ime , jms , jme , kms , kme , &
                                   its , ite , jts , jte , kts , kte

   integer                      :: cv_size
   real                         :: j_grad_norm_target ! TArget j norm.

   IF (trace_use) CALL da_trace_entry("solve_v3d")

!------------------------------------------------------------------------------

   CALL wrf_message("***  VARIATIONAL ANALYSIS ***")

!------------------------------------------------------------------------------
!  [1.0] Read WRFVAR namelist:
!------------------------------------------------------------------------------

   CALL DA_Read_Namelist

!------------------------------------------------------------------------------
!  [2.0] Initialise 3D-Var parameters:
!------------------------------------------------------------------------------

   call da_init_wrfvar( grid, xp, xb, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )

!------------------------------------------------------------------------------
!  [3.0] Set up first guess field (xb):
!------------------------------------------------------------------------------

   CALL DA_Setup_FirstGuess( xbx, grid, &
#include <em_dummy_args.inc>
                           )

!------------------------------------------------------------------------------
!  [4.0] Set up observations (ob):
!------------------------------------------------------------------------------

   call DA_Setup_Obs_Structures( xp, ob, iv )

!------------------------------------------------------------------------------
!  [5.0] Set up background errors (be):
!------------------------------------------------------------------------------

   call DA_Setup_Background_Errors( xb, xbx, be, xp, &
                                    its, ite, jts, jte, kts, kte, &
                                    ids, ide, jds, jde, kds, kde )
   cv_size = be % cv % size

!------------------------------------------------------------------------------
!  [6.0] Set up ensemble perturbation input:
!------------------------------------------------------------------------------

   ep % ne = be % ne
   if ( be % ne > 0 ) call da_setup_flow_predictors( ide, jde, kde, be % ne, ep )

!------------------------------------------------------------------------------
!  [7.0] Setup control variable (cv):
!------------------------------------------------------------------------------

   allocate( cvt(1:cv_size) )
   allocate( xhat(1:cv_size) )
   call da_initialize_cv( cv_size, cvt )
   call da_initialize_cv( cv_size, xhat )

   call da_zero_vp_type( vv )
   call da_zero_vp_type( vp )

!------------------------------------------------------------------------------
!  [8] Outerloop
!------------------------------------------------------------------------------

   j_grad_norm_target = 1.0

   DO it = 1, max_ext_its

!     [8.1] Calculate nonlinear model trajectory 

      if (lvar4d) then
         call system("runvar4dnl.csh")
      endif

!     [8.2] Calculate innovation vector (O-B):

      CALL da_get_innov_vector( it, ob, iv, &
                                grid , config_flags , &
#include <em_dummy_args.inc>
                 )

      if ( testing_wrfvar ) then
         call da_check( cv_size, xb, xbx, be, ep, iv, &
                        xa, vv, vp, xp, ob, y, &
                        ids, ide, jds, jde, kds, kde, &
                        ims, ime, jms, jme, kms, kme, &
                        its, ite, jts, jte, kts, kte )
      end if

!     Write "clean" QCed observations if requested:
      if ( write_filtered_obs .and. it == 1 ) call da_write_filtered_obs( ob, iv, xp )

      if (lmonitoring) call wrf_shutdown

!     [8.3] Interpolate x_g to low resolution grid

!     [8.4] Minimize cost function:

      call da_allocate_y( iv, re )
      call da_allocate_y( iv, y )

      CALL DA_Minimise_CG( grid, config_flags,                  &
                           it, be % cv % size, &
                           ob, xb, xbx, be, ep, iv, &
                           j_grad_norm_target, xhat, cvt, &
                           xa, vv, vp, xp, re, y, j,    &
                           ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           its, ite, jts, jte, kts, kte         )

!     [8.5] Update latest analysis solution:

      call da_transform_vtox( cv_size, xb, xbx, be, ep, xhat, vv, vp, xp, xa,  &
                              ids, ide, jds, jde, kds, kde,             &
                              ims, ime, jms, jme, kms, kme,             &
                              its, ite, jts, jte, kts, kte )

!     [8.6] Only when use_RadarObs = .false. and W_INCREMENTS =.true.,
!           the W_increment need to be diagnosed:

     if (W_INCREMENTS .and. .not.use_RadarObs) then

         CALL DA_UVPRho_To_W_Lin( xb, xa, xp,                 &
                                  ids,ide, jds,jde, kds,kde,  &
                                  ims,ime, jms,jme, kms,kme,  &
                                  its,ite, jts,jte, kts, kte )

         call wrf_dm_halo(xp%domdesc,xp%comms,xp%halo_id13)

      endif

!     [8.7] Write out diagnostics

      CALL DA_Write_Diagnostics( ob, iv, re, y, xp, xa, j )

!------------------------------------------------------------------------------
!  [8.0] Output WRFVAR analysis and analysis increments:
!------------------------------------------------------------------------------

      call DA_Transfer_XatoAnalysis( it, xbx, grid, config_flags ,&
#include <em_dummy_args.inc>
                           )
   END DO

!------------------------------------------------------------------------------
!  [9.0] Tidy up:
!------------------------------------------------------------------------------

   deallocate ( cvt )
   deallocate ( xhat )
   call da_deallocate_obs(iv)
   call da_deallocate_y( re )
   call da_deallocate_y( y )

   CALL wrf_message("*** WRF-Var completed successfully ***")

   CALL wrf_debug ( 200 , ' call end of solve_v3d' )

   IF (trace_use) CALL da_trace_exit("solve_v3d")

CONTAINS

#include "da_init_wrfvar.inc"

END SUBROUTINE solve_v3d

