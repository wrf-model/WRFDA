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

! Registry generated module
   USE module_state_description

   IMPLICIT NONE

   TYPE(domain),                intent(inout) :: grid
   TYPE (grid_config_rec_type), intent(inout) :: config_flags

   !  Definitions of dummy arguments to solve
#include <em_dummy_decl.inc>

   TYPE (xbx_type)              :: xbx         ! For header & non-grid arrays.
   TYPE (be_type)               :: be          ! Background error structure.
   TYPE (y_type)                :: ob          ! Observation structure.
   TYPE (ob_type)               :: iv          ! Obs. increment structure.
   TYPE (y_type)                :: re          ! Residual (o-a) structure.
   TYPE (y_type)                :: y           ! y = H(x_inc) structure.
   TYPE (cv_type)               :: cv1d        ! Control variable structure (local processor grid).
   INTEGER                      :: it          ! External loop counter.
   type (j_type)                :: j           ! Cost function.

   INTEGER                      :: ids , ide , jds , jde , kds , kde , &
                                   ims , ime , jms , jme , kms , kme , &
                                   its , ite , jts , jte , kts , kte

   TYPE (cv_type)               :: xhat  ! Increament control variable
   INTEGER                      :: ierr  

!------------------------------------------------------------------------------

   write (6, '(/,A,/)') ' ***  VARIATIONAL ANALYSIS ***'

!------------------------------------------------------------------------------
!  [1.0] Read 3DVAR namelist:
!------------------------------------------------------------------------------

   CALL DA_Read_Namelist

!------------------------------------------------------------------------------
! If it is verification run set check_max_iv as .false.
!------------------------------------------------------------------------------
   if( (analysis_type(1:6) == "VERIFY" .or.  analysis_type(1:6) == "verify") ) then
     check_max_iv = .false.
   endif
!------------------------------------------------------------------------------
!  [2.0] Initialise 3D-Var parameters:
!------------------------------------------------------------------------------

   call da_init_3dvar( grid, xp, xb, &
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

   call DA_Setup_Background_Errors( xb, xbx, be, xp )

!------------------------------------------------------------------------------
!  [6.0] Allocate cv:
!------------------------------------------------------------------------------

   call calculate_cv_local_size( xbx, xp, be, cv1d, cv_size )
   call da_allocate_cv( cv_size, cv1d )
   call da_allocate_cv( cv_size, xhat )
      
!------------------------------------------------------------------------------
!  [7.0] Test:
!------------------------------------------------------------------------------

   if ( test_transforms .or. Testing_3DVAR ) then
      CALL da_get_innov_vector( it, ob, iv, &
                                grid , config_flags , &
#include <em_dummy_args.inc>
                 )


      call da_allocate_y( iv, re )
      call da_allocate_y( iv, y )

      call da_check( xb, xbx, be, iv, cv1d, &
                     xa, vv, vp, xp, ob, y, &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     its, ite, jts, jte, kts, kte )
   endif

!------------------------------------------------------------------------------
!  [8] Outerloop
!------------------------------------------------------------------------------

   DO it = 1, max_ext_its

!     [8.1] Calculate nonlinear model trajectory 

      if (lvar4d) then
#ifdef WRFVAR
         call system("runvar4dnl.csh")
#endif
      endif

!------------------------------------------------------------------------------
!     [8.2] Calculate innovation vector (O-B):

      CALL da_get_innov_vector( it, ob, iv, &
                                grid , config_flags , &
#include <em_dummy_args.inc>
                 )

!     [8.3] Interpolate x_g to low resolution grid

!     [8.4] Minimize cost function:

      call da_allocate_y( iv, re )
      call da_allocate_y( iv, y )

!------------------------------------------------------------------------------
      CALL DA_Minimise( grid, config_flags,                  &
                        it, ob, xb, xbx, be, iv, xhat%array, &
                        cv1d, xa, vv, vp, xp, re, y, j,      &
                        ids, ide, jds, jde, kds, kde,        &
                        ims, ime, jms, jme, kms, kme,        &
                        its, ite, jts, jte, kts, kte         )
!------------------------------------------------------------------------------
   
     if( (analysis_type(1:6) == "QC-OBS" .or.  analysis_type(1:6) == "qc-obs") )then
       if( ob_format == 1) then
         write(*,'(a)')  &
         'Since input Obs are in BUFR-format Filtered-Obs (fort.192) will not be written'
       elseif( ob_format == 2) then
#ifdef DM_PARALLEL 
         call MPI_COMM_SIZE(MPI_COMM_WORLD, numb_procs, ierr)
         print*,' I am using num_proc = ',numb_procs   
         if( numb_procs == 1) then
           if( it == 1) CALL da_write_filtered_obs(ob, iv, xp)
         else
           write(*,'(a,i3)')'Since it is MPI-run with processore (>1) ',numb_procs
           write(*,'(a)')   ' Filtered-Obs (fort.192)  will not be written'
         endif
#else
         if( it == 1) CALL da_write_filtered_obs(ob, iv, xp)
#endif
       else
         print*,' Unknown Observation format: ',ob_format
       endif
     end if

!     [8.5] Update latest analysis solution:

      call da_transform_vtox( xb, xbx, be, xhat%array, vv, vp, xp, xa,  &
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
!  [8.0] Output 3DVAR analysis and analysis increments:
!------------------------------------------------------------------------------

!------------------------------------------------------------------------------
! Following lines are added by  Syed RH Rizvi due to following reasons:
! (1) To accomodate KMA FG(fg_format =  3)
! (2) Seperate treatment for WRF and KMA updating of Xa and Xb array
! (3) Treatment for MM5 format FG(fg_format = 2) remains same as original
!------------------------------------------------------------------------------
      call DA_Transfer_XatoAnalysis( it, xbx, grid, config_flags ,&
#include <em_dummy_args.inc>
                           )
   END DO

!------------------------------------------------------------------------------
!  [9.0] Tidy up:
!------------------------------------------------------------------------------

   call da_deallocate_cv( cv1d )
   call da_deallocate_cv( xhat )
   call da_deallocate_y( re )
   call da_deallocate_y( y )
   
   write(6,'(a)') ' *** 3DVAR completed successfully ***'
   write(6,*)

   CALL wrf_debug ( 200 , ' call end of solve_v3d' )

CONTAINS

#include "da_init_3dvar.inc"

END SUBROUTINE solve_v3d

