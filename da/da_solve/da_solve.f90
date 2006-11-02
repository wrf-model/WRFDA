!MEDIATION_LAYER:SOLVE_DA

subroutine da_solve ( grid , config_flags , &
#include "em_dummy_new_args.inc"
                 )

   ! Driver layer modules
   use module_domain
   use module_configure
   use module_machine
   use module_tiles
   use module_dm
   ! Mediation layer modules
   ! Model layer modules
   use module_model_constants

   use da_control
   use da_define_structures
   use da_setup_structures
   use da_test
   use da_minimisation
   use da_tracing
   use da_reporting
   use module_get_file_names ! for system interface on cray

   implicit none

   type (domain),                intent(inout) :: grid
   type (grid_config_rec_type), intent(inout) :: config_flags

   ! Definitions of dummy arguments to solve
#include "em_dummy_new_decl.inc"

   type (xbx_type)              :: xbx         ! For header & non-grid arrays.
   type (be_type)               :: be          ! Background error structure.
   real, allocatable            :: cvt(:)      ! Control variable structure.
   real, allocatable            :: xhat(:)     ! Control variable structure.
   type (y_type)                :: ob          ! Observation structure.
   type (ob_type)               :: iv          ! Obs. increment structure.
   type (y_type)                :: re          ! Residual (o-a) structure.
   type (y_type)                :: y           ! y = H(x_inc) structure.
   integer                      :: it          ! External loop counter.
   type (j_type)                :: j           ! Cost function.

   integer                      :: ids , ide , jds , jde , kds , kde , &
                                   ims , ime , jms , jme , kms , kme , &
                                   its , ite , jts , jte , kts , kte

   integer                      :: cv_size
   real                         :: j_grad_norm_target ! TArget j norm.
#ifdef DM_PARALLEL
   integer                      :: ierr,comm
#endif
   integer                      :: wrf_done_unit

   if (trace_use) call da_trace_entry("da_solve")

   !---------------------------------------------------------------------------
   ! If it is verification run set check_max_iv as .false.
   !---------------------------------------------------------------------------

   if (anal_type_verify) then
      check_max_iv = .false.
   endif

   if (cv_options_hum < 1 .or. cv_options_hum > 3) then
      write(unit=errmsg(1),fmt='(A,I3)') &
         'Invalid cv_options_hum = ', cv_options_hum
      call wrf_error_fatal(errmsg(1))
   end if

   if (vert_corr == 2) then
      if (vertical_ip < 0 .or. vertical_ip > 2) then
         write (unit=errmsg(1),fmt='(A,I3)') &
           'Invalid vertical_ip = ', &
           vertical_ip
         call da_warning(__FILE__,__LINE__,errmsg(1:1))
      end if
   end if

   if (0.5 * real(rf_passes) /= real(rf_passes / 2)) then
      write(unit=stdout,fmt='(A,I4,A)')'rf_passes = ', &
                         rf_passes, ' .Should be even.'
      rf_passes = int(real(rf_passes / 2))
      write(unit=stdout,fmt='(A,I4)') 'Resetting rf_passes = ', rf_passes
   end if

   if (anal_type_randomcv) then
      ntmax = 0
      write(unit=stdout)' Resetting ntmax = 0 for analysis_type = randomcv' 
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Initialise wrfvar parameters:
   !---------------------------------------------------------------------------

   call da_solve_init( grid, grid%xp, grid%xb, &
                       ids, ide, jds, jde, kds, kde, &
                       ims, ime, jms, jme, kms, kme, &
                       its, ite, jts, jte, kts, kte )

   !---------------------------------------------------------------------------
   ! [3.0] Set up first guess field (grid%xb):
   !---------------------------------------------------------------------------

   call da_setup_firstguess( xbx, grid, &
#include "em_dummy_new_args.inc"
                           )

   !---------------------------------------------------------------------------
   ! [4.0] Set up observations (ob):
   !---------------------------------------------------------------------------

   call da_setup_obs_structures( grid%xp, ob, iv )

   !---------------------------------------------------------------------------
   ! [5.0] Set up background errors (be):
   !---------------------------------------------------------------------------

   call da_setup_background_errors( grid%xb, xbx, be, grid%xp, &
                                    its, ite, jts, jte, kts, kte, &
                                    ids, ide, jds, jde, kds, kde )
   cv_size = be % cv % size

   !---------------------------------------------------------------------------
   ! [6.0] Set up ensemble perturbation input:
   !---------------------------------------------------------------------------

   grid%ep % ne = be % ne
   if (be % ne > 0) then
      call da_setup_flow_predictors( ide, jde, kde, be % ne, grid%ep )
   end if

   !---------------------------------------------------------------------------
   ! [7.0] Setup control variable (cv):
   !---------------------------------------------------------------------------

   allocate( cvt(1:cv_size) )
   allocate( xhat(1:cv_size) )
   call da_initialize_cv( cv_size, cvt )
   call da_initialize_cv( cv_size, xhat )
      
   call da_zero_vp_type( grid%vv )
   call da_zero_vp_type( grid%vp )

   if (test_transforms .or. Testing_WRFVAR) then
      call da_get_innov_vector( it, ob, iv, &
                                grid , config_flags , &
#include "em_dummy_new_args.inc"
                 )

      call da_allocate_y (iv, re)
      call da_allocate_y (iv, y)

      allocate (cvt(1:cv_size))
      allocate (xhat(1:cv_size))
      call da_initialize_cv (cv_size, cvt)
      call da_initialize_cv (cv_size, xhat)

      call da_check (cv_size, grid%xb, xbx, be, grid%ep, iv, &
                     grid%xa, grid%vv, grid%vp, grid%xp, ob, y, &
                     ids, ide, jds, jde, kds, kde, &
                     ims, ime, jms, jme, kms, kme, &
                     its, ite, jts, jte, kts, kte)
      call da_zero_vp_type (grid%vv)
      call da_zero_vp_type (grid%vp)
   endif

   !---------------------------------------------------------------------------
   ! [8] Outerloop
   !---------------------------------------------------------------------------

   j_grad_norm_target = 1.0

   DO it = 1, max_ext_its

      ! [8.1] Calculate nonlinear model trajectory 

      if (var4d) then
         call da_trace ("da_solve","Starting da_run_wrf_nl.ksh")
#ifdef DM_PARALLEL
         if (var4d_coupling == var4d_coupling_disk_simul) then
            call da_system_4dvar ("da_run_wrf_nl.ksh pre ")
            ! call system("./wrf.exe -rmpool 1")
            IF (rootproc) THEN
               call system("rm -rf nl/wrf_done")
               call system("touch nl/wrf_go_ahead")
               call da_get_unit(wrf_done_unit)
               do while ( .true. )
                  open(wrf_done_unit,file="wrf_done",status="old",err=303)
                  close(wrf_done_unit)
                  exit
303                  continue
                  call system("sync")
                  call system("slegrid%ep 1")
               enddo
               call da_free_unit(wrf_done_unit)
            endif
            call wrf_get_dm_communicator ( comm )
            call mpi_barrier( comm, ierr )

            call da_system_4dvar("da_run_wrf_nl.ksh post ")
         else
            call system("da_run_wrf_nl.ksh")
         end if
#else
         call system("da_run_wrf_nl.ksh")
#endif
         call da_trace("da_solve","Finished da_run_wrf_nl.ksh")
      endif

      ! [8.2] Calculate innovation vector (O-B):

      call da_get_innov_vector( it, ob, iv, &
                                grid , config_flags , &
#include "em_dummy_new_args.inc"
                 )

      if (test_transforms) then
         call da_check( cv_size, grid%xb, xbx, be, grid%ep, iv, &
                        grid%xa, grid%vv, grid%vp, grid%xp, ob, y, &
                        ids, ide, jds, jde, kds, kde, &
                        ims, ime, jms, jme, kms, kme, &
                        its, ite, jts, jte, kts, kte )
      end if

      if (testing_wrfvar) then
         call da_check( cv_size, grid%xb, xbx, be, grid%ep, iv, &
                        grid%xa, grid%vv, grid%vp, grid%xp, ob, y, &
                        ids, ide, jds, jde, kds, kde, &
                        ims, ime, jms, jme, kms, kme, &
                        its, ite, jts, jte, kts, kte )
      end if

      ! Write "clean" QCed observations if requested:
      if (anal_type_qcobs) then
         if (it == 1) then
            CALL da_write_filtered_obs(ob, iv, grid%xb, grid%xp, &
                          grid%moad_cen_lat, grid%stand_lon,&
                          grid%truelat1, grid%truelat2 )
         end if     
      endif

      if (monitoring) call wrf_shutdown

      ! [8.3] Interpolate x_g to low resolution grid

      ! [8.4] Minimize cost function:

      call da_allocate_y( iv, re )
      call da_allocate_y( iv, y )

      call da_minimise_cg( grid, config_flags,                  &
                           it, be % cv % size, & 
                           grid%xb, xbx, be, grid%ep, iv, &
                           j_grad_norm_target, xhat, cvt, &
                           grid%xa, grid%vv, grid%vp, grid%xp, re, y, j,    &
                           ids, ide, jds, jde, kds, kde,        &
                           ims, ime, jms, jme, kms, kme,        &
                           its, ite, jts, jte, kts, kte         )

      !------------------------------------------------------------------------

      ! [8.5] Update latest analysis solution:

      call da_transform_vtox( cv_size, grid%xb, xbx, be, grid%ep, xhat, grid%vv, grid%vp, grid%xp, grid%xa,  &
                              ids, ide, jds, jde, kds, kde,             &
                              ims, ime, jms, jme, kms, kme,             &
                              its, ite, jts, jte, kts, kte )

      ! [8.6] Only when use_RadarObs = .false. and W_INCREMENTS =.true.,
      !       the W_increment need to be diagnosed:

      if (W_INCREMENTS .and. .not. use_RadarObs) then
         call da_uvprho_to_w_lin( grid%xb, grid%xa, grid%xp,                 &
                                  ids,ide, jds,jde, kds,kde,  &
                                  ims,ime, jms,jme, kms,kme,  &
                                  its,ite, jts,jte, kts, kte )

         call wrf_dm_halo(grid%xp%domdesc,grid%xp%comms,grid%xp%halo_id13)
      endif

      ! [8.7] Write out diagnostics

      call da_write_diagnostics( ob, iv, re, y, grid%xp, grid%xa, j )

      ! [8.8] Write Ascii radiance OMB and OMA file

      if (write_oa_rad_ascii) then
         write(unit=stdout,fmt=*)  ' writing radiance OMB and OMA ascii file'
         call da_write_oa_rad_ascii(grid%xp,ob,iv,re)
      end if

      !------------------------------------------------------------------------
      ! [8.0] Output WRFVAR analysis and analysis increments:
      !------------------------------------------------------------------------

      call da_transfer_xatoanalysis( it, xbx, grid, config_flags ,&
#include "em_dummy_new_args.inc"
         )
   end do

   !---------------------------------------------------------------------------
   ! [9.0] Tidy up:
   !---------------------------------------------------------------------------

   deallocate (cvt)
   deallocate (xhat)
   call da_deallocate_observations(iv)
   call da_deallocate_y (re)
   call da_deallocate_y (y)
   call da_deallocate_y (ob)
   call da_deallocate_background_errors (be)

   if (xbx%pad_num > 0) then
      deallocate(xbx%pad_loc)
      deallocate(xbx%pad_pos)
   end if

   deallocate (xbx % fft_factors_x)
   deallocate (xbx % fft_factors_y)
   deallocate (xbx % fft_coeffs)
   deallocate (xbx % trig_functs_x)
   deallocate (xbx % trig_functs_y)

   if (global) then
      deallocate (xbx%coslat)
      deallocate (xbx%sinlat)
      deallocate (xbx%coslon)
      deallocate (xbx%sinlon)
      deallocate (xbx%int_wgts)
      deallocate (xbx%alp)
      deallocate (xbx%wsave)
   end if

   deallocate (xbx % latc_mean)

   if (trace_use) call da_trace_exit("da_solve")

contains

#include "da_solve_init.inc"

end subroutine da_solve

