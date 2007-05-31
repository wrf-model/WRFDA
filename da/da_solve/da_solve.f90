subroutine da_solve ( grid , config_flags)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   use module_configure, only : grid_config_rec_type
   use module_domain, only : domain
   use module_driver_constants, only : max_comms
   use module_symbols_util, only : wrfu_finalize

   use module_radiance, only : satinfo, time_slots
#ifdef RTTOV
   use module_radiance, only : coefs, sensor_descriptor
#endif

   use module_state_description, only : num_moist, num_a_moist, num_g_moist, &
      num_scalar, num_a_scalar, num_g_scalar
   use module_tiles, only : set_tiles

#ifdef DM_PARALLEL
#ifdef RSL_LITE
   use module_dm, only : local_communicator, local_communicator_x, &
      local_communicator_y, ntasks_x, ntasks_y, data_order_xyz, mytask, &
      ntasks, data_order_xy
#else
   use module_dm, only : invalid_message_value, glen,setup_xpose_rsl, &
      add_msg_24pt_real,reset_msgs_24pt,stencil_24pt,setup_halo_rsl, &
      reset_msgs_xpose, add_msg_xpose_real, define_xpose, add_msg_24pt_integer
#endif
#endif

   use da_control, only : trace_use, comm, ierr, ids,ide,jds,jde,kds,kde, &
      ips,ipe, jps,jpe, vert_corr, sin_xle, testing_wrfvar, use_rad, &
      calc_w_increment, var4d_coupling_disk_simul, var4d_coupling, &
      write_oa_rad_ascii, var4d, cos_xls, vertical_ip, use_radarobs, stdout, &
      sin_xls, rf_passes, ntmax, rootproc,test_transforms,global, &
      cos_xle,anal_type_qcobs,check_max_iv,anal_type_randomcv,cv_options_hum, &
      max_ext_its,anal_type_verify, start_x, start_y,coarse_ix, coarse_jy, &
      rtm_option, rtm_option_crtm, rtm_option_rttov,read_biascoef, ims, ime, &
      kps, kpe, jms, jme, kms, kme
   use da_define_structures, only : y_type, j_type, ob_type, be_type, &
      xbx_type,da_deallocate_background_errors,da_initialize_cv, &
      da_zero_vp_type,da_allocate_y,da_deallocate_observations, &
      da_deallocate_y
   use da_minimisation, only : da_get_innov_vector,da_minimise_cg, &
      da_write_diagnostics
   use da_radiance1, only : da_write_oa_rad_ascii
   use da_obs_io, only : da_write_filtered_obs
   use da_par_util, only : da_system,da_copy_tile_dims,da_copy_dims
   use da_physics, only : da_uvprho_to_w_lin
   use da_reporting, only : message, da_warning
   use da_setup_structures, only : da_setup_obs_structures, &
      da_setup_background_errors,da_setup_flow_predictors
   use da_test, only : da_check
   use da_tools1, only : da_get_unit, da_free_unit
   use da_tracing, only : da_trace_entry, da_trace_exit, da_trace
   use da_transfer_model, only : da_transfer_xatoanalysis,da_setup_firstguess
   use da_vtox_transforms, only : da_transform_vtox
   use da_wrf_interfaces, only : wrf_error_fatal3

   implicit none

   type (domain),               intent(inout) :: grid
   type (grid_config_rec_type), intent(inout) :: config_flags

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

   integer                      :: cv_size, i
   real                         :: j_grad_norm_target ! Target j norm.

#ifdef DM_PARALLEL
   integer                      :: wrf_done_unit
#endif

   if (trace_use) call da_trace_entry("da_solve")

#ifdef DM_PARALLEL
   call mpi_barrier(comm,ierr)
#endif

   !---------------------------------------------------------------------------
   ! If it is verification run set check_max_iv as .false.
   !---------------------------------------------------------------------------

   if (anal_type_verify) then
      check_max_iv = .false.
      ntmax=0
   end if

   if (cv_options_hum < 1 .or. cv_options_hum > 3) then
      write(unit=message(1),fmt='(A,I3)') &
         'Invalid cv_options_hum = ', cv_options_hum
      call wrf_error_fatal(message(1:1))
   end if

   if (vert_corr == 2) then
      if (vertical_ip < 0 .or. vertical_ip > 2) then
         write (unit=message(1),fmt='(A,I3)') &
           'Invalid vertical_ip = ', &
           vertical_ip
         call da_warning(__FILE__,__LINE__,message(1:1))
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
      write(unit=stdout,fmt='(a)')' Resetting ntmax = 0 for analysis_type = randomcv' 
   end if

   !---------------------------------------------------------------------------
   ! [2.0] Initialise wrfvar parameters:
   !---------------------------------------------------------------------------

   call da_solve_init(grid &
#include "em_actual_new_args.inc"
)

   !---------------------------------------------------------------------------
   ! [3.0] Set up first guess field (grid%xb):
   !---------------------------------------------------------------------------

   call da_setup_firstguess( xbx, grid)

   !---------------------------------------------------------------------------
   ! [4.0] Set up observations (ob):
   !---------------------------------------------------------------------------

   call da_setup_obs_structures( grid, ob, iv)

   if (use_rad) then
      allocate (j % jo % rad(1:iv%num_inst))
      do i=1,iv%num_inst
         allocate (j % jo % rad(i) % jo_ichan(iv%instid(i)%nchan))
         allocate (j % jo % rad(i) % num_ichan(iv%instid(i)%nchan))
      end do
   end if

   !---------------------------------------------------------------------------
   ! [5.0] Set up background errors (be):
   !---------------------------------------------------------------------------

   call da_setup_background_errors( grid%xb, be)
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

   allocate (cvt(1:cv_size))
   allocate (xhat(1:cv_size))
   call da_initialize_cv (cv_size, cvt)
   call da_initialize_cv (cv_size, xhat)
      
   call da_zero_vp_type (grid%vv)
   call da_zero_vp_type (grid%vp)

   !---------------------------------------------------------------------------
   ! [8] Outerloop
   !---------------------------------------------------------------------------

   j_grad_norm_target = 1.0
   do it = 1, max_ext_its

      ! [8.1] Calculate nonlinear model trajectory 

      if (var4d) then
         call da_trace ("da_solve","Starting da_run_wrf_nl.ksh")
#ifdef DM_PARALLEL
         if (var4d_coupling == var4d_coupling_disk_simul) then
            call da_system ("da_run_wrf_nl.ksh pre")
            ! call da_system("./wrf.exe -rmpool 1")
            if (rootproc) then
               call da_system("rm -rf wrfnl_done")
               call da_system("touch wrfnl_go_ahead")
               call da_get_unit(wrf_done_unit)
               do while ( .true. )
                  open(wrf_done_unit,file="wrfnl_done",status="old",err=303)
                  close(wrf_done_unit)
                  exit
303                  continue
                  call da_system("sync")
                  call da_system("sleep 1")
               end do
               call da_free_unit(wrf_done_unit)
            end if
            ! Wait until PE thinks NL model has finished
            call mpi_barrier( comm, ierr )
            call da_system("da_run_wrf_nl.ksh post")
            call da_system("touch wrfnl_stop_now")
         end if
#else
         call da_system("da_run_wrf_nl.ksh")
#endif
         call da_trace("da_solve","Finished da_run_wrf_nl.ksh")
      end if

      ! [8.2] Calculate innovation vector (O-B):

      call da_get_innov_vector( it, ob, iv, grid , config_flags)

      if (test_transforms) then
         call da_check(grid, cv_size, grid%xb, xbx, be, grid%ep, iv, &
                        grid%xa, grid%vv, grid%vp, grid%xp, y)
         call wrfu_finalize
         call wrf_shutdown
         stop
      end if

      if (testing_wrfvar) then
         call da_check(grid, cv_size, grid%xb, xbx, be, grid%ep, iv, &
                        grid%xa, grid%vv, grid%vp, grid%xp, y)
         call wrfu_finalize
         call wrf_shutdown
         stop
      end if

      ! Write "clean" QCed observations if requested:
      if (anal_type_qcobs) then
        if (it == 1) then
        call da_write_filtered_obs(ob, iv, grid%xb, grid%xp, &
                          grid%moad_cen_lat, grid%stand_lon,&
                          grid%truelat1, grid%truelat2,     &
                          coarse_ix, coarse_jy, start_x, start_y)
         end if     
      end if

      ! [8.3] Interpolate x_g to low resolution grid

      ! [8.4] Minimize cost function:

      call da_allocate_y( iv, re )
      call da_allocate_y( iv, y )

      call da_minimise_cg( grid, config_flags,                  &
                           it, be % cv % size, & 
                           xbx, be, iv, &
                           j_grad_norm_target, xhat, cvt, &
                           re, y, j)

      !------------------------------------------------------------------------

      ! [8.5] Update latest analysis solution:

      call da_transform_vtox(grid, cv_size, grid%xb, xbx, be, grid%ep, xhat, &
         grid%vv, grid%vp, grid%xp, grid%xa)

      ! [8.6] Only when use_RadarObs = .false. and calc_w_increment =.true.,
      !       the w_increment need to be diagnosed:

      if (calc_w_increment .and. .not. use_RadarObs) then
         call da_uvprho_to_w_lin( grid%xb, grid%xa, grid%xp)

#include "HALO_RADAR_XA_W.inc"
      end if

      ! [8.7] Write out diagnostics

      call da_write_diagnostics( ob, iv, re, y, grid%xp, grid%xa, j )

      ! [8.8] Write Ascii radiance OMB and OMA file

      if (write_oa_rad_ascii) then
         write(unit=stdout,fmt='(A)')  'Writing radiance OMB and OMA ascii file'
         write(unit=stdout,fmt=*)  " "
         call da_write_oa_rad_ascii (grid%xp,ob,iv,re)
      end if

      !------------------------------------------------------------------------
      ! [8.0] Output WRFVAR analysis and analysis increments:
      !------------------------------------------------------------------------

      call da_transfer_xatoanalysis( it, xbx, grid, config_flags)
   end do

   !---------------------------------------------------------------------------
   ! [9.0] Tidy up:
   !---------------------------------------------------------------------------

   if (var4d) then
      call da_system("touch wrf_stop_now")
   end if

   deallocate (cvt)
   deallocate (xhat)

   if (use_rad) then
      do i =1, iv%num_inst
         deallocate (j % jo % rad(i) % jo_ichan)
         deallocate (j % jo % rad(i) % num_ichan)
         deallocate (satinfo(i) % ichan)
         deallocate (satinfo(i) % iuse)
         deallocate (satinfo(i) % error)
         deallocate (satinfo(i) % polar)

         if (read_biascoef) then
            deallocate (satinfo(i) % scanbias)
            deallocate (satinfo(i) % scanbias_b)
            deallocate (satinfo(i) % bcoef)
            deallocate (satinfo(i) % bcoef0)
            deallocate (satinfo(i) % error_std)
         end if

         deallocate (ob%instid(i) % ichan)
         deallocate (iv%instid(i)%ichan)

         if (iv%instid(i)%num_rad > 0) then
            deallocate (iv%instid(i)%info)
            deallocate (iv%instid(i)%loc)
            deallocate (iv%instid(i)%loc_i)
            deallocate (iv%instid(i)%loc_j)
            deallocate (iv%instid(i)%loc_k)
            deallocate (iv%instid(i)%loc_dx)
            deallocate (iv%instid(i)%loc_dy)
            deallocate (iv%instid(i)%loc_dz)
            deallocate (iv%instid(i)%loc_dxm)
            deallocate (iv%instid(i)%loc_dym)
            deallocate (iv%instid(i)%loc_dzm)
            deallocate (iv%instid(i)%zk)
            deallocate (iv%instid(i)%t)
            deallocate (iv%instid(i)%mr)
            deallocate (iv%instid(i)%tm)
            deallocate (iv%instid(i)%qm)
            deallocate (iv%instid(i)%qrn)
            deallocate (iv%instid(i)%qcw)
            deallocate (iv%instid(i)%qci)
            deallocate (iv%instid(i)%qsn)
            deallocate (iv%instid(i)%qgr)
            deallocate (iv%instid(i)%pm)
            deallocate (iv%instid(i)%pf)
            deallocate (iv%instid(i)%u10)
            deallocate (iv%instid(i)%v10)
            deallocate (iv%instid(i)%t2m)
            deallocate (iv%instid(i)%q2m)
            deallocate (iv%instid(i)%mr2m)
            deallocate (iv%instid(i)%psfc)
            deallocate (iv%instid(i)%ts)
            deallocate (iv%instid(i)%smois)
            deallocate (iv%instid(i)%tslb)
            deallocate (iv%instid(i)%snowh)
            deallocate (iv%instid(i)%isflg)
            deallocate (iv%instid(i)%soiltyp)
            deallocate (iv%instid(i)%landsea_mask)
            deallocate (iv%instid(i)%elevation)
            deallocate (iv%instid(i)%vegfra)
            deallocate (iv%instid(i)%vegtyp)
            deallocate (iv%instid(i)%clwp)
            deallocate (iv%instid(i)%ps)
            deallocate (iv%instid(i)%tb_xb)
            deallocate (iv%instid(i)%tb_qc)
            deallocate (iv%instid(i)%tb_inv)
            deallocate (iv%instid(i)%tb_error)
            deallocate (iv%instid(i)%emiss)
            deallocate (iv%instid(i)%scanpos)
            deallocate (iv%instid(i)%scanline)
            deallocate (iv%instid(i)%ifgat)
            deallocate (iv%instid(i)%cloud_flag)
            deallocate (iv%instid(i)%satzen)
            deallocate (iv%instid(i)%satazi)
            deallocate (iv%instid(i)%solzen)
            deallocate (iv%instid(i)%solazi)
            deallocate (iv%instid(i)%proc_domain)

            if (rtm_option == rtm_option_crtm) then
               deallocate(iv%instid(i)%water_coverage)
               deallocate(iv%instid(i)%land_coverage)
               deallocate(iv%instid(i)%ice_coverage)
               deallocate(iv%instid(i)%snow_coverage)
               deallocate(iv%instid(i)%ps_jacobian)
               deallocate(iv%instid(i)%t_jacobian)
               deallocate(iv%instid(i)%q_jacobian)
            end if
         end if
#ifdef RTTOV
         if (rtm_option == rtm_option_rttov) then
            call rttov_dealloc_coef (ierr,coefs(i))
         end if
#endif
      end do
      deallocate (iv%instid)
      deallocate (j % jo % rad)
      deallocate (satinfo)
      deallocate (time_slots)
#ifdef RTTOV
      if (rtm_option == rtm_option_rttov) then
         deallocate (coefs)
      end if
#endif
   end if

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
      if (grid%xb%jts == grid%xb%jds) then
         deallocate(cos_xls)
         deallocate(sin_xls)
      end if
                                                                                
      if (grid%xb%jte == grid%xb%jde) then
         deallocate(cos_xle)
         deallocate(sin_xle)
      end if
   end if

   deallocate (xbx % latc_mean)

#ifdef DM_PARALLEL
   call mpi_barrier(comm,ierr)
#endif

   if (trace_use) call da_trace_exit("da_solve")

contains

#include "da_solve_init.inc"

end subroutine da_solve

