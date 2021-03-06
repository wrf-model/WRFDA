  subroutine da_solve ( grid , config_flags)

   !-----------------------------------------------------------------------
   ! Purpose: TBD
   !-----------------------------------------------------------------------

   implicit none

   type (domain),               intent(inout) :: grid
   type (grid_config_rec_type), intent(inout) :: config_flags

   type (xbx_type)              :: xbx         ! For header & non-grid arrays.
   type (be_type)               :: be          ! Background error structure.
   real, allocatable            :: cvt(:)      ! Control variable structure.
   real, allocatable            :: xhat(:)     ! Control variable structure.
   type (y_type)                :: ob          ! Observation structure.
   type (iv_type)               :: iv          ! Obs. increment structure.
   type (y_type)                :: re          ! Residual (o-a) structure.
   type (y_type)                :: y           ! y = H(x_inc) structure.
   integer                      :: it          ! External loop counter.
   type (j_type)                :: j           ! Cost function.

   integer                      :: cv_size, i, ichan
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

   if (cv_options_hum /= cv_options_hum_specific_humidity .and. &
       cv_options_hum /= cv_options_hum_relative_humidity) then
      write(unit=message(1),fmt='(A,I3)') &
         'Invalid cv_options_hum = ', cv_options_hum
      call da_error(__FILE__,__LINE__,message(1:1))
   end if

   if (vert_corr == vert_corr_2) then
      if (vertical_ip < vertical_ip_0 .or. vertical_ip > vertical_ip_delta_p) then
         write (unit=message(1),fmt='(A,I3)') &
           'Invalid vertical_ip = ', vertical_ip
         call da_error(__FILE__,__LINE__,message(1:1))
      end if
   end if

   if (0.5 * real(rf_passes) /= real(rf_passes / 2)) then
      write(unit=stdout,fmt='(A,I4,A)') &
         'rf_passes = ', rf_passes, ' .Should be even.'
      rf_passes = int(real(rf_passes / 2))
      write(unit=stdout,fmt='(A,I4)') 'Resetting rf_passes = ', rf_passes
   end if

   if (anal_type_randomcv) then
      ntmax = 0
      write(unit=stdout,fmt='(a)') &
         ' Resetting ntmax = 0 for analysis_type = randomcv' 
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

   call da_setup_firstguess(xbx, grid, config_flags)

   !---------------------------------------------------------------------------
   ! [4.0] Set up observations (ob):
   !---------------------------------------------------------------------------
   call da_setup_obs_structures (grid, ob, iv)
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

   if( .not. anal_type_verify) then
      call da_setup_background_errors (grid%xb, be)

   !---------------------------------------------------------------------------
   ! [5.1] Set up observation bias correction (Varbc):
   !---------------------------------------------------------------------------

      be % cv % size_jp = 0.0
#if defined(RTTOV) || defined(CRTM)
      if (use_varbc.or.freeze_varbc) call da_varbc_init(iv, be)
#endif
      cv_size = be % cv % size

   !---------------------------------------------------------------------------
   ! [6.0] Set up ensemble perturbation input:
   !---------------------------------------------------------------------------

      grid%ep % ne = be % ne
      if (be % ne > 0) then
         call da_setup_flow_predictors (ide, jde, kde, be % ne, grid%ep )
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

   else
      it = 1
      num_qcstat_conv=0
      call da_get_innov_vector (it, num_qcstat_conv, ob, iv, grid , config_flags)
      call da_allocate_y (iv, re)
      call da_write_obs(it, ob, iv, re)
      call da_write_obs_etkf(ob, iv, re)
      call da_final_write_obs(it, iv)
      call da_deallocate_y(re)
      call da_deallocate_observations (iv)
      return

   endif
   !---------------------------------------------------------------------------
   ! [8] Outerloop
   !---------------------------------------------------------------------------

   j_grad_norm_target = 1.0
   do it = 1, max_ext_its

      ! [8.1] Calculate nonlinear model trajectory 

      if (var4d .and. multi_inc /= 2 ) then
         call da_trace ("da_solve","Starting da_run_wrf_nl.ksh")
#ifdef DM_PARALLEL
         if (var4d_coupling == var4d_coupling_disk_simul) then
            if (rootproc) then
               call da_system ("da_run_wrf_nl.ksh pre")
               call da_system ("rm -rf wrfnl_done")
               call da_system ("touch wrfnl_go_ahead")
               call da_get_unit (wrf_done_unit)
               do while ( .true. )
                  open(wrf_done_unit,file="wrfnl_done",status="old",err=303)
                  close(wrf_done_unit)
                  exit
303                  continue
                  call da_system ("sync")
                  call da_system ("sleep 1")
               end do
               call da_free_unit (wrf_done_unit)
               call da_system ("da_run_wrf_nl.ksh post")
               call da_system ("touch wrfnl_stop_now")
            end if
            ! Wait until PE thinks NL model has finished
            call mpi_barrier (comm, ierr)
         end if
#else
         call da_system ("da_run_wrf_nl.ksh")
#endif
         call da_trace ("da_solve","Finished da_run_wrf_nl.ksh")
      elseif (var4d .and. multi_inc == 2 ) then
#ifdef DM_PARALLEL
         call da_system ("touch wrfnl_stop_now")
#endif
      end if

      ! [8.2] Calculate innovation vector (O-B):

      num_qcstat_conv=0
      call da_get_innov_vector (it, num_qcstat_conv, ob, iv, grid , config_flags)

      if (test_transforms) then
         call da_check (grid, config_flags, cv_size, xbx, be, grid%ep, iv, &
            grid%vv, grid%vp, y)
         if (var4d) then
            call da_system ("touch wrf_stop_now")
         end if
         ! No point continuing, as data corrupted
         call wrf_shutdown
         stop
      end if

      ! [8.4] Minimize cost function:

      call da_allocate_y (iv, re)
      call da_allocate_y (iv, y)

      call da_minimise_cg( grid, config_flags, it, be % cv % size, & 
         xbx, be, iv, j_grad_norm_target, xhat, cvt, re, y, j)
      !------------------------------------------------------------------------

      ! reset cv to random noise
      if (anal_type_randomcv) then
         call da_set_randomcv (cv_size, xhat)
      end if

      ! [8.5] Update latest analysis solution:

      call da_transform_vtox (grid,cv_size,xbx,be,grid%ep,xhat,grid%vv,grid%vp)
      call da_transform_xtoxa (grid)

      ! [8.6] Only when use_radarobs = .false. and calc_w_increment =.true.,
      !       the w_increment need to be diagnosed:

      if (calc_w_increment .and. .not. use_radarobs) then
         call da_uvprho_to_w_lin (grid)

#ifdef DM_PARALLEL
#include "HALO_RADAR_XA_W.inc"
#endif
      end if

      ! [8.7] Write out diagnostics

      call da_write_diagnostics (it, grid, num_qcstat_conv, ob, iv, re, y, j)

      ! Write "clean" QCed observations if requested:
      if (anal_type_qcobs) then
         if (it == 1) then
            call da_write_filtered_obs (grid, ob, iv, &
               coarse_ix, coarse_jy, start_x, start_y)
          end if     
      end if

      ! [8.3] Interpolate x_g to low resolution grid

      ! [8.8] Write Ascii radiance OMB and OMA file

#if defined(CRTM) || defined(RTTOV)
      if (write_oa_rad_ascii) then
         write(unit=stdout,fmt='(A)')  'Writing radiance OMB and OMA ascii file'
         write(unit=stdout,fmt=*)  " "
         call da_write_oa_rad_ascii (ob,iv,re)
      end if
#endif

      ! [8.9] Update VarBC parameters and write output file
#if defined(CRTM) || defined(RTTOV)      
      if ( (use_varbc).or.(freeze_varbc) ) call da_varbc_update(cv_size, xhat, iv)
#endif
      !------------------------------------------------------------------------
      ! [8.10] Output WRFVAR analysis and analysis increments:
      !------------------------------------------------------------------------

      call da_transfer_xatoanalysis (it, xbx, grid, config_flags)
   end do

   !---------------------------------------------------------------------------
   ! [9.0] Tidy up:
   !---------------------------------------------------------------------------

   if (var4d) then
      call da_system ("touch wrf_stop_now")
   end if

   deallocate (cvt)
   deallocate (xhat)

#if defined(RTTOV) || defined(CRTM)
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
 
            deallocate (iv%instid(i)%info%date_char)
            deallocate (iv%instid(i)%info%name)
            deallocate (iv%instid(i)%info%platform)
            deallocate (iv%instid(i)%info%id)
            deallocate (iv%instid(i)%info%levels)     
            deallocate (iv%instid(i)%info%lat)      
            deallocate (iv%instid(i)%info%lon)      
            deallocate (iv%instid(i)%info%elv)   

            deallocate (iv%instid(i)%info%pstar)
            deallocate (iv%instid(i)%info%i)
            deallocate (iv%instid(i)%info%j)
            deallocate (iv%instid(i)%info%k)
            deallocate (iv%instid(i)%info%zk)
            deallocate (iv%instid(i)%info%dx)
            deallocate (iv%instid(i)%info%dy)
            deallocate (iv%instid(i)%info%dz)
            deallocate (iv%instid(i)%info%dxm)
            deallocate (iv%instid(i)%info%dym)
            deallocate (iv%instid(i)%info%dzm)
            deallocate (iv%instid(i)%info%proc_domain)

            deallocate (iv%instid(i)%t)
            deallocate (iv%instid(i)%mr)
            deallocate (iv%instid(i)%tm)
            deallocate (iv%instid(i)%qm)
            deallocate (iv%instid(i)%qrn)
            deallocate (iv%instid(i)%qcw)
            if ( crtm_cloud) then
               deallocate (iv%instid(i)%qci)
               deallocate (iv%instid(i)%qsn)
               deallocate (iv%instid(i)%qgr)
               deallocate (iv%instid(i)%qhl)
               deallocate (iv%instid(i)%rcw)
               deallocate (iv%instid(i)%rci)
               deallocate (iv%instid(i)%rrn)
               deallocate (iv%instid(i)%rsn)
               deallocate (iv%instid(i)%rgr)
               deallocate (iv%instid(i)%rhl)
            end if
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
            deallocate (iv%instid(i)%rad_xb)
            deallocate (iv%instid(i)%rad_obs)
            deallocate (iv%instid(i)%rad_ovc)
            deallocate (iv%instid(i)%emiss)
            deallocate (iv%instid(i)%scanpos)
            deallocate (iv%instid(i)%scanline)
            deallocate (iv%instid(i)%ifgat)
            deallocate (iv%instid(i)%cloud_flag)
            deallocate (iv%instid(i)%rain_flag)
            deallocate (iv%instid(i)%satzen)
            deallocate (iv%instid(i)%satazi)
            deallocate (iv%instid(i)%solzen)
            deallocate (iv%instid(i)%solazi)

	    if ( (use_varbc).or.(freeze_varbc) ) then
               deallocate (iv%instid(i)%varbc_info%pred)
               deallocate (iv%instid(i)%varbc_info%pred_mean)
               deallocate (iv%instid(i)%varbc_info%pred_std)
               deallocate (iv%instid(i)%varbc_info%nbgerr)
	       do ichan = 1, iv%instid(i)%nchan
	         if (iv%instid(i)%varbc(ichan)%npred <= 0) cycle	 
		 deallocate (iv%instid(i)%varbc(ichan)%pred_use)
		 deallocate (iv%instid(i)%varbc(ichan)%ipred)
		 deallocate (iv%instid(i)%varbc(ichan)%index)
		 deallocate (iv%instid(i)%varbc(ichan)%param)
    	         deallocate (iv%instid(i)%varbc(ichan)%bgerr)
		 deallocate (iv%instid(i)%varbc(ichan)%vtox)
	       end do
   	       deallocate (iv%instid(i)%varbc)
	    end if

            if (rtm_option == rtm_option_crtm) then
               deallocate(iv%instid(i)%water_coverage)
               deallocate(iv%instid(i)%land_coverage)
               deallocate(iv%instid(i)%ice_coverage)
               deallocate(iv%instid(i)%snow_coverage)
               if (use_crtm_kmatrix) then
                  deallocate(iv%instid(i)%ts_jacobian)
                  deallocate(iv%instid(i)%t_jacobian)
                  deallocate(iv%instid(i)%q_jacobian)
                  deallocate(iv%instid(i)%windspeed_jacobian)
                  deallocate(iv%instid(i)%emiss_jacobian)
                  deallocate(iv%instid(i)%gamma_jacobian)
                 if ( crtm_cloud ) then
                     deallocate(iv%instid(i)%water_jacobian)
                     deallocate(iv%instid(i)%ice_jacobian)
                     deallocate(iv%instid(i)%rain_jacobian)
                     deallocate(iv%instid(i)%snow_jacobian)
                     deallocate(iv%instid(i)%graupel_jacobian)
                     deallocate(iv%instid(i)%hail_jacobian)
                     deallocate(iv%instid(i)%water_r_jacobian)
                     deallocate(iv%instid(i)%ice_r_jacobian)
                     deallocate(iv%instid(i)%rain_r_jacobian)
                     deallocate(iv%instid(i)%snow_r_jacobian)
                     deallocate(iv%instid(i)%graupel_r_jacobian)
                     deallocate(iv%instid(i)%hail_r_jacobian)
                 end if
              end if
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
#endif

   call da_deallocate_observations (iv)
   call da_deallocate_y (re)
   call da_deallocate_y (y)
   call da_deallocate_y (ob)
   call da_deallocate_background_errors (be)

   if (xbx%pad_num > 0) then
      deallocate (xbx%pad_loc)
      deallocate (xbx%pad_pos)
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
      if (jts == jds) then
         deallocate (cos_xls)
         deallocate (sin_xls)
      end if
                                                                                
      if (jte == jde) then
         deallocate (cos_xle)
         deallocate (sin_xle)
      end if
   end if

   deallocate (xbx % latc_mean)

#ifdef DM_PARALLEL
   call mpi_barrier (comm,ierr)
#endif

   if (trace_use) call da_trace_exit ("da_solve")

contains

#include "da_solve_init.inc"

end subroutine da_solve

