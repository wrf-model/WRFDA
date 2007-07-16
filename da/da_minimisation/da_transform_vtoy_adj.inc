subroutine da_transform_vtoy_adj(iter,cv_size, be, ep, cv, iv, vp, vv, &
                                  xbx, y, &
                                  grid, config_flags, cg_jcdf)

   !-------------------------------------------------------------------------
   ! Purpose:  Does Adjoint of control variable (V) transform to Obs-space(Y)
   !-------------------------------------------------------------------------

   implicit none

   integer, intent(in)              :: iter
   integer, intent(in)              :: cv_size ! Size of cv array.
   type (be_type), intent(in)       :: be     ! background error structure.
   type (ep_type), intent(in)       :: ep     ! ensemble perturbation structure.
   real, intent(out)                :: cv(1:cv_size) ! control variables.
   type (ob_type), intent(inout)    :: iv     ! innovation vector (o-b).
   type (vp_type), intent(inout)    :: vp     ! Grdipt/level CV.
   type (vp_type), intent(inout)    :: vv     ! Grdipt/EOF CV.
   type (xbx_type),intent(in)       :: xbx    ! For header & non-grid arrays.
   type (y_type) , intent(inout)    :: y      ! y = H(x_inc).

   type(domain), intent(inout) :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags
   integer, intent(in) :: cg_jcdf
   type (xbx_type)                  :: xbx_tmp

   integer :: nobwin,ndynopt
#ifdef DM_PARALLEL
   integer :: wrf_done_unit
#endif

   character(len=4) :: filnam

   call da_trace_entry("da_transform_vtoy_adj")

   if (var4d) then
      do nobwin=num_fgat_time,1,-1
         write(unit=filnam, fmt='(a, i2.2)') 'fg', nobwin
         call da_med_initialdata_input( grid , config_flags, &
            filnam)
         call da_setup_firstguess( xbx_tmp, grid)

         iv%current_ob_time = nobwin
         call da_zero_x(grid%xa)
         call da_transform_xtoy_adj(grid, iv, y, grid%xa)
         write(unit=filnam,fmt='(a2,i2.2)') 'af',nobwin

         call da_transfer_wrftltoxa_adj(grid, config_flags, filnam)
      end do

      ndynopt      = grid%dyn_opt
      grid%dyn_opt = DYN_EM_TL
      call nl_set_dyn_opt (1 , DYN_EM_TL)

      if (jcdfi_use .AND. iter > 0 .AND. cg_jcdf == 1) then
         call da_med_initialdata_input(grid , config_flags, 'tldf')
      else
         grid%em_g_u_2 = 0.0
         grid%em_g_v_2 = 0.0
         grid%em_g_w_2 = 0.0
         grid%em_g_t_2 = 0.0
         grid%em_g_ph_2 = 0.0
         grid%em_g_mu_2 = 0.0
         grid%g_moist = 0.0
      end if

      call med_hist_out(grid , 3 , config_flags)

      grid%dyn_opt = ndynopt
      call nl_set_dyn_opt (1 , DYN_EM)

      call da_trace("da_transform_vtoy_adj","Starting da_run_wrfplus_ad.ksh")

#ifdef DM_PARALLEL
      if (var4d_coupling == var4d_coupling_disk_simul) then
         call da_system("da_run_wrfplus_ad.ksh pre")

         if (rootproc) then
            call da_system("rm -rf wrf_done")
            call da_system("touch wrf_go_ahead")
            call da_get_unit(wrf_done_unit)
            do while (.true.)
               open(wrf_done_unit,file="wrf_done",status="old",err=303)
               close(wrf_done_unit)
               exit
303            continue
               call da_system("sleep 1")
            end do
            call da_free_unit(wrf_done_unit)
         end if
         ! Wait until PE 0 agrees that AD finished
         CALL wrf_get_dm_communicator ( comm )
         call mpi_barrier(comm, ierr)
         call da_system("da_run_wrfplus_ad.ksh post")
      end if
#else
      call da_system("da_run_wrfplus_ad.ksh")
#endif
      call da_trace("da_transform_vtoy_adj","Finished da_run_wrfplus_ad.ksh")

      call da_transfer_xatowrftl_adj(grid, config_flags, 'gr01')
   else  ! var4d
      call da_zero_x(grid%xa)
      call da_transform_xtoy_adj(grid, iv, y,grid%xa)
   end if ! var4d

   cv = 0.0
   call da_transform_vtox_adj(grid, cv_size, xbx, be, ep, vp, vv, cv)

   call da_trace_exit("da_transform_vtoy_adj")

end subroutine da_transform_vtoy_adj


