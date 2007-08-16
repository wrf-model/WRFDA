subroutine da_transform_vtoy(cv_size, be, ep, cv, iv, vp, vv, xbx, &
                              y, &
                              grid, config_flags)

   !----------------------------------------------------------------------
   ! Purpose:  Does transform of control variable (V) to Obs-space (Y)
   !----------------------------------------------------------------------

   implicit none

   integer,                    intent(in)    :: cv_size ! Size of cv array.
   type(be_type),              intent(in)    :: be     ! background error structure.
   type(ep_type),              intent(in)    :: ep     ! Ensemble perturbation structure.
   real,                       intent(in)    :: cv(1:cv_size)     ! control variables.
   type(ob_type),              intent(inout) :: iv     ! innovation vector (o-b).
   type(vp_type),              intent(inout) :: vp     ! Grdipt/level CV.
   type(vp_type),              intent(inout) :: vv     ! Grdipt/EOF CV.
   type(xbx_type),             intent(in)    :: xbx    ! For header & non-grid arrays.
   type(y_type),               intent(inout) :: y      ! y = H(x_inc).
   type(domain),               intent(inout) :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   integer :: nobwin
#ifdef DM_PARALLEL
   integer :: wrf_done_unit
#endif

   character(len=4) :: filnam

   if (trace_use) call da_trace_entry("da_transform_vtoy")

   call da_transform_vtox(grid, cv_size, xbx, be, ep, cv, vv, vp)

   if (var4d) then
      call da_transfer_xatowrftl(grid, config_flags, 'tl01')

      if (trace_use) call da_trace("da_transform_vtoy","Starting da_run_wrfplus_tl.ksh")
#ifdef DM_PARALLEL
      if (var4d_coupling == var4d_coupling_disk_simul) then
         call da_system("da_run_wrfplus_tl.ksh pre")

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
         ! Wait until PE 0 agrees that TL finished
         call wrf_get_dm_communicator ( comm )
         call mpi_barrier(comm, ierr)
         call da_system("da_run_wrfplus_tl.ksh post")
      end if
#else
      call da_system("da_run_wrfplus_tl.ksh")
#endif
      if (trace_use) call da_trace("da_transform_vtoy","Finished da_run_wrfplus_tl.ksh")

      do nobwin=1, num_fgat_time
         iv%time = nobwin
         write(filnam,'(a2,i2.2)') 'tl',nobwin
         call da_transfer_wrftltoxa(grid, config_flags, filnam)
         call da_transform_xtoy(grid, iv, y)
      end do
   else  ! not var4d
      call da_transform_xtoy(grid, iv, y)
   end if ! var4d

   if (trace_use) call da_trace_exit("da_transform_vtoy")

end subroutine da_transform_vtoy


