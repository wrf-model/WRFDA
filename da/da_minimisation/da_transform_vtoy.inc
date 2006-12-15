subroutine da_transform_vtoy( cv_size, be, ep, cv, iv, vp, vv, xa, xb, xbx, &
                              xp, y, ids, ide, jds, jde, kds, kde,  &
                              ims, ime, jms, jme, kms, kme,  &
                              its, ite, jts, jte, kts, kte,  &
                              grid, config_flags )

   !----------------------------------------------------------------------
   ! Purpose:  Does transform of control variable (V) to Obs-space (Y)
   !----------------------------------------------------------------------

   implicit none

   integer, intent(in)                :: cv_size ! Size of cv array.
   type (be_type), intent(in)         :: be     ! background error structure.
   type (ep_type), intent(in)         :: ep     ! Ensemble perturbation structure.
   real, intent(in)                   :: cv(1:cv_size)     ! control variables.
   type (ob_type), intent(inout)      :: iv     ! innovation vector (o-b).
   type (vp_type), intent(inout)      :: vp     ! Grdipt/level CV.
   type (vp_type), intent(inout)      :: vv     ! Grdipt/EOF CV.
   type (x_type) , intent(inout)      :: xa     ! gridded analy. incs. (local)
   type (xb_type), intent(in)         :: xb     ! first guess (local).
   type (xbx_type),intent(in)         :: xbx    ! For header & non-grid arrays.
   type (xpose_type), intent(inout)   :: xp     ! Domain decomposition vars.
   type (y_type) , intent(inout)      :: y      ! y = H(x_inc).

   integer, intent(in)           :: ids,ide, jds,jde, kds,kde ! domain dims.
   integer, intent(in)           :: ims,ime, jms,jme, kms,kme ! memory dims.
   integer, intent(in)           :: its,ite, jts,jte, kts,kte ! tile   dims.

   type(domain), intent(inout)           :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   integer :: nobwin
   integer :: wrf_done_unit

   character(len=4) :: filnam

   call da_trace_entry("da_transform_vtoy")

   call da_transform_vtox( cv_size, xb, xbx, be, ep, cv, vv, vp, xp, xa,  &
                           ids, ide, jds, jde, kds, kde,     &
                           ims, ime, jms, jme, kms, kme,     &
                           its, ite, jts, jte, kts, kte )

   if (var4d) then
      call da_transfer_xatowrftl( grid, config_flags, 'tl01', &
#include "em_actual_new_args.inc"
                                 )

      call da_trace("da_transform_vtoy","Starting da_run_wrfplus_tl.ksh")
#ifdef DM_PARALLEL
      if (var4d_coupling == var4d_coupling_disk_simul) then
         call da_system_4dvar("da_run_wrfplus_tl.ksh pre")

         if (rootproc) then
            call da_system("rm -rf tl/wrf_done")
            call da_system("touch tl/wrf_go_ahead")
            call da_get_unit(wrf_done_unit)
            do while ( .true. )
               open(wrf_done_unit,file="tl/wrf_done",status="old",err=303)
               close(wrf_done_unit)
               exit
303            continue
               call da_system("sleep 1")
            end do
            call da_free_unit(wrf_done_unit)
         end if
         call mpi_barrier( comm, ierr )

         call da_system_4dvar("da_run_wrfplus_tl.ksh post ")
      else
         call da_system("da_run_wrfplus_tl.ksh")
      end if
#else
      call da_system("da_run_wrfplus_tl.ksh")
#endif
      call da_trace("da_transform_vtoy","Finished da_run_wrfplus_tl.ksh")

      do nobwin=1, num_fgat_time
         iv%current_ob_time = nobwin
         write(filnam,'(a2,i2.2)') 'tl',nobwin
         call da_transfer_wrftltoxa( grid, config_flags, filnam, &
#include "em_actual_new_args.inc"
                                 )
         call da_transform_xtoy( xb, iv, xa, xp, y )
      end do
   else  ! not var4d
      call da_transform_xtoy( xb, iv, xa, xp, y )
   end if ! var4d

   call da_trace_exit("da_transform_vtoy")

end subroutine da_transform_vtoy

