subroutine da_transform_vtoy_adj( iter,cv_size, be, ep, cv, iv, vp, vv, xa, xb, &
                                  xbx, xp, y, &
                                  ids, ide, jds, jde, kds, kde,     &
                                  ims, ime, jms, jme, kms, kme,     &
                                  its, ite, jts, jte, kts, kte,     &
                                  grid, config_flags )

   !-------------------------------------------------------------------------
   ! Purpose:  Does Adjoint of control variable (V) transform to Obs-space(Y)
   !-------------------------------------------------------------------------

   implicit none

   integer, intent(in)                :: iter
   integer, intent(in)                :: cv_size ! Size of cv array.
   type (be_type), intent(in)         :: be     ! background error structure.
   type (ep_type), intent(in)         :: ep     ! ensemble perturbation structure.
   real, intent(out)                  :: cv(1:cv_size) ! control variables.
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

   type(domain) ,   TARGET                   :: grid
   type(grid_config_rec_type), intent(inout) :: config_flags

   integer :: nobwin,ndynopt
   integer :: ierr, comm, wrf_done_unit

   character(len=4) :: filnam

   call da_trace_entry("da_transform_vtoy_adj")

   if (var4d) then
      do nobwin=num_fgat_time,1,-1
         iv%current_ob_time = nobwin
         call da_zero_x(xa)
         call da_transform_xtoy_adj( xb, iv, xp, y, xa )
         write(UNIT=filnam,FMT='(a2,i2.2)') 'af',nobwin

         call da_transfer_wrftltoxa_adj( grid, config_flags, filnam, &
#include "em_actual_new_args.inc"
                              )
      enddo

      ndynopt      = grid%dyn_opt
      grid%dyn_opt = DYN_EM_TL
      call nl_set_dyn_opt ( 1 , DYN_EM_TL )

      if ( jcdfi_use .AND. iter > 0 ) then
         call da_med_initialdata_input( grid , config_flags, 'tldf' )
      else
         grid%em_g_u_2 = 0.0
         grid%em_g_v_2 = 0.0
         grid%em_g_w_2 = 0.0
         grid%em_g_t_2 = 0.0
         grid%em_g_ph_2 = 0.0
         grid%em_g_mu_2 = 0.0
         grid%g_moist = 0.0
      endif

      call da_med_initialdata_output( grid , config_flags, 'afdf')

      grid%dyn_opt = ndynopt
      call nl_set_dyn_opt ( 1 , DYN_EM )

      call da_trace("da_transform_vtoy_adj","Starting da_run_wrfplus_ad.ksh")

#ifdef DM_PARALLEL
      if (var4d_coupling == var4d_coupling_disk_simul) then
         call da_system_4dvar("da_run_wrfplus_ad.ksh pre ")

         IF (rootproc) THEN
            call da_system("rm -rf ad/wrf_done")
            call da_system("touch ad/wrf_go_ahead")
            call da_get_unit(wrf_done_unit)
            DO WHILE ( .true. )
               OPEN(wrf_done_unit,file="ad/wrf_done",status="old",err=303)
               CLOSE(wrf_done_unit)
               EXIT
   303         CONTINUE
               CALL da_system("sleep 1")
            ENDDO
            call da_free_unit(wrf_done_unit)
         ENDIF
         CALL wrf_get_dm_communicator ( comm )
         CALL mpi_barrier( comm, ierr )

         call da_system_4dvar("da_run_wrfplus_ad.ksh post ")
      else
         call da_system("da_run_wrfplus_ad.ksh")
      end if
#else
      call da_system("da_run_wrfplus_ad.ksh")
#endif
      call da_trace("da_transform_vtoy_adj","Finished da_run_wrfplus_ad.ksh")

      call da_transfer_xatowrftl_adj( grid, config_flags, 'gr01', &
#include "em_actual_new_args.inc"
                              )
   else  ! var4d

      call da_zero_x(xa)
      call da_transform_xtoy_adj( xb, iv, xp, y, xa )

   endif ! var4d

   cv = 0.0
   call da_transform_vtox_adj( cv_size, xb, xbx, be, ep, xa, xp, vp, vv, cv, &
                               ids, ide, jds, jde, kds, kde,       &
                               ims, ime, jms, jme, kms, kme,       &
                               its, ite, jts, jte, kts, kte )

   call da_trace_exit("da_transform_vtoy_adj")

end subroutine da_transform_vtoy_adj

